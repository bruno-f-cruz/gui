using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;
using Hexa.NET.ImGui;


// This is a C# port of ocornut's imgui_memory_editor.h
// Original: https://github.com/ocornut/imgui_club
// Licensed under The MIT License (MIT)

namespace Bonsai.Gui.ImGui.HexViewer
{
    /// <summary>
    /// Read-only hexadecimal memory viewer widget for Dear ImGui (Hexa.NET.ImGui).
    /// Supports displaying a single byte[] or a history of byte[] snapshots with visual separators.
    /// </summary>
    public class HexViewer
    {
        public HexViewer()
        {
            columnCount = 16;
            ptShowOptions = true;
            optShowDataPreview = false;
            optShowHexII = false;
            optShowAscii = true;
            optGreyOutZeroes = true;
            optUpperCaseHex = true;
            optMidColsCount = 8;
            optAddrDigitsCount = 0;
            highlightColor = 0x32FFFFFF;
            history = 0;
            isDarkThemeEnabled = false;
        }
        private enum DataTypeEnum
        {
            S8, U8, S16, U16, S32, U32, S64, U64, Float, Double, _COUNT
        }

        private enum DataFormatEnum
        {
            Bin = 0, Dec = 1, Hex = 2, _COUNT
        }

        public uint HistorySize => history;
        public bool IsDarkThemeEnabled => isDarkThemeEnabled;

        // --- Settings ---
        private int columnCount;
        private bool ptShowOptions;
        private bool optShowDataPreview;
        private bool optShowHexII;
        private bool optShowAscii;
        private bool optGreyOutZeroes;
        private bool optUpperCaseHex;
        private int optMidColsCount;
        private int optAddrDigitsCount;
        private uint highlightColor;
        private uint history;
        private bool isDarkThemeEnabled;

        /// This will remain unused (null) for now, but probably worth exploring in the future.
        public Func<byte[], long, bool>? HighlightFn = null;

        // --- Internal State ---
        private long _dataPreviewAddr = -1;   // global flat address for preview
        private long _selectedAddr = -1;       // global flat address for selection
        private byte[] _addrInputBuf = new byte[32];
        private long _gotoAddr = -1;
        private long _highlightMin = -1;
        private long _highlightMax = -1;
        private int _previewEndianness = 0;
        private DataTypeEnum _previewDataType = DataTypeEnum.S32;

        // Cached layout for history mode
        private readonly List<EntryLayout> _entryLayouts = new List<EntryLayout>();
        private int _totalVirtualLines = 0;

        private struct EntryLayout
        {
            public int EntryIndex;         // index into the entries list
            public long EntrySize;         // byte count for this entry
            public int DataLineCount;      // number of hex data lines
            public int VirtualLineStart;   // first virtual line index for this entry
            public long GlobalByteOffset;  // byte offset in the flat concatenated view
                                           // Virtual lines: [VirtualLineStart .. VirtualLineStart + DataLineCount - 1] = data lines
                                           // VirtualLineStart + DataLineCount = separator line (except for last entry)
            public int TotalVirtualLines => DataLineCount + 1; // +1 for separator (always allocated, last just not drawn)
        }

        private struct Sizes
        {
            public int AddrDigitsCount;
            public float LineHeight;
            public float GlyphWidth;
            public float HexCellWidth;
            public float SpacingBetweenMidCols;
            public float PosHexStart;
            public float PosHexEnd;
            public float PosAsciiStart;
            public float PosAsciiEnd;
        }

        /// <summary>
        /// Programmatically scroll to a global address and highlight a range.
        /// </summary>
        public void GotoAddrAndHighlight(long addrMin, long addrMax)
        {
            _gotoAddr = addrMin;
            _highlightMin = addrMin;
            _highlightMax = addrMax;
        }

        // --- CalcSizes uses the total byte count across all visible entries ---
        private void CalcSizes(ref Sizes s, long totalBytes, long baseDisplayAddr)
        {
            s.AddrDigitsCount = optAddrDigitsCount;
            if (s.AddrDigitsCount == 0)
                for (long n = baseDisplayAddr + totalBytes - 1; n > 0; n >>= 4)
                    s.AddrDigitsCount++;
            if (s.AddrDigitsCount < 2) s.AddrDigitsCount = 2;
            s.LineHeight = Hexa.NET.ImGui.ImGui.GetTextLineHeight();
            s.GlyphWidth = Hexa.NET.ImGui.ImGui.CalcTextSize("F").X + 1;
            s.HexCellWidth = (int)(s.GlyphWidth * 2.5f);
            s.SpacingBetweenMidCols = (int)(s.HexCellWidth * 0.25f);
            s.PosHexStart = (s.AddrDigitsCount + 2) * s.GlyphWidth;
            s.PosHexEnd = s.PosHexStart + (s.HexCellWidth * columnCount);
            s.PosAsciiStart = s.PosAsciiEnd = s.PosHexEnd;
            if (optShowAscii)
            {
                s.PosAsciiStart = s.PosHexEnd + s.GlyphWidth * 1;
                if (optMidColsCount > 0)
                    s.PosAsciiStart += (float)((columnCount + optMidColsCount - 1) / optMidColsCount) * s.SpacingBetweenMidCols;
                s.PosAsciiEnd = s.PosAsciiStart + columnCount * s.GlyphWidth;
            }
        }

        private void BuildEntryLayouts(IReadOnlyList<byte[]> entries, int count)
        {
            _entryLayouts.Clear();
            int virtualLine = 0;
            long globalOffset = 0;
            for (int i = 0; i < count; i++)
            {
                long size = entries[i].LongLength;
                int dataLines = (int)((size + columnCount - 1) / columnCount);
                if (dataLines == 0) dataLines = 1; // at least 1 line even for empty

                var layout = new EntryLayout
                {
                    EntryIndex = i,
                    EntrySize = size,
                    DataLineCount = dataLines,
                    VirtualLineStart = virtualLine,
                    GlobalByteOffset = globalOffset
                };
                _entryLayouts.Add(layout);
                virtualLine += layout.TotalVirtualLines;
                globalOffset += size;
            }
            _totalVirtualLines = virtualLine;
        }

        /// <summary>
        /// Resolve a virtual line index to which entry it belongs and whether it's a separator.
        /// </summary>
        private bool ResolveVirtualLine(int virtualLine, out int entryIdx, out int localDataLine, out bool isSeparator)
        {
            entryIdx = 0;
            localDataLine = 0;
            isSeparator = false;

            // Binary search for the entry
            int lo = 0, hi = _entryLayouts.Count - 1;
            while (lo <= hi)
            {
                int mid = (lo + hi) / 2;
                var e = _entryLayouts[mid];
                if (virtualLine < e.VirtualLineStart)
                    hi = mid - 1;
                else if (virtualLine >= e.VirtualLineStart + e.TotalVirtualLines)
                    lo = mid + 1;
                else
                {
                    entryIdx = mid;
                    int offset = virtualLine - e.VirtualLineStart;
                    if (offset < e.DataLineCount)
                    {
                        localDataLine = offset;
                        isSeparator = false;
                    }
                    else
                    {
                        isSeparator = true;
                    }
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Resolve a global flat byte address to the entry index and local offset within that entry.
        /// </summary>
        private bool ResolveGlobalAddr(long globalAddr, out int entryIdx, out long localAddr)
        {
            entryIdx = 0;
            localAddr = 0;
            for (int i = 0; i < _entryLayouts.Count; i++)
            {
                var e = _entryLayouts[i];
                if (globalAddr < e.GlobalByteOffset + e.EntrySize)
                {
                    entryIdx = i;
                    localAddr = globalAddr - e.GlobalByteOffset;
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Get the virtual line for a given global byte address (for scrolling).
        /// </summary>
        private int GlobalAddrToVirtualLine(long globalAddr)
        {
            if (ResolveGlobalAddr(globalAddr, out int entryIdx, out long localAddr))
            {
                return _entryLayouts[entryIdx].VirtualLineStart + (int)(localAddr / columnCount);
            }
            return 0;
        }

        // -----------------------------------------------------------------------
        //  DrawContents overloads
        // -----------------------------------------------------------------------

        /// <summary>Draw viewer contents for a single byte array.</summary>
        public void DrawContents(byte[] memData, long baseDisplayAddr = 0)
        {
            DrawContents(new[] { memData }, baseDisplayAddr);
        }

        /// <summary>Draw viewer contents for a list of byte arrays (history).</summary>
        public void DrawContents(IReadOnlyCollection<byte[]> entries, long baseDisplayAddr = 0)
        {
            if (entries.Count == 0) return;

            // History = 0 means show 1 entry (most recent only)
            // History = N means show N+1 entries (most recent + N previous)
            int count = (int)Math.Min(history + 1, entries.Count);
            var visibleEntries = new List<byte[]>(count);

            int skip = entries.Count - count;
            int i = 0;

            foreach (var entry in entries)
            {
                if (i++ >= skip)
                    visibleEntries.Add(entry);
            }

            if (visibleEntries.Count == 0) return;

            // Reverse order so most recent entry appears at top
            visibleEntries.Reverse();

            BuildEntryLayouts(visibleEntries, visibleEntries.Count);

            long totalBytes = 0;
            foreach (var entry in visibleEntries)
                totalBytes += entry.LongLength;

            if (totalBytes == 0) return;

            Sizes s = default;
            CalcSizes(ref s, Math.Max(totalBytes, 1), baseDisplayAddr);
            var style = Hexa.NET.ImGui.ImGui.GetStyle();

            // Footer
            float heightSeparator = style.ItemSpacing.Y;
            float footerHeight = 0;
            if (ptShowOptions)
                footerHeight += heightSeparator + Hexa.NET.ImGui.ImGui.GetFrameHeightWithSpacing() * 2; // Account for the 2 rows in the footer
            if (optShowDataPreview)
                footerHeight += heightSeparator + Hexa.NET.ImGui.ImGui.GetFrameHeightWithSpacing() + Hexa.NET.ImGui.ImGui.GetTextLineHeightWithSpacing() * 3;

            Hexa.NET.ImGui.ImGui.BeginChild("##scrolling", new Vector2(0, -footerHeight), ImGuiChildFlags.None,
                ImGuiWindowFlags.NoMove | ImGuiWindowFlags.NoNav);
            var drawList = Hexa.NET.ImGui.ImGui.GetWindowDrawList();

            Hexa.NET.ImGui.ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, new Vector2(0, 0));
            Hexa.NET.ImGui.ImGui.PushStyleVar(ImGuiStyleVar.ItemSpacing, new Vector2(0, 0));

            ImGuiListClipper clipper = new ImGuiListClipper();
            clipper.Begin(_totalVirtualLines, s.LineHeight);

            // Handle goto address (must be inside child window scope to set scroll)
            if (_gotoAddr != -1 && _gotoAddr >= 0 && _gotoAddr < totalBytes)
            {
                int targetLine = GlobalAddrToVirtualLine(_gotoAddr);
                Hexa.NET.ImGui.ImGui.SetScrollFromPosY(
                    Hexa.NET.ImGui.ImGui.GetCursorStartPos().Y + targetLine * s.LineHeight);
                _selectedAddr = _dataPreviewAddr = _gotoAddr;
                _gotoAddr = -1;
            }

            // Clamp selection
            if (_selectedAddr >= totalBytes)
                _selectedAddr = -1;
            if (_dataPreviewAddr >= totalBytes)
                _dataPreviewAddr = -1;

            long previewDataTypeSize = optShowDataPreview ? DataTypeGetSize(_previewDataType) : 0;

            // Vertical separator
            Vector2 windowPos = Hexa.NET.ImGui.ImGui.GetWindowPos();
            if (optShowAscii)
            {
                drawList.AddLine(
                    new Vector2(windowPos.X + s.PosAsciiStart - s.GlyphWidth, windowPos.Y),
                    new Vector2(windowPos.X + s.PosAsciiStart - s.GlyphWidth, windowPos.Y + 9999),
                    Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.Border));
            }

            uint colorText = Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.Text);
            uint colorDisabled = optGreyOutZeroes ? Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.TextDisabled) : colorText;
            uint sepColor = Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.Separator);

            while (clipper.Step())
            {
                for (int virtualLine = clipper.DisplayStart; virtualLine < clipper.DisplayEnd; virtualLine++)
                {
                    if (!ResolveVirtualLine(virtualLine, out int entryIdx, out int localDataLine, out bool isSeparator))
                        continue;

                    if (isSeparator)
                    {
                        // Draw separator between entries (skip for the very last entry)
                        if (entryIdx < visibleEntries.Count - 1)
                        {
                            DrawSeparatorLine(drawList, s, sepColor, entryIdx, visibleEntries.Count);
                        }
                        continue;
                    }

                    var entry = visibleEntries[entryIdx];
                    var layout = _entryLayouts[entryIdx];
                    long entrySize = layout.EntrySize;
                    long globalByteOffset = layout.GlobalByteOffset;
                    long localAddr = (long)localDataLine * columnCount;

                    // Address column - shows address relative to this entry
                    long displayAddr = baseDisplayAddr + localAddr;
                    string addrFmt = optUpperCaseHex ? $"X{s.AddrDigitsCount}" : $"x{s.AddrDigitsCount}";
                    Hexa.NET.ImGui.ImGui.TextUnformatted($"{displayAddr.ToString(addrFmt)}: ");

                    // Hex column
                    long addr = localAddr;
                    for (int n = 0; n < columnCount && addr < entrySize; n++, addr++)
                    {
                        float bytePosX = s.PosHexStart + s.HexCellWidth * n;
                        if (optMidColsCount > 0)
                            bytePosX += (float)(n / optMidColsCount) * s.SpacingBetweenMidCols;
                        Hexa.NET.ImGui.ImGui.SameLine(bytePosX);

                        long globalAddr = globalByteOffset + addr;

                        // Highlights
                        bool hlRange = (globalAddr >= _highlightMin && globalAddr < _highlightMax);
                        bool hlFunc = (HighlightFn != null && HighlightFn(entry, addr));
                        bool hlPreview = (globalAddr >= _dataPreviewAddr && globalAddr < _dataPreviewAddr + previewDataTypeSize);
                        bool hlSelected = (globalAddr == _selectedAddr);
                        if (hlRange || hlFunc || hlPreview || hlSelected)
                        {
                            Vector2 pos = Hexa.NET.ImGui.ImGui.GetCursorScreenPos();
                            float hlWidth = s.GlyphWidth * 2;
                            bool nextHl = (addr + 1 < entrySize)
                                && ((_highlightMax != -1 && globalAddr + 1 < _highlightMax)
                                || (HighlightFn != null && HighlightFn(entry, addr + 1)));
                            if (nextHl || (n + 1 == columnCount))
                            {
                                hlWidth = s.HexCellWidth;
                                if (optMidColsCount > 0 && n > 0 && (n + 1) < columnCount && ((n + 1) % optMidColsCount) == 0)
                                    hlWidth += s.SpacingBetweenMidCols;
                            }
                            uint hlColor = hlSelected ? Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.TextSelectedBg) : highlightColor;
                            drawList.AddRectFilled(pos, new Vector2(pos.X + hlWidth, pos.Y + s.LineHeight), hlColor);
                        }

                        // Byte display
                        byte b = entry[addr];
                        if (optShowHexII)
                        {
                            if (b >= 32 && b < 128)
                                Hexa.NET.ImGui.ImGui.TextUnformatted($".{(char)b} ");
                            else if (b == 0xFF && optGreyOutZeroes)
                                Hexa.NET.ImGui.ImGui.TextDisabled("## ");
                            else if (b == 0x00)
                                Hexa.NET.ImGui.ImGui.TextUnformatted("   ");
                            else
                                Hexa.NET.ImGui.ImGui.TextUnformatted(optUpperCaseHex ? $"{b:X2} " : $"{b:x2} ");
                        }
                        else
                        {
                            if (b == 0 && optGreyOutZeroes)
                                Hexa.NET.ImGui.ImGui.TextDisabled("00 ");
                            else
                                Hexa.NET.ImGui.ImGui.TextUnformatted(optUpperCaseHex ? $"{b:X2} " : $"{b:x2} ");
                        }

                        // Click to select
                        if (Hexa.NET.ImGui.ImGui.IsItemHovered(ImGuiHoveredFlags.None) && Hexa.NET.ImGui.ImGui.IsMouseClicked(ImGuiMouseButton.Left))
                            _selectedAddr = _dataPreviewAddr = globalAddr;
                    }

                    // ASCII column
                    if (optShowAscii)
                    {
                        Hexa.NET.ImGui.ImGui.SameLine(s.PosAsciiStart);
                        Vector2 asciiPos = Hexa.NET.ImGui.ImGui.GetCursorScreenPos();
                        addr = (long)localDataLine * columnCount;

                        Hexa.NET.ImGui.ImGui.PushID(virtualLine);
                        Vector2 asciiSize = new Vector2(s.PosAsciiEnd - s.PosAsciiStart, s.LineHeight);
                        if (Hexa.NET.ImGui.ImGui.InvisibleButton("ascii", asciiSize))
                        {
                            long clickedLocal = addr + (long)((Hexa.NET.ImGui.ImGui.GetIO().MousePos.X - asciiPos.X) / s.GlyphWidth);
                            if (clickedLocal >= entrySize) clickedLocal = entrySize - 1;
                            if (clickedLocal < 0) clickedLocal = 0;
                            _selectedAddr = _dataPreviewAddr = globalByteOffset + clickedLocal;
                        }
                        Hexa.NET.ImGui.ImGui.PopID();

                        for (int n = 0; n < columnCount && addr < entrySize; n++, addr++)
                        {
                            long globalAddr = globalByteOffset + addr;
                            if (globalAddr == _selectedAddr)
                            {
                                drawList.AddRectFilled(asciiPos, new Vector2(asciiPos.X + s.GlyphWidth, asciiPos.Y + s.LineHeight),
                                    Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.TextSelectedBg));
                            }
                            byte c = entry[addr];
                            char displayC = (c < 32 || c >= 128) ? '.' : (char)c;
                            drawList.AddText(asciiPos, (displayC == '.') ? colorDisabled : colorText, displayC.ToString());
                            asciiPos.X += s.GlyphWidth;
                        }
                    }
                }
            }

            clipper.End();
            Hexa.NET.ImGui.ImGui.PopStyleVar(2);
            Hexa.NET.ImGui.ImGui.EndChild();

            // --- Options footer ---
            bool nextShowDataPreview = optShowDataPreview;
            if (ptShowOptions)
            {
                Hexa.NET.ImGui.ImGui.Separator();

                // First row: Range/entries info, Go to, History, Theme
                string addrFmt = optUpperCaseHex ? $"X{s.AddrDigitsCount}" : $"x{s.AddrDigitsCount}";
                if (visibleEntries.Count <= 1)
                {
                    Hexa.NET.ImGui.ImGui.TextUnformatted($"Range {baseDisplayAddr.ToString(addrFmt)}..{(baseDisplayAddr + totalBytes - 1).ToString(addrFmt)}");
                }
                else
                {
                    Hexa.NET.ImGui.ImGui.TextUnformatted($"{visibleEntries.Count} entries | {totalBytes} bytes total");
                }

                // Goto address
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.TextUnformatted("Go to:");
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.PushItemWidth((s.AddrDigitsCount + 2) * s.GlyphWidth + style.FramePadding.X * 2.0f);
                unsafe
                {
                    fixed (byte* bufPtr = _addrInputBuf)
                    {
                        if (Hexa.NET.ImGui.ImGui.InputText("##goto", bufPtr, (nuint)_addrInputBuf.Length,
                            ImGuiInputTextFlags.CharsHexadecimal | ImGuiInputTextFlags.EnterReturnsTrue))
                        {
                            string addrStr = ReadStringFromBuf(_addrInputBuf);
                            if (long.TryParse(addrStr, System.Globalization.NumberStyles.HexNumber, null, out long parsed))
                            {
                                _gotoAddr = parsed - baseDisplayAddr;
                                _highlightMin = _highlightMax = -1;
                            }
                        }
                    }
                }
                Hexa.NET.ImGui.ImGui.PopItemWidth();

                // History control
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.TextUnformatted("History:");
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.PushItemWidth(50);
                int historyVal = (int)history;
                if (Hexa.NET.ImGui.ImGui.InputInt("##history", ref historyVal, 0, 0))
                {
                    if (historyVal < 0) historyVal = 0;
                    history = (uint)historyVal;
                }
                Hexa.NET.ImGui.ImGui.PopItemWidth();

                // Theme toggle
                Hexa.NET.ImGui.ImGui.SameLine();
                if (Hexa.NET.ImGui.ImGui.Button(isDarkThemeEnabled ? "Light" : "Dark"))
                {
                    isDarkThemeEnabled = !isDarkThemeEnabled;
                }

                // Second row: Options flattened
                Hexa.NET.ImGui.ImGui.PushItemWidth(56);
                Hexa.NET.ImGui.ImGui.DragInt("Cols", ref columnCount, 0.2f, 4, 32, "%d");
                Hexa.NET.ImGui.ImGui.PopItemWidth();
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.Checkbox("Preview", ref nextShowDataPreview);
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.Checkbox("HexII", ref optShowHexII);
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.Checkbox("Ascii", ref optShowAscii);
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.Checkbox("Grey 0s", ref optGreyOutZeroes);
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.Checkbox("Uppercase", ref optUpperCaseHex);
            }

            // --- Data Preview footer ---
            if (optShowDataPreview)
            {
                Hexa.NET.ImGui.ImGui.Separator();
                Hexa.NET.ImGui.ImGui.AlignTextToFramePadding();
                Hexa.NET.ImGui.ImGui.TextUnformatted("Preview as:");
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.PushItemWidth((s.GlyphWidth * 10.0f) + style.FramePadding.X * 2.0f + style.ItemInnerSpacing.X);
                string[] typeDescs = { "Int8", "Uint8", "Int16", "Uint16", "Int32", "Uint32", "Int64", "Uint64", "Float", "Double" };
                int idx = (int)_previewDataType;
                if (Hexa.NET.ImGui.ImGui.BeginCombo("##combo_type", typeDescs[idx], ImGuiComboFlags.HeightLargest))
                {
                    for (int n = 0; n < (int)DataTypeEnum._COUNT; n++)
                        if (Hexa.NET.ImGui.ImGui.Selectable(typeDescs[n], idx == n))
                            _previewDataType = (DataTypeEnum)n;
                    Hexa.NET.ImGui.ImGui.EndCombo();
                }
                Hexa.NET.ImGui.ImGui.PopItemWidth();
                Hexa.NET.ImGui.ImGui.SameLine();
                Hexa.NET.ImGui.ImGui.PushItemWidth((s.GlyphWidth * 6.0f) + style.FramePadding.X * 2.0f + style.ItemInnerSpacing.X);
                string[] endianDescs = { "LE", "BE" };
                Hexa.NET.ImGui.ImGui.Combo("##combo_endianness", ref _previewEndianness, endianDescs, endianDescs.Length);
                Hexa.NET.ImGui.ImGui.PopItemWidth();

                float x = s.GlyphWidth * 6.0f;
                bool hasValue = _dataPreviewAddr >= 0 && _dataPreviewAddr < totalBytes;

                // Resolve to the correct entry for preview
                byte[]? previewData = null;
                long previewLocalAddr = 0;
                long previewLocalSize = 0;
                if (hasValue && ResolveGlobalAddr(_dataPreviewAddr, out int pEntryIdx, out long pLocalAddr))
                {
                    previewData = visibleEntries[pEntryIdx];
                    previewLocalAddr = pLocalAddr;
                    previewLocalSize = previewData.LongLength;
                }

                Hexa.NET.ImGui.ImGui.TextUnformatted("Dec"); Hexa.NET.ImGui.ImGui.SameLine(x);
                Hexa.NET.ImGui.ImGui.TextUnformatted(previewData != null
                    ? FormatPreview(previewData, previewLocalSize, previewLocalAddr, _previewDataType, DataFormatEnum.Dec) : "N/A");

                Hexa.NET.ImGui.ImGui.TextUnformatted("Hex"); Hexa.NET.ImGui.ImGui.SameLine(x);
                Hexa.NET.ImGui.ImGui.TextUnformatted(previewData != null
                    ? FormatPreview(previewData, previewLocalSize, previewLocalAddr, _previewDataType, DataFormatEnum.Hex) : "N/A");

                Hexa.NET.ImGui.ImGui.TextUnformatted("Bin"); Hexa.NET.ImGui.ImGui.SameLine(x);
                Hexa.NET.ImGui.ImGui.TextUnformatted(previewData != null
                    ? FormatPreview(previewData, previewLocalSize, previewLocalAddr, _previewDataType, DataFormatEnum.Bin) : "N/A");
            }

            optShowDataPreview = nextShowDataPreview;
        }

        // -----------------------------------------------------------------------
        //  Separator rendering
        // -----------------------------------------------------------------------

        private void DrawSeparatorLine(ImDrawListPtr drawList, in Sizes s, uint color, int entryIdx, int totalEntries)
        {
            Vector2 pos = Hexa.NET.ImGui.ImGui.GetCursorScreenPos();
            float lineY = pos.Y + s.LineHeight * 0.5f;
            float x1 = pos.X;
            float x2 = pos.X + s.PosAsciiEnd;

            // Horizontal line
            drawList.AddLine(new Vector2(x1, lineY), new Vector2(x2, lineY), color, 1.0f);

            // Entry label
            string label = $" #{entryIdx + 1} ";
            Vector2 labelSize = Hexa.NET.ImGui.ImGui.CalcTextSize(label);
            float labelX = x1 + 4;
            // Background behind label for readability
            drawList.AddRectFilled(
                new Vector2(labelX - 2, lineY - labelSize.Y * 0.5f - 1),
                new Vector2(labelX + labelSize.X + 2, lineY + labelSize.Y * 0.5f + 1),
                Hexa.NET.ImGui.ImGui.GetColorU32(ImGuiCol.WindowBg));
            drawList.AddText(new Vector2(labelX, lineY - labelSize.Y * 0.5f), color, label);

            // Advance cursor past this line
            Hexa.NET.ImGui.ImGui.Dummy(new Vector2(0, s.LineHeight));
        }

        // -----------------------------------------------------------------------
        //  Data Preview
        // -----------------------------------------------------------------------

        private static long DataTypeGetSize(DataTypeEnum dt) => dt switch
        {
            DataTypeEnum.S8 or DataTypeEnum.U8 => 1,
            DataTypeEnum.S16 or DataTypeEnum.U16 => 2,
            DataTypeEnum.S32 or DataTypeEnum.U32 or DataTypeEnum.Float => 4,
            DataTypeEnum.S64 or DataTypeEnum.U64 or DataTypeEnum.Double => 8,
            _ => 1
        };

        private byte[] ReadPreviewBytes(byte[] data, long size, long addr, DataTypeEnum dt)
        {
            int elemSize = (int)DataTypeGetSize(dt);
            int avail = (int)Math.Min(elemSize, size - addr);
            byte[] buf = new byte[8];
            if (avail > 0)
                Buffer.BlockCopy(data, (int)addr, buf, 0, avail);

            bool hostLE = BitConverter.IsLittleEndian;
            if ((_previewEndianness == 1 && hostLE) || (_previewEndianness == 0 && !hostLE))
                Array.Reverse(buf, 0, elemSize);

            return buf;
        }

        private string FormatPreview(byte[] data, long size, long addr, DataTypeEnum dt, DataFormatEnum fmt)
        {
            if (addr < 0 || addr >= size) return "N/A";
            if (addr + DataTypeGetSize(dt) > size) return "N/A";

            byte[] buf = ReadPreviewBytes(data, size, addr, dt);

            if (fmt == DataFormatEnum.Bin)
                return FormatBinary(buf, (int)DataTypeGetSize(dt));

            return dt switch
            {
                DataTypeEnum.S8 => fmt == DataFormatEnum.Dec ? ((sbyte)buf[0]).ToString() : $"0x{buf[0]:x2}",
                DataTypeEnum.U8 => fmt == DataFormatEnum.Dec ? buf[0].ToString() : $"0x{buf[0]:x2}",
                DataTypeEnum.S16 => FmtVal(BitConverter.ToInt16(buf, 0), fmt, "x4"),
                DataTypeEnum.U16 => FmtVal(BitConverter.ToUInt16(buf, 0), fmt, "x4"),
                DataTypeEnum.S32 => FmtVal(BitConverter.ToInt32(buf, 0), fmt, "x8"),
                DataTypeEnum.U32 => FmtVal(BitConverter.ToUInt32(buf, 0), fmt, "x8"),
                DataTypeEnum.S64 => FmtVal(BitConverter.ToInt64(buf, 0), fmt, "x16"),
                DataTypeEnum.U64 => FmtVal(BitConverter.ToUInt64(buf, 0), fmt, "x16"),
                DataTypeEnum.Float => FmtFlt(BitConverter.ToSingle(buf, 0), fmt),
                DataTypeEnum.Double => FmtDbl(BitConverter.ToDouble(buf, 0), fmt),
                _ => "?"
            };
        }

        private static string FmtVal<T>(T val, DataFormatEnum fmt, string hexFmt) where T : IFormattable =>
            fmt == DataFormatEnum.Dec ? val.ToString(null, null)! : $"0x{val.ToString(hexFmt, null)}";

        private static string FmtFlt(float v, DataFormatEnum fmt) =>
            fmt == DataFormatEnum.Dec ? v.ToString("G") : $"0x{BitConverter.ToInt32(BitConverter.GetBytes(v), 0):X8}";

        private static string FmtDbl(double v, DataFormatEnum fmt) =>
            fmt == DataFormatEnum.Dec ? v.ToString("G") : $"0x{BitConverter.ToInt64(BitConverter.GetBytes(v), 0):X16}";

        private static string FormatBinary(byte[] buf, int byteCount)
        {
            var sb = new StringBuilder(byteCount * 9);
            for (int j = byteCount - 1; j >= 0; j--)
            {
                for (int i = 7; i >= 0; i--)
                    sb.Append((buf[j] & (1 << i)) != 0 ? '1' : '0');
                sb.Append(' ');
            }
            return sb.ToString();
        }

        private static string ReadStringFromBuf(byte[] buf)
        {
            int len = 0;
            while (len < buf.Length && buf[len] != 0) len++;
            return Encoding.ASCII.GetString(buf, 0, len);
        }
    }
}
