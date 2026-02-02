using Bonsai;
using Bonsai.Design;
using Bonsai.Expressions;
using Bonsai.Gui.ImGui.HexViewer;
using Hexa.NET.ImPlot;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using Hexa.NET.ImGui;

[assembly: TypeVisualizer(typeof(ByteArrayVisualizer), Target = typeof(byte[]))]

namespace Bonsai.Gui.ImGui.HexViewer
{

    public class ByteArrayVisualizer : BufferedVisualizer
    {
        ImGuiControl imGuiControl;
        HexViewer hexViewer;
        private Queue<byte[]> buffer = new Queue<byte[]>();


        public override void Show(object value)
        {
        }

        protected override void ShowBuffer(IList<System.Reactive.Timestamped<object>> values)
        {
            // Buffer size = History + 1 (History=0 means 1 entry, History=N means N+1 entries)
            int maxBufferSize = (int)hexViewer.HistorySize + 1;

            var casted = values.Select(v => (byte[])v.Value);
            foreach (var item in casted)
            {
                buffer.Enqueue(item);
                while (buffer.Count > maxBufferSize)
                {
                    buffer.Dequeue();
                }
            }
            base.ShowBuffer(values);
            imGuiControl.Invalidate();
        }
        void StyleColors()
        {
            if (hexViewer.IsDarkThemeEnabled)
            {
                Hexa.NET.ImGui.ImGui.StyleColorsDark();
                ImPlot.StyleColorsDark(ImPlot.GetStyle());
            }
            else
            {
                Hexa.NET.ImGui.ImGui.StyleColorsLight();
                ImPlot.StyleColorsLight(ImPlot.GetStyle());
            }
        }
        /// <inheritdoc/>
        public override void Load(IServiceProvider provider)
        {
            var context = (ITypeVisualizerContext)provider.GetService(typeof(ITypeVisualizerContext));
            imGuiControl = new ImGuiControl();
            hexViewer = new HexViewer();
            imGuiControl.Dock = DockStyle.Fill;
            imGuiControl.Render += (sender, e) =>
            {
                var dockspaceId = Hexa.NET.ImGui.ImGui.DockSpaceOverViewport(
                    0,
                    Hexa.NET.ImGui.ImGui.GetMainViewport(),
                    ImGuiDockNodeFlags.AutoHideTabBar | ImGuiDockNodeFlags.NoUndocking);

                StyleColors();
                Hexa.NET.ImGui.ImGui.PushFont(Hexa.NET.ImGui.ImGui.GetFont(), 20f);

                if (Hexa.NET.ImGui.ImGui.Begin("HexViewer"))
                {
                    hexViewer.DrawContents(buffer);
                }
                bool isDocked = Hexa.NET.ImGui.ImGui.IsWindowDocked();
                Hexa.NET.ImGui.ImGui.End();

                // Reset font
                Hexa.NET.ImGui.ImGui.PopFont();

                var centralNode = ImGuiP.DockBuilderGetCentralNode(dockspaceId);
                if (!isDocked && !centralNode.IsNull)
                {
                    unsafe
                    {
                        var handle = centralNode.Handle;
                        uint dockId = handle->ID;
                        ImGuiP.DockBuilderDockWindow("HexViewer", dockId);
                    }
                }
            };

            var visualizerService = (IDialogTypeVisualizerService)provider.GetService(typeof(IDialogTypeVisualizerService));
            if (visualizerService != null)
            {
                visualizerService.AddControl(imGuiControl);
            }
        }

        /// <inheritdoc/>
        public override void Unload()
        {
            if (imGuiControl != null)
            {
                imGuiControl.Dispose();
            }
        }

    }
}
