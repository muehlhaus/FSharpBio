namespace FSharp.Charting


[<AutoOpen>]
module ChartingX =

    open FSharp.Charting
    open FSharp.Charting.ChartTypes        
    open FSharp.CoreX

    module ChartTypes = 
        
        [<Sealed>]
        type StripLine( ?Color, ?BorderColor,?BorderDashStyle,?BorderWidth, ?Interval, ?IntervalOffset, ?IntervalOffsetType, ?StripWidth, ?StripWidthType) = 
            let stripLine = new System.Windows.Forms.DataVisualization.Charting.StripLine()        
            do
                Color              |> Option.iter stripLine.set_BackColor
                BorderColor        |> Option.iter stripLine.set_BorderColor                
                BorderWidth        |> Option.iter (stripLine.set_BorderWidth)
                BorderDashStyle    |> Option.iter (stripLine.set_BorderDashStyle)

                Interval           |> Option.iter stripLine.set_Interval
                IntervalOffset     |> Option.iter stripLine.set_IntervalOffset
                IntervalOffsetType |> Option.iter stripLine.set_IntervalOffsetType
            
                StripWidth         |> Option.iter stripLine.set_StripWidth
                StripWidthType     |> Option.iter stripLine.set_StripWidthType
            
            member internal x.Handle = stripLine





    let private optionalFont(family:string option, size:float option, style:System.Drawing.FontStyle option) =
        match family, size, style with 
        | None,None,None -> None
        | _ -> Some (StyleHelper.Font(?Family=family,?Size=size,?Style=style))

    let private AxisFormatHelper (axis:System.Windows.Forms.DataVisualization.Charting.Axis)
                            (enabled, angle, color, crossing, format, interval, intervalOffset, intervalOffsetType:DateTimeIntervalType option, intervalType:DateTimeIntervalType option, isEndLabelVisible, isStaggered, 
                                lineColor, lineWidth, majorGrid, minorGrid, majorTickMark, minorTickMark, truncatedLabels, fontName:string option, fontStyle:System.Drawing.FontStyle option, fontSize:float option) =                            
                        

            let labelStyle = axis.LabelStyle            
            angle |> Option.iter labelStyle.set_Angle
            color |> Option.iter labelStyle.set_ForeColor
            format |> Option.iter labelStyle.set_Format
            
            interval |> Option.iter axis.set_Interval
            intervalOffset |> Option.iter axis.set_IntervalOffset
            intervalOffsetType |> Option.iter (int >> enum >> axis.set_IntervalOffsetType)
            intervalType |> Option.iter (int >> enum >> axis.set_IntervalType)
            
            isStaggered |> Option.iter labelStyle.set_IsStaggered
            isEndLabelVisible |> Option.iter labelStyle.set_IsEndLabelVisible
            truncatedLabels |> Option.iter labelStyle.set_TruncatedLabels            
            optionalFont(fontName, fontSize, fontStyle) |> Option.iter labelStyle.set_Font 

            enabled        |> Option.iter (labelStyle.set_Enabled)
            crossing       |> Option.iter (axis.set_Crossing)
            lineColor      |> Option.iter (axis.set_LineColor)
            lineWidth      |> Option.iter (axis.set_LineWidth)
            majorGrid      |> Option.iter (fun (grid:ChartTypes.Grid) -> (axis.set_MajorGrid grid.Handle ))  
            minorGrid      |> Option.iter (fun (grid:ChartTypes.Grid) -> (axis.set_MinorGrid grid.Handle ))  
            majorTickMark  |> Option.iter (fun (tickMark:ChartTypes.TickMark) -> (axis.set_MajorTickMark tickMark.Handle ))  
            minorTickMark  |> Option.iter (fun (tickMark:ChartTypes.TickMark) -> (axis.set_MinorTickMark tickMark.Handle ))  
            //stripLine      |> Option.iter (fun (sl:seq<ChartTypes.StripLine>) -> (sl |> Seq.iter (fun s -> axis.StripLines.Add s.Handle)))                        
            //stripLine      |> Option.iter (fun (sl:ChartTypes.StripLine) -> (axis.StripLines.Add sl.Handle))                        
            
            axis





    type DataZipper =

        static member Xy (x:seq<'a>) (y:seq<'a>) =
            Seq.zip x y


        static member RangeXy (x:seq<float>) (y:seq<float>) (r:seq<float>) =
            Seq.zip3 x y r
            |> Seq.map (fun (x,y,r) -> (x,y,y+r,y-r))
                
        static member Range (x:seq<float>) (y:seq<float>) (r:seq<float>) =
            Seq.zip3 x y r
            |> Seq.map (fun (xValue,yValue,rValue) -> (xValue,yValue + rValue,yValue - rValue))
        
        
    type Chart with 


                        
        /// <summary>Display the chart in a new ChartControl in a new Form() (by piping)</summary>
        static member ShowChart (ch : #GenericChart) = // TM : support piping style
            //fun (ch : GenericChart) -> ch.ShowChart()
            ch.ShowChart()
            ch

        /// <summary>Display the chart in a new ChartControl in a new Form() (by piping)</summary>
        static member ShowChartWithSize width height (ch : #GenericChart) = // TM : support piping style
            let frm = new System.Windows.Forms.Form(Visible = true, TopMost = true, Width = 700, Height = 500)
                        
            let ctl = new ChartControl(ch, Dock = System.Windows.Forms.DockStyle.Fill)
          
            frm.Controls.Add(ctl)
            frm.Show()
            ctl.Focus() |> ignore

            ctl.AutoSize <- false
            ctl.Size <- System.Drawing.Size(width,height)  

            printfn "Height: %i" ctl.Size.Height
            ch


        /// <summary>Save the chart as an image in the specified image format (by piping)</summary>
        static member SaveChartAs filename format width height (ch : #GenericChart) =  // TM : support piping style            
            let frm = new System.Windows.Forms.Form(Visible = false, TopMost = true) //, Width = width, Height = height)
            //ch.Chart.MaximumSize <- System.Drawing.Size(width,height)
            ch.Chart.Size  <- System.Drawing.Size(width,height)
            let ctl = new ChartControl(ch, Dock = System.Windows.Forms.DockStyle.Fill)  
            ctl.Size <- System.Drawing.Size(width,height)
            frm.Controls.Add(ctl)
//            frm.Show()
//            ctl.Focus() |> ignore
            
            ch.SaveChartAs(filename,format)



        // StripLine 
        /// <summary>Apply additional styling to the chart area</summary>
        static member WithAreaStyle(?BorderColor,?BorderWidth,?BorderDashStyle:ChartTypes.DashStyle) =
            fun (ch : #GenericChart) ->
                let area = ch.Area
                BorderColor     |> Option.iter (area.set_BorderColor)
                BorderWidth     |> Option.iter (area.set_BorderWidth)
                BorderDashStyle |> Option.iter (int >> enum >> area.set_BorderDashStyle)
                
                ch


        /// <summary>Apply additional styling to the x-axis label</summary>
        static member WithXAxisLabelStyle
                        (?Enabled, ?Angle, ?Color, ?Crossing, ?Format, ?Interval, ?IntervalOffset, ?IntervalOffsetType, ?IntervalType, ?IsEndLabelVisible, ?IsStaggered, 
                                ?LineColor, ?LineWidth, ?MajorGrid, ?MinorGrid, ?MajorTickMark, ?MinorTickMark, ?TruncatedLabels, ?FontName, ?FontStyle, ?FontSize) =       
            fun (ch : #GenericChart) ->                                            
                ch.Area.AxisX <- AxisFormatHelper ch.Area.AxisX (Enabled, Angle, Color, Crossing, Format, Interval, IntervalOffset, IntervalOffsetType, IntervalType, IsEndLabelVisible, IsStaggered, 
                                                                    LineColor, LineWidth, MajorGrid, MinorGrid, MajorTickMark, MinorTickMark, TruncatedLabels, FontName, FontStyle, FontSize)                                
                ch

        /// <summary>Apply additional styling to the second x-axis label</summary>
        static member WithXAxis2LabelStyle
                        (?Enabled, ?Angle, ?Color, ?Crossing, ?Format, ?Interval, ?IntervalOffset, ?IntervalOffsetType, ?IntervalType, ?IsEndLabelVisible, ?IsStaggered, 
                                ?LineColor, ?LineWidth, ?MajorGrid, ?MinorGrid, ?MajorTickMark, ?MinorTickMark, ?TruncatedLabels, ?FontName, ?FontStyle, ?FontSize) =       
            fun (ch : #GenericChart) ->                                            
                ch.Area.AxisX2 <- AxisFormatHelper ch.Area.AxisX2 (Enabled, Angle, Color, Crossing, Format, Interval, IntervalOffset, IntervalOffsetType, IntervalType, IsEndLabelVisible, IsStaggered, 
                                                                    LineColor, LineWidth, MajorGrid, MinorGrid, MajorTickMark, MinorTickMark, TruncatedLabels, FontName, FontStyle, FontSize)                                
                ch
            

        /// <summary>Apply additional styling to the y-axis label</summary>
        static member WithYAxisLabelStyle
                        (?Enabled, ?Angle, ?Color, ?Crossing, ?Format, ?Interval, ?IntervalOffset, ?IntervalOffsetType, ?IntervalType, ?IsEndLabelVisible, ?IsStaggered, 
                                ?LineColor, ?LineWidth, ?MajorGrid, ?MinorGrid, ?MajorTickMark, ?MinorTickMark, ?TruncatedLabels, ?FontName, ?FontStyle, ?FontSize) =       
            fun (ch : #GenericChart) ->                                            
                ch.Area.AxisY <- AxisFormatHelper ch.Area.AxisY (Enabled, Angle, Color, Crossing, Format, Interval, IntervalOffset, IntervalOffsetType, IntervalType, IsEndLabelVisible, IsStaggered, 
                                                                    LineColor, LineWidth, MajorGrid, MinorGrid, MajorTickMark, MinorTickMark, TruncatedLabels, FontName, FontStyle, FontSize)                                
                ch

        /// <summary>Apply additional styling to the second y-axis label</summary>
        static member WithYAxis2LabelStyle
                        (?Enabled, ?Angle, ?Color, ?Crossing, ?Format, ?Interval, ?IntervalOffset, ?IntervalOffsetType, ?IntervalType, ?IsEndLabelVisible, ?IsStaggered, 
                                ?LineColor, ?LineWidth, ?MajorGrid, ?MinorGrid, ?MajorTickMark, ?MinorTickMark, ?TruncatedLabels, ?FontName, ?FontStyle, ?FontSize) =       
            fun (ch : #GenericChart) ->                                            
                ch.Area.AxisY2 <- AxisFormatHelper ch.Area.AxisY2 (Enabled, Angle, Color, Crossing, Format, Interval, IntervalOffset, IntervalOffsetType, IntervalType, IsEndLabelVisible, IsStaggered, 
                                                                    LineColor, LineWidth, MajorGrid, MinorGrid, MajorTickMark, MinorTickMark, TruncatedLabels, FontName, FontStyle, FontSize)                                
                ch


        /// Create a combined chart with the given charts placed in columns 
        static member ColumnsWithBreak (max:int) (charts:seq<GenericChart>) = 
            charts
            |> Seq.groupsOfAtMost max
            |> Seq.map Chart.Columns
            |> Chart.Rows                    


        /// Create a combined chart with the given charts placed in rows  
        static member RowsWithBreak (max:int) (charts:seq<GenericChart>) = 
            charts
            |> Seq.groupsOfAtMost max
            |> Seq.map Chart.Rows
            |> Chart.Columns 


//            /// <summary>Consists of lines with markers that are used to display statistical information about the data displayed in a graph.</summary>
//            /// <param name="data">The data for the chart.</param>
//            /// <param name="Name">The name of the data set.</param>
//            /// <param name="Title">The title of the chart.</param>
//            /// <param name="Labels">The labels that match the data.</param>
//            /// <param name="Color">The color for the data.</param>
//            /// <param name="XTitle">The title of the X-axis.</param>
//            /// <param name="YTitle">The title of the Y-axis.</param>
//        static member ErrorBarPM(data,?Name,?Title,?Labels, ?Color,?XTitle,?YTitle) =                 
//            let inline plus a b = a + b
//            let inline minus a b = a - b 
//            GenericChart.Create(mergeDataAndLabelsForXY3 (data) Labels, fun () -> ErrorBarChart () )
//                |> Helpers.ApplyStyles(?Name=Name,?Title=Title,?Color=Color,?AxisXTitle=XTitle,?AxisYTitle=YTitle)
