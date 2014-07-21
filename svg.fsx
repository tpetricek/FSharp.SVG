#r "System.Xml.Linq.dll"
open System
open System.Globalization
open System.Drawing
open System.Xml.Linq

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

let (!) s = XName.Get(s)

let writeHtml (elements:XElement list) =
  let template = """
<!DOCTYPE html>
<html lang="en" xmlns="http://www.w3.org/1999/xhtml">
<body>
  <svg id="root" height="100" width="100">[SVG]</svg>
</body>
</html>"""
  let body = elements |> Seq.map (fun e -> e.ToString()) |> String.concat ""
  let res = template.Replace("[SVG]", body)
  System.IO.File.WriteAllText("C:\\temp\\diagrams.html", res)

// --------------------------------------------------------------------------------------
// SVG domain model
// --------------------------------------------------------------------------------------

type Size = 
  | Relative of float 
  | Absolute of float

type Margin = 
  { Top : Size
    Right : Size
    Bottom : Size
    Left : Size }

type SplitKind = 
  | Vertical 
  | Horizontal

type LineCap = Butt | Square | Round
type LineJoin = Miter | Bevel | Round

type Fill = 
  | Color of Color
  //| Gradient
  //| Pattern

type Stroke = 
  { Color : Color 
    Width : float
    LineCap : LineCap
    LineJoin : LineJoin
    DashArray : int list }

type Text = string

type TextAlignment = 
  | TopLeft 
  | TopMiddle
  | TopRight
  | CenterLeft
  | Center
  | CenterRight
  | BottomLeft
  | BottomMiddle
  | BottomRight

type Graphics =
  | Empty
  | Rectangle 
  | Ellipse
  | Text of Text

  | Margin of Margin * Graphics

  | Combine of Graphics list
  | Split of SplitKind * Graphics list // better name

  | Align of TextAlignment * Graphics
  | Fill of Fill * Graphics
  | Stroke of Stroke * Graphics

// --------------------------------------------------------------------------------------
// SVG rendering
// --------------------------------------------------------------------------------------

type Rectangle =
  { Left : float
    Top : float
    Width : float
    Height : float }

type RenderContext =
  { BoundingBox : Rectangle 
    Alignment : TextAlignment
    Stroke : Stroke 
    Fill : Fill }

let applyMargin (margin:Margin) (rect:Rectangle) = 
  let size total = function
    | Absolute size -> size
    | Relative size -> total * size / 100.0
  let l, t = size rect.Width margin.Left, size rect.Height margin.Top
  let r, b = size rect.Width margin.Right, size rect.Height margin.Bottom
  { Rectangle.Left = rect.Left + l
    Top = rect.Top + t
    Width = rect.Width - l - r
    Height = rect.Height - t - b }

let formatColor (color:Color) = 
  (float color.A) / 255.0, sprintf "rgb(%d,%d,%d)" color.R color.G color.B

let formatFill (fill:Fill) = 
  match fill with 
  | Fill.Color color ->
      let opacity, color = formatColor color
      [ XAttribute(!"fill", color)
        XAttribute(!"fill-opacity", opacity) ]
  
let formatStroke (stroke:Stroke) = 
  if stroke.Width > 0.0 then
    let opacity, color = formatColor stroke.Color
    [ XAttribute(!"stroke", color)
      XAttribute(!"stroke-opacity", opacity)
      XAttribute(!"stroke-linecap", (sprintf "%A" stroke.LineCap).ToLower())
      XAttribute(!"stroke-linejoin", (sprintf "%A" stroke.LineJoin).ToLower())
      XAttribute(!"stroke-width", stroke.Width) ] @
    match stroke.DashArray with
    | [] -> []
    | nums -> [XAttribute(!"stroke-dasharray", nums |> Seq.map (sprintf "%d") |> String.concat ",") ]
  else []

let formatRectangle (rect:Rectangle) = 
  [ XAttribute(!"width", rect.Width)
    XAttribute(!"height", rect.Height)
    XAttribute(!"x", rect.Left)
    XAttribute(!"y", rect.Top) ]

let formatEllipse (rect:Rectangle) =
  [ XAttribute(!"rx", rect.Width/2.0)
    XAttribute(!"ry", rect.Height/2.0)
    XAttribute(!"cx", rect.Left + rect.Width/2.0)
    XAttribute(!"cy", rect.Top + rect.Height/2.0) ]

let formatTextAlignment (rect:Rectangle) align =
  [ match align with 
    | TopLeft | TopMiddle | TopRight ->
        yield XAttribute(!"y", rect.Top)
        yield XAttribute(!"dy", "1em")
    | CenterLeft | Center | CenterRight ->
        yield XAttribute(!"y", rect.Top + rect.Height/2.0)
        yield XAttribute(!"dy", "0.30em")
    | BottomLeft | BottomMiddle | BottomRight ->
        yield XAttribute(!"y", rect.Top + rect.Height) 
    match align with
    | TopLeft | CenterLeft | BottomLeft ->
        yield XAttribute(!"x", rect.Left)
        yield XAttribute(!"text-anchor", "start") 
    | TopMiddle | Center | BottomMiddle ->
        yield XAttribute(!"x", rect.Left + rect.Width/2.0)
        yield XAttribute(!"text-anchor", "middle") 
    | TopRight | CenterRight | BottomRight ->
        yield XAttribute(!"x", rect.Left + rect.Width)
        yield XAttribute(!"text-anchor", "end") ]

let formatText ctx (t:string) = 
  [ for attr in formatTextAlignment ctx.BoundingBox ctx.Alignment do
      yield box attr
    yield box t ]

let rec formatGraphics ctx = function
  | Empty -> []
  | Rectangle -> 
      [ XElement(!"rect", formatRectangle ctx.BoundingBox, formatStroke ctx.Stroke, formatFill ctx.Fill) ]
  | Ellipse -> 
      [ XElement(!"ellipse", formatEllipse ctx.BoundingBox, formatStroke ctx.Stroke, formatFill ctx.Fill) ]
  | Text t ->
      [ XElement(!"text", formatText ctx t) ]

  | Align(align, graphics) -> formatGraphics { ctx with Alignment = align } graphics
  | Fill(fill, graphics) -> formatGraphics { ctx with Fill = fill } graphics
  | Stroke(stroke, graphics) -> formatGraphics { ctx with Stroke = stroke } graphics

  | Margin(margin, graphics) ->
      let bbox = applyMargin margin ctx.BoundingBox
      let ctx = { ctx with BoundingBox = bbox }
      formatGraphics ctx graphics

  | Combine(graphics) ->
      graphics |> List.collect (formatGraphics ctx)

  | Split(Horizontal, graphics) ->
      let count = float graphics.Length
      let subSize = ctx.BoundingBox.Width / count
      let bbox = { ctx.BoundingBox with Width = subSize }
      graphics 
      |> Seq.mapi (fun i gr ->
        let left = bbox.Left + (float i) * subSize
        let ctx = { ctx with BoundingBox = { bbox with Left = left } }
        formatGraphics ctx gr )
      |> Seq.concat |> List.ofSeq 

  | Split(Vertical, graphics) ->
      let count = float graphics.Length
      let subSize = ctx.BoundingBox.Height / count
      let bbox = { ctx.BoundingBox with Height = subSize }
      graphics 
      |> Seq.mapi (fun i gr ->
        let top = bbox.Top + (float i) * subSize
        let ctx = { ctx with BoundingBox = { bbox with Top = top } }
        formatGraphics ctx gr )
      |> Seq.concat |> List.ofSeq 

module Palette =
  let Vega10 = seq {
      while true do
        for c in [ "ff1f77b4"; "ffff7f0e"; "ff2ca02c"; "ffd62728"; "ff9467bd"; "ff8c564b"; "ffe377c2"; "ff7f7f7f"; "ffbcbd22"; "ff17becf" ] do
          yield Color.FromArgb(Int32.Parse(c, NumberStyles.HexNumber)) }
  let Vega20 = seq {
      while true do
        for c in [ "ff1f77b4"; "ffaec7e8"; "ffff7f0e"; "ffffbb78"; "ff2ca02c"; "ff98df8a"; "ffd62728"; "ffff9896"; "ff9467bd"; "ffc5b0d5"; "ff8c564b"; "ffc49c94"; "ffe377c2"; "fff7b6d2"; "ff7f7f7f"; "ffc7c7c7"; "ffbcbd22"; "ffdbdb8d"; "ff17becf"; "ff9edae5" ] do
          yield Color.FromArgb(Int32.Parse(c, NumberStyles.HexNumber)) }


module Chart = 
  let Bar(yvalues) = 
    let lo, hi = Seq.min yvalues, Seq.max yvalues
    let margin = 
      { Margin.Top = Absolute 0.0; Left = Absolute 0.0; Bottom = Absolute 0.0; Right = Absolute 1.0 }
    let bars = 
      [ for yval, clr in Seq.zip yvalues Palette.Vega10 ->
          let margin = { margin with Top = Relative (100.0 - yval/hi*100.0) }
          let body = Combine[Rectangle; Align(TopMiddle, Text(string yval)) ]
          Fill(Fill.Color clr, Margin(margin, body)) ]
    Split(Horizontal, bars)

let rect = { Rectangle.Left = 0.0; Top = 0.0; Width = 100.0; Height = 100.0 }
let stroke = { Stroke.Color = Color.MidnightBlue; LineCap = LineCap.Round; LineJoin = LineJoin.Round; Width = 0.0; DashArray = [] }
let fill = Fill.Color Color.LightSteelBlue
let ctx = { BoundingBox = rect; Stroke = stroke; Fill = fill; Alignment = TopRight }


// let m = { Margin.Left = Relative 10.0; Right = Relative 10.0; Top = Relative 20.0; Bottom = Relative 20.0 }
// let svg = Split(Horizontal, [Rectangle; Margin(m, Rectangle); Ellipse])

let svg = Chart.Bar [ 10.0; 40.0; 15.0; 60.0; 55.0; 25.0 ]
//let svg = Split(Vertical, [ Combine [ Rectangle; Text("Hello world") ]; Ellipse ])

let data = [ 60.0; 50.0; 30.0; 20.0; 10.0; 5.0; ]

let half = (Seq.sum data) / 2.0
let prefixes = [ for i in 1 .. data.Length-2 -> abs((data |> Seq.take i |> Seq.sum)-half)]
let count = prefixes |> Seq.mapi (fun i v -> i, v) |> Seq.minBy snd

svg |> formatGraphics ctx |> writeHtml
