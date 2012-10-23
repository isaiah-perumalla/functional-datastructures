#r "PresentationCore.dll";;
#r "PresentationFramework.dll";;
#r "System.Xaml.dll";;
#r "WindowsBase.dll";;

open System.Windows
type Tree<'a> = Node of 'a * (Tree<'a> list)
type 'a rbTree = |E  
                 | R of 'a rbTree * 'a * 'a rbTree 
                 | B of 'a rbTree * 'a * 'a rbTree

type color = Red | Black

let rec toTree = function
    |R(l,v,r) -> mknode Red l v r
    |B(l,v,r) -> mknode Black l v r
    |_ -> invalidOp "cannot trasform empty"
    
and mknode clr l v r = 
        let value = clr,v
        in
        match l,r with
        |E,E -> Node(value,[])
        |E,a |a,E -> Node(value,[toTree a;])
        |a,b -> Node(value, [toTree a; toTree b;]  )
   
let moveExtent e (dx: float) =
        [ for p, q in e -> p + dx, q + dx ]

let rec merge ps qs =
            match ps, qs with
                | [], xs | xs, [] -> xs
                | (p, _)::ps, (_, q)::qs -> (p, q)::merge ps qs
let rec fit ps qs =
        Seq.zip ps qs
        |> Seq.fold (fun a ((_, p), (q, _)) -> p - q + 1.0 |> max a) 0.0

let fitsLeft es =
        let fitLeft (a, xs) e =
          let x = fit a e
          merge a (moveExtent e x), x::xs
        let _, xs = List.fold fitLeft ([], []) es
        List.rev xs

let fitsRight es =
        let fitRight e (a, xs) =
          let x = -fit e a
          merge (moveExtent e x) a, x::xs
        let _, xs = List.foldBack fitRight es ([], [])
        xs

let fitList es =
        [ for x, y in List.zip (fitsLeft es) (fitsRight es) ->
            0.5 * (x + y) ]
    
let layout tree =
        let rec layout (Node(label, subtrees)) =
          let trees, extents = List.unzip (List.map layout subtrees)
          let positions = fitList extents
          let pTrees =
            [ for Node((label, x), subtrees), dx in List.zip trees positions ->
                Node((label, x + dx), subtrees) ]
          let pExtents =
            [ for extent, dx in List.zip extents positions ->
                moveExtent extent dx ]
          Node((label, 0.0), pTrees),
          (0.0, 0.0)::List.fold merge [] pExtents
        layout tree |> fst



let rec (|Tree|_|) = function
        | c::'('::Trees(ts, ')'::cs) -> Some(Node(string c, ts), cs)
        | c::cs when c<>')' -> Some(Node(string c, []), cs)
        | _ -> None
      and (|Trees|) = function
        | Tree(t, Trees(ts,cs)) -> t::ts, cs
        | cs -> [], cs

let scale = 40.0
    
let thickness = 1.0

let size = 0.5 * scale

let tree = "A(B(C(DE(F(G()H(IJKL)MN(O))))P(QR))S(T(UVW(X(Y)Z(abcd)))e(f(g)hi(j(klmn))))o(p(q(rstu)v(wx(yz)01)2)))"

let rec verticesOf (x, y) (Node((_, dx), ts)) = seq {
       let x, y = x + scale * dx, y + scale
       yield x, y
       for t in ts do
          yield! verticesOf (x, y) t
             }

let boundOf t =
        let bound ((x0, y0), (x1, y1)) (x, y) =
          (min x0 x, min y0 y), (max x1 x, max y1 y)
        let p = 0.0, 0.0
        verticesOf p t
        |> Seq.fold bound (p, p)

let line (x0, y0) (x1, y1) =
        let shape = Shapes.Line(X1=x0, Y1=y0, X2=x1, Y2=y1)
        shape.Stroke <- Media.Brushes.Black
        shape.StrokeStartLineCap <- Media.PenLineCap.Round
        shape.StrokeEndLineCap <- Media.PenLineCap.Round
        shape.StrokeThickness <- thickness
        shape

let labelAt label (x, y) =
        let label = Controls.Button(Content=label)
        label.Padding <- Thickness(0.5*size, 0.0, 0.5*size, 0.0)
        label.SizeChanged.Add(fun e ->
          let w, h = e.NewSize.Width, e.NewSize.Height
          label.RenderTransform <-
            Media.TranslateTransform(x - 0.5*w, y - 0.5*h))
        label

let rbLabel (clr,v) (x,y) = 
                let lbl = labelAt v (x,y)
                in
                match clr with
                |Red -> lbl.Background <- Media.Brushes.Red
                |_ -> lbl.Background <- Media.Brushes.Gray
                lbl 

let linesTo (x1, y1) ts =
        [ match [ for (Node((_, dx), _)) in ts -> x1 + scale * dx ] with
          | [] -> ()
          | xs ->
              let y = y1 + 0.5*scale
              yield line (x1, y1) (x1, y)
              yield line (Seq.min xs, y) (Seq.max xs, y)
              for x in xs do
                yield line (x, y) (x, y1 + scale) ]

let rec draw render (x0, y0) (Node((label, dx), ts)) = seq {
        let x1, y1 = x0 + scale * dx, y0 + scale
        yield linesTo (x1, y1) ts, render label (x1, y1)
        for t in ts do
          yield! draw render (x1, y1) t
      }

let renderOnCanvas renderer t =
    let tree = layout t
    let (x0, y0), (x1, y1) = boundOf tree
    let (x0, y0), (x1, y1) =
        let d = thickness + size
        (x0-d, y0-d), (x1+d, y1+d)
    let canvas = Controls.Canvas()
    canvas.Width <- x1 - x0
    canvas.Height <- y1 - y0
    let shapes = draw renderer (0.5 * (x1 - x0), 0.0) tree
    for shapes, _ in shapes do
          Seq.iter (canvas.Children.Add >> ignore) shapes
    for _, label in shapes do
          canvas.Children.Add label |> ignore
    canvas

[<System.STAThread>]
let displayTree renderer t =
      do
        let canvas = renderOnCanvas renderer t
        let scroll = Controls.ScrollViewer(Content=canvas)
        scroll.HorizontalScrollBarVisibility <- Controls.ScrollBarVisibility.Visible
        Application().Run(Window(Content=scroll)) |> ignore


let (Tree(t, cs)) = List.ofSeq tree
displayTree labelAt t