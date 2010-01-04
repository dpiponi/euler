\documentclass{article}
\usepackage{hyperref}

%if False

> main = print stage3''''''

%endif
\usepackage{tikz}
\usetikzlibrary{shapes,arrows}

%include lhs2TeX.fmt
%include lhs2TeX.sty

%options ghci

\begin{document} 

Does this work?

> import Data.Array

> grid f = array ((0, 0), (19, 19)) [((x, y), f (x, y))| x <- [0..19], y <- [0..19]]

> unGrid b a (x, y) = if x>=0 && x<=19 && y>=0 && y<=19 then a!(x, y) else b

> faceIn f (x, y) = f (x, y)
> hEdgeIn f (x, y) = f (x, y) || f (x, y-1)
> vEdgeIn f (x, y) = f (x, y) || f (x-1, y)
> vertexIn f (x, y) = f (x, y) || f (x-1, y) || f (x, y-1) || f (x-1, y-1)

> count f xs = length (filter f xs)

> faces = [(x, y) | x <- [0..19], y <- [0..19]]
> hEdges = [(x, y) | x <- [0..19], y <- [0..20]]
> vEdges = [(x, y) | x <- [0..20], y <- [0..19]]
> vertices = [(x, y) | x <- [0..20], y <- [0..20]]

> euler f = let g = unGrid False f
>               nVertices = count (vertexIn g) vertices
>               nEdges = count (hEdgeIn g) hEdges+count (vEdgeIn g) vEdges
>               nFaces = count (faceIn g) faces
>           in nVertices-nEdges+nFaces

> circle (cx, cy) r (x, y) = if (x-cx)^2+(y-cy)^2 < r*r then 1 else 0

> fn (x, y) = circle (12, 9) 2 (x, y) + circle (14, 9) 2 (x, y) + circle (8, 14) 1 (x, y)
> fn' = unGrid 0 (grid fn)

> test s = euler (grid (\(x,y) -> fn (x,y)>s))

> sense = sum $ map test [0..10]

> kernel (False, False, True,  False) =  1
> kernel (True,  False, False, True)  = -1
> kernel (True,  False, True,  True)  = -1
> kernel _                            =  0

> sense' f = let coords = [(x, y) | x <- [0..20], y <- [0..20]]
>                h g (i, j) = (g (i, j), g (i+1, j), g (i, j+1), g (i+1, j+1))
>                t n p = kernel (h (\p -> f p>n) p)
>                s n = sum $ map (t n) coords
>            in s 0+s 1+s 2+s 3

> main = print $ sense' fn

\begin{tikzpicture}
\draw (0,0) -- (1,1) -- (1,0) -- (0,0);
\end{tikzpicture}

\end{document}
