Does this work?

> import Data.Array

> grid f = array ((0, 0), (19, 19)) [((x, y), f (x, y))| x <- [0..19], y <- [0..19]]

> unGrid a (x, y) = if x>=0 && x<=19 && y>=0 && y<=19 then a!(x, y) else False

> faceIn f (x, y) = f (x, y)
> hEdgeIn f (x, y) = f (x, y) || f (x, y-1)
> vEdgeIn f (x, y) = f (x, y) || f (x-1, y)
> vertexIn f (x, y) = f (x, y) || f (x-1, y) || f (x, y-1) || f (x-1, y-1)

> count f xs = length (filter f xs)

> faces = [(x, y) | x <- [0..19], y <- [0..19]]
> hEdges = [(x, y) | x <- [0..19], y <- [0..20]]
> vEdges = [(x, y) | x <- [0..20], y <- [0..19]]
> vertices = [(x, y) | x <- [0..20], y <- [0..20]]

> euler f = let g = unGrid f
>               nVertices = count (vertexIn g) vertices
>               nEdges = count (hEdgeIn g) hEdges+count (vEdgeIn g) vEdges
>               nFaces = count (faceIn g) faces
>           in nVertices-nEdges+nFaces

> circle (cx, cy) r (x, y) = if (x-cx)^2+(y-cy)^2 < r*r then 1 else 0

> fn (x, y) = circle (8, 9) 7 (x, y) + circle (13, 13) 3 (x, y) + circle (8, 14) 6 (x, y)

> test s = euler (grid (\(x,y) -> fn (x,y)>s))

> sense = sum $ map test [0..10]

> kernel (False, False, True,  False) =  1
> kernel (True,  False, False, True)  = -1
> kernel (True,  False, True,  True)  = -1
> kernel _                            =  0

> sense' f = let coords = [(x, y) | x <- [0..20], y <- [0..20]]
>                h g (i, j) = (g (i, j), g (i+1, j), g (i, j+1), g (i+1, j+1))
>                t n = \(x, y) -> fn (x, y)>n
>                s n = sum $ map (t n) coords
>            in s 0+s 1+s 2+s 3
