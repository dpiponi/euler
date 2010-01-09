\documentclass{article}
\usepackage{hyperref}

%if False

> import Data.Array
> import Data.Char

%endif
\usepackage{tikz}
\usetikzlibrary{shapes,arrows}

%include lhs2TeX.fmt
%include lhs2TeX.sty

%options ghci

\begin{document} 
\section{Introduction}
The problem I ultimately want to solve is this: suppose we have a set of targets we want to count. This could be anything from enemy tanks rolling over the plains to electronically tagged wildlife roaming the countryside. Each target has a region of influence, which might simply be circular in shape, or might be more complex and depend on the target. Now suppose that we have a high density of sensors scattered over our domain and that each sensor can tell us how many regions of influence it lies in. So, roughly speaking, each sensor counts how many targets are nearby. How do we compute how many targets we have in total. At first it seems like a trivial problem. The sensors can all count, and if every target is in range of a sensor, every target will be counted. But we can't simply add the numbers from all of the sensors as many sensors will be in the domain of influence of the same target. If we sum all of the numbers we'll be counting each target many time over. We need to be able to subtract off the targets that are counted twice. But some targets will be counted 3 times and so on. And how do we tell when a target has been counted twice when all we have are counts?

We'll make one simplifying assumption in solving this problem: that the regions of influence are simply connected. In other words, they are basically some kind of shape that doesn't have holes in it. That could mean anything from a square or disk to a shape like the letter 'W'. But it excludes shapes like annuli or the letter 'B'. If we make this assumption then we can solve this problem with a very simple algorithm that will work in almost all cases. In fact, the only time it fails will be situations where no algorithm could possibly work. But there's a little ground to cover before getting to the solution.

> type Field = Int -> Int -> Int

> data Grid = Grid Int Int Field

> gsum (Grid w h f) = sum [f x y | x <- [0..w-1], y <- [0..h-1]]

> test9 = gsum (Grid 10 10 (\x y -> if (x,y) == (3,4) then 1 else 0))

> data EGrid = EGrid {
>       eWidth::Int, eHeight::Int,
>       faces::Field, hedges::Field, vedges::Field, vertices::Field
>   }

> g2e (Grid w h f) = EGrid w h
>   f
>   (\x y -> f (x-1) y `max` f x y)
>   (\x y -> f x (y-1) `max` f x y)
>   (\x y -> f (x-1) (y-1) `max` f (x-1) y `max` f x (y-1) `max` f x y)

> fsum (EGrid w h f _ _ _) = gsum (Grid w h f)
> esum (EGrid w h _ e f _) = gsum (Grid (w+1) h e)+gsum (Grid w (h+1) f)
> vsum (EGrid w h _ _ _ v) = gsum (Grid (w+1) (h+1) v)

> point x y = Grid (x+1) (y+1) (\x0 y0 -> if (x0, y0) == (x,y) then 1 else 0)

> digit 0 = ' '
> digit n = chr (48+n)

> instance Eq Grid where
>   _ == _ = False

> instance Show Grid where
>   show (Grid w h f) = concat [[digit (f x y) | x <- [0..w-1]] ++ "\n" | y <- [0..h-1]]

> instance Num Grid where
>   Grid w0 h0 f0 + Grid w1 h1 f1 = Grid (w0 `max` w1) (h0 `max` h1) (\x y -> f0 x y + f1 x y)

> scale n (Grid w h f) = Grid (w*n) (h*n) (\x y -> f (x `div` n) (y `div` n))

> euler g = vsum g - esum g + fsum g
> semiperim g = esum g - 2*fsum g
> area g = fsum g

\def\bloba{(0,0) circle (2)}
\def\blobb{(2,0) circle (1.7)}
\def\blobc{(3,1.5) circle (1)}
\def\blobd{(1.5,-0.5) ellipse (3 and 0.5)}
\def\blobe{(-4,-4) .. controls ++(2,0) .. ++(1,0) .. controls ++(0,1) .. ++(-1,1) .. controls ++(1,1) .. ++(0,1)
  .. controls ++(-1,0) .. ++(-1,0) .. controls ++(-1,-1) .. ++(0,-1) .. controls ++(1,1) .. ++(1,0)}

\begin{tikzpicture}

\begin{scope}
\fill[opacity = 0.4, fill = red] \bloba;
\fill[opacity = 0.4, fill = blue] \blobb;
\fill[opacity = 0.4, fill = green] \blobc;
\fill[opacity = 0.4, fill = yellow] \blobd;
\fill[opacity = 0.4, fill = cyan] \blobe;
\end{scope}

\end{tikzpicture}

Does this work?

> grid f = array ((0, 0), (19, 19)) [((x, y), f (x, y))| x <- [0..19], y <- [0..19]]

> unGrid b a (x, y) = if x>=0 && x<=19 && y>=0 && y<=19 then a!(x, y) else b

> faceIn f (x, y) = f (x, y)
> hEdgeIn f (x, y) = f (x, y) || f (x, y-1)
> vEdgeIn f (x, y) = f (x, y) || f (x-1, y)
> vertexIn f (x, y) = f (x, y) || f (x-1, y) || f (x, y-1) || f (x-1, y-1)

> count f xs = length (filter f xs)

> {-
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
> -}

\end{document}
