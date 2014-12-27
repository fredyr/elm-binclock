module BinClock where

import Graphics.Element (Element, container, midTop)
import Html (..)
import Html.Attributes (..)
import Signal
import Window
import List
import Bitwise
import Time (..)
import Date 
    
cellCol : Bool -> Html
cellCol bit =
    let color = if bit then "light" else "dark"
    in div [class ("cell" ++ " " ++ color)] []

cellVal : Int -> Html
cellVal digit = div [class "cell"]
                [text (toString digit)]                
    
type alias BitNum = (Int, List Bool)

column : BitNum -> Html
column (digit, bits) =
    let cells = (List.map cellCol bits) ++ [cellVal digit]
    in div [class "col"] cells

columns : List BitNum -> Html
columns cs =
    let cols = List.map column cs
    in div [class "colpair"] cols  

legend : List Int -> Html
legend digits =
    let cells = List.map cellVal digits
    in div [class "col legend"] cells
       
decimalParts : Int -> List Int
decimalParts n = [n // 10, rem n 10]

toBitNum : Int -> BitNum
toBitNum n =
    let masks = List.map2 Bitwise.and [8, 4, 2, 1] (List.repeat 4 n)
        setp  = \x -> x > 0
        bits  = List.map setp masks
    in (n, bits)

view : Time -> Html
view t =
    let conv = \x -> x |> decimalParts |> (List.map toBitNum)
        d = Date.fromTime t
        cols = [Date.hour d, Date.minute d, Date.second d]
               |> List.map conv
               |> List.map columns
    in div [] ([legend [8,4,2,1]] ++ cols)
       
-- Just stole the kickstarting code from the TODO example
main : Signal Element
main = Signal.map2 scene (every second) Window.dimensions

scene : Time -> (Int,Int) -> Element
scene t (w,h) =
    container w h midTop (toElement w h (view t))
