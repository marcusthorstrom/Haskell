import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages

import Expr



canWidth  = 300
canHeight = 300

--increase :: Elem -> IO()
modify el op = do
                value <- (getProp el "value")
                let val = (read (value) :: Double) in
                  setProp el "value" (show(op val))

readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw f s c = do
                  Just val <- getValue f
                  Just scale <- getValue s
                  let Just expr = (readExpr val) in
                    if (read scale :: Double) /= 0.0 then
                      let ps = points expr (read scale :: Double) (canWidth, canHeight) in
                      ( render c ( color (RGB 0 0 0) (stroke (path ps) ) ) )
                    else
                      print "Not available"
                    --print (show ps)

readAndDrawDerivat :: Elem -> Elem -> Canvas -> IO ()
readAndDrawDerivat f s c = do
                  Just val <- getValue f
                  Just scale <- getValue s
                  let Just expr = (readExpr val) in
                    if (read scale :: Double) /= 0.0 then
                      let deriv = differentiate expr in
                      let ps = points deriv (read scale :: Double) (canWidth, canHeight) in
                      ( render c ( color (RGB 255 0 0) (stroke (path ps) ) ) )
                    else
                      print "Not available"


main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    scaleV  <- mkInput 10 "2.0"              -- The formula input
    zoomIn  <- mkButton "+"
    zoomOut <- mkButton "-"
    draw    <- mkButton "Draw graph"         -- The draw button
    diff    <- mkButton "Differentiate graph"         -- The diff button

      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    scale <- mkDiv
    differ <- mkDiv
    row formula [fx,input]
    row scale [zoomIn, scaleV, zoomOut]
    row differ [diff]
    column documentBody [canvas,scale,formula,draw, differ]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input scaleV can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input scaleV can
    -- "Enter" key has code 13

    onEvent zoomIn Click $ \_ -> modify scaleV (+1)
    onEvent zoomIn Click $ \_ -> readAndDraw input scaleV can
    onEvent zoomOut Click $ \_ -> modify scaleV (subtract 1)
    onEvent zoomOut Click $ \_ -> readAndDraw input scaleV can


    onEvent diff Click $ \_ -> readAndDrawDerivat input scaleV can



-- H
--        Expr    Scale     W, H        Points
points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = [ ((x+ (fromIntegral w/2)), ((eval e x) + (fromIntegral h/2))) | x <- [(-bound), -bound+s..bound]]
    where bound = (s* fromIntegral w)/2
