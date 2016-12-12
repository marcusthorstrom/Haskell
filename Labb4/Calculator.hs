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
                  if (op val) > 0 then
                    setProp el "value" (show(op val))
                  else
                    return ()

readAndDraw :: Elem -> Elem -> Elem -> Canvas -> IO ()
readAndDraw f s d c = do
                  Just val <- getValue f
                  Just scale <- getValue s
                  diff <- (getProp d "checked")
                  let Just expr = (readExpr val) in
                    if (read scale :: Double) /= 0.0 then
                      let ps = points expr (read scale :: Double) (canWidth, canHeight) in
                      ( render c ( color (RGB 0 0 0) (stroke (path ps) ) ) ) >>
                      if diff == "true" then -- Also show the differentiate
                        let dPs = points (differentiate expr) (read scale :: Double) (canWidth, canHeight) in
                          print ( simplify $ differentiate expr) >>
                          ( renderOnTop c ( color (RGB 255 0 0) (stroke (path dPs)) ) )
                      else
                        return ()
                    else
                      return ()
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
    scaleV  <- mkInput 10 "0.04"              -- The formula input
    zoomIn  <- mkButton "+"
    zoomOut <- mkButton "-"
    draw    <- mkButton "Draw graph"         -- The draw button
    diff    <- mkInput 10 "diff"        -- The diff button
    diffB   <- mkButton "Differentiate"

      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    scale <- mkDiv
    row formula [fx,input, diff]
    row scale [zoomOut, scaleV, zoomIn]
    column documentBody [canvas,scale,formula,draw, diffB]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input
    setAttr diff "type" "checkbox"
    setAttr scaleV "type" "hidden"


    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input scaleV diff can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input scaleV diff can
    -- "Enter" key has code 13

    onEvent zoomIn Click $ \_ -> modify scaleV (subtract 0.01)
    onEvent zoomIn Click $ \_ -> readAndDraw input scaleV diff can
    onEvent zoomOut Click $ \_ -> modify scaleV (+ 0.01)
    onEvent zoomOut Click $ \_ -> readAndDraw input scaleV diff can
    onEvent diff Click $ \_ -> readAndDraw input scaleV diff can


    onEvent diffB Click $ \_ -> readAndDrawDerivat input scaleV can



-- H
--        Expr    Scale     W, H        Points

points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = [ (x, evaluate x) | x <- [0..realW] ]
    where
      (realW, realH) = (fromIntegral w, fromIntegral h)
      pxlToReal :: Double -> Double
      pxlToReal x = (x - realW/2) * s
      realToPxl :: Double -> Double
      realToPxl y = (-y / s) + (realH/2)
      evaluate :: Double -> Double
      evaluate x = realToPxl (eval e (pxlToReal x))
