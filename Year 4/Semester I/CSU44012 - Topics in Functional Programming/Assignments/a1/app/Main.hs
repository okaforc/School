{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Shapes
import Render (render,defaultWindow)

import Data.Text.Lazy
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai.Middleware.Static

-- Shear Example
exampleDrawing =  [ (shear (point 0.5 0.25) <+> translate (point 0 1), (circle, (100, 100, 0))) ]
-- Polygon Example
exampleDrawing2 =  [
    -- pentagon
    (scale (point 0.5 0.5) <+> translate (point (-2) (-1.5)), (polygon [
        point 0.5 3,
        point 1.5 2.125,
        point 1 1,
        point 0 1,
        point (-0.5) 2.125,
        point 0.5 3
        ], (200, 255, 170))),

    -- sheared hexagon
    (shear (point 0.5 0.5) <+> scale (point 0.25 0.25) <+> translate (point 0 0), (polygon [
        point 0 3,
        point 1 3,
        point 1.75 2,
        point 1 1,
        point 0 1,
        point (-0.75) 2,
        point 0 3
        ], (255, 200, 200))),

    -- thingy
    (scale (point 0.5 0.5) <+> translate (point 0.6 2.5), (polygon [
        point 0 0,
        point 0.5 0,
        point 1 (-2),
        point 2 (-4),
        point 0.25 (-5),
        point (-1) (-4),
        point (-2) (-2),
        point 0 0
        ], (255, 150, 255)))
    ]

-- Mask Example
exampleDrawing3 =  [
    (scale (point 0.5 0.25) <+> translate (point (-1.2) 0.4), (circle, (255, 200, 200))),
    (scale (point 0.5 0.25) <+> translate (point (-1.2) (-0.4)), (circle, (255, 200, 255))),

    (translate (point 0 0),
        (imageMask
            (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), (circle, (255, 200, 200)))
            (scale (point 0.5 0.25) <+> translate (point 1.2 (-0.4)), (circle, (255, 200, 255))),
        (200, 255, 227)))
    ]

-- Complex Example 1 (SCP Foundation logo (https://upload.wikimedia.org/wikipedia/commons/e/ec/SCP_Foundation_%28emblem%29.svg))
exampleDrawing4 =  [
    -- arrows
    -- -- arrow heads
    (translate (point 0 0.05) <+> rotate (deg2Rad 90) <+> scale (point 0.1 0.1), (polygon [
        point 0 0,
        point 1 (-1.25),
        point (-1) (-1.25),
        point 0 0
    ], (1, 1, 1))),

    (translate (point (-0.05) 0) <+> rotate (deg2Rad 210) <+> scale (point 0.1 0.1), (polygon [
        point 0 0,
        point 1 (-1.25),
        point (-1) (-1.25),
        point 0 0
    ], (1, 1, 1))),

    (translate (point 0.05 0) <+> rotate (deg2Rad 330) <+> scale (point 0.1 0.1), (polygon [
        point 0 0,
        point 1 (-1.25),
        point (-1) (-1.25),
        point 0 0
    ], (1, 1, 1))),

    -- -- arrow bodies
    translate (point (-0.3) (-0.15)) <+> rotate (deg2Rad 30) <+> scale (point 0.05 0.05) <++> rect 1 4.5 (1, 1, 1),
    translate (point 0.3 (-0.15)) <+> rotate (deg2Rad (-30)) <+> scale (point 0.05 0.05) <++> rect 1 4.5 (1, 1, 1),
    translate (point 0 0.375) <+> scale (point 0.05 0.05) <++> rect 1 4.5 (1, 1, 1),

    -- inner circle
    ellipse 0.4 0.4 (255, 255, 255),
    ellipse 0.5 0.5 (1, 1, 1),

    -- outer circle
    ellipse 0.8 0.8 (255, 255, 255),

    -- little nubs
    translate (point (-0.55) (-0.30)) <+> rotate (deg2Rad 30) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255),
    translate (point 0.55 (-0.30)) <+> rotate (deg2Rad (-30)) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255),
    translate (point 0 0.65) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255)
    ]

-- a glass of liquid with a paper straw sticking out
exampleDrawing5 = [
    -- straw
    translate (point 0.35 (-0.3)) <+> rotate (deg2Rad 63) <++> rect 0.1 1 (255, 255, 255),
    -- boba things
    translate (point 0 (-0.7)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point (-0.3) (-0.73)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point 0.3 (-0.68)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point 0.2 (-0.9)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point (-0.2) (-0.9)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point (-0.1) (-1.1)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point 0.1 (-1.1)) <++> ellipse 0.1 0.1 (107, 43, 0),
    -- drink
    {- mask gradients for drink -}
        (translate (point 0 0), (imageMask (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (200, 255, 227))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (200, 255, 227))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (200, 255, 227))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.32)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (199, 254, 226))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.32)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (199, 254, 226))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.32)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (199, 254, 226))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.34)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (198, 253, 225))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.34)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (198, 253, 225))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.34)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (198, 253, 225))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.36)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (197, 252, 224))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.36)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (197, 252, 224))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.36)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (197, 252, 224))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.38)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (196, 251, 223))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.38)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (196, 251, 223))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.38)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (196, 251, 223))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.40)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (195, 250, 222))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.40)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (195, 250, 222))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.40)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (195, 250, 222))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.42)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (194, 249, 221))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.42)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (194, 249, 221))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.42)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (194, 249, 221))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.44)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (193, 248, 220))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.44)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (193, 248, 220))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.44)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (193, 248, 220))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.46)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (192, 247, 219))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.46)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (192, 247, 219))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.46)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (192, 247, 219))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.48)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (191, 246, 218))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.48)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (191, 246, 218))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.48)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (191, 246, 218))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.50)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (190, 245, 217))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.50)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (190, 245, 217))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.50)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (190, 245, 217))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.52)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (189, 244, 216))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.52)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (189, 244, 216))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.52)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (189, 244, 216))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.54)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (188, 243, 215))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.54)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (188, 243, 215))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.54)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (188, 243, 215))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.56)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (187, 242, 214))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.56)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (187, 242, 214))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.56)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (187, 242, 214))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.58)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (186, 241, 213))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.58)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (186, 241, 213))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.58)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (186, 241, 213))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.60)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (185, 240, 212))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.60)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (185, 240, 212))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.60)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (185, 240, 212))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.62)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (184, 239, 211))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.62)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (184, 239, 211))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.62)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (184, 239, 211))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.64)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (183, 238, 210))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.64)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (183, 238, 210))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.64)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (183, 238, 210))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.66)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (182, 237, 209))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.66)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (182, 237, 209))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.66)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (182, 237, 209))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.68)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (181, 236, 208))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.68)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (181, 236, 208))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.68)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (181, 236, 208))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.70)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (180, 235, 207))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.70)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (180, 235, 207))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.70)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (180, 235, 207))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.72)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (179, 234, 206))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.72)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (179, 234, 206))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.72)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (179, 234, 206))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.74)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (178, 233, 205))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.74)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (178, 233, 205))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.74)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (178, 233, 205))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.76)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (177, 232, 204))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.76)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (177, 232, 204))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.76)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (177, 232, 204))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.78)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (176, 231, 203))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.78)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (176, 231, 203))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.78)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (176, 231, 203))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.80)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (175, 230, 202))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.80)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (175, 230, 202))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.80)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (175, 230, 202))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.82)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (174, 229, 201))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.82)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (174, 229, 201))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.82)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (174, 229, 201))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.84)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (173, 228, 200))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.84)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (173, 228, 200))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.84)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (173, 228, 200))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.86)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (172, 227, 199))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.86)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (172, 227, 199))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.86)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (172, 227, 199))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.88)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (171, 226, 198))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.88)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (171, 226, 198))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.88)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (171, 226, 198))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.90)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (170, 225, 197))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.90)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (170, 225, 197))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.90)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (170, 225, 197))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.92)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (169, 224, 196))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.92)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (169, 224, 196))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.92)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (169, 224, 196))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.94)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (168, 223, 195))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.94)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (168, 223, 195))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.94)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (168, 223, 195))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.96)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (167, 222, 194))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.96)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (167, 222, 194))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.96)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (167, 222, 194))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (166, 221, 193))),

        (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.6 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.6 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),
        (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.6 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (166, 221, 193))),

    -- glass
    (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [
        point 0 0,
        point 0 1,
        point 3 0,
        point 0 0
    ], (215, 195, 215))),
    (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [
        point 0 0,
        point 0 1,
        point 3 0,
        point 0 0
    ], (215, 195, 215))),
    translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215),

    -- bg
    rect 10 10 (255, 235, 235)
    ]

main = do
    -- uncomment before submission.
  render "static/Image1.png" defaultWindow exampleDrawing
  render "static/Image2.png" defaultWindow exampleDrawing2
  render "static/Image3.png" defaultWindow exampleDrawing3
  render "static/Image4.png" defaultWindow exampleDrawing4
  render "static/Image5.png" defaultWindow exampleDrawing5
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (addBase "static")
    S.get "/" $ S.html home
    S.get "/display/Image1" $ do
        S.html imgResponse1
    S.get "/display/Image2" $ do
        S.html imgResponse2
    S.get "/display/Image3" $ do
        S.html imgResponse3
    S.get "/display/Image4" $ do
        S.html imgResponse4
    S.get "/display/Image5" $ do
        S.html imgResponse5


home :: Text
home = do R.renderHtml $ do
                  H.title "CSU44012 A1"
                  H.h1 "CSU44012 Assignment 1" H.! A.style "text-align:center;width:100%;"
                  -- idk how else to push it down
                  H.br
                  H.br
                  H.br
                  H.br
                  H.br
                  H.br
                  H.footer "Iris Onwa - 20332400" H.! A.style "margin: 0 auto;position: absolute; bottom: 10;text-align:center;width: 98%;"
                  H.div H.! A.style "float:left;width:19.5%;padding:2px" $
                    H.a H.! A.href "display/Image1" $ 
                        H.img H.! A.src "../Image1.png" H.! A.alt "Image 1" H.! A.style "margin:auto;width:100%;" <>
                        H.br
                    <> H.h3 "Shear Example" H.! A.style "text-align:center"
                  H.div H.! A.style "float:left;width:19.5%;padding:2px" $
                    H.a H.! A.href "display/Image2" $
                        H.img H.! A.src "../Image2.png" H.! A.alt "Image 2" H.! A.style "margin:auto;width:100%;" <>
                        H.br
                    <> H.h3 "Polygon Example" H.! A.style "text-align:center"
                  H.div H.! A.style "float:left;width:19.5%;padding:2px" $
                    H.a H.! A.href "display/Image3" $ 
                        H.img H.! A.src "../Image3.png" H.! A.alt "Image 3" H.! A.style "margin:auto;width:100%;" <>
                        H.br
                    <> H.h3 "Mask Example" H.! A.style "text-align:center"
                  H.div H.! A.style "float:left;width:19.5%;padding:2px" $
                    H.a H.! A.href "display/Image4" $ 
                        H.img H.! A.src "../Image4.png" H.! A.alt "Image 4" H.! A.style "margin:auto;width:100%;" <>
                        H.br
                    <> H.h3 "Complex Example 1" H.! A.style "text-align:center"
                  H.div H.! A.style "float:left;width:19.5%;padding:2px" $
                    H.a H.! A.href "display/Image5" $ 
                        H.img H.! A.src "../Image5.png" H.! A.alt "Image 5" H.! A.style "margin:auto;width:100%;" <>
                        H.br
                    <> H.h3 "Complex Example 2" H.! A.style "text-align:center"

smth :: H.Html
smth = H.img H.! A.src "../Image1.png" H.! A.alt "Image 1" H.! A.style "padding = 10px;"

goHome :: H.Html
goHome = H.a "Go back" H.! A.href "/"
displayOutput1 :: H.Html
displayOutput1 = H.a "Shear Example" H.! A.href "display/Image1"
displayOutput2 :: H.Html
displayOutput2 = H.a "Polygon Example" H.! A.href "display/Image2"
displayOutput3 :: H.Html
displayOutput3 = H.a "Mask Example" H.! A.href "display/Image3"
displayOutput4 :: H.Html
displayOutput4 = H.a "Complex Example 1" H.! A.href "display/Image4"
displayOutput5 :: H.Html
displayOutput5 = H.a "Complex Example 2" H.! A.href "display/Image5"

imgResponse :: Text -> Text
imgResponse img =
    do R.renderHtml $ do
        H.title "CSU44012 A1"
        H.h1 "You chose " >> H.h1 (H.toMarkup img)
        H.img H.! A.src ("../" <> H.toValue img <> ".png") H.! A.alt "circle :3"
        H.br
        H.br
        H.button goHome


imgResponse1 :: Text
imgResponse1 =
    do R.renderHtml $ do
        H.title "CSU44012 A1 - Shear"
        H.h1 "You chose Image 1"
        H.h2 "Shear Example"
        H.img H.! A.src "../Image1.png" H.! A.alt "A circle sheared" H.! A.style "float:left;"
        H.div H.! A.style "margin:auto;width:60%;padding-left:8px;padding-right:8px;overflow:auto;height:500px;white-space:pre-wrap;border:3px solid black;" $
            H.h3 "Code used to generate this:" <>
            H.code "(shear (point 0.5 0.25) <+> translate (point 0 1), (circle, (100, 100, 0)))"
        H.br
        H.button goHome H.! A.style "float:left;"

imgResponse2 :: Text
imgResponse2 =
    do R.renderHtml $ do
        H.title "CSU44012 A1 - Polygon"
        H.h1 "You chose Image 2"
        H.h2 "Polygon Example"
        H.img H.! A.src "../Image2.png" H.! A.alt "Two polygons overlaid on top of each other." H.! A.style "float:left;"
        H.div H.! A.style "margin:auto;width:60%;padding-left:8px;padding-right:8px;overflow:auto;height:500px;white-space:pre-wrap;border:3px solid black;" $
            H.h3 "Code used to generate this:" <>
            H.code "-- pentagon\n\
\(scale (point 0.5 0.5) <+> translate (point (-2) (-1.5)), (polygon [\n\
\    point 0.5 3,\n\
\    point 1.5 2.125,\n\
\    point 1 1,\n\
\    point 0 1,\n\
\    point (-0.5) 2.125,\n\
\    point 0.5 3\n\
\    ], (200, 255, 170))),\n\
\\n\
\-- sheared hexagon\n\
\(shear (point 0.5 0.5) <+> scale (point 0.25 0.25) <+> translate (point 0 0), (polygon [\n\
\    point 0 3,\n\
\    point 1 3,\n\
\    point 1.75 2,\n\
\    point 1 1,\n\
\    point 0 1,\n\
\    point (-0.75) 2,\n\
\    point 0 3\n\
\    ], (255, 200, 200))),\n\
\\n\
\-- thingy\n\
\(scale (point 0.5 0.5) <+> translate (point 0.6 2.5), (polygon [\n\
\    point 0 0,\n\
\    point 0.5 0,\n\
\    point 1 (-2),\n\
\    point 2 (-4),\n\
\    point 0.25 (-5),\n\
\    point (-1) (-4),\n\
\    point (-2) (-2),\n\
\    point 0 0\n\
\    ], (255, 150, 255)))"
        H.br
        H.button goHome H.! A.style "float:left;"

imgResponse3 :: Text
imgResponse3 =
    do R.renderHtml $ do
        H.title "CSU44012 A1 - Mask"
        H.h1 "You chose Image 3"
        H.h2 "Mask Example"
        H.img H.! A.src "../Image3.png" H.! A.alt "Two ellipses overlaid on top of each other on the left. On the right, a mask of where the ellipses overlapped." H.! A.style "float:left;"
        H.div H.! A.style "margin:auto;width:60%;padding-left:8px;padding-right:8px;overflow:auto;height:500px;white-space:pre-wrap;border:3px solid black;" $
            H.h3 "Code used to generate this:" <>
            H.code "(scale (point 0.5 0.25) <+> translate (point (-1.2) 0.4), (circle, (255, 200, 200))), \n\
\(scale (point 0.5 0.25) <+> translate (point (-1.2) (-0.4)), (circle, (255, 200, 255))), \n\
\ \n\
\(translate (point 0 0),  \n\
\    (imageMask  \n\
\        (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), (circle, (255, 200, 200)))  \n\
\        (scale (point 0.5 0.25) <+> translate (point 1.2 (-0.4)), (circle, (255, 200, 255))),  \n\
\    (200, 255, 227)))"
        H.br
        H.button goHome H.! A.style "float:left;"

imgResponse4 :: Text
imgResponse4 =
    do R.renderHtml $ do
        H.title "CSU44012 A1 - Complex 1"
        H.h1 "You chose Image 4"
        H.h2 "Complex Example 1"
        H.img H.! A.src "../Image4.png" H.! A.alt "The logo of the fictional SCP Foundation." H.! A.style "float:left;"
        H.div H.! A.style "margin:auto;width:60%;padding-left:8px;padding-right:8px;overflow:auto;height:500px;white-space:pre-wrap;border:3px solid black;" $
            H.h3 "Code used to generate this:" <>
            H.code "-- arrows\n\
\-- -- arrow heads\n\
\(translate (point 0 0.05) <+> rotate (deg2Rad 90) <+> scale (point 0.1 0.1), (polygon [\n\
\    point 0 0,\n\
\    point 1 (-1.25),\n\
\    point (-1) (-1.25),\n\
\    point 0 0\n\
\], (1, 1, 1))),\n\
\\n\
\(translate (point (-0.05) 0) <+> rotate (deg2Rad 210) <+> scale (point 0.1 0.1), (polygon [\n\
\    point 0 0,\n\
\    point 1 (-1.25),\n\
\    point (-1) (-1.25),\n\
\    point 0 0\n\
\], (1, 1, 1))),\n\
\\n\
\(translate (point 0.05 0) <+> rotate (deg2Rad 330) <+> scale (point 0.1 0.1), (polygon [\n\
\    point 0 0,\n\
\    point 1 (-1.25),\n\
\    point (-1) (-1.25),\n\
\    point 0 0\n\
\], (1, 1, 1))),\n\
\\n\
\-- -- arrow bodies\n\
\translate (point (-0.3) (-0.15)) <+> rotate (deg2Rad 30) <+> scale (point 0.05 0.05) <++> rect 1 4.5 (1, 1, 1),\n\
\translate (point 0.3 (-0.15)) <+> rotate (deg2Rad (-30)) <+> scale (point 0.05 0.05) <++> rect 1 4.5 (1, 1, 1),\n\
\translate (point 0 0.375) <+> scale (point 0.05 0.05) <++> rect 1 4.5 (1, 1, 1),\n\
\\n\
\-- inner circle\n\
\ellipse 0.4 0.4 (255, 255, 255),\n\
\ellipse 0.5 0.5 (1, 1, 1),\n\
\\n\
\-- outer circle\n\
\ellipse 0.8 0.8 (255, 255, 255),\n\
\\n\
\-- little nubs\n\
\translate (point (-0.55) (-0.30)) <+> rotate (deg2Rad 30) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255),\n\
\translate (point 0.55 (-0.30)) <+> rotate (deg2Rad (-30)) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255),\n\
\translate (point 0 0.65) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255)"
        H.br
        H.a "SCP Foundation logo" H.! A.href "https://upload.wikimedia.org/wikipedia/commons/e/ec/SCP_Foundation_%28emblem%29.svg" H.! A.target "_blank"
        H.br
        H.br
        H.button goHome H.! A.style "float:left;"

imgResponse5 :: Text
imgResponse5 =
    do R.renderHtml $ do
        H.title "CSU44012 A1 - Complex 2"
        H.h1 "You chose Image 5"
        H.h2 "Complex Example 2"
        H.img H.! A.src "../Image5.png" H.! A.alt "A cup of bubble tea. The tea is bluish-green in colour, with chocolate balls at the bottom and a long, wide white straw sticking out of the cup." H.! A.style "float:left;"
        H.div H.! A.style "margin:auto;width:60%;padding-left:8px;padding-right:8px;overflow:auto;height:500px;white-space:pre-wrap;border:3px solid black;" $
            H.h3 "Code used to generate this:" <>
            H.code "-- straw\n\
\translate (point 0.35 (-0.3)) <+> rotate (deg2Rad 63) <++> rect 0.1 1 (255, 255, 255),\n\
\-- boba things\n\
\translate (point 0 (-0.7)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\translate (point (-0.3) (-0.73)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\translate (point 0.3 (-0.68)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\translate (point 0.2 (-0.9)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\translate (point (-0.2) (-0.9)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\translate (point (-0.1) (-1.1)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\translate (point 0.1 (-1.1)) <++> ellipse 0.1 0.1 (107, 43, 0),\n\
\-- drink\n\
\{- mask gradients for drink -}\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (200, 255, 227))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (200, 255, 227))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (200, 255, 227))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.32)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (199, 254, 226))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.32)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (199, 254, 226))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.32)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (199, 254, 226))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.34)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (198, 253, 225))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.34)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (198, 253, 225))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.34)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (198, 253, 225))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.36)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (197, 252, 224))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.36)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (197, 252, 224))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.36)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (197, 252, 224))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.38)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (196, 251, 223))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.38)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (196, 251, 223))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.38)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (196, 251, 223))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.40)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (195, 250, 222))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.40)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (195, 250, 222))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.40)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (195, 250, 222))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.42)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (194, 249, 221))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.42)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (194, 249, 221))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.42)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (194, 249, 221))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.44)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (193, 248, 220))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.44)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (193, 248, 220))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.44)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (193, 248, 220))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.46)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (192, 247, 219))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.46)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (192, 247, 219))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.46)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (192, 247, 219))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.48)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (191, 246, 218))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.48)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (191, 246, 218))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.48)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (191, 246, 218))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.50)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (190, 245, 217))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.50)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (190, 245, 217))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.50)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (190, 245, 217))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.52)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (189, 244, 216))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.52)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (189, 244, 216))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.52)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (189, 244, 216))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.54)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (188, 243, 215))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.54)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (188, 243, 215))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.54)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (188, 243, 215))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.56)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (187, 242, 214))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.56)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (187, 242, 214))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.56)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (187, 242, 214))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.58)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (186, 241, 213))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.58)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (186, 241, 213))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.58)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (186, 241, 213))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.60)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (185, 240, 212))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.60)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (185, 240, 212))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.60)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (185, 240, 212))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.62)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (184, 239, 211))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.62)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (184, 239, 211))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.62)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (184, 239, 211))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.64)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (183, 238, 210))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.64)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (183, 238, 210))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.64)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (183, 238, 210))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.66)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (182, 237, 209))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.66)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (182, 237, 209))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.66)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (182, 237, 209))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.68)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (181, 236, 208))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.68)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (181, 236, 208))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.68)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (181, 236, 208))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.70)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (180, 235, 207))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.70)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (180, 235, 207))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.70)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (180, 235, 207))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.72)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (179, 234, 206))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.72)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (179, 234, 206))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.72)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (179, 234, 206))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.74)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (178, 233, 205))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.74)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (178, 233, 205))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.74)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (178, 233, 205))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.76)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (177, 232, 204))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.76)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (177, 232, 204))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.76)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (177, 232, 204))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.78)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (176, 231, 203))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.78)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (176, 231, 203))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.78)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (176, 231, 203))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.80)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (175, 230, 202))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.80)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (175, 230, 202))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.80)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (175, 230, 202))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.82)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (174, 229, 201))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.82)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (174, 229, 201))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.82)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (174, 229, 201))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.84)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (173, 228, 200))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.84)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (173, 228, 200))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.84)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (173, 228, 200))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.86)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (172, 227, 199))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.86)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (172, 227, 199))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.86)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (172, 227, 199))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.88)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (171, 226, 198))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.88)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (171, 226, 198))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.88)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (171, 226, 198))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.90)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (170, 225, 197))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.90)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (170, 225, 197))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.90)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (170, 225, 197))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.92)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (169, 224, 196))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.92)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (169, 224, 196))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.92)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (169, 224, 196))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.94)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (168, 223, 195))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.94)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (168, 223, 195))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.94)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (168, 223, 195))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.96)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (167, 222, 194))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.96)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (167, 222, 194))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.96)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (167, 222, 194))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (166, 221, 193))),\n\
\\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.6 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.6 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (166, 221, 193))),\n\
\    (translate (point 0 0), (imageMask (translate (point 0 (-0.98)) <++> rect 1 0.6 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (166, 221, 193))),\n\
\\n\
\-- glass\n\
\(translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [\n\
\    point 0 0,\n\
\    point 0 1,\n\
\    point 3 0,\n\
\    point 0 0\n\
\], (215, 195, 215))),\n\
\(translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [\n\
\    point 0 0,\n\
\    point 0 1,\n\
\    point 3 0,\n\
\    point 0 0\n\
\], (215, 195, 215))),\n\
\translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215),\n\
\\n\
\-- bg\n\
\rect 10 10 (255, 235, 235)"
        H.br
        H.button goHome H.! A.style "float:left;"

