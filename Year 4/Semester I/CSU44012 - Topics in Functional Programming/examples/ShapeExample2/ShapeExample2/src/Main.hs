module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ shear (point 0.5 0.25) <+> translate (point 0 1) <++> ellipse 1 1 (255, 100, 10) ]
-- exampleDrawing2 =  [ (scale (point 0.5 0.25) <+> translate [(point 0 1), (point 0 2), (point 1 1), (point 2 2)], polygon) ]
exampleDrawing2 =  [
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
    -- (translate (point 0 0), (imageMask (ellipse 0.3 0.3 (255, 255, 255)) (ellipse 0.4 0.4 (255, 255, 255)), (255, 200, 0)))
    ellipse 0.4 0.4 (255, 255, 255),
    ellipse 0.5 0.5 (1, 1, 1),

    -- outer circle
    -- (translate (point 0 0), (imageMask (ellipse 0.3 0.3 (255, 255, 255)) (ellipse 0.4 0.4 (255, 255, 255)), (255, 200, 0)))
    ellipse 0.8 0.8 (255, 255, 255),
    -- ellipse 1 1 (1, 1, 1),

    -- little nubs
    translate (point (-0.55) (-0.30)) <+> rotate (deg2Rad 30) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255),
    translate (point 0.55 (-0.30)) <+> rotate (deg2Rad (-30)) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255),
    translate (point 0 0.65) <+> scale (point 0.05 0.05) <++> rect 5 5 (255, 255, 255)
    ]
exampleDrawing3 =  [ translate (point 0.6 0.1) <++> ellipse 0.5 0.25 (255, 200, 200) ]

exampleDrawing4 =  [
    (scale (point 0.5 0.25) <+> translate (point (-1.2) 0.4), (circle, (255, 200, 200))),
    (scale (point 0.5 0.25) <+> translate (point (-1.2) (-0.4)), (circle, (255, 200, 255))),

    (translate (point 0 0), (imageMask (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), (circle, (255, 200, 200))) (scale (point 0.5 0.25) <+> translate (point 1.2 (-0.4)), (circle, (255, 200, 255))), (200, 255, 227)))
    ]

exampleDrawing5 = [
    -- mask shapes for liquid
    -- -- straw
    translate (point 0.35 (-0.3)) <+> rotate (deg2Rad 63) <++> rect 0.1 1 (255, 255, 255),
    -- -- boba things
    translate (point 0 (-0.7)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point (-0.3) (-0.73)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point 0.3 (-0.68)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point 0.2 (-0.9)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point (-0.2) (-0.9)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point (-0.1) (-1.1)) <++> ellipse 0.1 0.1 (107, 43, 0),
    translate (point 0.1 (-1.1)) <++> ellipse 0.1 0.1 (107, 43, 0),
    -- -- drink
    -- (translate (point 0 0), (imageMask (translate (point 0 (-0.75)) <++> rect 1 0.6 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (200, 255, 227))),
    -- (translate (point 0 0), (imageMask (translate (point 0 (-0.75)) <++> rect 1 0.6 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (200, 255, 227))),
    -- (translate (point 0 0), (imageMask (translate (point 0 (-0.75)) <++> rect 1 0.6 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (200, 255, 227))),

    -- (translate (point 0 0), (
    --     imageMask 
    --     (translate (point 0 (-0.30)) <++> rect 1 0.1 (1, 1, 1)) 
    --     (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 3 0, point 0 1, point 0 0], (215, 195, 215))), 
    --     (200, 255, 227))),








    {- gradient -}
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






    -- rect 1 0.6 (1, 1, 1),
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
    rect 10 10 (255, 235, 235) -- bg
    ]

exampleDrawing6 = [
    (identity, (polygon [
        point 1 0,
        point 1 (-1),
        point 0 (-1),
        point 0 0,
        point 1 0
    ], (255, 255, 255)))
    ]

exampleDrawing7 =  [
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
main = render "output.png" defaultWindow exampleDrawing3
