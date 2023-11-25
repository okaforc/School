r = 200; g = 255; b = 227
with open("h.txt", "w") as f:
    for i in range(3, 10):
        for j in range(0, 10, 2):
            f.write("""(translate (point 0 0), (imageMask (translate (point 0 (-0.%s%s)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0.25 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 0.5), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (%s, %s, %s))),
(translate (point 0 0), (imageMask (translate (point 0 (-0.%s%s)) <++> rect 1 0.1 (1, 1, 1)) (translate (point (-0.25) 0.25) <+> rotate (deg2Rad 180) <+> scale (point 0.5 (-0.5)), (polygon [ point 0 0, point 0 1, point 3 0, point 0 0], (215, 195, 215))), (%s, %s, %s))),
(translate (point 0 0), (imageMask (translate (point 0 (-0.%s%s)) <++> rect 1 0.1 (1, 1, 1)) (translate (point 0 (-0.5)) <++> rect 0.25 0.75 (215, 195, 215)), (%s, %s, %s))),
""" % (i, j, r, g, b, i, j, r, g, b, i, j, r, g, b))
            r -= 1
            g -= 1
            b -= 1