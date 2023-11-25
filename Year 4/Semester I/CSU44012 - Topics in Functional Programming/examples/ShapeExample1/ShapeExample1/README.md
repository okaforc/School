# ShapeExample1

This is a simple drawing system with a design inspired by the Region
and Pan languages that we looked at in class.

The module src/Shapes.hs contains a simple shape language which lets
one specify "Drawings". A Drawing is a collection of shapes and
transformations.  Only a few simple shapes are supported right
now. Each "Shape" is of unit size and is centered on the origin, but
each one can also have a "Transform" applied to it. Each transform can
either be a single simple operation - resize ("Scale") the shape by
some x and y factor, move it away from the origin ("Transform" by some
vector), and rotate it (around the origin) by some angle, or it can be
a compound of two transforms joined together with the "<+>" operator.

The module src/Render.hs takes a Drawing and paints it into a terminal.
A "Window" value defined in this module sets up the
correspondence between the abstract "points" of the Drawing and the
concrete "Pixels" (really Characters) of the output drawing 
(so an 80x25 terminal window might be used top draw only a region of
the drawing from (-1,-1) to (1,1) in the coordinates used in the drawing).

The module uses the some ANSI terminal escape codes to move the print
position in the window ("goto") and prints a "*" character for each
"on" pixel. 
As noted in the source code, the mapping from abstract
coordinates to pixel coordinates is done in a pretty simplistic and
inefficient way, but a typical terminal window is small enough that it
doesn't really matter.

The module src/Main.hs uses the other two modules to draw a sample image
into the terminal. You might want to empty the terminal window before running
it so that you can see what's going on (the 'clear' command on a unix-like
system will do this, or the 'cls' command on a Windows system).
