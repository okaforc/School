import processing.core.*; 
import processing.data.*; 
import processing.event.*; 
import processing.opengl.*; 

import java.util.HashMap; 
import java.util.ArrayList; 
import java.io.File; 
import java.io.BufferedReader; 
import java.io.PrintWriter; 
import java.io.InputStream; 
import java.io.OutputStream; 
import java.io.IOException; 

public class Ex_3 extends PApplet {

public void settings() {
    // creates a full screen window
    fullScreen();
}

// global variables
float xPos, yPos, speed;
float startXPos;
float rectSize;
public void setup() {
    startXPos = 0;
    rectSize = 100;
    speed = 5;
    xPos = startXPos;
    yPos = height/2;
}

public void draw() {
    // draws background over each frame to avoid overlap
    background(255);
    fill(0,0,0);
    text(xPos,10,10);   // rect coordinates
    xPos += speed;

    if(xPos < width-rectSize) {
        rect(xPos, yPos, rectSize, rectSize);
    }
    else {
        // draw the amount of the rectangle past width at the left of the screen
        rect(0, yPos, rectSize-(width-xPos), rectSize);     
        // draw a new rectangle at the right of the screen without the part that gets cut off
        rect(xPos, yPos, width-xPos, rectSize);
    }

    if (xPos >= width) {
        // draw the original rectangle once the entire rectangle has gone past the screen
        xPos = startXPos;
    }

}
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "Ex_3" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
