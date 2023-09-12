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

public class Ex_2 extends PApplet {

public void settings() {
    // creates a full screen window
    fullScreen();
}

// global variables
float xPos, yPos;
float startXPos;
public void setup() {
    startXPos = width/2;
    xPos = startXPos;
    yPos = height/2;
}

public void draw() {
    // draws background over each frame to avoid overlap
    background(128, 128, 128);
    xPos += 5;

    if (xPos > width) {
        xPos = startXPos;
    }

    rect(xPos, yPos, 50, 50);
}
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "Ex_2" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
