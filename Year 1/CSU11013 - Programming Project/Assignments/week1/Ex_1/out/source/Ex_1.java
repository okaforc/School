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

public class Ex_1 extends PApplet {

public void settings() {
    // creates a full screen window
	fullScreen();
}

// global variables
float xPos1, yPos1;
float xPos2, yPos2;
float xPos3, yPos3;
float rectWidth, rectHeight;

public void setup() {
    xPos1 = width/2;
    yPos1 = height/2;
    xPos2 = width/2;
    yPos2 = height/2;
    xPos3 = width/2;
    yPos3 = height/2;
    rectWidth = 30;
    rectHeight = 30;
}
public void draw() {
    // draws background over each frame to avoid overlap
	background(128,128,128);

    // different directions and speeds of squares
    xPos1 += 5;
    xPos2 += -5;
	yPos3 += 5;
	
	
	fill(255,0,0);
	rect(xPos1, yPos1, rectWidth, rectHeight);

	fill(0,255,0);
	rect(xPos2, yPos2, rectWidth, rectHeight);

	fill(0,0,255);
	rect(xPos3, yPos3, rectWidth, rectHeight);
	
}

  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "Ex_1" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
