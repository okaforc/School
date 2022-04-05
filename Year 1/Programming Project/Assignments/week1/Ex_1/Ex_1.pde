void settings() {
    // creates a full screen window
	fullScreen();
}

// global variables
float xPos1, yPos1;
float xPos2, yPos2;
float xPos3, yPos3;
float rectWidth, rectHeight;

void setup() {
    xPos1 = width/2;
    yPos1 = height/2;
    xPos2 = width/2;
    yPos2 = height/2;
    xPos3 = width/2;
    yPos3 = height/2;
    rectWidth = 30;
    rectHeight = 30;
}
void draw() {
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

