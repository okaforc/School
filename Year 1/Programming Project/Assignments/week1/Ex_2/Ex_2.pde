void settings() {
    // creates a full screen window
    fullScreen();
}

// global variables
float xPos, yPos;
float startXPos;
void setup() {
    startXPos = width/2;
    xPos = startXPos;
    yPos = height/2;
}

void draw() {
    // draws background over each frame to avoid overlap
    background(128, 128, 128);
    xPos += 5;

    if (xPos > width) {
        xPos = startXPos;
    }

    rect(xPos, yPos, 50, 50);
}
