void settings() {
    // creates a full screen window
    fullScreen();
}

// global variables
float xPos, yPos, speed;
float startXPos;
float rectSize;
void setup() {
    startXPos = 0;
    rectSize = 100;
    speed = 5;
    xPos = startXPos;
    yPos = height/2;
}

void draw() {
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
