void settings() {
    // creates a full screen window
    fullScreen();
}

// global variables
float xPos1, yPos1, xPos2, yPos2;
float startXPos1, startXPos2;
float speed, rectSize;
void setup() {
    startXPos1 = 0;
    startXPos2 = width;

    xPos1 = startXPos1;
    yPos1 = 1*height/4;

    xPos2 = startXPos2;
    yPos2 = 3*height/4;
    speed = 5;
    rectSize = 100;
}

void draw() {
    // draws background over each frame to avoid overlap
    background(128, 128, 128);
    text(xPos2,10,10);   // rect coordinates
    xPos1 += speed;
    xPos2 -= speed;


    // first square
    fill(128,0,0);
    if(xPos1 < width-rectSize) {
        rect(xPos1, yPos1, rectSize, rectSize);
    } else {
        // draw the amount of the rectangle past width at the left of the screen
        rect(0, yPos1, rectSize-(width-xPos1), rectSize);     
        // draw a new rectangle at the right of the screen without the part that gets cut off
        rect(xPos1, yPos1, width-xPos1, rectSize);
    }
    if (xPos1 >= width) {
        // draw the original rectangle in the start position once the entire rectangle has gone past the screen
        xPos1 = startXPos1;
    }


    // second square
    fill(0,128,0);
    if(xPos2 > 0) {
        rect(xPos2, yPos2, rectSize, rectSize);
    } else {
        rect(width, yPos2, width-rectSize, rectSize);
        rect(xPos2, yPos2, width-(xPos2+rectSize), rectSize);
    }

    if (xPos2 <= 0) {
        xPos2 = startXPos2;
    }

    
}
