class Player {
    float xPos, yPos;
    float pX;                   // paddle's position on the previous frame
    color pColor;
    int MAX_LIVES, lives, pLives;
    
    Player() {
        MAX_LIVES = 3;
        lives = MAX_LIVES;
        pLives = lives;
    }
    
    void movePaddle() {
        if (width - pWidth <= xPos) {
            xPos = width - (pWidth);
        }
        if (xPos <= 0) {
            xPos = 0;
        }
    }
    
    void draw() {
        fill(pColor);
        rect(xPos, yPos, pWidth, pHeight, 30);
        if (frameCount % 2 == 0) {
            pX = xPos; // update new positions every second frame
        }
    }
    
    void loseLife() {
        if (!debugMode) {
            lives--;
            pLives = lives;
        }
    }
}
