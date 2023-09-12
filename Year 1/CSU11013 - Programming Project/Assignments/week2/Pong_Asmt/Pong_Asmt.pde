/* 
Audio credits: https://www.storyblocks.com/audio/stock/8-bit-pong-sound-rx9hv9c3idrk0wy4wj5.html
Font credits: https://fontmeme.com/fonts/sf-atarian-system-font/
*/


import processing.sound.*;

// global variables
Player player;
Player enemy;
Ball ball;
final float COL_INCREMENT = 0.05;
float speedErrorAlpha = 0;
int ballCollisions;                             // how many times has the ball collided with a paddle?
boolean gameOver = false;                       // has anyone won?
float pWidth, pHeight, pSpeed, pInitSpeed;      // paddle width, height, and speed

boolean debugMode;                              // boolean to enable / disable debug mode

PFont scorefont;                                // score font
SoundFile sf_paddle, sf_wall;                   // collision audio

void settings() {
    fullScreen();
}

void setup() {
    debugMode = false;
    scorefont = createFont("scorefont.ttf", 75, true);
    sf_paddle = new SoundFile(this, "collisionAudio_Paddle.wav");
    sf_wall = new SoundFile(this, "collisionAudio_Wall.wav");
    
    
    ellipseMode(RADIUS);
    player = new Player();
    enemy = new Player();
    ball = new Ball();
    
    ballCollisions = 0;
    pWidth = width / 10;
    pHeight = height / 40;
    pInitSpeed = 7.5;
    pSpeed = pInitSpeed;
    enemy.xPos = width / 2;
    
    ball.launchBall();
}

void draw() {
    background(0);
    noStroke();
    player.xPos = mouseX;
    player.yPos = height - (height / 8);
    
    enemy.yPos = height / 8;
    
    player.movePaddle();
    player.draw();
    player.pColor = color(255);
    enemy.pColor = color(105,105,105);
    
    
    enemy.movePaddle();
    enemy.draw();
    
    if (enemy.xPos + pWidth / 2 > ball.x) {
        enemy.xPos -= pSpeed;
    }
    if (enemy.xPos + pWidth / 2 < ball.x) {
        enemy.xPos += pSpeed;
    }
    
    if (!debugMode) {
        player.lives = player.pLives;
    }
    
    ball.moveBall();
    ball.wallCollision();
    ball.playerCollisions(player);
    ball.playerCollisions(enemy);
    ball.draw();
    ball.speedCheck(player);
    ball.speedCheck(enemy);
    drawBoard();
    
    speedErrorAlpha--;
    
    
    
    if (gameOver) {
        ball.x = width / 2;
        ball.y = height / 2;
    }
    gameManager();
}


void drawBoard() { 
    stroke(255, 255, 255);
    // dotted center line
    for (float i = width / 100; i <= width + 1000; i += width / 10) {
        line(i, height / 2, i - width / 20, height / 2);
    }
    
    // life bars
    /* 
     These are formed with the equation:
               (width)(MAXLIVES - currentLives)
      width -   ________________________________  = barLength
    
                             MAXLIVES
       so changing MAXLIVES will always result in equal-sized bars
    */
    noStroke();
    
    // player
    fill(50);
    rect(0, height - (height / 100), width, height / 50);  // simulate an empty bar
    fill(0, 255, 0);
    rect(0, height - (height / 100), width - ((player.MAX_LIVES - player.lives) * width / player.MAX_LIVES), height / 50);    // health bar
    fill(0);
    for (int i = width / player.MAX_LIVES; i <= width - width / player.MAX_LIVES; i += width / player.MAX_LIVES) {
        rect(i, height - (height / 100), width / 200, height / 50); // bar separations
    }
    
    // enemy
    fill(50);
    rect(0, height / 100, width, - height / 50);
    fill(255, 0, 0);
    rect(0, height / 100, width - ((enemy.MAX_LIVES - enemy.lives) * width / enemy.MAX_LIVES), - height / 50);
    fill(0);
    for (int i = width / enemy.MAX_LIVES; i <= width - width / enemy.MAX_LIVES; i += width / enemy.MAX_LIVES) {
        rect(i, height / 100, width / 200, - height / 50);
    }
    
    textSize(30);
    fill(200, 200, 200, speedErrorAlpha);
    text("Mind your position!", width / 12.5, 45 * height / 100);   
}

void gameManager() {    
    // draw score
    textFont(scorefont);
    
    if (ball.y - ball.radius - 10 > height) {
        player.loseLife();
        if (debugMode) {
            reset(0);
        } else {
            reset(500);
        }
    }
    if (ball.y + ball.radius + 10 < 0) {
        enemy.loseLife();
        if (debugMode) {
            reset(0);
        } else {
            reset(500);
        }
        pSpeed *= 1.25;  // increase difficulty on every life lost
    }
    
    // lives
    textSize(width / 34.15);
    fill(50,205,50);
    text(player.lives, width / 45, height - (height / 45));
    fill(178,34,34);
    text(enemy.lives, width - (width / 50), height / 15);
    
    // enemy speed
    textSize(width / 68.3);
    text("Enemy speed: " + nf(pSpeed, 0, 3), width - (width / 18), 45 * height / 100);
    
    // FOR DEBUGGING
    if (debugMode) {
        fill(100, 0, 0, 50);
        rect(width - (width / 7.5), 55 * height / 100, width, height / 3.5);
        fill(200);
        text("Debug Mode \n(press SPACE to disable)", width - (width / 15), 60 * height / 100);
        fill(128);
        text("X speed: " + nf(ball.speedX, 0, 3), width - (width / 15), 65 * height / 100);
        text("Y speed: " + nf(ball.speedY, 0, 3), width - (width / 15), 70 * height / 100);
        text("pX pos: " + nf(player.pX, 0, 3), width - (width / 15), 75 * height / 100);
        text("X pos: " + nf(player.xPos, 0, 3), width - (width / 15), 80 * height / 100);
    }
       
    // check for game over
    textAlign(CENTER);
    if (player.lives <= 0) {
        fill(0);
        rect( - 100, - 100, 2 * width, 2 * height);
        fill(255, 0, 0);
        textSize(width / 19.5);
        text("You lost!", width / 2, height / 2);
        delay(1000);
        textSize(width / 45.5333);
        fill(128, 128, 128);
        text("Click anywhere to retry, or ESC to quit.", width / 2, height - (height / 4));
        sf_wall.amp(0);
        sf_paddle.amp(0);
        gameOver = true;
        
    }
    
    if (enemy.lives <= 0) {
        fill(0);
        rect( - 100, - 100, 2 * width, 2 * height);
        fill(128, 255, 128);
        textSize(width / 19.5);
        text("You won!", width / 2, height / 2);
        textSize(width / 45.5333);
        fill(128, 128, 128);
        text("Click anywhere to retry, or ESC to quit.", width / 2, height - (height / 4));
        sf_wall.amp(0);
        sf_paddle.amp(0);
        gameOver = true;
    }
}


void reset(int pauseTime) {
    delay(pauseTime);
    // speedErrorAlpha = 0;
    sf_wall.amp(1);
    sf_paddle.amp(1);
    ball.x = width / 2;
    ball.y = height / 2;
    enemy.xPos = width / 2;
    delay(pauseTime);
    pSpeed -= ballCollisions * COL_INCREMENT;       // remove any extra speed increments
    
    if (debugMode) {
        println("ball.speedX before: " + ball.speedX + "\nball collisions: " + ballCollisions + "\nsubtracting: " + (ballCollisions * 10 * COL_INCREMENT) + "\nball.speedX now: " + (ball.speedX - (ballCollisions * 10 * COL_INCREMENT)) + "\n");
    }
    
    ball.speedX -= ballCollisions * 10 * COL_INCREMENT;
    ball.speedY -= ballCollisions * 5 * COL_INCREMENT;
    ballCollisions = 0;
    ball.launchBall();
}  


void mousePressed() {
    if (gameOver) {
        debugMode = false;
        restartGame();
    }
    if (debugMode) {
        reset(0);
    }
}

void keyPressed() {
    // enable / disable debug mode on space key press
    if (key == 32) {
        debugMode = !debugMode;
    }
}

void restartGame() {
    player.pLives = player.MAX_LIVES;
    enemy.pLives = player.MAX_LIVES;
    player.lives = player.MAX_LIVES;
    enemy.lives = enemy.MAX_LIVES;
    pSpeed = pInitSpeed;
    gameOver = false;
    reset(500);
}