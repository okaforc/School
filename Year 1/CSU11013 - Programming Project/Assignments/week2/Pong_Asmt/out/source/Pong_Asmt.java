import processing.core.*; 
import processing.data.*; 
import processing.event.*; 
import processing.opengl.*; 

import processing.sound.*; 

import java.util.HashMap; 
import java.util.ArrayList; 
import java.io.File; 
import java.io.BufferedReader; 
import java.io.PrintWriter; 
import java.io.InputStream; 
import java.io.OutputStream; 
import java.io.IOException; 

public class Pong_Asmt extends PApplet {

/* 
Audio credits: https://www.storyblocks.com/audio/stock/8-bit-pong-sound-rx9hv9c3idrk0wy4wj5.html
Font credits: https://fontmeme.com/fonts/sf-atarian-system-font/
*/




// global variables
Player player;
Player enemy;
Ball ball;
final float COL_INCREMENT = 0.05f;
float speedErrorAlpha = 0;
int ballCollisions;                             // how many times has the ball collided with a paddle?
boolean gameOver = false;                       // has anyone won?
float pWidth, pHeight, pSpeed, pInitSpeed;      // paddle width, height, and speed

boolean debugMode;                              // boolean to enable / disable debug mode

PFont scorefont;                                // score font
SoundFile sf_paddle, sf_wall;                   // collision audio

public void settings() {
    fullScreen();
}

public void setup() {
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
    pInitSpeed = 7.5f;
    pSpeed = pInitSpeed;
    enemy.xPos = width / 2;
    
    ball.launchBall();
}

public void draw() {
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


public void drawBoard() { 
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
    text("Mind your position!", width / 12.5f, 45 * height / 100);   
}

public void gameManager() {    
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
        pSpeed *= 1.25f;  // increase difficulty on every life lost
    }
    
    // lives
    textSize(width / 34.15f);
    fill(50,205,50);
    text(player.lives, width / 45, height - (height / 45));
    fill(178,34,34);
    text(enemy.lives, width - (width / 50), height / 15);
    
    // enemy speed
    textSize(width / 68.3f);
    text("Enemy speed: " + nf(pSpeed, 0, 3), width - (width / 18), 45 * height / 100);
    
    // FOR DEBUGGING
    if (debugMode) {
        fill(100, 0, 0, 50);
        rect(width - (width / 7.5f), 55 * height / 100, width, height / 3.5f);
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
        textSize(width / 19.5f);
        text("You lost!", width / 2, height / 2);
        delay(1000);
        textSize(width / 45.5333f);
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
        textSize(width / 19.5f);
        text("You won!", width / 2, height / 2);
        textSize(width / 45.5333f);
        fill(128, 128, 128);
        text("Click anywhere to retry, or ESC to quit.", width / 2, height - (height / 4));
        sf_wall.amp(0);
        sf_paddle.amp(0);
        gameOver = true;
    }
}


public void reset(int pauseTime) {
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


public void mousePressed() {
    if (gameOver) {
        debugMode = false;
        restartGame();
    }
    if (debugMode) {
        reset(0);
    }
}

public void keyPressed() {
    // enable / disable debug mode on space key press
    if (key == 32) {
        debugMode = !debugMode;
    }
}

public void restartGame() {
    player.pLives = player.MAX_LIVES;
    enemy.pLives = player.MAX_LIVES;
    player.lives = player.MAX_LIVES;
    enemy.lives = enemy.MAX_LIVES;
    pSpeed = pInitSpeed;
    gameOver = false;
    reset(500);
}
class Ball {
    float x, y;                 // ball positions
    float speedX, speedY;
    float radius;
    
    Ball() {
        fill(255);
        ellipseMode(RADIUS);
        speedX = 0;     // initialise speeds. these values will not be used.
        speedY = 0;
        x = width / 2;
        y = height / 2;
        radius = width / 95;
    }
    
    public void draw() {
        fill(255, 255 - (ballCollisions * 5), 255 - (ballCollisions * 5));
        ellipse(x, y, radius, radius);

        if(speedErrorAlpha <= 0) {
            speedErrorAlpha = 0;
        }
    }
    
    public void moveBall() {
        x += speedX;
        y += speedY;
    }
    
    public void wallCollision() {
        if (x - radius <= 0 || x + radius >= width) {
            sf_wall.play();        // play wall collision audio
            speedX *= - 1;
        }
    }
    
    public void playerCollisions(Player player) {
        // top and bottom of paddle
        if (y + radius >= player.yPos && 
            y - radius <= (player.yPos + pHeight) && 
            x <= (player.xPos + pWidth) && 
            x >= player.xPos)
        {
            sf_paddle.play(); // play paddle collision audio

            // ### EXTRA CREDIT 2 ###
            // increase difficulty on collision as well as on point loss
            pSpeed += COL_INCREMENT;  
            
            if (speedX >= 0) {
                speedX += 10 * COL_INCREMENT;
            } else {
                speedX -= 10 * COL_INCREMENT;
                
            }
            if (speedY >= 0) {
                speedY += 5 * COL_INCREMENT;
            } else {
                speedY -= 5 * COL_INCREMENT;
            }
            ballCollisions += 1;
            speedY *= - 1;
            
            // ### EXTRA CREDIT 1 ###
            // friction on x-axis. adds paddle velocity to that of the ball's.
            speedX += (player.xPos - player.pX) / 5;
            
        }
        
        // paddle sides
        if (y >= player.yPos &&     
            y <= player.yPos + pHeight && 
            x+ radius >= player.xPos && 
            x- radius <= player.xPos + pWidth) {
            sf_paddle.play();
            speedX *= - 1;
        }
    }
    
    public void speedCheck(Player player) {
        // if the ball gets stuck inside the paddle, reset the ball's position
        if (y > player.yPos &&     
            y < player.yPos + pHeight && 
            x > player.xPos && 
            x < player.xPos + pWidth) {
                speedErrorAlpha = 255;
                reset(100);
           }
    }
    
    public void launchBall() {
        // set the ball's launch direction and speed to a random preset value
        switch(PApplet.parseInt(random(1, 13))) {
            case 1 :
              speedX = 3;
              speedY = - 5;
              if (debugMode) println("Launch 1");
              break;
            case 2:
              speedX = 4;
              speedY = - 4;
              if (debugMode) println("Launch 2");
              break;
            case 3 :
              speedX = 5;
              speedY = - 3;
              if (debugMode) println("Launch 3");
              break;
            case 4 :
             speedX = 5;
             speedY = 3;
               if (debugMode) println("Launch 4");
               break;
            case 5 :
             speedX = 4;
             speedY = 4;
               if (debugMode) println("Launch 5");
               break;
            case 6 :
              speedX = 3;
              speedY = 5;
              if (debugMode) println("Launch 6");
              break;
            case 7 :
              speedX = - 3;
              speedY = 5;
              if (debugMode) println("Launch 7");
              break;
            case 8 :
              speedX = - 4;
              speedY = 4;
              if (debugMode) println("Launch 8");
              break;
            case 9 :
              speedX = - 5;
              speedY = 3;
              if (debugMode) println("Launch 9");
              break;
            case 10 :
              speedX = - 3;
              speedY = - 5;
              if (debugMode) println("Launch 10");
              break;
            case 11:
              speedX = - 4;
              speedY = - 4;
              if (debugMode) println("Launch 11");
              break;
            case 12 :
              speedX = - 5;
              speedY = - 3;
              if (debugMode) println("Launch 12");
              break;
        }
    }
    
}
class Player {
    float xPos, yPos;
    float pX;                   // paddle's position on the previous frame
    int pColor;
    int MAX_LIVES, lives, pLives;
    
    Player() {
        MAX_LIVES = 3;
        lives = MAX_LIVES;
        pLives = lives;
    }
    
    public void movePaddle() {
        if (width - pWidth <= xPos) {
            xPos = width - (pWidth);
        }
        if (xPos <= 0) {
            xPos = 0;
        }
    }
    
    public void draw() {
        fill(pColor);
        rect(xPos, yPos, pWidth, pHeight, 30);
        if (frameCount % 2 == 0) {
            pX = xPos; // update new positions every second frame
        }
    }
    
    public void loseLife() {
        if (!debugMode) {
            lives--;
            pLives = lives;
        }
    }
}
    static public void main(String[] passedArgs) {
        String[] appletArgs = new String[] { "--present", "--window-color=#666666", "--stop-color=#cccccc", "Pong_Asmt" };
        if (passedArgs != null) {
          PApplet.main(concat(appletArgs, passedArgs));
        } else {
          PApplet.main(appletArgs);
        }
    }
}
