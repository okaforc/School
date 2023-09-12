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
    
    void draw() {
        fill(255, 255 - (ballCollisions * 5), 255 - (ballCollisions * 5));
        ellipse(x, y, radius, radius);

        if(speedErrorAlpha <= 0) {
            speedErrorAlpha = 0;
        }
    }
    
    void moveBall() {
        x += speedX;
        y += speedY;
    }
    
    void wallCollision() {
        if (x - radius <= 0 || x + radius >= width) {
            sf_wall.play();        // play wall collision audio
            speedX *= - 1;
        }
    }
    
    void playerCollisions(Player player) {
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
    
    void speedCheck(Player player) {
        // if the ball gets stuck inside the paddle, reset the ball's position
        if (y > player.yPos &&     
            y < player.yPos + pHeight && 
            x > player.xPos && 
            x < player.xPos + pWidth) {
                speedErrorAlpha = 255;
                reset(100);
           }
    }
    
    void launchBall() {
        // set the ball's launch direction and speed to a random preset value
        switch(int(random(1, 13))) {
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
