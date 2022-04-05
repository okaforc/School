class Alien {
    float x, y, pY;
    float speedX, speedY, pSpeedX;
    float w, h, explosion_h, explosion_w;   // alien dimensions
    PImage sprite, alien, dead;             // alien sprites
    PImage[] explosion = new PImage[26];    // PImage array of explosion frames
    boolean hasExploded = false, isSpecial = false, movingDown = false;
    int spriteCount;
    
    
    Alien(float xPos, float yPos) {
        alien = loadImage("Alien.png");
        explosion[0] = loadImage("Explosion Frames\\explosionFrame1.png");  
        explosion[1] = loadImage("Explosion Frames\\explosionFrame2.png");  
        explosion[2] = loadImage("Explosion Frames\\explosionFrame3.png");  
        explosion[3] = loadImage("Explosion Frames\\explosionFrame4.png");    
        explosion[4] = loadImage("Explosion Frames\\explosionFrame5.png");        
        explosion[5] = loadImage("Explosion Frames\\explosionFrame6.png");        
        explosion[6] = loadImage("Explosion Frames\\explosionFrame7.png");        
        explosion[7] = loadImage("Explosion Frames\\explosionFrame8.png");        
        explosion[8] = loadImage("Explosion Frames\\explosionFrame9.png");        
        explosion[9] = loadImage("Explosion Frames\\explosionFrame10.png");       
        explosion[10] = loadImage("Explosion Frames\\explosionFrame11.png");      
        explosion[11] = loadImage("Explosion Frames\\explosionFrame12.png");      
        explosion[12] = loadImage("Explosion Frames\\explosionFrame13.png");      
        explosion[13] = loadImage("Explosion Frames\\explosionFrame14.png");      
        explosion[14] = loadImage("Explosion Frames\\explosionFrame15.png");      
        explosion[15] = loadImage("Explosion Frames\\explosionFrame16.png");      
        explosion[16] = loadImage("Explosion Frames\\explosionFrame17.png");      
        explosion[17] = loadImage("Explosion Frames\\explosionFrame18.png");      
        explosion[18] = loadImage("Explosion Frames\\explosionFrame19.png");  
        explosion[19] = loadImage("Explosion Frames\\explosionFrame20.png");  
        explosion[20] = loadImage("Explosion Frames\\explosionFrame21.png");
        explosion[21] = loadImage("Explosion Frames\\explosionFrame22.png");
        explosion[22] = loadImage("Explosion Frames\\explosionFrame23.png");
        explosion[23] = loadImage("Explosion Frames\\explosionFrame24.png");
        explosion[24] = loadImage("Explosion Frames\\explosionFrame25.png");
        explosion[25] = loadImage("Explosion Frames\\explosionFrame26.png");
        dead = loadImage("Explosion Frames\\explosionFrame26.png");
        
        x = xPos;
        y = yPos;
        pY = y;
        speedX = 3;
        speedY = 5;
        spawnSpecialType();
        
        /* 
         |_________sprite_________|___width:height ratio____|___orig. dimensions___|___new dimensions___|
         |        Alien.png       |          4 : 3          |       192 x 144      |       48 x 36      |
         |  explosionFrame1.png*  |          1 : 1          |       200 x 200      |       50 x 50      |
        
          * all subsequent frames have the same dimensions as this one
        */
        
        explosion_w = 50;
        explosion_h = 50;
        spriteCount = 0;
        h = alien_h;
        w = alien_w;
    }
    
    void draw() {
        checkDeath();
        if (isSpecial) {
            tint(25, 200, 25);  // color special aliens green
        } else {
            tint(255, 255, 255);
        }
        image(sprite, x, y, w, h);
        speedX *= 1.0005;   // increase speed over time
    }
    
    void move() {
        if (movingDown == false) {
            pY = y;
            pSpeedX = speedX;
            x += speedX;
            if (isSpecial) {
                y += sin(frameCount / 8);   // special aliens have a sinusiodal wave pattern horizontally
            }
        } else {
            if (y <= pY + h) {
                y += 5;     // move down if not at next y position
            } else {
                speedX = - pSpeedX;
                movingDown = false;
            }
        }
    }
    
    void collision() {
        // if an alien is at the edge of the screen and has not reached its next y position
        if ((x + w >= width || x <= 0) && y < pY + h) {
            if (x + w >= width && y < pY + h) x = width - w;    // don't let the alien move off screen
            if (x <= 0 && y < pY + h) x = 0;
            movingDown = true;
        }
    }
    
    
    void explode() {
        // every 30 frames, give a 1 in 20 chance to explode
        if (frameCount % 30 == 0 && !hasExploded) {
            int explosionChance = int(random(1, 101));
            if (explosionChance % 20  == 0) {
                hasExploded = true;
                aliveAliens--;
            }
        }
    }
    
    void spawnSpecialType() {
        // give a 1 in 5 chance to spawn as 'special'
        int specialChance = int(random(1, 6));
        if (specialChance == 3) {
            isSpecial = true;
        }
    }
    
    void checkDeath() {
        explode();
        if (hasExploded) {
            if (spriteCount < explosion.length) {
                sprite = explosion[frameCount % explosion.length];  // 'play' gif once
            } else {
                sprite = dead;
            }
            spriteCount++;
            w = explosion_w;
            h = explosion_h;
            speedX = 0;
            speedY = 0;
            isSpecial = false;
        } else {
            sprite = alien;
            w = alien_w;
            h = alien_h;
        }
    }
}