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

public class SpaceInvaders_Asmt extends PApplet {

// global variables
float alien_w = 48;
float alien_h = 36;

Alien[] aliens;
int alienCount, aliveAliens;

public void settings() {
    fullScreen();
}

public void setup() {
    alienCount = 10;
    aliens = new Alien[alienCount];
    init_array(aliens);
    aliveAliens = alienCount;
}

public void draw() {
    background(0);
    textAlign(CENTER);
    textSize(width/5);
    fill(200, 200, 200, 100);
    text(aliveAliens, width/2, height/2);
    
    if (aliveAliens == 0) {
        textSize(width/20);
        fill(200, 250, 200, 100);
        text("All aliens are dead.", width/2, height - height/3);
    }
    draw_array(aliens);
    move_array(aliens);
}

public void init_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        arr[i] = new Alien(alien_w * i * 1.5f, height / 80); // spawn each alien a set distance from each other
    }
}

public void draw_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        arr[i].draw();
    }
}

public void move_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        arr[i].move();
        arr[i].collision();
    }
}
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
    
    public void draw() {
        checkDeath();
        if (isSpecial) {
            tint(25, 200, 25);  // color special aliens green
        } else {
            tint(255, 255, 255);
        }
        image(sprite, x, y, w, h);
        speedX *= 1.0005f;   // increase speed over time
    }
    
    public void move() {
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
    
    public void collision() {
        // if an alien is at the edge of the screen and has not reached its next y position
        if ((x + w >= width || x <= 0) && y < pY + h) {
            if (x + w >= width && y < pY + h) x = width - w;    // don't let the alien move off screen
            if (x <= 0 && y < pY + h) x = 0;
            movingDown = true;
        }
    }
    
    
    public void explode() {
        // every 30 frames, give a 1 in 20 chance to explode
        if (frameCount % 30 == 0 && !hasExploded) {
            int explosionChance = PApplet.parseInt(random(1, 101));
            if (explosionChance % 20  == 0) {
                hasExploded = true;
                aliveAliens--;
            }
        }
    }
    
    public void spawnSpecialType() {
        // give a 1 in 5 chance to spawn as 'special'
        int specialChance = PApplet.parseInt(random(1, 6));
        if (specialChance == 3) {
            isSpecial = true;
        }
    }
    
    public void checkDeath() {
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
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "--present", "--window-color=#666666", "--stop-color=#cccccc", "SpaceInvaders_Asmt" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
