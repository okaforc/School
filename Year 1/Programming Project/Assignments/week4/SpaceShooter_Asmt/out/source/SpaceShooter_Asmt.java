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

public class SpaceShooter_Asmt extends PApplet {

// global variables
float alien_w = 48;
float alien_h = 36;

Alien[] aliens;
Player player;
ArrayList<Bullet> bullets;
ArrayList<Powerup> powerups;
int alienCount, aliveAliens;
boolean canAttack;


public void settings() {
    fullScreen();
}

public void setup() {
    noCursor(); // hides cursor when running
    alienCount = 100;
    aliens = new Alien[alienCount];
    player = new Player(mouseX, height - height / 20);
    bullets = new ArrayList<Bullet>();
    powerups = new ArrayList<Powerup>();
    init_array(aliens);
    aliveAliens = alienCount;
    canAttack = false;
}

public void draw() {
    background(0);
    fill(128, 0, 0, 100);
    rect(0, 0, width, alien_h * 2.25f);
    
    for (int i = 0; i < alien_h * 2.25f; ++i) {
        stroke(i * 4, 0, 0, i / 2);
        line(0, i, width, i);    
    }

    textAlign(CENTER);
    textSize(width / 5);
    fill(200, 200, 200, 100);
    text(aliveAliens, width / 2, height / 2);
    player.xPos = mouseX;
    player.yPos = height - height / 40;
    
    if (aliveAliens == 0) {
        textSize(width / 20);
        fill(150, 250, 250, 100);
        text("All aliens are dead.", width / 2, height - height / 3);
        textSize(width / 40);
        text("You win!", width / 2, height - height / 5);
    }
    
    draw_array(aliens);
    move_array(aliens);
    player.move();
    player.draw();
    
    for (int i = bullets.size() - 1; i > 0; i--) {
        Bullet b = bullets.get(i);
        if (b.y > 0) {
            b.draw();
        }
        if (aliens[5].canAttack) {
            b.collide(aliens);
        }
    }
    
    for (int j = powerups.size() - 1; j > 0; j--) {
        Powerup p = powerups.get(j);
        p.draw(player);
        p.collision(player);
    }
    
    
}

public void init_array(Alien[] arr) {
    for (int i = 0; i < arr.length; i++) {
        arr[i] = new Alien(alien_w * (i % 10) * 2, - alien_h * ((int)i / 10) * 2); // spawn each alien a set distance from each other
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
        if (arr[i].y > alien_h * 2) {
            arr[i].canAttack = true;
        }
    }
}
class Alien {
    float x, y, pY;
    float speedX, speedY, pSpeedX;
    float w, h, explosion_h, explosion_w;   // alien dimensions
    PImage sprite, alien, dead;             // alien sprites
    PImage[] explosion = new PImage[26];    // PImage array of explosion frames
    boolean hasExploded = false, isSpecial = false, movingDown = false, droppedPower, canAttack = false;
    int spriteCount;
    int powerupChance;
    Powerup powerup;
    
    
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
        
        * all subsequent frames follow the same dimensions as this one
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
        speedX *= 1.00075f;   // increase speed over time
    }
    
    public void move() {
        if (movingDown == false) {
            pY = y;
            pSpeedX = speedX;
            x += speedX;
            if (isSpecial) {
                y += 0.35f * sin(frameCount / 8);   // special aliens have a sinusiodal wave pattern horizontally
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
            // don't let the alien move off screen
            if (x + w >= width && y < pY + h) {
                x = width - w;
            }    
            if (x <= 0 && y < pY + h) {
                x = 0;
            }
            movingDown = true;
        }
    }
    
    
    public void explode() {
        if (!hasExploded && canAttack) {
            hasExploded = true;
            aliveAliens--;
        }
        if (hasExploded && !droppedPower) {
            powerupChance = (int)random(151);
            if (powerupChance <= 30) {
                powerups.add(new Powerup(x + w / 2, y + h));      // drop powerups from bottom of alien
            }
            droppedPower = true;
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
        if (hasExploded) {
            if (spriteCount < explosion.length) {
                sprite = explosion[frameCount % explosion.length];  // play gif once
                
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
public class Bullet{
    float xPos, yPos;
    float x, y;
    float speed;
    float radius;
    PImage sprite;
    
    
    public Bullet(PVector pos, float r) {
        speed = 5;
        radius = width / 100;
        x = pos.x;
        y = pos.y;
        radius = r;
        sprite = loadImage("Bullet.png");
    }
    
    
    public void draw() {
        tint(255);
        image(sprite, x, y, radius, 2 * radius);
        y -= speed;
    }
    
    public void collide(Alien[] aliens) {
        for (Alien a : aliens) {
            if (x + radius <= a.x + alien_w && 
                x + radius >= a.x && 
                y + radius <= a.y + alien_h && 
                y + radius >= a.y) {
                a.explode();
            }
        }
    }
    
}
public class Player{
    float xPos, yPos, pWidth, pHeight;
    float bRadius;
    Bullet bullet;
    Powerup powerup;
    int pColor = color(255,255,255);
    float timer, maxTimer;
    float pTimer, pMaxTimer;
    boolean hasPower1, hasPower2, hasPower3;
    boolean hasReset;
    PVector gunPos1, gunPos2, gunPos3;
    
    
    public Player(float tx, float ty) {
        xPos = tx;
        yPos = ty;
        pWidth = width / 30;
        pHeight = height / 70;
        maxTimer = 1.5f;
        timer = maxTimer;
        pMaxTimer = 1;
        pTimer = pMaxTimer;
        bRadius = width / 100;
        hasReset = false;
    }
    
    public void move() {
        if (width - pWidth <= xPos) {
            xPos = width - pWidth;
        }
        
        if (xPos <= 0) {
            xPos = 0;
        }
    }
    
    public void draw() {
        noStroke();
        fill(255);
        rect(xPos, yPos, pWidth, pHeight);
        gunPos1 = new PVector(xPos + pWidth / 2, yPos);
        gunPos2 = new PVector(xPos, yPos);
        gunPos3 = new PVector(xPos + pWidth, yPos);
        
        if (mousePressed && timer <= 0) {
            timer = maxTimer;
            bullets.add(new Bullet(new PVector(gunPos1.x, gunPos1.y - (2 * bRadius)), bRadius));    // add a new bullet to be shot before shooting it
        }
        timer -= 0.05f;
    }
    
    
    public void resetAttributes(int type) {
        if (!hasReset) {
            switch(type) {
                case 1:
                hasPower1 = false;
                maxTimer = 1.5f;
                break;
                
                case 2 : 
                hasPower2 = false;
                bRadius = width / 100;
                break;
                
                case 3:
                hasPower3 = false;
                break;
                
                default :
                break;	
            }
            hasReset = true;
        }
    }
}

public class Powerup {
    float x, y;
    float radius;
    int pColor;
    float transparency;
    Player player;
    int powerType;
    float timer, maxTimer;
    
    Powerup(float xPos, float yPos) {
        powerType = 0;
        radius = 15;
        x = xPos;
        y = yPos;
        transparency = 255;
        powerType = (int)random(1, 4);
        if (powerType == 1) {
            pColor = color(255, 0, 0);
        } else if (powerType == 2) {
            pColor = color(0, 255, 0);
        } else {
            pColor = color(0, 0, 255);
        }
        maxTimer = 10;
        timer = maxTimer;
    }
    
    public void draw(Player player) {
        fill(pColor, transparency);
        ellipse(x, y, radius, radius);
        y += 5;
        timer -= 0.05f;
        choosePower(player);
    }
    
    public void collision(Player player) {
        if (x <= player.xPos + player.pWidth &&
            x >= player.xPos && 
            y >= player.yPos && 
            y <= player.yPos + player.pHeight) {
            // player gets a powerup
            transparency = 0;
            if (powerType == 1) {
                player.hasPower1 = true;
            }
            if (powerType == 2) {
                player.hasPower2 = true;
            }
            if (powerType == 3) {
                player.hasPower3 = true;
            }
            timer = maxTimer;
        }
    }
    
    
    public void choosePower(Player player) {
        if (timer > 0 && player.hasReset) {
            player.hasReset = false;
        }
        if (timer > 0) {
            if (powerType == 1 && player.hasPower1) {
                player.hasReset = false;
                powerOne(player);
                return;
            }
            if (powerType == 2 && player.hasPower2) {
                player.hasReset = false;
                powerTwo(player);
                return;
            }
            if (powerType == 3 && player.hasPower3) {
                player.hasReset = false;
                powerThree(player);
                return;
            }
        } 
        if (timer <= 0 && !player.hasReset) {
            player.resetAttributes(powerType);
            player.hasReset = true;
        }
    }
    
    public void powerOne(Player p) {
        p.hasPower1 = true;
        p.maxTimer = 0.6f;
    }
    
    public void powerTwo(Player p) {
        p.hasPower2 = true;
        p.bRadius = width / 50;
    }
    
    public void powerThree(Player p) {
        if (p.timer <= 0 && mousePressed) {
            p.hasPower3 = true;
            bullets.add(new Bullet(p.gunPos2, p.bRadius));
            bullets.add(new Bullet(p.gunPos3, p.bRadius));
        }
    }
}
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "--present", "--window-color=#666666", "--stop-color=#cccccc", "SpaceShooter_Asmt" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
