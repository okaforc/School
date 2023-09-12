public class Powerup {
    float x, y;
    float radius;
    color pColor;
    float transparency;
    Player player;
    int powerType;
    float timer, maxTimer;
    PImage sprite, p1, p2, p3;
    
    Powerup(float xPos, float yPos) {
        powerType = 0;
        radius = 30;
        x = xPos;
        y = yPos;
        p1 = loadImage("Powerups\\powerup1.png");
        p2 = loadImage("Powerups\\powerup2.png");
        p3 = loadImage("Powerups\\powerup3.png");
        
        transparency = 255;
        powerType = (int)random(1, 4);
        if (powerType == 1) {
            // speed
            pColor = color(255, 0, 0);
            sprite = p1;
        } else if (powerType == 2) {
            // size
            pColor = color(0, 255, 0);
            sprite = p2;
        } else {
            // more bullets
            pColor = color(0, 0, 255);
            sprite = p3;
        }
        maxTimer = 10;
        timer = maxTimer;
    }
    
    void draw(Player player) {
        tint(255, 255, 255, transparency);
        image(sprite, x, y, radius, radius);
        y += 5;
        timer -= 0.05;
        choosePower(player);
    }
    
    void collision(Player player) {
        if (x <= player.xPos + player.pWidth &&
            x >= player.xPos && 
            y + radius >= player.yPos) {
            // player gets a powerup
            x = width * 4;
            y = - height;
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
    
    
    void choosePower(Player player) {
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
    
    void powerOne(Player p) {
        p.hasPower1 = true;
        p.maxTimer = 0.6;
    }
    
    void powerTwo(Player p) {
        p.hasPower2 = true;
        p.bRadius = width / 50;
    }
    
    void powerThree(Player p) {
        if (p.timer <= 0 && mousePressed && !gameWon) {
            p.hasPower3 = true;
            bullets.add(new Bullet(p.gunPos2, p.bRadius));
            bullets.add(new Bullet(p.gunPos3, p.bRadius));
        }
    }
}
