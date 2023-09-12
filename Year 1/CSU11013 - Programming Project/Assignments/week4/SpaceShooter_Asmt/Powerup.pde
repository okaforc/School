public class Powerup {
    float x, y;
    float radius;
    color pColor;
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
    
    void draw(Player player) {
        fill(pColor, transparency);
        ellipse(x, y, radius, radius);
        y += 5;
        timer -= 0.05;
        choosePower(player);
    }
    
    void collision(Player player) {
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
        if (p.timer <= 0 && mousePressed) {
            p.hasPower3 = true;
            bullets.add(new Bullet(p.gunPos2, p.bRadius));
            bullets.add(new Bullet(p.gunPos3, p.bRadius));
        }
    }
}
