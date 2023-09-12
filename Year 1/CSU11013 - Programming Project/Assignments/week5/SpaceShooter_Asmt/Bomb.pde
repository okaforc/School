public class Bomb {
    float x, y, radius;
    float speed;
    float yLimit;
    boolean hasHit = false;
    
    
    Bomb(float xPos, float yPos, float lim) {
        x = xPos;
        y = yPos;
        speed = 3;
        radius = height / 38;
        yLimit = lim;
    }
    
    void draw() {
        if (!hasHit) {
            fill(255);
            ellipse(x, y, radius, radius);
            collide(player);                // uses universal player object
        }
    }
    
    void move() {
        if (!hasHit) {
            y += speed;
        }
    }
    
    boolean collide(Player player) {
        if (x <= player.xPos + player.pWidth &&
            x >= player.xPos && 
            y >= player.yPos && 
            y <= player.yPos + player.pHeight) {
            return true;
        }
        return false;
    }
    
    boolean offScreen() {
        if (y - radius > yLimit) {
            return true;
        }
        return false;
    }
    
}
