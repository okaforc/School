public class Bullet{
    float xPos, yPos;
    float x, y;
    float speed;
    float radius;
    PImage sprite;
    boolean hasHit = false;
    
    
    public Bullet(PVector pos, float r) {
        speed = 4;
        radius = width / 100;
        x = pos.x;
        y = pos.y;
        radius = r;
        sprite = loadImage("Bullet.png");
    }
    
    
    void draw() {
        if (!hasHit) {
            tint(255);
            image(sprite, x, y, radius, 2 * radius);
            y -= speed;
        }
    }
    
    void collide(Alien[] aliens) {
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
