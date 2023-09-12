public class Barrier {
    Bullet bull;
    float x, y;
    float h, w;
    int lives;
    PImage sprite, damage0, damage1, damage2, damage3;
    
    Barrier(float xPos, float yPos, float wi, float he) {

        damage0 = loadImage("Barriers\\damage0.png");
        damage1 = loadImage("Barriers\\damage1.png");
        damage2 = loadImage("Barriers\\damage2.png");
        damage3 = loadImage("Barriers\\damage3.png");

        x = xPos;
        y = yPos;
        w = wi;
        h = he;
        lives = 10;
    }
    
    void draw() {
        if (lives > 0) {
            chooseSprite();
            noStroke();
            tint(255);
            image(sprite, x, y, w, h);
        }
    }

    void chooseSprite() {
        switch (lives) {
            case 1:
            case 2:
            case 3:
                sprite = damage3;
                break;
            case 4:
            case 5:
            case 6:
                sprite = damage2;
                break;
            case 7:
            case 8:
            case 9:
                sprite = damage1;
                break;
            case 10: 
                sprite = damage0;
                break;
            
            default: 
                sprite = damage0;
                break;
        }
    }
    
    void collide(Alien[] aliens, ArrayList<Bullet> bullets) {
        if (lives > 0) {
            for (Bullet bull : bullets) {
                if (bull.x + bull.radius <= x + w &&
                    bull.x - bull.radius >= x && 
                    bull.y - bull.radius <= y + h)
                {
                    lives--;
                    bull.y = 2*height;          // move bullet out of barrier before stopping draw()
                    bull.hasHit = true;
                }
            }
            
            for (Alien a : aliens) {
                Bomb t = a.bomb;
                if (t.x + t.radius <= x + w && 
                    t.x - t.radius >= x && 
                    t.y + t.radius >= y)
                {
                    lives--;
                    t.y = 0;
                    t.hasHit = true;
                }
            }
            
        }
    }
    
}
