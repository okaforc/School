public class Player{
    float xPos, yPos, pWidth, pHeight, pX;
    float bRadius;
    Powerup powerup;
    color pColor = color(255,255,255);
    float timer, maxTimer;
    float pTimer;
    boolean hasPower1, hasPower2, hasPower3;
    boolean hasReset, isDead;
    PVector gunPos1, gunPos2, gunPos3;
    
    
    public Player(float tx, float ty) {
        xPos = tx;
        yPos = ty;
        pX = xPos;
        pWidth = width / 20;
        pHeight = height / 70;
        bRadius = width / 100;
        hasReset = false;
        isDead = false;
        
        maxTimer = 1.5;
        timer = maxTimer;
    }
    
    void move() {
        if (width - pWidth <= xPos) {
            xPos = width - pWidth;
        }
        
        if (xPos <= 0) {
            xPos = 0;
        }
    }
    
    void draw() {
        noStroke();
        if (!isDead) {
            fill(255);
            rect(xPos, yPos, pWidth, pHeight);
            gunPos1 = new PVector(xPos + pWidth / 2, yPos);
            gunPos2 = new PVector(xPos, yPos);
            gunPos3 = new PVector(xPos + pWidth, yPos);
            
            if (mousePressed && timer <= 0 && !gameWon) {
                timer = maxTimer;
                bullets.add(new Bullet(new PVector(gunPos1.x, gunPos1.y - (2 * bRadius)), bRadius));    // add a new bullet to be shot before shooting it
            }
            timer -= 0.05;
            pX = xPos;
        }
    }
    
    
    void resetAttributes(int type) {
        if (!hasReset) {
            switch(type) {
                case 1:
                hasPower1 = false;
                maxTimer = 1.5;
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

