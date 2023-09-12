// global variables
float alien_w = 48;
float alien_h = 36;

Alien[] aliens;
Player player;
ArrayList<Bullet> bullets;
ArrayList<Powerup> powerups;
int alienCount, aliveAliens;
boolean canAttack;


void settings() {
    fullScreen();
}

void setup() {
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

void draw() {
    background(0);
    fill(128, 0, 0, 100);
    rect(0, 0, width, alien_h * 2.25);
    
    for (int i = 0; i < alien_h * 2.25; ++i) {
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

void init_array(Alien[] arr) {
    for (int i = 0; i < arr.length; i++) {
        arr[i] = new Alien(alien_w * (i % 10) * 2, - alien_h * ((int)i / 10) * 2); // spawn each alien a set distance from each other
    }
}

void draw_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        arr[i].draw();
    }
}

void move_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        arr[i].move();
        arr[i].collision();
        if (arr[i].y > alien_h * 2) {
            arr[i].canAttack = true;
        }
    }
}
