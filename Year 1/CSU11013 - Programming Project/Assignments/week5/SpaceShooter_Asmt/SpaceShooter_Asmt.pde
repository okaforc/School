// global variables
float alien_w = 48;
float alien_h = 36;

Alien[] aliens;
Player player;
Barrier barr1, barr2, barr3, barr4;
ArrayList<Bullet> bullets;
ArrayList<Powerup> powerups;
int alienCount, aliveAliens;
boolean canAttack;
float endGameTimer;
boolean gameLost, gameWon;

void settings() {
    // fullScreen();
    size(1024, 512);
}

final int HEIGHT_LIMIT = height;

void setup() {
    noCursor(); // hides cursor when running
    frameRate(60);
    alienCount = 10;
    aliens = new Alien[alienCount];
    player = new Player(mouseX, height - height / 20);
    bullets = new ArrayList<Bullet>();
    powerups = new ArrayList<Powerup>();
    init_array(aliens);
    aliveAliens = alienCount;
    canAttack = false;
    endGameTimer = 3;
    gameLost = false;
    gameWon = false;
    
    // barrier positions
    barr1 = new Barrier(3 * width / 20, height - height / 4, width / 10, height / 15);
    barr2 = new Barrier(7 * width / 20, height - height / 4, width / 10, height / 15);
    barr3 = new Barrier(11 * width / 20, height - height / 4, width / 10, height / 15);
    barr4 = new Barrier(15 * width / 20, height - height / 4, width / 10, height / 15);   
}

void draw() {
    background(0);
    
    fill(128, 0, 0, 100);
    rect(0, 0, width, alien_h * 2.25);
    for (int i = 0; i < alien_h * 2.25; ++i) {
        stroke(i * 4, 0, 0, i / 2);
        line(0, i, width, i);    
    }
    
    barrierManager();
    
    player.xPos = mouseX;
    player.yPos = height - height / 40;
    
    draw_array(aliens);
    move_array(aliens);
    if (!player.isDead) {
        player.move();
        player.draw();
    }
    
    for (int i = bullets.size() - 1; i > 0; i--) {
        Bullet b = bullets.get(i);
        if (b.y > 0) {
            b.draw();
        }
        if (gameLost || gameWon) {
            b.speed = endGameTimer * (4 / 3);
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
    
    gameManger();
}

void gameManger() {
    textAlign(CENTER);
    textSize(width / 5);
    fill(200, 200, 200, 100);
    text(aliveAliens, width / 2, height / 2);       // draw alive aliens on screen
    
    if (aliveAliens == 0) {
        gameWon = true;
        fill(0,0,0,200);
        rect(0, 0, width, height);
        textSize(width / 20);
        fill(150, 250, 250, 100);
        text("All aliens are dead.", width / 2, height - height / 3);
        textSize(width / 40);
        text("You win!", width / 2, height - height / 5);
        player.xPos = player.pX;
        endGameTimer -= 0.05;
    }
    if (player.isDead) {
        gameLost = true;
        fill(0,0,0,200);
        rect(0, 0, width, height);
        textSize(width / 20);
        fill(150, 250, 250, 100);
        text("You died.", width / 2, height - height / 3);
        textSize(width / 40);
        text("The aliens have invaded the planet. How could you let this happen?", width / 2, height - height / 5);
        endGameTimer -= 0.05;
    }
    
    if (endGameTimer < 0) {
        endGameTimer = 0;
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
        if (arr[i].bomb.collide(player)) {
            player.isDead = true;
        }
    }
}

void move_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        if (gameLost) {
            arr[i].speedX = endGameTimer;
            arr[i].speedY = 0;
            arr[i].movingDown = false;
        }
        arr[i].move();
        arr[i].collision();
        if (arr[i].y > alien_h * 2) {
            arr[i].canAttack = true;
        }
    }
}


void barrierManager() {
    barr1.draw();
    barr1.collide(aliens, bullets);
    
    barr2.draw();
    barr2.collide(aliens, bullets);
    
    barr3.draw();
    barr3.collide(aliens, bullets);
    
    barr4.draw();
    barr4.collide(aliens, bullets);
}