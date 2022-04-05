// global variables
float alien_w = 48;
float alien_h = 36;

Alien[] aliens;
int alienCount, aliveAliens;

void settings() {
    fullScreen();
}

void setup() {
    alienCount = 10;
    aliens = new Alien[alienCount];
    init_array(aliens);
    aliveAliens = alienCount;
}

void draw() {
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

void init_array(Alien arr[]) {
    for (int i = 0; i < arr.length; i++) {
        arr[i] = new Alien(alien_w * i * 1.5, height / 80); // spawn each alien a set distance from each other
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
    }
}