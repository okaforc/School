class Scrollbar {
    float xpos, ypos, dif;
    float backupx, backupy;
    String scrollbar;
    int scrollCol = 150;
    Scrollbar(float x, float y, String type) {
        xpos = x;
        ypos = y;
        backupx = x; 
        backupy = y;
        scrollbar = type;
    }
    void draw() {
        fill(scrollCol);
        if (scrollbar == "y") {
            rect(xpos, ypos, 15, dif);
        } else if (scrollbar == "x") {
            rect(xpos, ypos, dif, 15);
        }
    }
    void updatex() {
        if (scrollbar== "x") {
            // if (mousePressed) {
            //     xpos = mouseX - dif/2;
            //     if (mouseX>=xpos)offsetX = mouseX-xpos;
            //     if (xpos + dif >= width)xpos = width - dif;
            //     // make scrollbar stop at limits
            //     if (xpos <= 0) {    
            //         xpos = 0;
            //         offsetX = xpos;
            //     }
            // }
            // if (mouseY >= ypos && mouseY <= ypos+dif && mouseX >= xpos && mouseX <= xpos+dif)scrollCol = 150;
        }
    }
    void updatey() {
        if (scrollbar== "y") {
        //     if (mousePressed) {
        //         ypos = mouseY - dif/2;
        //         if (ypos <= 0) {
        //             offsetY = ypos;
        //             ypos = 0;
        //         }
        //         if (mouseY >= ypos) offsetY = mouseY - ypos + dif;
        //         if (ypos + dif >= height) {
        //             ypos = height - dif;
        //         }
        //     }
        //     if (mouseY >= ypos && mouseY <= ypos+dif && mouseX >= xpos && mouseX <= xpos+dif)scrollCol = 150;
        }
    }
    void reset() {
        xpos = backupx;
        ypos = backupy;
        draw();
    }
    boolean hover() {
        boolean toReturn = false;
        // if (mouseY >= ypos-50 && mouseY <= ypos+dif && mouseX >= xpos-50 && mouseX <= xpos+dif) toReturn = true;
        // else scrollCol = 175;
        return toReturn;
    }
    String currentBar() {
        String toReturn = "";
        // if (mouseY >= ypos && mouseY <= ypos+dif && mouseX >= xpos && mouseX <= xpos+dif) toReturn = scrollbar;
        return toReturn;
    }
}
