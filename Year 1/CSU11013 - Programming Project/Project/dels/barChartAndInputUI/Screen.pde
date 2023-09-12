class Screen {
    color backgroundColor;
    ArrayList widgetList = new ArrayList<Widget>();
    ArrayList checkBoxList = new ArrayList<CheckBox>();
    ArrayList radioList1 = new ArrayList<Radio>();
    ArrayList radioList2 = new ArrayList<Radio>();
    SearchBar searchBar;
    int xpos = 300;
    
    Screen(color backgroundColor) {
        this.backgroundColor = backgroundColor;
    }
    
    void addWidget(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event) {
        widgetList.add(new Widget(x, y, width, height, label, widgetColor, labelColor, widgetFont, event));
    }
    
    //void addRadio(int x, int y, int width, int height, String label, color widgetColor, PFont widgetFont, int event) {
    // widgetList.add(new Radio(x, y, width, height, label, widgetColor, widgetFont, event));
//}
    
    void addCheckBox(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event) {
        Widget item = new CheckBox(x, y, width, height, label, widgetColor, labelColor, widgetFont, event);
        widgetList.add(item);
        checkBoxList.add(item);
    }
    
    void addRadio(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event, int group) {
        Widget item = new Radio(x, y, width, height, label, widgetColor, labelColor, widgetFont, event, group);
        widgetList.add(item);
        switch (group) {
            case 1:
                radioList1.add(item);
                break;
            case 2:
                radioList2.add(item);
                break;
        }
    }
    
    void createSearchBar(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event) {
        searchBar = new SearchBar(x, y, width, height, label, widgetColor, labelColor, widgetFont, event);
    }
    
    int getEvent(int mX, int mY) {
        for (int i = 0; i < widgetList.size(); i++) {
            Widget aWidget = (Widget) widgetList.get(i);
            if (mX > aWidget.x && mX < aWidget.x + aWidget.width && mY > aWidget.y && mY < aWidget.y + aWidget.height) {
                return aWidget.event;
            }
        }
        return EVENT_NULL;
    }
    
    void checkBoxes() {
        for (int i = 0; i < checkBoxList.size(); i++) {
            CheckBox aBox = (CheckBox) checkBoxList.get(i);
            int event = aBox.getEvent(mouseX, mouseY);
            if (event == EVENT_BUTTON29) {
                aBox.tickedOpacity *= -1;
                aBox.checked = !aBox.checked;
            }
        }
    }

    void radioToggle(int group) {
        ArrayList<Radio> radList = new ArrayList<Radio>();
        switch (group) {
            case 1:
            radList = radioList1;
            break;
            case 2:
            radList = radioList2;
            break;
        }
        for (int j = 0; j < radList.size(); j++) {
            Radio radio = (Radio) radList.get(j);
            int event = radio.getEvent(mouseX, mouseY);
            if (radio.ticked) {
                radio.tickedOpacity = 255;
            }
            if (!radio.ticked && event == EVENT_BUTTON28) {
                radio.ticked = true;
                radio.tickedOpacity = 255;
                for (int k = 0; k < radList.size(); k++) {
                    Radio tempRad = (Radio) radList.get(k);
                    if (k != j) {
                        tempRad.ticked = false;
                        tempRad.tickedOpacity = 0;
                    }
                }
            }
        }
    }
    
    void drawWidgets() {
        stroke(BLACK);
        for (int i = 0; i < widgetList.size(); i++) {
            Widget aWidget = (Widget) widgetList.get(i);
            if (aWidget.getBorderIsWhite()) {
                stroke(WHITE);
            } else if (aWidget.event == 24 && hideOut) {
                stroke(OCEAN_COLOUR);
            } else{
                stroke(BLACK);
            }
            aWidget.drawTheWidget();

            if (aWidget.isRadio || aWidget.isCheck) {
                noStroke();
                fill(0, 0, 0, aWidget.tickedOpacity);
                ellipse(aWidget.x + aWidget.width / 2, aWidget.y + aWidget.height / 2, aWidget.width / 2, aWidget.height / 2);
            }
        }
        if (searchBar != null) {
            if (searchBar.getBorderIsWhite()) {
                stroke(WHITE);
            } else {
                stroke(BLACK);
            }
            searchBar.drawTheWidget();
        }
    }
    
    void updatingSlider(int mX, int mY) {
        if (mY >= 390 && mY <= 410) xpos = mX;
    }
    
    void drawSlider() {
        strokeWeight(10); 
        stroke(BLACK);
        line(0, 400, 600, 400);
        text(0, 20, 380);
        text(100, 570, 380);
        fill(0, 100, 200);
        noStroke();
        rect(xpos, 390, 20, 20);
        int chosenVal = xpos / 6;
        if (xpos <= 0) xpos = 0;
        if (xpos >= 600) xpos = 600;
        if (chosenVal <= 0) chosenVal = 0;
        if (chosenVal >= 100) chosenVal = 100;
        fill(BLACK);
        text("chosenValue: " + chosenVal, 240, 500);
    }
    void moveScreenY() {
        if (currentScreen == screen0 || currentScreen == screen4_2) {
            centerY = 0;
        } else {
            centerY = (mouseY - offsetY) *- 1;
        }
    }
    void moveScreenX() {
        if (currentScreen == screen0 || currentScreen == screen4_2) {
            centerX = 0;
        } else {
            centerX = (mouseX - offsetX) *- 1;
        }
    }
    void zoomIn() {
        scaler +=.5;
        // translate(width>>1, height>>1);
        hideOut = false;
        zoomIn = false;
    }
    void zoomOut() {
        if (!hideOut) {
            if (scaler == 1)hideOut = true;
            scaler -=.5;
            // translate(width<<1, height<<1);
            zoomOut = false;
        }
    }
    void zoomReset() {
        scaler = 1;
        zoomReset = false;
    }
}
