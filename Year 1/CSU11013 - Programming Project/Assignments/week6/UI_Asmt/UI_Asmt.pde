ArrayList widgetList, radioButtons, checkButtons, sliders;
PFont stdFont;
final int EVENT_BUTTON1 = 1;        // turn the square red
final int EVENT_BUTTON2 = 2;        // turn the square green    
final int EVENT_BUTTON3 = 3;        // turn the square blue
final int EVENT_BUTTON4 = 4;        // a radio button has been clicked
final int EVENT_BUTTON5 = 5;        // a checkbox has been checked
final int EVENT_BUTTON6 = 6;        // a slider has been used
final int EVENT_BUTTON7 = 7;        // change from Widget demo to Screen demo
final int EVENT_NULL = 0;
final int SCREEN_1 = 1, SCREEN_2 = 2;
final int apple = 3, orange = 4;
float value = 0;                    // slider value
int currentScreen = 0;              // Widget demo is screen 0
int mode = 1;                       // Widget demo is mode 1, Screen demo is mode -1
color squareCol = color(128);       // square colour defaults to grey
Screen screen, screen1, screen2;    // screens

void settings() {
    size(1024, 512);
}

void setup() {
    Widget widget1, widget2, widget3, widget4, widget5, widget6, widget7, widget8, widget9, widget10, widget11;
    stdFont = loadFont("SegoeUI-30.vlw");
    textFont(stdFont);
    widget1 = new Widget(100, 100, 100, 40, "Red", color(100), color(0), stdFont, EVENT_BUTTON1, "button");
    widget2 = new Widget(100, 200, 100, 40, "Green", color(125), color(0), stdFont, EVENT_BUTTON2, "button");
    widget3 = new Widget(100, 300, 100, 40, "Blue", color(150), color(0), stdFont, EVENT_BUTTON3, "button");
    widget4 = new Widget(300, 100, 50, 50, "a", color(150), color(0), stdFont, EVENT_BUTTON4, "radio");
    widget6 = new Widget(300, 200, 50, 50, "b", color(150), color(0), stdFont, EVENT_BUTTON4, "radio");
    widget5 = new Widget(300, 300, 50, 50, "c", color(150), color(0), stdFont, EVENT_BUTTON4, "radio");
    widget7 = new Widget(500, 100, 50, 50, "d", color(150), color(0), stdFont, EVENT_BUTTON5, "check");
    widget8 = new Widget(500, 200, 50, 50, "e", color(150), color(0), stdFont, EVENT_BUTTON5, "check");
    widget9 = new Widget(500, 300, 50, 50, "f", color(150), color(0), stdFont, EVENT_BUTTON5, "check");
    widget10 = new Widget(100, 400, 40, 40, "", color(150), color(255,0,0), stdFont, EVENT_BUTTON6, "slider", 500);
    widget11 = new Widget(700, 100, 200, 40, "Screen Demo", color(150), color(0), stdFont, EVENT_BUTTON7, "button");
    widgetList = new ArrayList();
    radioButtons = new ArrayList();
    checkButtons = new ArrayList();
    sliders = new ArrayList();
    widgetList.add(widget1); 
    widgetList.add(widget2);
    widgetList.add(widget3);
    widgetList.add(widget4);
    widgetList.add(widget5);
    widgetList.add(widget6);
    widgetList.add(widget7);
    widgetList.add(widget8);
    widgetList.add(widget9);
    widgetList.add(widget10);
    widgetList.add(widget11);
    for (int p = 0; p < widgetList.size(); p++) {
        Widget tempWidget1 = (Widget) widgetList.get(p);
        if (tempWidget1.type == "radio") {
            radioButtons.add(tempWidget1);
        }
    }
    for (int q = 0; q < widgetList.size(); q++) {
        Widget tempWidget2 = (Widget) widgetList.get(q);
        if (tempWidget2.type == "check") {
            checkButtons.add(tempWidget2);
        }
    }
    for (int r = 0; r < widgetList.size(); r++) {
        Widget tempWidget3 = (Widget) widgetList.get(r);
        if (tempWidget3.type == "slider") {
            sliders.add(tempWidget3);
        }
    }
    ArrayList scOneWidgets = new ArrayList();
    ArrayList scTwoWidgets = new ArrayList();
    // a universal widget to swap between the Widget demo and the Screen demo
    Widget toWidgetDemo = new Widget(700, 100, 200, 40, "Widget Demo", color(150), color(0), stdFont, EVENT_BUTTON7, "button");

    scOneWidgets.add(new Widget(width / 4, 100, 180, 40, "Next Page >", color(100), color(0), stdFont, SCREEN_2, "button"));
    scOneWidgets.add(new Widget(width / 4, 300, 140, 40, "Apples", color(100), color(0), stdFont, apple, "button"));
    scOneWidgets.add(toWidgetDemo);
    
    scTwoWidgets.add(new Widget(width / 4, 100, 180, 40, "< Last Page", color(100), color(0), stdFont, SCREEN_1, "button"));
    scTwoWidgets.add(new Widget(width / 4, 300, 140, 40, "Oranges", color(100), color(0), stdFont, orange, "button"));
    scTwoWidgets.add(toWidgetDemo);
    
    
    screen1 = new Screen(1, color(100, 150, 100), scOneWidgets);
    screen2 = new Screen(2, color(150, 100, 150), scTwoWidgets);
    screen = screen1;
}

void draw() {
    if (mode == 1) {
        background(64);
        stroke(0);
        fill(squareCol);
        rect(width - width / 4, height / 2, 100, 100);
        for (int i = 0; i < widgetList.size(); i++) {
            Widget aWidget = (Widget) widgetList.get(i);
            aWidget.draw();
        }
        fill(255);
        text((int)value, 100, 500);
    }
    
    if (mode == - 1) {
        screen.draw();
    }
}

void mousePressed() {
    int event;
    if (mode == 1) {
        for (int i = 0; i < widgetList.size(); i++) {
            Widget aWidget = (Widget) widgetList.get(i);
            event = aWidget.getEvent(mouseX, mouseY);
            switch(event) {
                case EVENT_BUTTON1:
                    println("button 1!");
                    squareCol = color(255, 0, 0);
                    break;
                case EVENT_BUTTON2:
                    squareCol = color(0, 255, 0);
                    println("button 2!");
                    break;
                case EVENT_BUTTON3:
                    squareCol = color(0, 0, 255);
                    println("button 3!");
                    break;
                case EVENT_BUTTON7:
                    mode *= - 1;
                break;
            }
        }
        for (int j = 0; j < radioButtons.size(); j++) {
            Widget radio = (Widget) radioButtons.get(j);
            event = radio.getEvent(mouseX, mouseY);
            
            if (!radio.ticked && event == EVENT_BUTTON4) {
                radio.ticked = true;
                radio.tickedColor = 255;
                for (int k = 0; k < radioButtons.size(); k++) {
                    Widget tempRad = (Widget) radioButtons.get(k);
                    if (k != j) {
                        tempRad.ticked = false;
                        tempRad.tickedColor = 0;
                    }
                }
            }
        }
        for (int c = 0; c < checkButtons.size(); c++) {
            Widget check = (Widget) checkButtons.get(c);
            event = check.getEvent(mouseX, mouseY);            
            if (event == EVENT_BUTTON5) {
                boolean toCheck = check.ticked;
                toCheck = !toCheck;
                check.tickedColor *= - 1;
            }
        }
    } else if (mode == - 1) {
        screen.chooseScreen();
    }
}

void checkHover(ArrayList arr) {
    int event;
    for (int i = 0; i < arr.size(); i++) {
        Widget w = (Widget) arr.get(i);
        event = w.getEvent(mouseX, mouseY);
        if (event != 0) {
            w.borderColor = color(255);
            w.labelColor = color(255);
        } else {
            w.borderColor = color(0);
            w.labelColor = color(0);
        }
    }
}

void mouseMoved() {
    checkHover(widgetList);
    checkHover(screen.widgets);
}

void mouseDragged() {
    int event;
    if (mode == 1) {
        for (int s = 0; s < sliders.size(); s++) {
            Widget slider = (Widget) sliders.get(s);
            event = slider.getEvent(mouseX, mouseY);
            if (event == EVENT_BUTTON6) {
                slider.x = mouseX - slider.width / 2;
                float tLimit = slider.limit + slider.tX;        // true limit (position of end) = slider bar length + slider bar start pos
                float tMin = slider.tX - slider.width / 2;      // true minimum value = slider bar start pos - slider control width / 2
                if (slider.x + slider.width / 2 >= tLimit) {
                    slider.x = (int)tLimit - (slider.width / 2);
                }
                if (slider.x + (slider.width / 2) <= slider.tX) {
                    slider.x = (int)tMin;
                }
                // the value is the percentage position of the slider control (SC) relative to the length of the slider bar.
                // this should be calculated by 
                //      (SC pos - min) / (max - min), 
                // but as SC pos is altered, it needs to be changed slightly
                // the formula is now 
                //      (SC pos - min) / (max - min - SC width / 2) 
                // as SC width / 2 is subtracted from the mouse position to center the mouse in the middle of the SC
                // it is then multiplied by 100 to create a whole number.
                value = 100 * (((slider.x - tMin) / (tLimit - tMin - slider.width / 2)));
            }
        }
    }
}



//  DEBUG
void keyPressed() {
    if (key == ENTER) {
        mode *= - 1;
    }
}

