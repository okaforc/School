class Widget {
    int x, y, width, height;
    String type;
    String label; 
    int event;
    color widgetColor, labelColor, borderColor;
    boolean ticked = false;
    int tickedColor = - 255;
    int limit;
    PFont widgetFont;
    int tX, tY;
    
    Widget(int x, int y, int width, int height, String label,
        color widgetColor, color labelColor, PFont widgetFont, int event, String type) {
        this.x = x; this.y = y; this.width = width; this.height = height;
        this.label = label; this.event = event;
        this.widgetColor = widgetColor; this.labelColor = labelColor; 
        this.widgetFont = widgetFont; this.type = type;
        labelColor = color(0);
        borderColor = color(0);
    }
    
    Widget(int x, int y, int width, int height, String label,
        color widgetColor, color labelColor, PFont widgetFont, int event, String type, int limit) {
        this.x = x; this.y = y; this.width = width; this.height = height;
        this.label = label; this.event = event;
        this.widgetColor = widgetColor; this.labelColor = labelColor; 
        this.widgetFont = widgetFont; this.type = type; this.limit = limit;
        labelColor = color(0);
        borderColor = color(0);
        tX = x;
        tY = y;
    }
    
    void draw() {
        switch(type) {
            case "button":
                stroke(borderColor);
                fill(widgetColor);
                rect(x,y,width,height);
                fill(labelColor);
                textFont(widgetFont);
                text(label, x + 10, y + height - 10);
                break;
            case "radio":
                stroke(borderColor);
                fill(widgetColor);
                rect(x, y, width, height, 300);
                noStroke();
                fill(0, 0, 0, tickedColor);
                ellipse(x + width / 2, y + height / 2, width / 2, height / 2);
                fill(tickedColor, tickedColor, tickedColor);
                text(label, x + width * 1.5, y + height * 0.75);
                break;
            case "check":
                stroke(borderColor);
                fill(widgetColor);
                rect(x, y, width, height);
                noStroke();
                fill(0, 0, 0, tickedColor);
                ellipse(x + width / 2, y + height / 2, width / 2, height / 2);
                fill(tickedColor, tickedColor, tickedColor);
                text(label, x + width * 1.5, y + height * 0.75);
                break;
            case "slider":
                fill(widgetColor);
                rect(tX, tY + height / 2 - 5, limit, 10);
                stroke(borderColor);
                rect(x, y, width, height);
                break;
        }
    }
    
    
    int getEvent(int mX, int mY) {
        if (mX > x && mX < x + width && mY > y && mY < y + height) {
            return event;
        }
        return EVENT_NULL;
    }
}