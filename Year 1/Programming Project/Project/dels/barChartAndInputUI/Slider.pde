public class Slider {
    float x, y, width, height;
    float barWidth;
    String type;
    String label; 
    int event;
    color widgetColor, labelColor, borderColor;
    boolean ticked = false;
    int tickedColor = - 255;
    int limit;
    float tX, tY;
    String axis;
    float tLimit, tMin;
    boolean canMove = true;
    
    Slider(float x, float y, float width, float height, String label, color widgetColor, color labelColor, color borderColor, int event, int limit, String axis) {
        this.x = x; this.y = y; this.width = width; this.height = height;
        this.label = label; this.event = event;
        this.widgetColor = widgetColor; this.labelColor = labelColor; 
        this.borderColor = borderColor; this.limit = limit; this.axis = axis;
        labelColor = color(BLACK);
        borderColor = color(BLACK);
        tX = x;
        tY = y;
        barWidth = 10;
        resetPos();
    }
    
    void draw() {
        if (axis.equalsIgnoreCase("x")) xSlider();
        if (axis.equalsIgnoreCase("y")) ySlider();
    }

    void resetPos() {
        switch (axis) {
            case "x":
                x = tX;
                break;
            case "y":
                y = tY;
                break;
        }
    }

    void bound() {
        // Move the slider controller to the mouse position and hold it within the boundaries set by the bar length

        // The value is the percentage position of the beginning of the slider control (SC) relative to the length of the slider bar.
        // The formula is to calculate this is:
        //      (SC pos - min) / (max - min - SC width)
        // where SC pos is the position of the slider control, max is highest position it can have, and min is the lowest position it can have.
        // 
        // To create a bar out of this, use the formula:
        //      value = range * (((slider.axis - tMin) / (tLimit - tMin - slider.length))) - min
        // where:
        //      min is the minimum allowed value (leave blank for 0), 
        //      slider.axis is the axis on which the slider is moving (x or y), 
        //      slider.length is the slider's size (width or height), 
        //      tMin is the slider bar start position, and
        //      tLimit is tMin + the specified limit.

        switch (this.axis) {
            case "x":
                tLimit = this.limit + this.tX;              // true limit (position of end) = slider bar length + slider bar start pos
                tMin = this.tX;                             // true minimum value = slider bar start pos - slider control width / 2
                this.x = mouseX - this.width / 2;
                if (this.x + this.width >= tLimit) {
                    this.x = (int)tLimit - this.width;
                }
                if (this.x <= this.tX) {
                    this.x = (int)tMin;
                }
            break;
            case "y":
                tLimit = this.limit + this.tY;
                tMin = this.tY;
                this.y = mouseY - this.height / 2;
                if (this.y + this.height >= tLimit) {
                    this.y = (int)tLimit - this.height;
                }
                if (this.y <= this.tY) {
                    this.y = (int)tMin;
                }
            break;
        }
    }
    
    int getEvent(int mX, int mY) {
        if (mX > x && mX < x + width && mY > y && mY < y + height) {
            
            return event;
        }
        return EVENT_NULL;
    }
    
    float getX() {
        return x;
    }
    
    float getY() {
        return y;
    }
    
    public void setBarWidth(int w) {
        barWidth = w;
    }
    
    public void setControlWidth(float w) {
        this.width = w;
    }
    
    public void setControlHeight(float h) {
        this.height = h;
    }

    public void setLabel(String s) {
        this.label = s;
    }
    
    void xSlider() {
        fill(widgetColor*1.2);
        rect(tX, tY + (height / 2) - (barWidth / 2), limit, barWidth); // body of slider; min and max
        fill(widgetColor);
        stroke(borderColor);
        rect(x, y, width, height); // slider controller
        fill(0);
        textAlign(CENTER);
        text(label, tX + limit, tY - height / 2.5); // slider value label
        textAlign(LEFT);
    }
    
    void ySlider() {
        fill(widgetColor*1.2);
        rect(tX + (width / 2) - (barWidth / 2), tY, barWidth, limit); // body of slider; min and max
        fill(widgetColor);
        stroke(borderColor);
        rect(x, y, width, height); // slider controller
        fill(0);
        textAlign(CENTER);
        text(label, tX + width/2.5, tY/2); // slider value label
        textAlign(LEFT);
    }
}
