class Widget {
    int x, y, width, height;
    String label; 
    int event;
    int group;
    color widgetColor, labelColor;
    PFont widgetFont;
    boolean borderIsWhite = false;
    boolean ticked = false;
    boolean isRadio = false;
    boolean checked = false;
    boolean isCheck = false;
    int tickedOpacity = -255;
    
    Widget(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event) {
        this.x = x;
        this.y = y; 
        this.width = width; 
        this.height = height;
        this.label = label; 
        this.event = event;
        this.widgetColor = widgetColor; 
        this.widgetFont = widgetFont;
        this.labelColor = labelColor;
    }
    
    Widget(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event, int group) {
        this.x = x;
        this.y = y; 
        this.width = width; 
        this.height = height;
        this.label = label; 
        this.event = event;
        this.widgetColor = widgetColor; 
        this.widgetFont = widgetFont;
        this.labelColor = labelColor;
        this.group = group;
    }
    
    void drawTheWidget() {
        fill(widgetColor);
        rect(x, y, width, height, 10);
        fill(labelColor);
        textFont(widgetFont);
        textSize(12);
        text(label, x + 10, y + height - 10);
        //println(x + "  " + y);
    }
    
    int getEvent(int mX, int mY) {
        if (mX > x && mX < x + width && mY > y && mY < y + height) {
            return event;
        }
        return EVENT_NULL;
    }
    
    boolean getBorderIsWhite() {
        return borderIsWhite;
    }
    
    void setBorderIsWhite(boolean updatedBorderIsWhite) {
        borderIsWhite = updatedBorderIsWhite;
    }
}


class Radio extends Widget {
    Radio(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event, int group) {
        super(x, y, width, height, label, widgetColor, labelColor, widgetFont, event, group);
        isRadio = true;
    }
}


class CheckBox extends Widget {
    boolean ticked = false;
    CheckBox(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event) {
        super(x, y, width, height, label, widgetColor, labelColor, widgetFont, event);
        isCheck = true;
    }
}

class SearchBar extends Widget {
    SearchBar(int x, int y, int width, int height, String label, color widgetColor, color labelColor, PFont widgetFont, int event) {
        super(x, y, width, height, label, widgetColor, labelColor, widgetFont, event);
    }
    
    void setLabel(String label) {
        this.label = label;
    }
}
