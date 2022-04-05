public class Screen {
    ArrayList widgets = new ArrayList(3);
    color bg = color(100, 150, 100);
    boolean hasPrinted = false;     // avoid printing text multiple times per mouse click
    
    
    Screen(int position, color bg, ArrayList widgets) {
        this.widgets = widgets;
        this.bg = bg;
        currentScreen = position;
    }
    
    void draw() {
        background(bg);
        for (int i = 0; i < widgets.size(); i++) {
            Widget w = (Widget) widgets.get(i);
            w.draw();
        }
    }
    
    void chooseScreen() {
        int event;
        for (int i = 0; i < widgets.size(); i++) {
            Widget w = (Widget) widgets.get(i);
            event = getEvent(w);
            w.draw();
            
            switch(event) {
                case SCREEN_1:
                    currentScreen = 1;
                    screen = screen1;
                    break;
                case SCREEN_2:
                    currentScreen = 2;
                    screen = screen2;
                    break;
                case apple:
                    if (!hasPrinted) {
                        println("Apples");
                        hasPrinted = true;
                    }
                    break;
                case orange:
                    if (!hasPrinted) {
                        println("Oranges");
                        hasPrinted = true;
                    }
                    break;
                case EVENT_BUTTON7:
                    mode *= - 1;
                    break;
            }
        }
        hasPrinted = false;
    }
    
    int getEvent(Widget w) {
        return w.getEvent(mouseX, mouseY);
    }
    
    void addWidget(int x, int y, int width, int height, String label,
        color widgetColor, color labelColor, PFont widgetFont, int event, String type) {
        widgets.add(new Widget(x, y, width, height, label, widgetColor, labelColor, widgetFont, event, type));
    }
    
    void addWidget(Widget w) {
        widgets.add(w);
    }
}