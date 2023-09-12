// graph types:
//      - bar chart: bar
//      - histogram: hist
//      - line chart: lchart
//      - scatter plot: scatter
//      - horizontal bar chart: hbar
//      - area chart: area
//  - that scatter plots aren't useful since this data isn't all that noisy.    
//  - an area chart is more useful for multiple datasets, for which there isn't yet any functionality.
// background types: 
//      - blank: no background
//      - grid: gridded background. grid size controlled with setGridSize(int)
//      - ybar: vertical (or horizontal if using hbar) line that acts as a guide. amount dependent on ySteps.
//    - may add more (patterns and stuff, idk)


public class Graph {
    float xPos, yPos, gWidth, gHeight;
    float xMin, xMax, yMin, yMax;
    int textSize, xSteps, ySteps;
    color bg;
    color limitCol, elemCol;
    boolean grid = false, ybar = false, blank = false;
    boolean data_gen = false, data_set = false;
    boolean trunc = false;
    boolean mouseHover = false;
    String graphType;
    String title;
    boolean showY1 = false, showY2 = false;
    float gridSize = 30;
    float barGap = 200;
    int valWidth = 0;
    PVector graphOrigin;
    DecimalFormat df = new DecimalFormat("###,###,###");
    LinkedHashMap<String, Integer> data = new LinkedHashMap<String, Integer>(); 
    ArrayList<GraphElement> elems = new ArrayList<GraphElement>();
    Graph(float x, float y, float gWidth, float gHeight, color bg, color elemCol, int textSize, String graphType, String design, boolean mh) {
        xPos = x;
        yPos = y; 
        mouseHover = mh;
        this.gWidth = gWidth; 
        this.gHeight = gHeight; 
        this.bg = bg; 
        this.graphType = graphType; 
        this.textSize = textSize; 
        this.elemCol = elemCol;
        textSize(textSize);
        graphOrigin = new PVector(xPos, yPos + gHeight);
        switch(design) {
        case "grid":
            grid = true;
            break;
        case "ybar":
            ybar = true;
            break;
        case "blank":
            blank = true;
            break;
        default :
            blank = true;
            break;
        }
    }

    public void setLimits(String type, float x1, float x2, float y1, float y2, int xSteps, int ySteps, color col, boolean trunc) {
        /* 
         * "gen": generic data; integers and/or floats/doubles on both the x and y axis
         * "set": an array of objects; e.g. strings and integers
         */
        this.ySteps = ySteps;
        this.trunc = trunc;
        limitCol = col;
        xMin = x1;
        xMax = x2;
        yMin = y1;
        yMax = y2;
        valWidth = String.valueOf((int)(yMin + yMax / ySteps)).length();
        // if data is generic, set xSteps to given xSteps. otherwise, set xSteps to the size of the dataset
        if (type.equalsIgnoreCase("gen")) {
            this.xSteps = xSteps;
            data_gen = true;
        } else if (type.equalsIgnoreCase("set")) {
            this.xSteps = data.size();
            data_set = true;
        }
    }

    public void loadData(LinkedHashMap<String, Integer> data) {
        this.data = data;
    }

    public void loadData(ArrayList<String> xValues, ArrayList<Integer> yValues) {
        data.clear();
        for (int i = 0; i < yValues.size(); i++) {
            data.put(xValues.get(i), yValues.get(i));
        }
    }

    public void setGridSize(float value) {
        if (value <= 0) {
            this.gridSize = 1;
            return;
        }
        this.gridSize = value;
    }

    public void setBarGap(float value) {
        if (value <= xSteps) {
            this.barGap = xSteps+10;
            return;
        }
        this.barGap = value;
    }

    public void setLabel(String xT, String yT, String title) {
        this.title = title;
        textSize(textSize * 1.5);
        fill(WHITE);
        textAlign(CENTER);
        text(title, xPos + gWidth / 2, yPos - gHeight/20);

        if (!graphType.equalsIgnoreCase("hbar")) {
            // x-axis label
            text(xT, xPos + gWidth / 2, yPos + gHeight * 1.225);      

            // rotate y-axis label
            pushMatrix();
            translate(xPos - gWidth/20 - textSize*2 - valWidth*5, yPos + gHeight / 2);
            rotate(radians(270));
            text(yT, 0, 0);
            popMatrix();
        } else {
            // x-axis label (cases)
            text(yT, xPos + gWidth / 2, yPos + gHeight * 1.225);

            // rotate y-axis label (dates)
            pushMatrix();
            translate(xPos - gWidth/20 - textSize*2 - valWidth*5, yPos + gHeight / 2);
            rotate(radians(270));
            text(xT, 0, 0);
            popMatrix();
        }
    }

    public void displayGraph() {
        textSize(textSize);
        rectMode(CORNER);
        noStroke();
        fill(bg);
        rect(xPos, yPos, gWidth, gHeight);

        if (grid) {
            stroke(255, 255, 255, 100);
            for (float i = graphOrigin.y; i >= graphOrigin.y - gHeight; i -= gridSize) {   // horizontal lines
                line(graphOrigin.x, i, gWidth + graphOrigin.x, i);
            }

            for (float i = graphOrigin.x; i <= graphOrigin.x + gWidth; i += gridSize) {     // vertical lines
                line(i, graphOrigin.y - gHeight, i, graphOrigin.y);
            }
        }

        fill(limitCol);

        if (!graphType.equalsIgnoreCase("hbar")) {          // if user wants a vertical graph
            // x-axis figures
            if (data_gen) {
                for (int i = 0; i <= xSteps; i++) {
                    // for each step on the x-axis, get the value at that step as a part of the max value
                    text(nf((xMin + xMax / xSteps * i), 0, 1), graphOrigin.x + (gWidth / xSteps) * i, graphOrigin.y + textSize * 1.5);
                }
            }

            if (data_set) {
                ArrayList<String> tObj = new ArrayList<String>();
                tObj.addAll(data.keySet());
                textAlign(CENTER);
                float maxSteps = floor(gWidth/tObj.get(0).length()/2);  // get a relative value for highest amount of steps based on graph width
                int forSteps = 1;
                if (xSteps >= maxSteps) {
                    // if the amount of values in the dataset is too high (resulting in them being squished together),
                    // set the iterator to a higher value
                    forSteps = (int)(gWidth/textSize/25);
                }
                for (int i = 0; i < xSteps; i+= forSteps) {

                    // if the graph is a histogram, align the x-axis values based on relative cardinal value
                    // otherwise, center them in the middle 
                    float tPos = 0;
                    if (!graphType.equalsIgnoreCase("hist")) tPos = (gWidth / (xSteps+1)) * (i+1);
                    else tPos = (gWidth / xSteps) * i + (gWidth / barGap * 1.5) * 4;

                    
                    int identifier; // last two letters of the entered string
                    try {
                        // if the last two letters can be formatted as numbers, set identifier to them
                        // in this case, they would either be "20" or "21", which shows which year it is
                        identifier = Integer.parseInt(tObj.get(i).substring(tObj.get(i).length()-2));
                    } catch (Exception e) {
                        // if not, 
                        identifier = 0;
                    }

                    if (identifier == 20 && !showY1) {
                        textSize(textSize*2);
                        text(tObj.get(i).substring(tObj.get(i).length()-4), graphOrigin.x + tPos, graphOrigin.y + textSize * 6);
                        showY1 = true;
                        textSize(textSize);
                    }
                    if (identifier == 21 && !showY2) {
                        textSize(textSize*2);
                        text(tObj.get(i).substring(tObj.get(i).length()-4), graphOrigin.x + tPos, graphOrigin.y + textSize * 6);
                        showY2 = true;
                        textSize(textSize);
                    }
                    if (identifier == 0) {
                        forSteps = 1;
                    }

                    if (identifier != 0) {
                        if (i % 2 == 0) {
                            if (trunc) {
                                text(tObj.get(i).substring(0, 5), graphOrigin.x + tPos, graphOrigin.y + textSize * 3.5);
                            } else {
                                text(tObj.get(i), graphOrigin.x + tPos, graphOrigin.y + textSize * 3.5);
                            }
                        } else {
                            if (trunc) {
                                text(tObj.get(i).substring(0, 5), graphOrigin.x + tPos, graphOrigin.y + textSize * 1.5);
                            } else {
                                text(tObj.get(i), graphOrigin.x + tPos, graphOrigin.y + textSize*5);
                            }
                        }
                    } else {
                        textSize(11);
                        pushMatrix();
                        translate(graphOrigin.x + tPos, graphOrigin.y + textSize * 4);
                        rotate(radians(270));
                        text(tObj.get(i), 0, 0);
                        popMatrix();
                        textSize(textSize);
                    }
                    
                }
                showY1 = false;
                showY2 = false;
            }

            // y-axis figures
            for (int i = ySteps; i >= 0; i--) {
                if (ybar) {
                    stroke(255, 255, 255, 100);
                    line(xPos, graphOrigin.y - (gHeight / ySteps) * i, xPos + gWidth - 1, graphOrigin.y - (gHeight / ySteps) * i);
                }
                if (i % 2 == 0) {
                    text(df.format((int)(yMin + yMax / ySteps * i)), graphOrigin.x - (textSize + valWidth-3) * 2, graphOrigin.y - (gHeight / ySteps) * i);
                } else {
                    text(df.format((int)(yMin + yMax / ySteps * i)), graphOrigin.x - (textSize + valWidth-3) * 4, graphOrigin.y - (gHeight / ySteps) * i);
                }
            }
        } else {                            // if user wants a horizontal graph (only with hbar)
            // y-axis figures
            if (data_gen) {
                for (int i = 0; i <= xSteps; i++) {
                    text(nf((xMin + xMax / xSteps * i), 0, 1), graphOrigin.x + textSize * 1.5, graphOrigin.y + (gHeight / xSteps) * i);
                }
            }

            if (data_set) {
                ArrayList<String> tObj = new ArrayList<String>();
                tObj.addAll(data.keySet());
                textAlign(CENTER);
                float maxSteps = floor(gHeight/tObj.get(0).length()/20);
                int forSteps = 1;
                if (xSteps >= maxSteps) {
                    forSteps = (int)(gHeight/textSize/15);
                }


                for (int i = 0; i < xSteps; i += forSteps) {
                    int identifier;
                    try {
                        identifier = Integer.parseInt(tObj.get(i).substring(tObj.get(i).length()-2));
                    } catch (Exception e) {
                        identifier = 0;
                    }
                    if (identifier == 20 && !showY1) {
                        textSize(textSize*2);
                        text(tObj.get(i).substring(tObj.get(i).length()-4), graphOrigin.x - textSize * 9, yPos + (gHeight / (xSteps + 1)) * (i + 1) - (gHeight / barGap)/2);
                        showY1 = true;
                        textSize(textSize/1.5);
                    }
                    if (identifier == 21 && !showY2) {
                        textSize(textSize*2);
                        text(tObj.get(i).substring(tObj.get(i).length()-4), graphOrigin.x - textSize * 9, yPos + (gHeight / (xSteps + 1)) * (i + 1) - (gHeight / barGap)/2);
                        showY2 = true;
                        textSize(textSize);
                    }
                    if (identifier == 0) {
                        forSteps = (int)(gWidth/textSize/15);
                    }
                    textSize(textSize);
                    if (i % 2 == 0) {
                        if (trunc) {
                            text(tObj.get(i).substring(0, 5), graphOrigin.x - textSize * 3.5, yPos + (gHeight / (xSteps + 1)) * (i + 1) - (gHeight / barGap)/2);
                        } else {
                            text(tObj.get(i), graphOrigin.x - textSize * 5, yPos + (gHeight / (xSteps + 1)) * (i + 1) - (gHeight / barGap)/2);
                        }
                    } else {
                        if (trunc) {
                            text(tObj.get(i).substring(0, 5), graphOrigin.x - textSize * 2.5, yPos + (gHeight / (xSteps + 1)) * (i + 1) - (gHeight / barGap)/2);
                        } else {
                            text(tObj.get(i), graphOrigin.x - textSize * 4, yPos + (gHeight / (xSteps + 1)) * (i + 1) - (gHeight / barGap)/2);
                        }
                    }
                }
                showY1 = false;
                showY2 = false;
            }

            // x-axis figures
            for (int i = ySteps; i >= 0; i--) {
                if (ybar) {
                    stroke(255, 255, 255, 100);
                    line(graphOrigin.x + (gWidth / ySteps) * i, yPos + gHeight - 1, graphOrigin.x + (gWidth / ySteps) * i, yPos);
                }
                if (i % 2 == 0) {
                    text(df.format((int)(yMin + yMax / ySteps * i)), graphOrigin.x + (gWidth / ySteps) * i, yPos + gHeight + textSize * 3);
                } else {
                    text(df.format((int)(yMin + yMax / ySteps * i)), graphOrigin.x + (gWidth / ySteps) * i, yPos + gHeight + textSize * 1.5);
                }
            }
        }


        setGraphElementType(graphType);

        ArrayList<Integer> tValues = new ArrayList<Integer>();
        tValues.addAll(data.values());
        float ballRad = gWidth/175;
        textAlign(CENTER);
        switch(graphType) {
        case "bar":
            elems.clear();
            if (mouseHover) {
                for (int i = 0; i < xSteps; i++) {
                    elems.add(new GraphElement(graphOrigin.x + (gWidth / (xSteps+1)) * (i+1) - (gWidth / barGap * 1.5)/2, graphOrigin.y, gWidth / barGap * 1.5, gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), elemCol+i));
                }
                mouseHover(elems);
            } else {
                for (int i = 0; i < xSteps; i++) {
                    fill(elemCol);
                    rect(graphOrigin.x + (gWidth / (xSteps+1)) * (i+1) - (gWidth / barGap * 1.5)/2, graphOrigin.y, gWidth / barGap * 1.5, gHeight * ((float) - tValues.get(i) / Collections.max(tValues)));
                }
            }

            break;

        case "hist":
            elems.clear();
            stroke(elemCol/5);
            if (mouseHover) {
                for (int i = 0; i < xSteps; i++) {
                    
                    elems.add(new GraphElement(graphOrigin.x + (gWidth / xSteps) * i, graphOrigin.y, gWidth / xSteps, gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), elemCol+i));
                }
                mouseHover(elems);
            } else {
                for (int i = 0; i < xSteps; i++) {
                    fill(elemCol);
                    rect(graphOrigin.x + gWidth / (xSteps) * i, graphOrigin.y, gWidth / xSteps, gHeight * ((float) - tValues.get(i) / Collections.max(tValues)));
                }
            }
            break;
        case "lchart":
            elems.clear();
            stroke(elemCol/5);
            if (mouseHover) {
                for (int i = 0; i < xSteps; i++) {
                    elems.add(new GraphElement(graphOrigin.x + (gWidth / (xSteps)) * (i+1), graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), ballRad, elemCol+i));
                    if (i < xSteps - 1) {
                        line(graphOrigin.x + (gWidth / (xSteps)) * (i+1), 
                            graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), 
                            graphOrigin.x + (gWidth / (xSteps)) * (i+2), 
                            graphOrigin.y + gHeight * ((float) - tValues.get(i+1) / Collections.max(tValues))
                            );
                    }
                }
                mouseHover(elems);
            } else {
                for (int i = 0; i < xSteps; i++) {
                    fill(elemCol);
                    if (i < xSteps - 1) {
                        line(graphOrigin.x + (gWidth / (xSteps)) * (i+1), 
                            graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), 
                            graphOrigin.x + (gWidth / (xSteps)) * (i+2), 
                            graphOrigin.y + gHeight * ((float) - tValues.get(i+1) / Collections.max(tValues))
                            );
                    }
                    ellipse(graphOrigin.x + (gWidth / (xSteps)) * (i+1), graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), ballRad, ballRad);
                }
            }

            break;
        case "hbar":
            elems.clear();
            if (mouseHover) {
                for (int i = 0; i < xSteps; i++) {
                    elems.add(new GraphElement(graphOrigin.x, (graphOrigin.y - gHeight) + (gHeight / (xSteps + 1)) * (i+1), gWidth * ((float)tValues.get(i) / Collections.max(tValues)), gHeight/barGap*1.5, elemCol+i));
                }
                mouseHover(elems);
            } else {
                for (int i = 0; i < xSteps; i++) {
                    fill(elemCol);
                    rect(graphOrigin.x, (graphOrigin.y - gHeight) + (gHeight / (xSteps + 1)) * (i+1), gWidth * ((float)tValues.get(i) / Collections.max(tValues)), gHeight/barGap*1.5);
                }
            }
            break;
        case "scatter":
            elems.clear();
            stroke(elemCol/5);
            if (mouseHover) {
                for (int i = 0; i < xSteps; i++) {
                    fill(elemCol);
                    elems.add(new GraphElement(graphOrigin.x + (gWidth / (xSteps+1)) * (i+1) + ballRad/2, graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), ballRad*2, elemCol + i));
                }
                mouseHover(elems);
            } else {
                for (int i = 0; i < xSteps; i++) {
                    fill(elemCol);
                    ellipse(graphOrigin.x + (gWidth / (xSteps+1)) * (i+1) + ballRad/2, graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), ballRad*2, ballRad*2);
                }
            }
            break;
        case "area":
            elems.clear();
            createArea(title, data, elemCol);

            // Doesn't really work with GraphElement class. put here in case something magically fixes it
            if (mouseHover) {
                mouseHover(elems);
            }
            break;
        default :
            // default to empty graph
            break;
        }


        stroke(255, 0, 0);
        line(graphOrigin.x, graphOrigin.y, graphOrigin.x + gWidth, graphOrigin.y); // x axis
        line(graphOrigin.x, graphOrigin.y, graphOrigin.x, graphOrigin.y - gHeight);  // y axis
    }

    public void setGraphBackgroundType(String type) {
        switch(type) {
        case "grid":
            grid = true;
            ybar = false;
            blank = false;
            break;
        case "ybar":
            ybar = true;
            grid = false;
            blank = false;
            break;
        case "blank":
            blank = true;
            ybar = false;
            grid = false;
            break;
        default :
            blank = true;
            ybar = false;
            grid = false;
            break;
        }
    }
    
    public void setGraphElementType(String type) {
        graphType = type;
    }

    public void setHoverEvent(boolean b) {
        mouseHover = b;
    }

    void mouseHover(ArrayList<GraphElement> elems) {
        for (GraphElement ge : elems) {
            ge.draw();
        }
        ArrayList<String> yKeys = new ArrayList<String>();
        ArrayList<Integer> yVal = new ArrayList<Integer>();
        yKeys.addAll(data.keySet());
        yVal.addAll(data.values());
        color c = get(mouseX, mouseY);
        textSize(textSize*1.4);
        for (int i = 0; i < xSteps; i++) {
            GraphElement ge = elems.get(i);
            if (c == ge.getCol()) {
                float boxX = mouseX + gWidth/110;
                float boxY = mouseY - gHeight/100;
                float boxSize = gWidth/10 * 1.25;
                if (boxX + boxSize >= width) boxX = width - boxSize;
                if (boxX <= 0) boxX = 0;
                if (boxY >= height) boxY = height;
                if (boxY - (boxSize) < 0) boxY = boxSize;
                fill(BLACK);
                rect(boxX, boxY, boxSize, -boxSize/2.5);
                fill(WHITE);
                text("Cases: " + df.format(yVal.get(i)) + "\n" + yKeys.get(i), boxX + boxSize/2, boxY - boxSize/4.5);
            }
        }
    }


    // this may be used to allow for multiple area graphs on the screen at once.
    public void createArea(String label, LinkedHashMap<String, Integer> gData, color col) {
        ArrayList<Integer> tValues = new ArrayList<Integer>();
        gData = data;
        tValues.addAll(gData.values());
        fill(col);  
        text(label, graphOrigin.x + gWidth, graphOrigin.y + gHeight);
        PShape area = createShape();

        beginShape();
        fill(col);
        stroke(col/2);
        if (mouseHover) {
            elems.add(new GraphElement(graphOrigin.x, graphOrigin.y, col, 0));
            for (int i = 0; i < xSteps; i++) {
                elems.add(new GraphElement(graphOrigin.x + (gWidth / (xSteps-1)) * i, graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)), col, i+1));
            }
            elems.add(new GraphElement(graphOrigin.x + gWidth, graphOrigin.y, col, xSteps+1));
        } else {
            vertex(graphOrigin.x, graphOrigin.y);
            for (int i = 0; i < xSteps; i++) {
                vertex(graphOrigin.x + (gWidth / (xSteps-1)) * i, graphOrigin.y + gHeight * ((float) - tValues.get(i) / Collections.max(tValues)));
            }
            vertex(graphOrigin.x + gWidth, graphOrigin.y);
        }
        endShape(CLOSE);

        shape(area);
    }

    public color getElemColor() {
        return elemCol;
    }
}


/* 
 * Class meant for aid in displaying value of graph element when mouse is hovered over it.
 * Can be very lag-inducing on lower-end devices, so the user should be able to turn the option (and this class) off with a boolean in the Graph constructor.
 */

class GraphElement {
    String type;
    float xPos, yPos, w, h;
    float radius;
    color col;
    GraphElement(float x, float y, float w, float h, color col) {
        this.w = w;
        this.h = h;
        this.col = col;
        xPos = x;
        yPos = y;
        type = "rect";
    }

    GraphElement(float x, float y, float radius, color col) {
        this.radius = radius;
        this.col = col;
        xPos = x;
        yPos = y;
        type = "ellipse";
    }

    GraphElement(float x, float y, color col) {
        this.col = col;
        xPos = x;
        yPos = y;
        type = "vertex";
    }

    void draw() {
        fill(col);
        switch (type) {
        case "rect":
            rect(xPos, yPos, w, h);
            break;

        case "ellipse":
            ellipse(xPos, yPos, radius, radius);
            break;

        case "vertex":
            vertex(xPos, yPos);
            break;
        }
    }

    public String getType() {
        return type;
    }

    public float getX() {
        return xPos;
    }

    public float getY() {
        return yPos;
    }

    public color getCol() {
        return col;
    }
}