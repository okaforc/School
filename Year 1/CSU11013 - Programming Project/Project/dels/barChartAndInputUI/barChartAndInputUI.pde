// beibhinn 23/04 - re-implementing a change that was overwritten (Clearing input in search bar if state is selected by geomap)
// chike 21/04 - Finalised all graphs, added data choosing system at top of screen 0, fixed slider bug (somewhat), added "View Raw Data" screen for states and areas on screens 3 and 4, decreased twitter box font to minimise overflow, fixed screen 4.2 widget allignment bug.
// chike 20/04 - Fully implemented all graphs; added all widgets to graphs, which let the user choose graph type, background, search, and search spread; cleared up screen 2 (only 2 options on left panel); added graph with all data to screen 1.
// odhran 20/04 - labeled screen 2 widgets + small bug fixing
// brendan 20/04 - redesigned home page.
// brendan 20/04 - reimplemented screen transitions and tweaked sound effects.
// beibhinn 19/04 - deleted scroll area list and minor bug fix when using full screen map, picking michigan overlapped with guam button. Just move mouse slightly after un-fullscreening the map to use territory buttons.
// brendan 19/04 - implemented sound effects.
// chike 18/04 - Fixed some scaling issues, added user options to screen 3 (g1), labelled event buttons, fixed radio buttons and checkboxes, and added quit screen. Slider on screen 2 is still not working.
// chike 16/04 - Added Slider class, fixed scaling and alignment issues on screen 2, added slider on screen 2 for area list, added extra screen for more geoMap sizes and its widgets, scaled all widgets
// odhran 14/04 - Implemented zooming in on screen 1
// beibhinn + brendan 13/04 - added NYTimes API with two different queries to display in horizontal scroll bar on home page 
//      -> not sure if it's pulling latest headlines or the same one every time - might need to make a string array of queries and switch it every so often
// chike     13/04 - Added the ability to scroll using the mouse wheel or touch pad on screen 5. Fixed scaling issues so screen 1 now scales with monitor screens. 
// brendan  13/04 - Implemented Screen transitions.
// odhran   12/04 - Implemented scrollbars
// brendan  12/04 - Fixed some bugs with the search bar on screen 1 that happened when invalid inputs are entered.
// beibhinn 11/04 - Some design changes to screen 2 - still have to add widgets so the other graphs can be accessed.
// beibhinn 10/04 - Added a scrollable listbox from the controlP5 library instead of the list of areas because it wasn't scrollable. Will fix the actual layout later!
// chike   10/04 - Fixed a bug where pressing "NEXT" before any dates were selected on screen 4.2 (g7) would cause the program to crash.
// chike    8/04 - Fixed a bug where entering dates in reverse order on screen 4.2 (g7) caused the program to crash.
// brendan  8/04 - cleaned/tidied up code by adding methods and creating a new GlobalVariables tab
// odhran   7/04 - added an image to home page and an embedded link button, a screen to see all the raw data and horizontally scrolling text bar
// brendan  7/04 - user can type in desired state alongside selecting the state from the map. if an invalid state is entered, an error is shown.
// beibhinn 7/04 - implemented the controlP5 GUI to begin making drop down lists for choosing dates - just need to get it to return a specific date and pass that to the graph 7
// beibhinn 6/04 - integrated Chike's code from loadTableData file and implemented widgets for user options to deal with graph 3,5,7 - problem with graph 7, entering dates
// Beibhinn 29/03 - integrated the design basis with the barChartAndInput program
// beibhinn 1/4 - added american territories as widgets

import controlP5.*;
import org.gicentre.geomap.*;
import java.util.*;
import java.text.*;
import com.temboo.core.*;
import com.temboo.Library.Twitter.Search.*;
import processing.sound.*;

void settings() {
    // size(888, 500);
    fullScreen();
}

void setup() {
    frameRate(60); // was originally 30, but a higher framerate makes the screen transitions look much smoother.
    sbx = new Scrollbar(0, height - 15, "x");
    sby = new Scrollbar(width - 15, 0, "y");

    // td = new TableData("daily-1M.csv"); // switched from 10k to 97k because the 10k dataset didn't contain Northern Mariana Islands
    td_mini = new TableData("cases-10k.csv");   // for all data in country ("sample")
    td_1m = new TableData("cases-97k.csv");      // daily 1M cases
    // td_c1m = new TableData("cases-1M.csv");     // total 1M cases
    // td_97k = new TableData("daily-97k.csv");    // daily 97k cases
    // td_c97k = new TableData("cases-97k.csv");   // total 97k cases

    session = new TembooSession("bevnr2020", "myFirstApp", "lffULM98DmpdPmADIHb8YT8XuBR0tpzc");
    //covidPic = loadImage("covid-pic.jpg");
    maskPeople = loadImage("maskPeople.png");
    vaccinePic = loadImage("vaccineImage.png");
    twitterLogo = loadImage("twitterLogo.png"); // using png with removed background
    blingSFX = new SoundFile(this, "bling.wav");
    blingSFX.amp(0.5); // volume
    clickSFX = new SoundFile(this, "click.wav");
    clickSFX.amp(0.2); // volume
    errorSFX = new SoundFile(this, "error.wav");
    errorSFX.amp(0.5); // volume
    slideSFX = new SoundFile(this, "slide.wav");
    slideSFX.amp(0.1); // volume

    stdFont = loadFont("MS-Gothic-26.vlw");   // changed font size to match text size
    stdFont_Big = loadFont("MS-Gothic-26.vlw");   // changed font size to match text size
    arialFont = createFont("ArialNovaCond-48.vlw", 32);
    textFont(stdFont);
    searchText = "covid";
    runTweetsChoreo(); // Run the Tweets Choreo function
    getTweetFromJSON(); // Parse the JSON response
    // displayText(); // Display the response

    apiKey = "9OaT50AMoryO2RMBWcIYEBCHQGLAhAIx";
    url = "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=coronavirus";
    query = "?q=covid&sort=newest";
    json = loadJSONObject(url + query + "&api-key=" + apiKey);
    headline = json.getJSONObject("response").getJSONArray("docs").getJSONObject(0).getJSONObject("headline").getString("main");

    // Adds all valid hover events (events for widgets that light up when hovered over) to an Integer arraylist.
    validHoverEvents.add(EVENT_BUTTON0);
    validHoverEvents.add(EVENT_BUTTON1);
    validHoverEvents.add(EVENT_BUTTON2);
    validHoverEvents.add(EVENT_BUTTON3);
    validHoverEvents.add(EVENT_BUTTON4);
    validHoverEvents.add(EVENT_BUTTON5);
    validHoverEvents.add(EVENT_BUTTON6);
    validHoverEvents.add(EVENT_BUTTON7);
    validHoverEvents.add(EVENT_BUTTON8);
    validHoverEvents.add(EVENT_BUTTON9);
    validHoverEvents.add(EVENT_BUTTON18);
    validHoverEvents.add(EVENT_BUTTON19);
    validHoverEvents.add(EVENT_BUTTON20);
    validHoverEvents.add(EVENT_BUTTON21);
    validHoverEvents.add(EVENT_BUTTON23);
    validHoverEvents.add(EVENT_BUTTON3);
    validHoverEvents.add(EVENT_BUTTON24);
    validHoverEvents.add(EVENT_BUTTON25);
    validHoverEvents.add(EVENT_BUTTON26);
    validHoverEvents.add(EVENT_BUTTON28);
    validHoverEvents.add(EVENT_BUTTON29);
    validHoverEvents.add(EVENT_BUTTON30);
    validHoverEvents.add(EVENT_BUTTON31);
    validHoverEvents.add(EVENT_BUTTON32);
    validHoverEvents.add(EVENT_STATE);
    validHoverEvents.add(EVENT_COUNTRY);
    validHoverEvents.add(EVENT_STATEAREAS);
    validHoverEvents.add(DATE_CONFIRMED);
    validHoverEvents.add(EVENT_T1M);
    validHoverEvents.add(EVENT_D1M);
    validHoverEvents.add(EVENT_T97K);
    validHoverEvents.add(EVENT_D97K);

    createScreens();                     // method to create screens
    currentScreen = screen0;

    createScrollableList();              // method to create scrollable list
    createGraphs();                      // method to create graphs
    createWidgets();                     // method to create widgets

    sld_2 = new Slider(47 * width / 60, height / 2, width / 45, height / 22, "", BLACK, BLACK, BLACK, EVENT_BUTTON27, 5 * height / 13, "y"); // slider instantiation
    sld_g = new Slider(floor(width / 2.25), 15.5 * height / 16, width / 40, height / 50, "1", color(150, 150, 200), color(BLACK), color(BLACK), EVENT_BUTTON27, 5 * height / 13, "x"); // slider instantiation
    sld_g.resetPos();
    // sld_2 = new Slider(47*width/60, height/2, 30, 35, "", color(BLACK), color(BLACK), color(BLACK), EVENT_BUTTON27, 5*height/13, "y"); // slider instantiation
    sliders.add(sld_2);
    sliders.add(sld_g);
    // sliders.add(sld_s);

    baseYCountT = height / 1.875; // variable top. can be changed with slider.
    baseYCountB = height / 1.875 + 9 * height / 26; // hardcoded bottom

    geoMap_st = new GeoMap(width / 22, height / 4, width / 2, height / 1.75, this); // standard geoMap - minimised one on screen 1
    geoMap_st.readFile("States_shapefile");   // Read shapefile. //tl_2017_us_state

    geoMap_sc_1 = new GeoMap(7 * width / 16, 9 * height / 20, width * 0.125, height * 0.1, this);   // zoom -2
    geoMap_sc_2 = new GeoMap(3 * width / 8, 2 * height / 5, width * 0.25, height * 0.2, this);      // zoom -1
    geoMap_sc_3 = new GeoMap(width / 4, height / 4, width * 0.5, height * 0.5, this);           // zoom 0
    geoMap_sc_4 = new GeoMap(width / 8, 3 * height / 20, width * 0.75, height * 0.7, this);       // zoom 1
    geoMap_sc_5 = new GeoMap(0, 0, width, height, this);                                // zoom 2
    geoMap_sc = geoMap_sc_3;                                                            // default scale map (mapScale 0)

    geoMap_c = geoMap_st;   // default geoMap is the standard one

    for (char c : tempAlpha.toCharArray()) {
        alphabet.add(c);
    }

    td = td_1m; // default data is daily 1M cases
}


void draw() {
    pushMatrix();
    if (moveScreenY) {
        currentScreen.moveScreenY();
    } else if (moveScreenX) {
        currentScreen.moveScreenX();
    }

    if (zoomIn) {
        if (mapScale <= 2) {
            mapScale += 1;
        }
    } else if (zoomOut) {
        if (mapScale >= - 2) {
            mapScale -= 1;
        }
    } else if (zoomReset) {
        mapScale = 0;
    }

    if (mapScale > 2) mapScale = 2;
    if (mapScale < - 2) mapScale = - 2;

    scale(scaler);
    translate(centerX + slide, centerY);
    moveScreenY = false;
    moveScreenX = false;
    stroke(BLACK);
    strokeWeight(1);
    background(currentScreen.backgroundColor);
    if (slide > 0) {
        slide -= width / 10;
    } else {
        slide = 0;
    }

    if (currentScreen == screen0) {
        drawScreen0();
    } else if (currentScreen == screen1) {
        drawScreen1();
    } else if (currentScreen == screen1_2) {
        drawScreen1_2();
    } else if (currentScreen == screen2) {
        drawScreen2();
    } else if (currentScreen == screen3) {
        drawScreen3();
    } else if (currentScreen == screen4) {
        drawScreen4();
    } else if (currentScreen == screen4_2) {
        drawScreen4_2();
    } else if (currentScreen == screen5) {
        td.getFormattedInfo(rawTextPos, 12);    // Using different method of printing raw data to screen to allow for easier scrolling.
    } else if (currentScreen == screen6) {
        drawScreen6();
    } else if (currentScreen == screen7 || currentScreen == screen8) {
        td.getFormattedInfo(searchTerm, miniData, rawTextPos, 12);    // Using different method of printing raw data to screen to allow for easier scrolling.
    }

    currentScreen.drawWidgets();
    popMatrix();
    fill(WHITE);


    if (hasQuit) {
        background(BLACK);
        textAlign(CENTER);
        fill(WHITE);
        textSize(26);
        text("Thanks, and stay safe!\n\n\nGroup 32", width / 2, height / 2);
        exitTimer -= exitSpeed;
    }

    if (exitTimer <= 0) {
        System.exit(0);
    }
}

static void removeDuplicates(ArrayList arr) {
    // send arraylist to linkedhashset to preserve order and remove duplicated
    Set<Object> tempSet = new LinkedHashSet<Object>(arr);   
    arr.clear();
    arr.addAll(tempSet);
}

// Return an ArrayList with all dates given data
public ArrayList<String> getDates(LinkedHashMap<String, Integer> tData) {
    ArrayList<String> dac = new ArrayList<String>();
    dac.addAll(tData.keySet());
    return dac;
}

void keyPressed() {
    selectedState = "";
    if (key == ENTER || key == RETURN) {
        if (currentScreen == screen0) {
            userID = input;
            if (input != "") {
                try {
                    tID = Integer.parseInt(userID);
                    Area tArea = td.getCountry(0).getArea(tID);
                    strcounty = tArea.getName();
                    selectedState = tArea.getState().getName();
                    currentScreen = screen3;
                    miniData = "geoid";
                    searchTerm = Integer.toString(tArea.getGeoID());

                    cp5.get(ScrollableList.class, "dropdown").setVisible(true);
                    cp5.get(ScrollableList.class, "dropdown").clear();
                    cp5.get(ScrollableList.class, "dropdown").addItems(getDates(tArea.getData()));     // add the dates for the chosen states
                    cp5b.get(ScrollableList.class, "dropdown").setVisible(true);
                    cp5b.get(ScrollableList.class, "dropdown").clear();
                    cp5b.get(ScrollableList.class, "dropdown").addItems(getDates(tArea.getData()));     // add the dates for the chosen states
                    datesEntered = false;
                    slide = width;
                    clickSFX.play();
                } catch (Exception e) {
                    errorInputID = true;
                    errorSFX.play();
                    input = "";
                }
            } else {
                errorInputID = true;
                errorSFX.play();
                input = "";
            }
        }
        else if (currentScreen == screen1) {
            if (input != "") {
                selectedState = input;
                co = td.getCountry("united states");
                if (co.containsState(selectedState)) {
                    state = co.getState(selectedState);
                    currentScreen = screen2;
                    sld_2.bound();
                    sld_2.resetPos();
                    errorInput = false;
                    slideSFX.play();
                    slide = width;
                } else {
                    errorInput = true;
                    selectedState = "";
                    errorSFX.play();
                }
                input = "";
                if (state != null) {
                    ArrayList<Area> areas = state.getAreas();
                    String[] arrAreas = new String[areas.size()];
                    int index = 0;
                    for (Area a : areas) {
                        arrAreas[index] = a.getName();
                        index++;
                    }
                }
            } else if (selectedState != "") {
                co = td.getCountry("united states");
                state = co.getState(selectedState);
                currentScreen = screen2;
                sld_2.resetPos();
                sld_2.bound();
                input = "";
                errorInput = false;
                noSelectedArea = false;
                slideSFX.play();
            } else {
                errorInput = true;
                errorSFX.play();
            }
        } else if (currentScreen == screen2) {
            if (input != "") {
                strcounty = input;
                State state = td.getCountry(0).getState(selectedState);
                if (state.containsArea(input)) {
                    currentScreen = screen3;
                    Area area = state.getArea(input);
                    cp5.get(ScrollableList.class, "dropdown").setVisible(true);
                    cp5.get(ScrollableList.class, "dropdown").clear();
                    cp5.get(ScrollableList.class, "dropdown").addItems(getDates(area.getData()));     // add the dates for the chosen states
                    cp5b.get(ScrollableList.class, "dropdown").setVisible(true);
                    cp5b.get(ScrollableList.class, "dropdown").clear();
                    cp5b.get(ScrollableList.class, "dropdown").addItems(getDates(area.getData()));     // add the dates for the chosen states
                    datesEntered = false;
                    slide = width;
                    clickSFX.play();
                } else {
                    input = "";
                    errorInput = true;
                    errorSFX.play();
                }
            } else {
                noSelectedArea = true;
                errorSFX.play();
            }
        }
    } else if (key == BACKSPACE) {
        if (input.equalsIgnoreCase("")) {
            input = "";
        } else {
            input = input.substring(0, input.length() - 1);
        }
    } else if (alphabet.contains(Character.toLowerCase(key))) {
        input += key;
    }
}

void mousePressed() {
    int id = geoMap_c.getID(int((mouseX - centerX) / scaler - centerX - sbx.xpos / scaler), int((mouseY - centerY) / scaler - centerY - sby.ypos / scaler));
    if ((currentScreen == screen1 || currentScreen == screen1_2) && id != - 1) {
        selectedState = geoMap_c.getAttributeTable().findRow(str(id), 0).getString("State_Name");
        geoMap_c = geoMap_st;
        currentScreen = screen1;
        blingSFX.play();
    }
    if (currentScreen == screen3 || currentScreen == screen4 || currentScreen == screen6) {
        if (mouseX >= width / 1.5 && mouseX <= 17 * width / 21 && mouseY >= 13 * height / 16 && mouseY <= 15 * height / 16) {
            pickingDate1 = true;
            pickingDate2 = false;
        }
        if (mouseX >= width / 1.2 && mouseX <= 41 * width / 42 && mouseY >= 13 * height / 16 && mouseY <= 15 * height / 16) {
            pickingDate1 = false;
            pickingDate2 = true;
        }
    }

    int event = currentScreen.getEvent(int((mouseX - centerX) / scaler - centerX - sbx.xpos / scaler), int(mouseY / scaler - centerY));
    State state = td.getCountry(0).getState(selectedState);
    Country co = td.getCountry(0);

    switch(event) {
    case EVENT_BUTTON0:
        hasQuit = true;
        break;
    case EVENT_BUTTON1 :  
        currentScreen = screen0;
        zoomReset = true;
        query = "?q=vaccine&sort=newest";
        json = loadJSONObject(url + query + "&api-key=" + apiKey);
        headline = json.getJSONObject("response").getJSONArray("docs").getJSONObject(0).getJSONObject("headline").getString("main");
        selectedState = "";
        input = "";
        errorInput = false;
        errorInputID = false;
        noSelectedArea = false;
        centerX = 0;
        centerY = 0;
        sbx.reset();
        sby.reset();
        slide = width;
        slideSFX.play();
        break;
    case EVENT_BUTTON2:
        zoomReset = true;
        sbx.reset();
        sby.reset();
        if (selectedState != "") {
            co = td.getCountry("united states");
            state = co.getState(selectedState);
            currentScreen = screen2;
            sld_2.resetPos();
            sld_2.bound();
            input = "";
            errorInput = false;
            noSelectedArea = false;
            slideSFX.play();
        } else if (input != "") {
            selectedState = input;
            co = td.getCountry("united states");
            if (co.containsState(selectedState)) {
                state = co.getState(selectedState);
                currentScreen = screen2;
                sld_2.resetPos();
                sld_2.bound();
                errorInput = false;
                noSelectedArea = false;
                slideSFX.play();
            } else {
                selectedState = "";
                errorInput = true;
                errorSFX.play();
            }
        } else {
            noStateSelected = true;
            errorSFX.play();
        }
        input = "";
        centerX = 0;
        centerY = 0;
        slide = width;
        if (state != null) {
            ArrayList<Area> areas = state.getAreas();
            String[] arrAreas = new String[areas.size()];
            int index = 0;
            for (Area a : areas) {
                arrAreas[index] = a.getName();
                index++;
            }
        }
        break;
    case EVENT_BUTTON3:
        zoomReset = true;
        cp5.get(ScrollableList.class, "dropdown").setVisible(false);
        cp5b.get(ScrollableList.class, "dropdown").setVisible(false);
        currentScreen = screen1;
        geoMap_c = geoMap_st;
        selectedState = "";
        input = "";
        errorInput = false;
        noStateSelected = false;
        centerX = 0;
        centerY = 0;
        slide = width;
        clickSFX.play();
        break;
    case EVENT_BUTTON4:
        zoomReset = true;
        currentScreen = screen2;
        sld_2.resetPos();
        sld_2.bound();
        input = "";
        errorInput = false;
        noStateSelected = false;
        cp5.get(ScrollableList.class, "dropdown").setVisible(false);
        cp5b.get(ScrollableList.class, "dropdown").setVisible(false);
        centerX = 0;
        centerY = 0;
        slide = width;
        clickSFX.play();
        break;
    case EVENT_BUTTON6:
        if (currentScreen == screen1 && !usingMap) {
            selectedState = "PUERTO RICO";
        }
        centerX = 0;
        centerY = 0;
        blingSFX.play();
        break;
    case EVENT_BUTTON7:
        if (currentScreen == screen1 && !usingMap) {
            selectedState = "GUAM";
        }
        centerX = 0;
        centerY = 0;
        blingSFX.play();
        break;
    case EVENT_BUTTON8:
        if (currentScreen == screen1 && !usingMap) {
            selectedState = "VIRGIN ISLANDS";
        }
        centerX = 0;
        centerY = 0;
        blingSFX.play();
        break;
    case EVENT_BUTTON9:
        if (currentScreen == screen1 && !usingMap) {
            selectedState = "NORTHERN MARIANA ISLANDS";
        }
        centerX = 0;
        centerY = 0;
        blingSFX.play();
        break;
    case EVENT_BUTTON18:
        currentScreen = screen5;
        rawTextPos = baseRawTextPos;
        centerX = 0;
        centerY = 0;
        clickSFX.play();
        break;
    case EVENT_BUTTON19:
        link("https://www.cdc.gov/coronavirus/2019-ncov/index.html");
        centerX = 0;
        centerY = 0;
        clickSFX.play();
        break;
    case EVENT_BUTTON20:
        // scroll down (ypos decreases)
        rawTextPos -= speed;
        clickSFX.play();
        // text cannot go past these limits.
        if (rawTextPos >= 1.5 * baseRawTextPos) {
            // if the top of the text is greater than 1.5 times the limit
            // keep it in place
            rawTextPos = baseRawTextPos;
        }
        if (rawTextPos <= - td.getMaxTextHeight()) {
            rawTextPos = - td.getMaxTextHeight();
        }
        break;
    case EVENT_BUTTON21:
        // scroll up (ypos increases)
        clickSFX.play();
        rawTextPos += speed;
        if (rawTextPos >= 1.5 * baseRawTextPos) {
            rawTextPos = baseRawTextPos;
        }
        if (rawTextPos <= - td.getMaxTextHeight()) {
            rawTextPos = - td.getMaxTextHeight();
        }
        break;
    case EVENT_BUTTON23:
        usingMap = true;
        currentScreen = screen1_2;
        clickSFX.play();
        break;
    case EVENT_BUTTON24:
        zoomIn = true;
        clickSFX.play();
        break;
    case EVENT_BUTTON25:
        zoomOut = true;
        clickSFX.play();
        break;
    case EVENT_BUTTON26:
        zoomReset = true;
        clickSFX.play();
        break;
    case EVENT_BUTTON27:
        sld_2.bound();
        errorInput = false;
        noSelectedArea = false;
        clickSFX.play();
        break;
    case EVENT_BUTTON28:
        currentScreen.radioToggle(1);
        currentScreen.radioToggle(2);
        clickSFX.play();
        break;
    case EVENT_BUTTON29:
        currentScreen.checkBoxes(); 
        clickSFX.play();
        break;
    case EVENT_BUTTON30:
        if (!input.equalsIgnoreCase("") && state.containsArea(input)) {
            currentScreen = screen3;
            Area tArea = new Area();
            // if the state or input has not been defined, use the tID to find the area
            try {
                tArea = state.getArea(input);
                tID = tArea.getGeoID();
            } catch (Exception e) {
                tArea = td.getCountry(0).getArea(tID);
                selectedState = tArea.getState().getName();
            }
            cp5.get(ScrollableList.class, "dropdown").setVisible(true);
            cp5.get(ScrollableList.class, "dropdown").clear();
            cp5.get(ScrollableList.class, "dropdown").addItems(getDates(tArea.getData()));     // add the dates for the chosen areas
            cp5b.get(ScrollableList.class, "dropdown").setVisible(true);
            cp5b.get(ScrollableList.class, "dropdown").clear();
            cp5b.get(ScrollableList.class, "dropdown").addItems(getDates(tArea.getData()));
            datesEntered = false;
            clickSFX.play();
        } else {
            errorSFX.play();
        }
        break;
    case EVENT_BUTTON31:
        if (!input.equalsIgnoreCase("")) {
            userID = input;
            try {
                tID = Integer.parseInt(userID);
                Area tArea = td.getCountry(0).getArea(tID);
                strcounty = tArea.getName(); // redefine chosen area and state if they haven't already been selected
                state = tArea.getState();
                currentScreen = screen3;
                cp5.get(ScrollableList.class, "dropdown").setVisible(true);
                cp5.get(ScrollableList.class, "dropdown").clear();
                cp5.get(ScrollableList.class, "dropdown").addItems(getDates(tArea.getData()));     // add the dates for the chosen areas
                cp5b.get(ScrollableList.class, "dropdown").setVisible(true);
                cp5b.get(ScrollableList.class, "dropdown").clear();
                cp5b.get(ScrollableList.class, "dropdown").addItems(getDates(tArea.getData()));
                datesEntered = false;
                clickSFX.play();
            } catch (Exception e) {
                errorInputID = true;
                errorSFX.play();
                input = "";
                userID = "";
            }
        }
        break;
    case EVENT_BUTTON32:
        currentScreen = screen7;
        cp5.get(ScrollableList.class, "dropdown").setVisible(false);
        cp5b.get(ScrollableList.class, "dropdown").setVisible(false);
        break;
    case EVENT_BUTTON33:
        currentScreen = screen8;
        cp5.get(ScrollableList.class, "dropdown").setVisible(false);
        cp5b.get(ScrollableList.class, "dropdown").setVisible(false);
        break;
    case EVENT_STATE:
        currentScreen = screen4;
        cp5.get(ScrollableList.class, "dropdown").setVisible(true);
        cp5.get(ScrollableList.class, "dropdown").clear();
        cp5.get(ScrollableList.class, "dropdown").addItems(getDates(state.totalData()));     // add the dates for the chosen states
        cp5b.get(ScrollableList.class, "dropdown").setVisible(true);
        cp5b.get(ScrollableList.class, "dropdown").clear();
        cp5b.get(ScrollableList.class, "dropdown").addItems(getDates(state.totalData()));
        datesEntered = false;
        centerX = 0;
        centerY = 0;
        slide = width;
        slideSFX.play();
        break;
    case EVENT_STATEAREAS:
        currentScreen = screen4_2;
        textAlign(RIGHT);
        centerX = 0;
        centerY = 0;
        slide = width;
        slideSFX.play();
        break;
    case EVENT_COUNTRY:
        currentScreen = screen6;
        Country tCo = td_mini.getCountry(0);
        cp5.get(ScrollableList.class, "dropdown").setVisible(true);
        cp5.get(ScrollableList.class, "dropdown").clear();
        cp5.get(ScrollableList.class, "dropdown").addItems(getDates(tCo.totalData()));     // add the dates for the country
        cp5b.get(ScrollableList.class, "dropdown").setVisible(true);
        cp5b.get(ScrollableList.class, "dropdown").clear();
        cp5b.get(ScrollableList.class, "dropdown").addItems(getDates(tCo.totalData()));
        datesEntered = false;
        centerX = 0;
        centerY = 0;
        break;
    case DATE_CONFIRMED:
        datesEntered = true;
        centerX = 0;
        centerY = 0;
        blingSFX.play();
        break;
    case EVENT_D1M:
        td = td_1m;
        cFileName = "Daily-1M";
        break;
    case EVENT_T1M:
        td = td_c1m;
        cFileName = "Total-1M";
        break;
    case EVENT_D97K:
        td = td_97k;
        cFileName = "Daily-97k";
        break;
    case EVENT_T97K:
        td = td_c97k;
        cFileName = "Total-97k";
        break;
    }
}

void mouseWheel(MouseEvent me) {
    // when the mouse wheel is scrolled, move the text on screen up or down.
    float event = me.getCount();

    // test position
    rawTextPos -= event * speed;

    // text cannot go past these limits.
    if (rawTextPos >= 1.5 * baseRawTextPos) {
        rawTextPos = baseRawTextPos;
    }
    if (rawTextPos <= - td.getMaxTextHeight()) {
        rawTextPos = - td.getMaxTextHeight();
    }
}

void mouseMoved() {
    if (currentScreen == screen1) usingMap = false;
    int event;
    for (int i = 0; i < currentScreen.widgetList.size(); i++) {
        Widget aWidget = (Widget) currentScreen.widgetList.get(i);
        event = aWidget.getEvent(int((mouseX - centerX) / scaler - centerX - sbx.xpos / scaler), int(mouseY / scaler - centerY));
        aWidget.setBorderIsWhite(false);
        if (validHoverEvents.contains(event)) {
            // Replaced second large switch-case statement with this.
            aWidget.setBorderIsWhite(true);
        }
    }
    if (sld_2.getEvent(mouseX, mouseY) == EVENT_BUTTON27) { // if hovering over slider controller, turn border white
        sld_2.borderColor = WHITE;
    } else {
        sld_2.borderColor = BLACK;
    }
}

void mouseDragged() {
    // chike - 16/04: added slider function
    // int event = currentScreen.getEvent(int((mouseX-centerX)/scaler - centerX-sbx.xpos/scaler), int((mouseY-centerY)/scaler - centerY-sby.ypos/scaler));
    for (Slider sld : sliders) {
        int event = sld.getEvent(mouseX, mouseY);
        if (event == EVENT_BUTTON27 && sld.canMove) {
            sld.bound();
        }
    }
}

void createScreens() {
    screen0 = new Screen(80); // home page
    screen1 = new Screen(100); // map page
    screen1_2 = new Screen(50); // interactive screen for map
    screen2 = new Screen(200); // area choice screen
    screen3 = new Screen(50); // graph all data in area
    screen4 = new Screen(50); // graph all data in state
    screen4_2 = new Screen(50); // graph all cases in area in state
    screen5 = new Screen(100);  // display formatted raw csv data
    screen6 = new Screen(50);  // graph all data in country
    screen7 = new Screen(50);  // display formatted raw csv data about a specific area
    screen8 = new Screen(50);  // display formatted raw csv data about a specific state
}

void createScrollableList() {
    cp5 = new ControlP5(this);
    cp5.addScrollableList("dropdown")
        .setVisible(false)
        .setPosition(width / 1.5, 13 * height / 16)
        .setBackgroundColor(200)
        .setSize(width / 7, height / 8)
        .setBarHeight(height / 40)
        .setItemHeight(height / 40);

    cp5b = new ControlP5(this);
    cp5b.addScrollableList("dropdown")
        .setVisible(false)
        .setPosition(width / 1.2, 13 * height / 16)
        .setBackgroundColor(200)
        .setSize(width / 7, height / 8)
        .setBarHeight(height / 40)
        .setItemHeight(height / 40);
}

void dropdown(int n) {
    /* request the selected item based on index n */

    if (pickingDate1) {
        Map item = cp5.get(ScrollableList.class, "dropdown").getItem(n);
        date1 = (String) item.get("name");
        println(item.get("name"));
        clickSFX.play();
    } else if (pickingDate2) {
        Map itemb = cp5b.get(ScrollableList.class, "dropdown").getItem(n);
        date2 = (String) itemb.get("name");
        println(itemb.get("name"));
        clickSFX.play();
    }

    //println(itemb.get("name"));
}

void createGraphs() {
    // All graph queries

    // Cases against dates given area   (query req. 1)
    g1 = new Graph(width/11, height/9, width-width/7, height/2, color(255, 255, 150, 80), color(100, 30, 100), 9, "hist", "grid", false);
    // 
    // Cases in all areas with a state given state
    g3 = new Graph(width/12.5, height/8, 8*width/9, height/1.5, color(255, 255, 150, 80), color(100, 30, 100), 9, "bar", "grid", true);
    // 
    // Largest difference between two consecutive dates in an state
    g5 = new Graph(width/11, height/9, width-width/7, height/2, color(255, 255, 150, 80), color(100, 30, 100), 9, "hist", "grid", true);
    // 
    // Cumulative cases against dates given country
    g10 = new Graph(width/11, height/9, width-width/7, height/2, color(255, 255, 150, 80), color(100, 30, 100), 9, "hist", "grid", false);
}

void createWidgets() {
    // chike - 13/04: changed some (not all; i'll do that later) position values to make them scale with the screen size
    // chike - 20/04: changed all position values to make them scale with the screen size
    screen0.addWidget(3*width/16, floor(2*height/5), width/9, height/20, "Pick a State", color(BLACK), color(WHITE), stdFont, EVENT_BUTTON3);
    screen0.addWidget(3*width/16, floor(2.5*height/5), width/9, height/20, "Raw Data", color(BLACK), color(WHITE), stdFont, EVENT_BUTTON18);
    screen0.createSearchBar(width/16, floor(3*height/5), width/4, height/20, "Search by GeoID", color(WHITE), color(BLACK), stdFont, EVENT_BUTTON17);
    screen0.addWidget(floor(5.5*width/16), floor(3*height/5), width/10, height/20, "Search", color(BLACK), color(WHITE), stdFont, EVENT_BUTTON31);
    screen0.addWidget(4*width/9, height-height/11, width/9, height/20, "More information", color(BLACK), color(WHITE), stdFont, EVENT_BUTTON19);
    screen0.addWidget(8*width/15, height/14, width/9, height/20, "Daily 97k", color(BLACK), color(WHITE), stdFont, EVENT_D97K);
    screen0.addWidget(8*width/15, height/7, width/9, height/20, "Total 97k", color(BLACK), color(WHITE), stdFont, EVENT_T97K);
    screen0.addWidget(10*width/15, height/14, width/9, height/20, "Daily 1M", color(BLACK), color(WHITE), stdFont, EVENT_D1M);
    screen0.addWidget(10*width/15, height/7, width/9, height/20, "Total 1M", color(BLACK), color(WHITE), stdFont, EVENT_T1M);
    screen0.addWidget(width/50, height/40, width/15, height/20, "Quit", color(BLACK), color(WHITE), stdFont, EVENT_BUTTON0);


    screen1.addWidget(width/13, height/8, width/13, height/20, "Back to Home", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON1);
    screen1.addWidget(floor(7.25*width/8), height/5, width/13, height/20, "Next", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON2); 
    screen1.addWidget(36*width/56, 4*height/10, width/13, height/20, "Puerto Rico", color(LAND_COLOUR), color(BLACK), stdFont, EVENT_BUTTON6);
    screen1.addWidget(36*width/56, 5*height/10, width/13, height/20, "Guam", color(LAND_COLOUR), color(BLACK), stdFont, EVENT_BUTTON7);
    screen1.addWidget(36*width/56, 6*height/10, width/13, height/20, "Virgin Islands", color(LAND_COLOUR), color(BLACK), stdFont, EVENT_BUTTON8);
    screen1.addWidget(36*width/56, 7*height/10, floor(width/8), height/20, "Northern Mariana Islands", color(LAND_COLOUR), color(BLACK), stdFont, EVENT_BUTTON9);
    screen1.createSearchBar(floor(4.5*width/7), height/5, width/4, height/20, "Type or select a state", color(WHITE), color(BLACK), stdFont, EVENT_BUTTON17);
    screen1.addWidget(floor(3*width/8), height/5+25, width/10, height/20, "Full Screen Mode", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON23); 
    screen1.addWidget(36*width/56, floor(8.25*height/10), width/6, height/19, "View all data in the country", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_COUNTRY);


    screen1_2.addWidget(width - width/10, 3*height/32, width/13, height/20, "Zoom in", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON24); 
    screen1_2.addWidget(width - width/10, 6*height/32, width/13, height/20, "Zoom out", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON25); 
    screen1_2.addWidget(width - width/10, 9*height/32, width/13, height/20, "Reset Scale", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON26); 
    screen1_2.addWidget(width/50, height/40, width/13, height/20, "Back", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON3);


    screen2.addWidget(width/50, height/40, width/14, height/19, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON3);
    screen2.createSearchBar(floor(width/1.875), height/3, floor(width/3), height/19, "Enter an area", color(WHITE), color(BLACK), stdFont, EVENT_BUTTON5);
    screen2.addWidget(floor(26.25*width/30), height/3, width/13, height/20, "Next", color(DARK_GREY), color(BLACK), stdFont, EVENT_BUTTON30);
    screen2.addWidget(width/15, floor(4*height/8), width/6, height/19, "View all data in this state", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_STATE);
    screen2.addWidget(width/15, floor(4.5*height/8), width/5, height/19, "View cases for each area in this state", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_STATEAREAS);


    screen3.addWidget(width/50, height/40, width/14, height/19, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON4);
    addGraphWidgets(screen3);


    screen4.addWidget(width/50, height/40, width/14, height/19, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON4);
    addGraphWidgets(screen4);


    screen4_2.addWidget(width/50, height/40, width/14, height/19, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON4);


    screen5.addWidget(floor(width/1.5), height/60, width/14, height/20, "Back to Home", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON1);
    screen5.addWidget(floor(width/1.1), floor(height/1.1), width/14, height/15, "Scroll Down", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON20);
    screen5.addWidget(floor(width/1.1), floor(height/1.3), width/14, height/15, "Scroll Up", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON21);

   
    screen6.addWidget(width/50, height/40, width/14, height/19, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON3);
    addGraphWidgets(screen6);
   
   
    screen7.addWidget(floor(width/1.5), height/60, width/14, height/20, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON30);
    screen7.addWidget(floor(width/1.1), floor(height/1.1), width/14, height/15, "Scroll Down", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON20);
    screen7.addWidget(floor(width/1.1), floor(height/1.3), width/14, height/15, "Scroll Up", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON21);

    
    screen8.addWidget(floor(width/1.5), height/60, width/14, height/20, "Back", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_STATE);
    screen8.addWidget(floor(width/1.1), floor(height/1.1), width/14, height/15, "Scroll Down", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON20);
    screen8.addWidget(floor(width/1.1), floor(height/1.3), width/14, height/15, "Scroll Up", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON21);
}
void createWidgets2() {
    State state = td.getCountry(0).getState(selectedState);
    fill(0);
    text("All of " + state.getName() + " (dot graph)", floor(width/5.5)+width/40+10, floor(height/2.56)+height/19/2+1);
    text("Totals for all areas in " + state.getName(), floor(width/5.5)+width/40+10, floor(height/1.92)+height/19/2+1);
    text("All of " + state.getName() + " (barchart)", floor(width/5.5)+width/40+10, floor(height/1.5)+height/19/2+1);
    text("All of " + state.getName() + " within two dates", floor(width/5.5)+width/40+10, floor(height/1.28)+height/19/2+1);
}
void addGraphWidgets(Screen sc) {
    // Add specific widgets to the graph screens. 
    // The widgets themselves consist of:
    //      - a "Search" button for finding data between two dates, 
    //      - a trio of radio buttons (graph background), 
    //      - 6 more radio buttons (graph element type), 
    //      - and 2 checkboxes (data on hover, highest difference).

    // Search between dates
    sc.addWidget(floor(width/1.2), 12*height/16, width/15, height/19, "Search", color(LIGHT_GREY), color(BLACK), stdFont, DATE_CONFIRMED);

    // Change graph background
    sc.addRadio(width/25, 13*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 1); 
    sc.addRadio(width/25, 14*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 1);
    sc.addRadio(width/25, 15*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 1);

    // Change graph element type (bar, scatter, etc.)
    sc.addRadio(width/5, 13*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 2); 
    sc.addRadio(width/5, 14*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 2);
    sc.addRadio(width/5, 15*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 2);
    sc.addRadio(floor(width/3.25), 13*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 2); 
    sc.addRadio(floor(width/3.25), 14*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 2);
    sc.addRadio(floor(width/3.25), 15*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON28, 2);

    // Show data on hover checkbox boolean
    sc.addCheckBox(floor(width/2.25), 13*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON29);

    // Show data about the highest difference in cases within data
    sc.addCheckBox(floor(width/2.25), 14*height/16, width/40, width/40, "", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON29);
}

void toggleGraphWidgets(Screen sc, Graph g) {
    // Give the graph widgets certain functions, in collaboration with addGraphWidgets. This function allows the widgets to actually perform actions.

    ArrayList<Radio> scRadios_1 = sc.radioList1;      // arraylist of radio buttons in first section (background type)
    ArrayList<Radio> scRadios_2 = sc.radioList2;      // arraylist of radio buttons in second section (graph type)
    ArrayList<CheckBox> scCheck_1 = sc.checkBoxList;  // arraylist of checkboxes
    displayOptions();

    g.setHoverEvent(scCheck_1.get(0).checked);        // toggle Graph.mouseHover using the checkbox state

    // select background type
    for (int i = 0; i < scRadios_1.size(); i++) {
        if (i == 0 && scRadios_1.get(i).ticked) {
            g.setGraphBackgroundType("grid");
        }
        if (i == 1 && scRadios_1.get(i).ticked) {
            g.setGraphBackgroundType("ybar");
        }
        if (i == 2 && scRadios_1.get(i).ticked) {
            g.setGraphBackgroundType("blank");
        }
    }

    // if all graph types are unselected, select "hist" by default
    int count = 0;
    for (int i = 0; i < scRadios_2.size(); i++) {
        if (scRadios_2.get(i).ticked) {
            break;
        }
        count++;
        if (count == 6) {
            scRadios_2.get(1).ticked = true;
        }
    }

    // select graph type
    for (int i = 0; i < scRadios_2.size(); i++) {
        if (scRadios_2.get(i).ticked) {
            switch (i) {
            case 0:
                g.setGraphElementType("bar");
                gType = "bar";
                break;
            case 1:
                g.setGraphElementType("hist");
                gType = "hist";
                break;
            case 2:
                g.setGraphElementType("lchart");
                gType = "lchart";
                break;
            case 3:
                g.setGraphElementType("scatter");
                gType = "scatter";
                break;
            case 4:
                g.setGraphElementType("hbar");
                gType = "hbar";
                break;
            case 5:
                if (!scCheck_1.get(0).checked) {
                    g.setGraphElementType("area");
                } else {
                    g.setGraphElementType(gType);
                }
                break;
            default:
                g.setGraphElementType(gType);
                break;
            }
        }
    }

    DateFormat df = new SimpleDateFormat("dd/MM/yyyy");
    Date x1 = new Date(), x2 = new Date();

    // date1 as a date object for comparing
    try { 
        x1 = (Date)df.parse(date1);
    } 
    catch(Exception e) {
        // acknowledge exception and do nothing
    }
    // date2 as a date object for comparing
    try { 
        x2 = (Date)df.parse(date2);
    } 
    catch(Exception e) {
        // acknowledge exception and do nothing
    }

    if (x2.compareTo(x1) < 0) {
        // if the second date is below the first (dates in reverse order), set the second date equal to the first.
        // you could set it to a further ahead date, but that would require the Calendar class, which seems a bit excessive
        x2 = x1;
        date2 = date1;
    }
}

void changeGraphData(Screen sc, Graph g, Area a, int spread, String d1, String d2) {
    ArrayList<CheckBox> scCheck_1 = sc.checkBoxList;  // arraylist of checkboxes
    LinkedHashMap<String, Integer> areaBT = new LinkedHashMap<String, Integer>(); // hashmap to store chosen area data
    ArrayList<String> dates = new ArrayList<String>();

    // Show raw data for this state
    sc.addWidget(9*width/10, height/40, width/14, height/19, "View Raw Data", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON32);
    miniData = "geoid";
    searchTerm = Integer.toString(a.getGeoID());
    
    dates.addAll(a.getData().keySet());

    // if first date is blank or is not within the dataset, select the first date
    if (d1.trim().isEmpty() || !dates.contains(d1)) {
        d1 = dates.get(0);
    }

    // if second date is blank or not within the dataset, select the last date
    if (d2.trim().isEmpty() || !dates.contains(d2)) {
        d2 = dates.get(dates.size()-1);
    }

    try {
        if (scCheck_1.get(1).checked && datesEntered && !date1.equalsIgnoreCase("") && !date2.equalsIgnoreCase("")) {
            areaBT = a.atDates(a.getData(), d1, d2);
            g.loadData(a.areaDiff(areaBT, spread));
            g.setLimits("set", 0, 100, 0, a.biggestDiffCases(), a.areaDiff(areaBT, spread).size(), 10, color(255), false);
            textAlign(CENTER);
            g.setLabel("Dates" + "\n diff: " + a.biggestDiff(), "Cases", a.getName() + ", " + a.getState().getName());
        } else {
            areaBT = a.atDates(a.getData(), d1, d2);
            g.loadData(areaBT);
            g.setLimits("set", 0, 100, 0, a.maxBetween, areaBT.size(), 15, color(255), true);
            g.setLabel("Dates", "Cases", a.getName() + ", " + a.getState().getName());
        }
    } 
    catch (Exception e) {
        println("Time doesn't work like that, you know. Try again.");
    }
}

void changeGraphData(Screen sc, Graph g, State s, int spread, String d1, String d2) {
    ArrayList<String> dates = new ArrayList<String>();
    dates.addAll(s.totalData().keySet());

    sc.addWidget(9*width/10, height/40, width/14, height/19, "View Raw Data", color(LIGHT_GREY), color(BLACK), stdFont, EVENT_BUTTON33);
    miniData = "county/state";
    searchTerm = s.getName();


    // if first date is blank, select the first date
    if (d1.trim().isEmpty() || !dates.contains(d1)) {
        d1 = dates.get(0);
    }

    // if second date is blank, select the last date
    if (d2.trim().isEmpty() || !dates.contains(d2)) {
        d2 = dates.get(dates.size()-1);
    }
    try {
        ArrayList<CheckBox> scCheck_1 = sc.checkBoxList;  // arraylist of checkboxes
        LinkedHashMap<String, Integer> stateBT = new LinkedHashMap<String, Integer>(); // hashmap to store chosen area data
        if (scCheck_1.get(1).checked && datesEntered && !date1.equalsIgnoreCase("") && !date2.equalsIgnoreCase("")) {   
            stateBT = s.atDates(s.totalData(), d1, d2);
            g.loadData(s.stateDiff(stateBT, spread));
            g.setLimits("set", 0, 100, 0, s.biggestDiffCases(), s.stateDiff(s.totalData(), spread).size(), 10, color(255), false);
            g.setLabel("Dates" + "\n diff: " + s.biggestDiff(), "Cases", s.getName());
        } else {
            stateBT = s.atDates(s.totalData(), d1, d2);
            g.loadData(stateBT);
            g.setLimits("set", 0, 100, 0, s.maxBetween, stateBT.size(), 15, color(255), true);
            g.setLabel("Dates", "Cases", s.getName());
        }
    } 
    catch (Exception e) {
        println("Time doesn't work like that, you know. Try again.");
    }
}

void drawScreen0() {
    stroke(BLACK);
    fill(230);
    rect(width/30, height/4, 13.5*width/30, height/2, 15); // Left rect
    rect(15.5*width/30, height/4, 13.5*width/30, height/2, 15); // Right rect
    
    fill(WHITE);
    rect(37*width/64, 35*height/64, 2.57*width/7.7, height/5.5, 25);
    fill(BLACK);
    stroke(BLACK);
    image(twitterLogo, 19*width/32, height/1.85, width/19.5, height/15);
    textSize(22);
    text("Latest From Twitter:", 41*width/64, height/1.7);
    textSize(13);
    // tweetText = "broken for now";
    //text(tweetText, 19*width/32, height/1.63, 2.4*width/7.7, height/2);
    //image(covidPic, width/2.7, height/5, width/2, height/2);
    image(vaccinePic, 37*width/64, 9*height/32, width/7.7, height/4.3);
    image(maskPeople, 50*width/64, 9*height/32, width/7.7, height/4.3);

    fill(BLACK);
    stroke(WHITE);
    textFont(stdFont_Big);
    textSize(40);
    text("Home", 9*width/40, height/3);
    textFont(stdFont);
    fill(WHITE);
    rect(0, height/1.2, width, height/25);

    String wrapText = "Latest from The NY Times: " + headline;
    float wrapLength = textWidth(wrapText);
    fill(BLACK);
    textSize(26);
    text(wrapText, textx, height/1.15);
    textx--;

    if (textx <=0) {
        text(headline, textx+width, height/1.15);
    }
    if (textx + wrapLength == 0) {
        textx = width-wrapLength;
    }
    fill(WHITE);
    text("Choose dataset to use", 8*width/15, height/20);
    textSize(20);
    text("Current dataset: \n" + cFileName, 7.5*width/9, height/20);
    fill(BLACK);

    SearchBar searchBar = (SearchBar) currentScreen.searchBar;
    if (selectedState.length() != 0) {
        searchBar.setLabel(selectedState);
    } else if (input.length() != 0) {
        searchBar.setLabel(input);
    } else {
        if (errorInput || noStateSelected) {
            searchBar.setLabel("Error - Invalid state");
        } else {
            searchBar.setLabel("Enter a GeoID");
        }
        if (errorInputID) {
            searchBar.setLabel("Error - Invalid GeoID");
        }
    }
}

void drawScreen1() {
    stroke(0, 40);              // Boundary colour

    noStroke();
    fill(230);                 // light gray for rect
    rect(width/56, height/16, 4*width/7, 14*height/16, 15);  // geomap case (rect 1)
    stroke(BLACK);
    fill(202, 226, 245);       // ocean color
    rect(width/28, height/12, 3.75*width/7, 13.25*height/16);  // ocean

    fill(230);
    rect(33.5*width/56, height/16, 2.75*width/7, 14*height/16, 10); // state search case (rect 2)
    fill(BLACK);
    textFont(stdFont);
    textSize(16);
    text("United States", width/4, height/8);

    // Draw entire world map.
    fill(206, 173, 146);        // Land colour
    geoMap_c.draw();              // Draw the current geoMap.

    fill(BLACK);
    textSize(14);
    text("Select a state or an American territory, then click next: ", 36*width/56, height/8);
    text("American territories: ", 36*width/56, height/2.75);
    noStroke();

    int id = geoMap_c.getID(int((mouseX-centerX)/scaler - centerX-sbx.xpos/scaler), int(mouseY/scaler - centerY));

    // Query the country at the mouse position.
    if (id != -1) {
        fill(180, 120, 120);
        geoMap_c.draw(id);
        textSize(width/85);

        String name = geoMap_c.getAttributeTable().findRow(str(id), 0).getString("State_Name"); 
        fill(color(255, 100));
        stroke(0);
        float xPos = mouseX, yPos = mouseY;
        if (xPos + textWidth(name)  + width/50>= width) xPos = width - textWidth(name) - width/50;
        if (yPos - height/35 <= 0) yPos = height/35;

        rect(xPos, yPos, textWidth(name) + width/50, -height/35);
        fill(BLACK);
        text(name, xPos + width/100, yPos - height/140);
    }

    SearchBar searchBar = (SearchBar) currentScreen.searchBar;
    if (selectedState.length() != 0) {
        searchBar.setLabel(selectedState);
    } else if (input.length() != 0) {
        searchBar.setLabel(input);
    } else {
        if (errorInput || noStateSelected) {
            searchBar.setLabel("Error - Please enter a state");
        } else {
            searchBar.setLabel("Type or select a state");
        }
    }
}


void drawScreen1_2() {
    fill(206, 173, 146);        // Land colour
    switch (mapScale) {
    case -2:
        geoMap_sc = geoMap_sc_1;
        break;
    case -1:
        geoMap_sc = geoMap_sc_2;
        break;
    case 0:
        geoMap_sc = geoMap_sc_3;
        break;
    case 1:
        geoMap_sc = geoMap_sc_4;
        break;
    case 2:
        geoMap_sc = geoMap_sc_5;
        break;
    }

    geoMap_sc.readFile("States_shapefile");   // Read shapefile. //tl_2017_us_state
    geoMap_c = geoMap_sc;
    geoMap_c.draw();

    // reset scale booleans
    zoomIn = false;     
    zoomOut = false;
    zoomReset = false;

    textSize(39);
    fill(WHITE);
    textAlign(CENTER);
    text("United States of America", width/1.9, height/15);
    textAlign(LEFT);
    // Find the country at the mouse position and draw it in different colour.
    int id = geoMap_c.getID(int((mouseX-centerX)/scaler - centerX-sbx.xpos/scaler), int((mouseY-centerY)/scaler - centerY-sby.ypos/scaler));

    // Query the country at the mouse position.
    if (id != -1) {
        fill(180, 120, 120);
        geoMap_c.draw(id);
        textSize(width/85);

        String name = geoMap_c.getAttributeTable().findRow(str(id), 0).getString("State_Name"); 
        fill(color(255, 100));
        stroke(0);
        float xPos = mouseX, yPos = mouseY;
        if (xPos + textWidth(name)  + width/50>= width) xPos = width - textWidth(name) - width/50;
        if (yPos - height/35 <= 0) yPos = height/35;

        rect(xPos, yPos, textWidth(name) + width/50, -height/35);
        fill(BLACK);
        text(name, xPos + width/100, yPos - height/140);
    }
}

void drawScreen2() {
    textSize(19.5);
    SearchBar searchBar = (SearchBar) currentScreen.searchBar;

    noStroke();
    fill(230);
    rect(width/30, height/5, width/2.25, 3.5*height/5, 15);  // state type selector panel
    fill(140);
    rect(width/1.95, height/5, width/2.25, 3.5*height/5, 15); // area selector panel
    fill(230);

    // Create the area selector panel. PShape is used to "cut a hole" inside the rectangle.
    PShape ap = createShape();  // area selector panel PShape
    ap.beginShape();

    ap.vertex(width/1.95, height/5);
    ap.vertex(width/1.95 + width/2.25, height/5);
    ap.vertex(width/1.95 + width/2.25, height/5 + 3.5*height/5);
    ap.vertex(width/1.95, height/5 + 3.5*height/5);
    ap.vertex(width/1.95, height/5);

    // hole in middle for hiding areas
    ap.beginContour();
    ap.vertex(width/1.875, height/2);
    ap.vertex(width/1.875, 23*height/26);
    ap.vertex(47*width/60, 23*height/26);
    ap.vertex(47*width/60, height/2);
    ap.vertex(width/1.875, height/2);
    ap.endContour();

    ap.endShape();
    shape(ap); // draw shape

    stroke(BLACK);
    fill(BLACK);


    text("Click one of the options below to see \na graph showing all areas within your chosen state: ", width/19.5, height/4);
    text("Or enter one of the areas from this list: ", width/1.875, height/4);


    if (input.length() != 0) {
        searchBar.setLabel(input);
    } else {
        if (errorInput) {
            searchBar.setLabel("Error - Please Enter an area inside the selected state");
        } else {
            searchBar.setLabel("Type in an area ");
        }
    }

    State state = td.getCountry(0).getState(selectedState);
    text("Areas in " + state.getName(), width/1.875, height/2.1);
    ArrayList<Area> areas = state.getAreas();
    float maxAreaHeight = 0;

    // calculate max area text height, depending on the size of the data available
    if (((height/26) * areas.size()) + height/1.875 <= 23*height/26) { // if fewer than 10 areas are on the screen, do not move height
        maxAreaHeight = -height/1.875; // negate max height, so when it's negated again, it's positive and at the correct position
    } else {
        maxAreaHeight = (height/26) * areas.size() - 23*height/26;
    }

    // set slider height and move limit
    if (areas.size() > 10) {
        sld_2.canMove = true;
        sld_2.setControlHeight((5*height/13) / 10); // set slider length to one tenth of total height
        sld_2.draw(); // draw screen 2 slider
    } else {
        sld_2.canMove = false;
        sld_2.setControlHeight(5*height/13);          // reference point
    }

    // the negative height of text.
    baseYCountT = -(areas.size()*height/26) * (((sld_2.y - sld_2.tMin) / (sld_2.tLimit - sld_2.tMin - sld_2.height + 0.01))) + height/1.875; // +0.01 to avoid division by 0

    // text cannot go past these limits
    if (baseYCountT >= height/1.875) baseYCountT = height/1.875;
    if (baseYCountT <= -maxAreaHeight) baseYCountT = -maxAreaHeight;

    yCount = baseYCountT;

    for (int i = 0; i < areas.size(); i++) {
        Area a = areas.get(i);
        fill(WHITE);
        stroke(BLACK);
        if (yCount >= height/1.875 && yCount <= baseYCountB) {
            // draw each area on the screen if it appears within the bounds set
            fill(BLACK);
            text(a.getName(), width/1.85, baseYCountT + (i*(height/26)));
            // if hovering over any area
            if (mouseX >= width/1.875 && mouseX <= 47*width/60 && mouseY >= baseYCountT + ((i-1)*(height/26)) && mouseY <= baseYCountT + (i*(height/26))) {
                // draw a hollow rectangle over the area
                fill(0, 0);
                stroke(0);
                rect(width/1.875, baseYCountT - height/30 + ((i)*(height/26)), width/4, height/26);
                fill(BLACK);
                if (mousePressed) {
                    // if hovering over an area and the mouse is pressed, set input to the area pressed
                    input = a.getName();
                    searchBar.setLabel(a.getName());
                }
            }
        }
        yCount += height/26;
    }
    textAlign(LEFT);

    textAlign(BASELINE);
    if (noSelectedArea) {
        errorInput = true;
    }
}

void drawScreen3() {
    Area area = new Area();
    area = td.getCountry(0).getArea(tID);
    state = area.getState();
    
    selectedState = area.getState().getName(); // (re)define the current state if it isn't already
    int spread = 0;

    ArrayList<String> dates = new ArrayList<String>();
    dates.addAll(area.getData().keySet());
    
    sld_g.draw();
    spread = (int)(((dates.indexOf(date2) - dates.indexOf(date1) + 2)/2) * (((sld_g.x - sld_g.tMin) / (sld_g.tLimit - sld_g.tMin - sld_g.width))));
    if (spread < 0) spread = 0;
    sld_g.setLabel(Integer.toString(spread));
    g1.setBarGap(2*(g1.gWidth / (g1.gWidth / area.getData().size())));
    currentGraph = g1;
    textAlign(CENTER);
    toggleGraphWidgets(currentScreen, currentGraph);
    changeGraphData(currentScreen, currentGraph, area, spread, date1, date2);
    g1.displayGraph();

    textAlign(LEFT);
}

void drawScreen4() {
    Country co = td.getCountry("united states");
    State state = co.getState(selectedState);
    int spread = 0;
    ArrayList<String> dates = new ArrayList<String>();
    dates.addAll(state.totalData().keySet());
    sld_g.draw();
    spread = (int)(((dates.indexOf(date2) - dates.indexOf(date1) + 2)/2) * (((sld_g.x - sld_g.tMin) / (sld_g.tLimit - sld_g.tMin - sld_g.width))));
    if (spread < 0) spread = 0;
    sld_g.setLabel(Integer.toString(spread));
    textAlign(CENTER);
    currentGraph = g5;
    g3.setBarGap(2*(g5.gWidth / (g5.gWidth / state.totalData().size())));
    changeGraphData(currentScreen, currentGraph, state, spread, date1, date2);
    toggleGraphWidgets(currentScreen, currentGraph);
    currentGraph.displayGraph();
    textAlign(LEFT);
}

// Graph the cases per area in a state
void drawScreen4_2() {
    textAlign(CENTER);
    Country co = td.getCountry("united states");
    State state = co.getState(selectedState);
    // Displays a graph with each area in the state on the x-axis and each area's total cases on the y-axis
    g3.setBarGap(2*(g3.gWidth / (g3.gWidth / state.totalData().size())));
    g3.loadData(state.getAreaNames(), state.areaCases());
    g3.setLimits("set", 0, 100, 0, state.maxAreaCases(), state.getAreas().size(), 15, color(255), false);
    g3.setLabel("Dates", "Cases", state.getName());
    g3.displayGraph();
    textAlign(LEFT);
}

void drawScreen6() {
    textAlign(CENTER);
    Country tCo = td_mini.getCountry(0);
    g10.setBarGap(2*(g10.gWidth / (g10.gWidth / tCo.totalData().size())));
    ArrayList<CheckBox> scCheck_1 = screen6.checkBoxList;  // arraylist of checkboxes
    toggleGraphWidgets(currentScreen, g10);
    LinkedHashMap<String, Integer> countryBT = new LinkedHashMap<String, Integer>(); // hashmap to store country data
    int spread = 0;
    ArrayList<String> dates = new ArrayList<String>();
    dates.addAll(tCo.totalData().keySet());
    sld_g.draw();
    spread = (int)(((dates.indexOf(date2) - dates.indexOf(date1) + 2)/2) * (((sld_g.x - sld_g.tMin) / (sld_g.tLimit - sld_g.tMin - sld_g.width))));
    if (spread < 0) spread = 0;
    sld_g.setLabel(Integer.toString(spread));

    fill(WHITE);
    text("(Using a sample of 10k values.)", 4*width/5, height/30);

    if (scCheck_1.get(1).checked && datesEntered && !date1.equalsIgnoreCase("") && !date2.equalsIgnoreCase("")) {   
        countryBT = tCo.atDates(tCo.totalData(), date1, date2);
        g10.loadData(tCo.stateDiff(countryBT, spread));
        g10.setLimits("set", 0, 100, 0, tCo.biggestDiffCases(), tCo.stateDiff(tCo.totalData(), spread).size(), 10, color(255), false);
        g10.setLabel("Dates" + "\n diff: " + tCo.biggestDiff(), "Cases", tCo.getName());
    } else {
        countryBT = tCo.totalData();
        g10.loadData(countryBT);
        g10.setLimits("set", 0, 100, 0, tCo.maxCases(countryBT), countryBT.size(), 15, color(255), true);
        g10.setLabel("Dates", "Cases", tCo.getName());
    }
    g10.displayGraph();
    textAlign(LEFT);
}

// display text options
void displayOptions() {
    textAlign(LEFT);
    fill(WHITE);
    float txtGap = width/30;

    textSize(17);
    text("Graph Backgrounds", width/25, (12*height/16) + (3*width/160));
    text("Graph Backgrounds", width/25, (12*height/16) + (3*width/160)); // duplicate to imitate a bold font
    textSize(13);
    text("Grid", width/25 + txtGap, (13*height/16) + (3*width/160));
    text("Horizontal Line", width/25 + txtGap, (14*height/16) + (3*width/160));
    text("Blank", width/25 + txtGap, (15*height/16) + (3*width/160));

    textSize(17);
    text("Graph Types", width/5, (12*height/16) + (3*width/160));
    text("Graph Types", width/5, (12*height/16) + (3*width/160));
    textSize(13);
    text("Bar Chart", width/5 + txtGap, (13*height/16) + (3*width/160));
    text("Histogram", width/5 + txtGap, (14*height/16) + (3*width/160));
    text("Line Chart", width/5 + txtGap, (15*height/16) + (3*width/160));
    text("Scatter Plot", width/3.25 + txtGap, (13*height/16) + (3*width/160));
    text("Horizontal Bar Chart", width/3.25 + txtGap, (14*height/16) + (3*width/160));
    text("Area chart", width/3.25 + txtGap, (15*height/16) + (3*width/160));

    textSize(17);
    text("Extra Customisation", width/2.25, (12*height/16) + (3*width/160));
    text("Extra Customisation", width/2.25, (12*height/16) + (3*width/160));
    textSize(13);
    text("Show data on hover", width/2.25 + txtGap, (13*height/16) + (3*width/160));
    text("Find highest difference in cases", width/2.25 + txtGap, (14*height/16) + (3*width/160));

    textSize(17);
    text("Change Spread", width/2.25, (14.75*height/16) + (3*width/160));
    text("Change Spread", width/2.25, (14.75*height/16) + (3*width/160));

    textSize(17);
    text("Search by Date", width/1.5, (12*height/16) + (3*width/160));
    text("Search by Date", width/1.5, (12*height/16) + (3*width/160));
}

void runTweetsChoreo() {
    // // Create the Choreo object using your Temboo session
    // Tweets tweetsChoreo = new Tweets(session);

    // // Set Profile
    // tweetsChoreo.setCredential(twitterProfile);

    // // Set inputs
    // tweetsChoreo.setQuery(searchText);

    // // Run the Choreo and store the results
    // TweetsResultSet tweetsResults = tweetsChoreo.run();

    // // Store results in a JSON object
    // searchResults = parseJSONObject(tweetsResults.getResponse());
}

void getTweetFromJSON() {
    // JSONArray statuses = searchResults.getJSONArray("statuses"); // Create a JSON array of the Twitter statuses in the object
    // JSONObject tweet = statuses.getJSONObject(0); // Grab the first tweet and put it in a JSON object
    // tweetText = tweet.getString("text"); // Pull the tweet text from tweet JSON object
}

// void displayText() {
//     println(tweetText); // Print tweet to console
// }
