boolean getState = true, getArea = true, noStateSelected = false, noSelectedArea = false, moveScreenY = true, moveScreenX = true, errorInput = false, errorInputID = false, startingDate = false, endingDate = false, zoomIn = false, zoomOut = false, zoomReset = false, hideOut = false, usingMap = false;
boolean pickingDate1 = false, pickingDate2 = false;
String strstate = "", strcounty = "", input = "";
String tempAlpha = "1234567890qwertyuiopasdfghjklzxcvbnm .,-";
ArrayList<Character> alphabet = new ArrayList<Character>();
String date1 = "", date2 = "";
Area area;
Country co;
State state;
ControlP5 cp5, cp5b;
Scrollbar sbx, sby;
Slider sld_2; // slider for screen 2 - area list
Slider sld_g, sld_s; // sliders for area and state graphs - controlling the spread
ArrayList<Slider> sliders = new ArrayList<Slider>();
String miniData = "area";
String searchTerm = "area";

JSONObject json;
String headline;
String apiKey;
String url;
String query;

PFont stdFont;          // standard font 
PFont stdFont_Big;      // larger standard font 
Screen screen0;         // home page
Screen screen1;         // pick state with map
Screen screen2;         // pick area with text box
Screen screen3;         // graph1 - for graphs choosing an area
Screen screen4;         // graph2 - for graphs taking all areas within a state
Screen screen4_2;       // graph4.2 - for taking all areas within a state with a date range box
Screen screen5;         // Used to display the raw data used, as well as widgets to control the data position.
Screen screen1_2;       // scaled map display and interaction
Screen screen6;         // graph10 - all data in country
Screen screen7;         // Used to display the raw data for an induvidual area
Screen screen8;         // Used to display the raw data for an induvidual state
Screen currentScreen;   // current screen

float centerX =0, offsetX = 0, centerY = 0, offsetY = 0;
int slide = 0;
float scaler = 1.0;

float baseRawTextPos = 50;
float rawTextPos = baseRawTextPos;
int speed = 250; // amount of text that moves when scrolled

float baseYCountT, baseYCountB, yCount; // top of area menu, buttom of area menu, current position in area menu
float tBaseYCountT = 0;

public static ArrayList<Integer> validHoverEvents = new ArrayList<Integer>(); // arraylist for storing valid hover events (widgets that light up)

String startDate = "";
String endDate = "";
int allAreasGraphType =1;
PFont arialFont;
boolean selected, datesEntered;
String originalName = "";
String selectedState = "";
String userID = "";
int tID;

// Simple interactive world map that highlights selected countries.
GeoMap geoMap_st, geoMap_sc, geoMap_c; // standard map, scaled map, current map
GeoMap geoMap_sc_1, geoMap_sc_2, geoMap_sc_3, geoMap_sc_4, geoMap_sc_5; // different levels of zoom
int mapScale = 0; // user-controlled level of zoom for the full screen map

// float mapHeight = (height/1.5) * mapScale;
PImage covidPic, maskPeople, vaccinePic, twitterLogo;
float textx = width;
TembooSession session;
String twitterProfile = "tembooTweets2";

String searchText, tweetText;

JSONObject searchResults;

TableData td, td_mini, td_97k, td_1m, td_c97k, td_c1m; // different data sets
String cFileName = "Daily-1M";
Graph g1, g3, g5, g10;
Graph currentGraph;
String gType = "bar"; // default graph element type

ScrollableList sl;

float exitTimer = 2f;
boolean hasQuit = false;
float exitSpeed = 0.01f;

SoundFile blingSFX, clickSFX, errorSFX, slideSFX;


/* 
        Graph Screens:
                - screen 3: data using only areas (g1, g4, g6, g8)
                - screen 4: data using only states (g2, g5, g7, g9)
                - screen 4.2: state areas (g3)
                - screen 6: data using the entire country (g10)
        
        All graph screens, with the exception of screen 4.2, should have all widgets, including the ControlP5 dropdown menu. This is because
        all code works within two chosen dates, and it help to avoid an overabundance screens.

        For the highest difference, a boolean checkbox will change the data used for that.
 */


/* 
        // Graph templates

        // Displays a graph with the date on the x-axis, and the cases at that date on the y-axis
        Area area = co.getArea("new york city");
        g1.setBarGap(400);
        g1.loadData(area.getData());
        g1.setLimits("set", 0, 100, 0, area.maxCases(), area.getData().size(), 15, color(WHITE), true);
        g1.setLabel("Dates", "Cases", area.getName() + ", " + area.getState().getName());
        g1.displayGraph();


        // Displays a graph with the date on the x-axis and the cumulative number of cases in each area in the state on the y-axis
        State state = co.getState("new york");
        g2.setBarGap(250);
        g2.loadData(state.totalData());
        g2.setLimits("set", 0, 100, 0, state.maxTotalData(), state.getAreas().size(), 15, color(WHITE), true);
        g2.setLabel("Dates", "Cases", state.getName());
        g2.displayGraph();


        // Displays a graph with each area in the state on the x-axis and each area's total cases on the y-axis
        State state = co.getState("washington");
        g3.setBarGap(250);
        g3.loadData(state.getAreaNames(), state.areaCases());
        g3.setLimits("set", 0, 100, 0, state.maxCases(), state.getAreas().size(), 15, color(WHITE), false);
        g3.setLabel("Dates", "Cases", state.getName());
        g3.displayGraph();


        // Displays a graph with values within the highest difference of cases in an area
        Area area = co.getArea("eagle");
        int spread = 4;
        g4.setBarGap(250);
        g4.loadData(area.areaDiff(area.getData(), spread));
        g4.setLimits("set", 0, 100, 0, area.biggestDiffCases(), area.areaDiff(area.getData(), spread).size(), 10, color(255), false);
        g4.setLabel("Dates" + "\n\n diff: " + area.biggestDiff(), "Cases", area.getName() + ", " + area.getState().getName());
        g4.displayGraph();


        // Displays a graph with values within the highest difference of cases in state
        int spread = 10;
        State state = co.getState("new york");
        g5.setBarGap(250);
        g5.loadData(state.stateDiff(state.totalData(), spread));
        g5.setLimits("set", 0, 100, 0, state.biggestDiffCases(), state.stateDiff(state.totalData(), spread).size(), 10, color(WHITE), false);
        g5.setLabel("Dates" + "\n\n diff: " + state.biggestDiff(), "Cases", state.getName());
        g5.displayGraph();


        // Displays a graph with data between two user-chosen dates in an area
        Area area = co.getArea("snohomish");
        LinkedHashMap<String, Integer> areaBT = area.atDates(area.getData(), "21/01/2020", "03/04/2020");
        g6.setBarGap(150);
        g6.loadData(areaBT);
        g6.setLimits("set", 0, 100, 0, area.maxBetween(), areaBT.size(), 9, color(WHITE), true);
        g6.setLabel("Dates", "Cases", area.getName() + ", " + area.getState().getName());
        g6.displayGraph();


        // Displays a graph with data between two user-chosen dates in a state
        State state = co.getState("louisiana");
        LinkedHashMap<String, Integer> stateBT = state.atDates(state.totalData(), "06/03/2020", "31/03/2020");
        g7.setBarGap(250);
        g7.loadData(stateBT);
        g7.setLimits("set", 0, 100, 0, state.maxBetween(), stateBT.size(), 10, color(WHITE), true);
        g7.setLabel("Dates", "Cases", state.getName());
        g7.displayGraph();


        // Displays a graph with highest difference between two consecutive dates between two user-chosen dates in an area and a spread
        int spread = 10;
        Area area = co.getArea("snohomish");
        LinkedHashMap<String, Integer> areaBT = area.atDates(area.getData(), "24/02/2020", "06/04/2020");
        g8.setBarGap(250);
        g8.loadData(area.areaDiff(areaBT, spread));
        g8.setLimits("set", 0, 100, 0, area.biggestDiffCases(), area.areaDiff(areaBT, spread).size(), 10, color(WHITE), false);
        g8.setLabel("Dates" + "\n\n diff: " + area.biggestDiff(), "Cases", area.getName() + ", " + area.getState().getName());
        g8.displayGraph();
        
        
        Displays a graph with highest difference between two consecutive dates between two user-chosen dates in a state and a spread
        int spread = 10;
        State state = co.getState("MaSsacHuseTts");
        LinkedHashMap<String, Integer> stateBT = state.atDates(state.totalData(), "24/02/2020", "22/04/2020");
        g9.setBarGap(250);
        g9.loadData(state.stateDiff(stateBT, spread));
        g9.setLimits("set", 0, 100, 0, state.biggestDiffCases(), state.stateDiff(stateBT, spread).size(), 10, color(WHITE), false);
        g9.setLabel("Dates" + "\n\n diff: " + state.biggestDiff(), "Cases", state.getName());
        g9.displayGraph();
        
        
        // Displays a graph with the date on the x-axis and the cumulative number of cases in each state in the state on the y-axis
        // NOTE: this is very slow for some reason
        g10.setBarGap(250);
        g10.loadData(co.totalData());
        g10.setLimits("set", 0, 100, 0, co.maxTotalData(), co.totalData().size(), 10, color(WHITE), true);
        g10.setLabel("Dates", "Cases", co.getName());
        g10.displayGraph();

 */
