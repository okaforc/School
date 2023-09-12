public class TableData {
    ArrayList<Country> world = new ArrayList<Country>();     // ArrayList of countries
    ArrayList<String> tCountries = new ArrayList<String>();  // ArrayList of countries
    ArrayList<String> tStates = new ArrayList<String>();     // ArrayList of states
    ArrayList<String> tAreas = new ArrayList<String>();      // ArrayList of areas
    Table table;
    int xpos = 12, ypos = 12; 

    float maxTextHeight = 0;

    TableData(Table table) {
        this.table = table;                
        loadTableData();
    }

    TableData(String file) {
        table = loadTable(file, "header");      // load csv data
        loadTableData();
    }

    void loadTableData() {
        TableRow tempTR = table.getRow(0);                      // get first row
        world.add(new Country(tempTR.getString("country")));    // initialise world with first country


        // add each country in the csv file to the countries ArrayList
        for (int i = 0; i < table.getRowCount(); ++i) {
            TableRow tr = table.getRow(i);
            tCountries.add(tr.getString("country"));
        }

        // remove any duplicates
        removeDuplicates(tCountries);

        // add each unique country to the world
        for (int i = 0; i < tCountries.size(); ++i) {
            String s = tCountries.get(i);
            world.add(new Country(s));
        }


        println("33%...");

        // debug - check if states are unique
        /* println(String.join(", ", states)); */

        // new ArrayList which keep track of which states have been added
        ArrayList<String> usedStates = new ArrayList<String>();

        // match each state to its country and add it to that country's state ArrayList, avoiding duplicates
        for (int k = 0; k < table.getRowCount(); k++) {
            TableRow tr = table.getRow(k);
            String state = tr.getString("county/state");
            String country = tr.getString("country");
            for (Country co : world) {
                if (co.getName().equalsIgnoreCase(country) && !usedStates.contains(state)) {
                    co.addState(new State(state));
                    usedStates.add(state);
                }
            }
        }


        println("66%...");


        // add all areas in the csv file to the areas ArrayList
        for (int i = 0; i < table.getRowCount(); i++) {
            TableRow tr = table.getRow(i);
            tAreas.add(tr.getString("area"));
        }

        removeDuplicates(tAreas);   // remove any duplicates

        // new ArrayList which keep track of which areas and IDs have been added
        ArrayList<Integer> usedIDs = new ArrayList<Integer>();

        // match each state to its country and add it to that country's state ArrayList, avoiding duplicates
        for (int i = 0; i < table.getRowCount(); i++) {
            TableRow tr = table.getRow(i);
            String state = tr.getString("county/state");
            String area = tr.getString("area");
            String date = tr.getString("date");
            int cases = tr.getInt("cases");
            int id = tr.getInt("geoid");
            if (cases < 0) cases = 0;
            for (Country co : world) {
                for (int j = 0; j < co.getStates().size(); j++) {
                    State st = co.getState(j);
                    if (st.getName().equalsIgnoreCase(state)) {
                        if (area.equalsIgnoreCase("Unknown")) {
                            id += j;
                            area = "Various areas";
                        }
                        if (!usedIDs.contains(id)) {
                            Area newArea = new Area(area, date, cases, id, st);
                            st.addArea(newArea);
                            usedIDs.add(id);
                        } else {
                            for (Area a : st.getAreas()) {
                                if ((a.getName().equalsIgnoreCase(area) || a.getName().equalsIgnoreCase("Various areas")) && id == a.getGeoID()) {
                                    a.addInfo(date, cases);
                                }
                            }
                        }
                    }
                }
            }
        }
        println("100%");
        println("Finished setup.");
    }

    public ArrayList<String> getInfo() {
        // Returns all data in the table
        ArrayList<String> info = new ArrayList<String>();
        for (TableRow tr : table.rows()) {
            info.add(
                "\""+ tr.getString("date") + "," + 
                tr.getString("area") +  "," + 
                tr.getString("county/state") + "," + 
                tr.getInt("geoid") +  "," + 
                tr.getInt("cases") +  "," + 
                tr.getString("country") + "\"" 
                );
        }
        return info;
    }

    public ArrayList<String> getInfo(String name, String type) {
        // Returns the rows where the chosen data is mentioned
        ArrayList<String> info = new ArrayList<String>();
        if (name != null) {
            for (TableRow tr : table.rows()) {
                int nn;
                try {
                    nn = Integer.parseInt(name);
                    if (nn == tr.getInt(type)) {
                        info.add(
                            "\""+ tr.getString("date") + "," + 
                            tr.getString("area") +  "," + 
                            tr.getString("county/state") + "," + 
                            tr.getInt("geoid") +  "," + 
                            tr.getInt("cases") +  "," + 
                            tr.getString("country") + "\"" 
                            );
                    }
                } 
                catch (Exception e1) {
                    try {
                        if (name.equalsIgnoreCase(tr.getString(type))) {
                            info.add(
                                "\""+ tr.getString("date") + "," + 
                                tr.getString("area") +  "," + 
                                tr.getString("county/state") + "," + 
                                tr.getInt("geoid") +  "," + 
                                tr.getInt("cases") +  "," + 
                                tr.getString("country") + "\"" 
                                );
                        }
                    } 
                    catch(Exception e2) {
                        e2.printStackTrace();
                        return null;
                    }
                }
            }
            return info;
        }
        return null;
    }


    public void getFormattedInfo(float yPos, int ts) {
        // Formats and displays all data in the table
        textSize(ts);
        fill(WHITE);
        ArrayList<String> info = new ArrayList<String>();       // arraylist to house data to be shown on screen
        String[] vals = new String[info.size()];                // an empty string array with the size of info
        info = getInfo();                                       // get all information in the table as an arraylist of strings per row
        info.add(0, "Date,Area,State,GeoID,Cases,Country");     // add the headers of the table to the beginning of the table
        float x = 0;
        float y = yPos;                                         // y position
        for (String s : info) {                                 // iterate over each row
            x = width/70;                                       // reset the x position
            vals = split(s, ",");
            for (String fs : vals) {
                // for each row, split it by a comma. this turns each row, represented as a string, into another string array, split by commas, this is also iterated over.
                text(fs.replace("\"", ""), x, y);               // remove the quotation marks in the string and show the value on the screen.
                x += width/9;                                   // update the x position for each value in this row.
            }
            y += 15;                                            // update the y position for each row.
        }

        // If the max text height is on the screen, disable scrolling past the screen.
        if (15 * info.size() < height) {
            maxTextHeight = 0;
        } else {
            // Otherwise, the max text height is defined by the y difference * dataset size - screen height.
            maxTextHeight = 15 * info.size() - height;
        }
    }


    public void getFormattedInfo(String name, String type, float yPos, int ts) {
        textSize(ts);
        fill(WHITE);
        // Formats and displays all chosen data in the table
        ArrayList<String> info = new ArrayList<String>();
        String[] vals = new String[info.size()];
        info = getInfo(name, type);                             // get all information relating to the search (e.g. "washington", "state")
        info.add(0, "Date,Area,State,GeoID,Cases,Country");
        float x = 0;
        float y = yPos;
        for (String s : info) {
            x = width/70;
            vals = split(s, ",");
            for (String fs : vals) {
                text(fs.replace("\"", ""), x, y);
                x += width/9;
            }
            y += 15;
        }

        if (15 * info.size() < height) {
            maxTextHeight = 0;
        } else {
            maxTextHeight = 15 * info.size() - height;
        }
    }

    public float getMaxTextHeight() {
        return maxTextHeight;
    }


    public ArrayList<Country> getWorld() {
        // Return the ArrayList of countries
        return world;
    }

    public Country getCountry(int index) {
        // Return a specific country from the ArrayList of countries at a chosen index
        return world.get(index);
    }

    public Country getCountry(String c) {
        // Return a specific country from the ArrayList of countries with a chosen country name
        for (Country co : world) {
            if (c.equalsIgnoreCase(co.getName())) {
                return co;
            }
        }
        return null;
    }

    public void printRawData() {
        fill(WHITE);
        int xpos = 12;
        int ypos = 12;
        text("Date", xpos, ypos);
        xpos += 100;
        text("Area", xpos, ypos);
        xpos += 100;
        text("State", xpos, ypos);
        xpos += 100;
        text("GeoID", xpos, ypos);
        xpos += 100;
        text("Cases", xpos, ypos);
        xpos += 100;
        text("Country", xpos, ypos);
        xpos = 12;
        ypos += 15;
        for (TableRow row : table.rows()) {                        // for every row of the  table
            String date = row.getString("date");                       // making a Strig variable to record the date of current row
            String area = row.getString("area");
            String countyState = row.getString("county/state");
            int geoid = row.getInt("geoid");
            int cases = row.getInt("cases");
            String country = row.getString("country");
            text(date, xpos, ypos);
            xpos += 100;
            text(area, xpos, ypos);
            xpos += 100;
            text(countyState, xpos, ypos);
            xpos += 100;
            text(geoid, xpos, ypos);
            xpos += 100;
            text(cases, xpos, ypos);
            xpos += 100;
            text(country, xpos, ypos);
            xpos = 12;
            ypos += 15;
        }
    }
}