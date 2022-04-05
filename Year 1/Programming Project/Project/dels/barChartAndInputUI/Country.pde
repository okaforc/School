public class Country {
    ArrayList<State> states;
    String name;
    int biggestDiff, biggestDiffCases, maxBetween;
    public Country(String name, ArrayList<State> states) {
        this.name = name;
        this.states = states;
    }
    
    public Country(String name) {
        this.name = name;
        states = new ArrayList<State>();
    }
    
    // Add a state to the ArrayList of states
    void addState(State c) {
        states.add(c);
    }
    
    // Add a state to the ArrayList of states given a name
    void addState(String c) {
        states.add(new State(c));
    }
    
    // Return a state from the ArrayList of states given a name
    public State getState(String name) {
        try {
            if (name != null && states != null) {
                for (State s : states) {
                    if (name.equalsIgnoreCase(s.getName())) {
                        return s;
                    }
                }
            }
        } 
        catch(Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    
    // Return a state from the ArrayList of states given an index
    public State getState(int index) {
        if (index >= 0 && states != null) {
            try {
                State s = states.get(index);
                if (s != null) {
                    return states.get(index);
                }
            } 
            catch(Exception e) {
                e.printStackTrace();
            }
        }
        return null;
    }
    
    // Return an area from a state in the ArrayList of states given a name
    public Area getArea(String name) {
        for (State st : states) {
            for (Area a : st.getAreas()) {
                if (name.equalsIgnoreCase(a.getName())) {
                    return a;
                }
            }
        }
        return null;
    }

    // Return an area from a state in the ArrayList of states given a GeoID
    public Area getArea(int id) {
        for (State st : states) {
            for (Area a : st.getAreas()) {
                if (id == a.getGeoID()) {
                    return a;
                }
            }
        }
        return null;
    }
    
    // Return an ArrayList of areas from each state
    public ArrayList<Area> getAreas() {
        ArrayList<Area> tAreas = new ArrayList<Area>();
        for (State st : states) {
            for (Area a : st.getAreas()) {
                tAreas.add(a);
            }
        }
        return tAreas;
    }
    
    // Return the total amount of cses from each area in each state
    public int getCases() {
        int totalCases = 0;
        for (State st : states) {
            totalCases = 0;
            for (Area a : st.getAreas()) {
                totalCases += a.getCases();
            }
        }
        return totalCases;
    }

    public int maxCases(LinkedHashMap<String, Integer> d) {
        return Collections.max(d.values());
    }
    
    // Return the ArrayList of states
    public ArrayList<State> getStates() {
        return states;
    }   
    
    // Return the name of the country
    public String getName() {
        return name;
    }
    
    public boolean containsState(String name) {
        for (State s : states) {
            if (s.getName().equalsIgnoreCase(name)) {
                return true;
            }
        }
        return false;
    }

    // Return a LinkedHashMap with the data from each area in each state.
    public LinkedHashMap<String, Integer> totalData() {
        LinkedHashMap<String, Integer> finData = new LinkedHashMap<String, Integer>();
        ArrayList<String> dates = new ArrayList<String>();
        for (State s : states) {
            for (String d : getDates(s.totalData())) {
                dates.add(d);
            }
        }

        removeDuplicates(dates);
        for (String date : dates) {
            int vad = 0;
            for (State s : states) {
                for (String d : s.totalData().keySet()) {
                    if (d.equalsIgnoreCase(date)) {
                        vad += s.getCases(date);
                    }
                }
            }
            finData.put(date, vad);
        }
        return finData;
    }

    // Return an ArrayList of dates given data
    public ArrayList<String> makeDates(LinkedHashMap<String, Integer> tData) {
        ArrayList<String> sDates = new ArrayList<String>();
        for (String ww : tData.keySet()) {
            sDates.add(ww);
        }
        return sDates;
    }
    
    // Return the highest amount of cases from the totalData method
    public int maxTotalData() {
        return Collections.max(totalData().values());
    }

    // Returns the highest difference in cases between two consecutive dates
    public int biggestDiff() {
        return biggestDiff;
    }


    // Returns the max cases from the dates with the highest difference in cases
    public int biggestDiffCases() {
        return biggestDiffCases;
    }


    // Return a LinkedHashMap with the values around the highest difference between two dates.
    // This is compared with the highest difference from each area in the state.
    public LinkedHashMap<String, Integer> stateDiff(LinkedHashMap<String, Integer> tData, int spread) {
        ArrayList<String> biggestDiffs = new ArrayList<String>();
        ArrayList<String> dates = new ArrayList<String>();
        LinkedHashMap<String, Integer> surroundingCases = new LinkedHashMap<String, Integer>();
        int lBound = 0, hBound = 0, diff = 0, spread_2;
        for (String ff : tData.keySet()) {
            dates.add(ff);
        }
        for (int i = 0; i < tData.keySet().size(); i++) {
            if (i == tData.keySet().size()-1) continue;
            String a = dates.get(i);
            String b = dates.get(i+1);
            lBound = tData.get(a);
            hBound = tData.get(b);
            int tDiff = hBound - lBound;
            if (tDiff >= diff) {
                biggestDiffs.clear();
                biggestDiffs.add(a);
                biggestDiffs.add(b);
                diff = tDiff;
                biggestDiff = diff;
            }
        }
        if (tData.keySet().size() < 2) {
            // if the size of values is one, set both values in biggestDiffs to the same date
            // this doesn't happen often, but one example of an issue is with Mariposa, California, which only had one row in the table.
            biggestDiffs.clear();
            biggestDiffs.add(dates.get(0));
            biggestDiffs.add(dates.get(0));
            biggestDiff = 0;
        }

        int dir = -1;
        if ((dates.size()-1) - dates.indexOf(biggestDiffs.get(1)) < spread) {
            spread_2 = dates.size() - dates.indexOf(biggestDiffs.get(1));
        } else {
            spread_2 = spread;
        }
        if (spread < 0) {
            spread = 0;
        }
        for (String c : biggestDiffs) {
            for (int i = 0; i < tData.size(); i++) {
                String d = dates.get(i);
                if (c.equalsIgnoreCase(d)) {
                    if (dir == -1) {
                        for (int k = spread-1; k >= 1; k--) {
                            if (i-k > 1) {
                                String cDate = dates.get(i-k);
                                surroundingCases.put(cDate, tData.get(cDate));
                            }
                        }
                        surroundingCases.put("l: " + c, tData.get(c));
                    } else {
                        surroundingCases.put("h: " + c, tData.get(c));
                        for (int k = 1; k < spread_2; k++) {
                            if (i+k <= tData.size() + 1) {
                                String cDate = dates.get(i+k);
                                surroundingCases.put(cDate, tData.get(cDate));
                            }
                        }
                    }
                }
            }
            dir *= -1;
        }
        biggestDiffCases = Collections.max(surroundingCases.values());
        return surroundingCases;
    }


    public LinkedHashMap<String, Integer> atDates(LinkedHashMap<String, Integer> tData, String d1, String d2) {
        LinkedHashMap<String, Integer> dbd = new LinkedHashMap<String, Integer>();  // data between dates
        ArrayList<String> dates = new ArrayList<String>();
        dates.addAll(tData.keySet());
        int ind1 = dates.indexOf(d1);
        int ind2 = dates.indexOf(d2);
        for (String d : dates) {
            try {    
                if (dates.indexOf(d) >= ind1 && dates.indexOf(d) <= ind2) {
                    dbd.put(d, tData.get(d));
                }
            } 
            catch (Exception e) {
                println("Date not in area");
            }
        }
        maxBetween = Collections.max(dbd.values());
        return dbd;
    }

    public int maxBetween() {
        return maxBetween;
    }
}
