public class State {
    ArrayList<Area> areas;
    String name;
    int biggestDiff = 0;
    int biggestDiffCases = 0;
    int maxBetween = 0;

    public State(String name, ArrayList<Area> areas) {
        this.name = name;
        this.areas = areas;
    }

    public State(String name) {
        this.name = name;
        this.areas = new ArrayList<Area>();
    }

    // Add an area to the ArrayList of areas given an Area object
    void addArea(Area a) {
        areas.add(a);
    }

    // Add an area to the ArrayList of areas given a name, HashMap data, and a GeoID
    void addArea(String name, LinkedHashMap<String, Integer> hm, int id) {
        areas.add(new Area(name, hm, id, this));
    }

    // Add an area to the ArrayList of areas given a name, two ArrayLists of data, and a GeoID
    void addArea(String name, ArrayList<String> dates, ArrayList<Integer> cases, int id) {
        areas.add(new Area(name, dates, cases, id, this));
    }

    // Return the ArrayList of areas
    ArrayList<Area> getAreas() {
        return areas;
    }

    // Return the ArrayList of area names
    ArrayList<String> getAreaNames() {
        LinkedList<String> tNames = new LinkedList<String>();
        for (Area a : areas) {
            tNames.add(a.getName());
        }
        ArrayList<String> names = new ArrayList<String>();
        names.addAll(tNames);
        return names;
    }

    // Return an area from the ArrayList of areas given a name
    Area getArea(String name) {
        for (Area a : areas) {
            if (a.getName().equalsIgnoreCase(name)) {
                return a;
            }
        }
        return null;
    }

    // Return an area from the ArrayList of areas given an index
    Area getArea(int index) {
        if (index >= 0 && areas != null) {
            try {
                Area a = areas.get(index);
                if (a != null) {
                    return areas.get(index);
                }
            } 
            catch(Exception e) {
            }
        }
        return null;
    }

    public int getCases(String date) {
        return totalData().get(date);
    }

    // Return the total amount of cases from each area in the state
    public int totalCases() {
        int cases = 0;
        for (Area a : areas) {
            cases += a.getCases();
        }
        return cases;
    }

    public int maxCases(LinkedHashMap<String, Integer> d) {
        return Collections.max(d.values());
    }

    // Return the cases at a given date
    public ArrayList<Integer> areaCases(String date) {
        LinkedList<Integer> stateCases = new LinkedList<Integer>();
        for (Area a : areas) {
            for (String d : a.getData().keySet()) {
                if (d.equalsIgnoreCase(date)) {
                    stateCases.add(a.getCases(date));
                }
            }
        }
        ArrayList<Integer> actCases = new ArrayList<Integer>();
        actCases.addAll(stateCases);
        return actCases;
    }

    // Return an ArrayList with the highest amount of cases in each state.
    // A LinkedList is used here to preserve the order of the cases. (I'm not sure if that actually does anything though)
    public ArrayList<Integer> areaCases() {
        LinkedList<Integer> stateCases = new LinkedList<Integer>();
        for (Area a : areas) {
            stateCases.add(a.maxCases());
        }
        ArrayList<Integer> actCases = new ArrayList<Integer>();
        actCases.addAll(stateCases);
        return actCases;
    }

    public int maxAreaCases() {
        return Collections.max(areaCases());
    }
    
    public int maxAreaCases(String date) {
        return Collections.max(areaCases(date));
    }

    // Return an index of an area given a GeoID
    public int indexOf(int id) {
        for (int i = 0; i < areas.size(); i++) {
            Area a = areas.get(i);
            if (id == a.getGeoID()) {
                return i;
            }
        }
        return -1;
    }

    // Return a LinkedHashMap with the data from each area in the state.
    public LinkedHashMap<String, Integer> totalData() {
        LinkedHashMap<String, Integer> finData = new LinkedHashMap<String, Integer>();
        ArrayList<String> dates = new ArrayList<String>();
        for (Area a : areas) {
            for (String d : getDates(a.getData())) {
                dates.add(d);
            }
        }

        removeDuplicates(dates);

        for (String date : dates) {
            int vad = 0;
            for (Area a : areas) {
                for (String d : a.getData().keySet()) {
                    if (d.equalsIgnoreCase(date)) {
                        vad += a.getCases(date);
                    }
                }
            }
            finData.put(date, vad);
        }
        return finData;
    }

    // Return the highest amount of cases from the totalData method
    public int maxTotalData() {
        return Collections.max(totalData().values());
    }

    // Return the name of the state
    public String getName() {
        return name;
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


    // Returns the actual difference from the stateDiff method.
    public int biggestDiff() {
        return biggestDiff;
    }

    // Returns the max cases from the dates with the highest difference in cases
    public int biggestDiffCases() {
        return biggestDiffCases;
    }


    public LinkedHashMap<String, Integer> atDates(LinkedHashMap<String, Integer> tData, String d1, String d2) {
        ArrayList<String> sDates = new ArrayList<String>();
        for (String ww : tData.keySet()) {
            sDates.add(ww);
        }
        LinkedHashMap<String, Integer> dbd = new LinkedHashMap<String, Integer>();  // data between dates
        int ind1 = sDates.indexOf(d1);
        int ind2 = sDates.indexOf(d2);
        for (String d : sDates) {
            try {    
                if (sDates.indexOf(d) >= ind1 && sDates.indexOf(d) <= ind2) {
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

    boolean containsArea(String name) {
        for (Area a : areas) {
            try {
                int id = Integer.parseInt(name);
                if (a.getGeoID() == id) {
                    return true;
                }
            } catch (Exception e) {
                if (a.getName().equalsIgnoreCase(name)) {
                    return true;
                }
            }
        }
        return false;
    }
    
    boolean containsArea(int id) {
        for (Area a : areas) {
            if (a.getGeoID() == id) {
                return true;
            }
        }
        return false;
    }
}
