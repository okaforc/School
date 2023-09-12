public class Area {
    String name;
    int id;
    State state;
    int biggestDiff, biggestDiffCases, maxBetween;
    ArrayList<String> dates = new ArrayList<String>();
    ArrayList<Integer> cases = new ArrayList<Integer>();
    LinkedHashMap<String, Integer> data = new LinkedHashMap<String, Integer>();


    // Contructors

    public Area(String name, ArrayList<String> dates, ArrayList<Integer> cases, int id, State st) {
        this.dates = dates;
        this.cases = cases;
        this.state = st;
        this.name = name;
        this.id = id;

        for (int i = 0; i < cases.size(); i++) {
            if (cases.get(i) == null) {
                cases.set(i, 0);
            }
            if (dates.get(i) == null) {
                if (i != cases.size() - 1) {
                    dates.set(i, dates.get(i + 1));
                } else {
                    dates.set(i, dates.get(i - 1));
                }
            }
            data.put(dates.get(i), cases.get(i));
        }
    }


    public Area(String name, LinkedHashMap<String, Integer> vals, int id, State st) {
        int i = 0;
        for (String s : vals.keySet()) {
            dates.add(s);
            cases.add(vals.get(i));
            i++;
        }

        this.state = st;
        this.name = name;
        this.data = vals;
        this.id = id;
    }


    public Area(String name, String date, Integer cases, int id, State st) {
        this.state = st;
        this.name = name;
        this.dates.add(date);
        this.cases.add(cases);
        this.id = id;
        data.put(date, cases);
    }


    public Area(String name, int id, State st) {
        this.state = st;
        this.name = name;
        this.data = new LinkedHashMap<String, Integer>();
        this.dates = new ArrayList<String>();
        this.cases = new ArrayList<Integer>();
        this.id = id;
    }

    public Area() {
        // empty area
    }


    // Data (ArrayLists cases and dates) sometimes doesn't initialise properly, so this does it again.
    private void initData() {
        dates.clear();
        cases.clear();
        int j = 0;
        for (String d : data.keySet()) {
            dates.add(d);
            cases.add(data.get(j));
            j++;
        }
    }


    // Add an ArrayList of dates and cases to the existing ArrayList of values
    public void addInfo(ArrayList<String> dates, ArrayList<Integer> cases) {
        for (int i = 0; i < cases.size(); i++) {
            data.put(dates.get(i), cases.get(i));
        }
    }


    // Add a date and amount of cases to the existing ArrayList of values
    public void addInfo(String date, Integer cases) {
        data.put(date, cases);
    }


    // Set the LinkedHaspMap of values to the one given
    public void addInfo(LinkedHashMap<String, Integer> values) {
        data = values;
    }


    // Return the LinkedHashMap of values in this array
    public LinkedHashMap<String, Integer> getData() {
        initData();
        return data;
    }


    // Return an amount of cases given a date
    public int getCases(String date) {
        return data.get(date);
    }


    // Return the total amount of cases
    public int getCases() {
        int c = 0;
        for (int i : data.values()) {
            c += i;
        }
        return c;
    }


    // Return an amount of cases given an index
    public int getCase(int index) {
        LinkedHashMap<String, Integer> hMap = new LinkedHashMap<String, Integer>();
        hMap = data;
        ArrayList<Integer> l = new ArrayList<Integer>(hMap.values());
        return l.get(index);
    }


    // Return the highest amount of cases in the area
    public int maxCases() {
        return Collections.max(data.values());
    }
    
    
    // Return the highest amount of cases in the dataset given
    public int maxCases(LinkedHashMap<String, Integer> d) {
        return Collections.max(d.values());
    }


    // Return the index in data where the amount of cases is
    public int indexOf(int cases) {
        ArrayList<Integer> nCases = new ArrayList<Integer>();
        nCases.addAll(data.values());
        return nCases.indexOf(cases);
    }


    // Return an ArrayList with the dates given an amount of cases
    public ArrayList<String> getDates(int cases) {
        ArrayList<String> dac = new ArrayList<String>();
        for (String d : data.keySet()) {
            if (data.get(d) >= cases) {
                dac.add(d);
            }
        }
        return dac;
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
    public LinkedHashMap<String, Integer> areaDiff(LinkedHashMap<String, Integer> tData, int spread) {
        ArrayList<String> biggestDiffs = new ArrayList<String>();
        LinkedHashMap<String, Integer> surroundingCases = new LinkedHashMap<String, Integer>();
        int lBound = 0, hBound = 0, diff = 0, spread_2;
        dates.clear();
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
        initData();
        LinkedHashMap<String, Integer> dbd = new LinkedHashMap<String, Integer>();  // data between dates
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

    // Return the name of this Area object
    public String getName() {
        return name;
    }


    // Return the GeoID of this Area object
    public int getGeoID() {
        return id;
    }


    // Set the GeoID of this Area object
    public void setID(int newID) {
        this.id = newID;
    }


    // Get the parent state of this area
    public State getState() {
        return state;
    }
}
