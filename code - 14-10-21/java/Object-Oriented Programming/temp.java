import java.util.ArrayList;
import java.util.HashMap;

public class temp {
    public static void main(String[] args) {
        /* ArrayList<String> dd = new ArrayList<String>(10);
        System.out.println(dd.size());
        String[] dates = { "1", "2", "3" };
        Integer[] cases = { 1, 2, 3 };
        Area d15 = new Area("Dublin 15", dates, cases);
        Area d14 = new Area("Dublin 14", dates, cases);
        State dublin = new State("Dublin");
        dublin.addArea(d15);
        dublin.addArea(d14);
        Country ireland = new Country("Ireland");
        ireland.addState(dublin);
        try {
            System.out.println(ireland.getState(0).getArea(1).getName());
        } catch (Exception e) {
            System.out.println("Unknown");
        } */
    }
    
}

class Area {
    String name;
    HashMap<String, Integer> data = new HashMap<String, Integer>();

    public Area(String name, String[] dates, Integer[] cases) {
        this.name = name;
        for (int i = 0; i < cases.length; i++) {
            data.put(dates[i], cases[i]);
        }
    }

    public Area(String name, HashMap<String, Integer> values) {
        this.name = name;
        this.data = values;
    }

    public Area(String name) {
        this.name = name;
        this.data = new HashMap<String, Integer>();
    }


    public void addInfo(String[] dates, Integer[] cases) {
        for (int i = 0; i < cases.length; i++) {
            data.put(dates[i], cases[i]);
        }
    }

    public void addInfo(HashMap<String, Integer> values) {
        data = values;
    }

    public void addInfo(String date, Integer cases) {
        data.put(date, cases);
    }

    public int getCases(String date) {
        return data.get(date);
    }

    public ArrayList<String> datesAtCase(int cases) {
        ArrayList<String> dac = new ArrayList<String>();
        for (String d : data.keySet()) {
            if (data.get(d) >= cases) {
                dac.add(d);
            }
        }
        return dac;
    }

    public String getName() {
        return name;
    }

}



class State {
    ArrayList<Area> areas;
    int id;
    String name;

    public State(String name, ArrayList<Area> areas, int id) {
        this.name = name;
        this.areas = areas;
        this.id = id;
    }

    public State(String name) {
        this.name = name;
        this.areas = new ArrayList<Area>();
    }

    void addArea(Area a) {
        areas.add(a);
    }

    void addArea(String name, HashMap<String, Integer> hm) {
        areas.add(new Area(name, hm));
    }

    void addArea(String name, String[] dates, Integer[] cases) {
        areas.add(new Area(name, dates, cases));
    }

    Area getArea(String name) {
        for (Area a : areas) {
            if (a.getName().equalsIgnoreCase(name)) {
                return a;
            }
        }
        return null;
    }

    Area getArea(int index) {
        if (index >= 0 && areas != null) {
            try {
                Area a = areas.get(index);
                if (a != null) {
                    return areas.get(index);
                }
            } catch (Exception e) {
            }
        }
        return null;
    }

    public String getName() {
        return name;
    }

    public int getGeoID() {
        return id;
    }

}

class Country {
    ArrayList<State> states;
    String name;

    public Country(String name, ArrayList<State> states) {
        this.name = name;
        this.states = states;
    }

    public Country(String name) {
        this.name = name;
        states = new ArrayList<State>();
    }

    void addState(State c) {
        states.add(c);
    }

    State getState(String name) {
        if (name != null && states != null) {
            for (State s : states) {
                if (name.equalsIgnoreCase(s.getName())) {
                    return s;
                }
            }
        }
        return null;
    }

    State getState(int index) {
        if (index >= 0 && states != null) {
            try {
                State s = states.get(index);
                if (s != null) {
                    return states.get(index);
                }
            } catch (Exception e) {
            }
        }
        return null;
    }

}
