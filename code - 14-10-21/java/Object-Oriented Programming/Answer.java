class Student {
    private String name;
    public static void main(String[] args) {
        
    }

    Student() {
        name = "Unknown";
    }

    Student(String n) {
        name = n;
    }

    public void printName() {
        System.out.println(name);
    }
}

public class Answer {
    public static void main(String[] args) {
        Student s = new Student("Iris");
        Student a = new Student();

        s.printName();
        a.printName();
    }
}
