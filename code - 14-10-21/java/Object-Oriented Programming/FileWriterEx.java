import java.io.BufferedReader;



import java.io.*;

public class FileWriterEx {
    public static void main(String[] args) {
        try {
            FileWriter fw = new FileWriter("out.txt");
            fw.write("strawberries are cool \nso are oranges");
            fw.close();

            BufferedReader br = new BufferedReader(new FileReader("out.txt"));
            int i;
            while ((i = br.read()) != -1) {
                System.out.print((char)i);
            }
            br.close();
        } catch (IOException | NullPointerException e) {
            e.printStackTrace();
        } catch (Exception d) {
            d.printStackTrace();
        }
    }
}
