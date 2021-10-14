public class annoying {
    public static void main(String[] args) {
        
        /*  
           -- register 0
            reg00 : reg4 PORT MAP(
                D => data_src_mux_out,
                load => load_reg0,
                Clk => Clk,
                Q => reg0_q
            );
         */
        
        // System.out.print("SIGNAL ");
        for (int i = 0; i < 32; i++) {
            String s = String.format("%5s", Integer.toBinaryString(i)).replaceAll(" ", "0");
            // System.out.printf("%s: %s\n", s, gate(s));
            // System.out.printf("in%d AFTER 5ns WHEN %s ELSE -- %s\n", i, gate(s).substring(0, gate(s).length() - 5), s);
            // System.out.printf("in%d AFTER 5ns WHEN %s ELSE \n", i, s, s);
            for (int j = 0; j < 5; j++) {
                System.out.printf("s%d <= '%s'; \n", j, s.toCharArray()[j]);
            }
            System.out.println("\n");
        }
        // System.out.print(": STD_LOGIC");
    }

    public static String gate(String s) {
        String r = "";

        // for (int i = s.length()-1; i >= 0; i--) {
        for (int i = 0; i < s.length(); i++) {
            r += String.format("s%d = '%c' and ", i, s.toCharArray()[i]);
            // if (s.toCharArray()[i] == '0') {
            // } else {
            //     r += String.format("A(%d) and ", i);
            // }
        }

        return r;
    }
}
