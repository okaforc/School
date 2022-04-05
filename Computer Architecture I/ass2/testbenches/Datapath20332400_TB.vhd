LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY Datapath_tb IS
END Datapath_tb;

ARCHITECTURE Behavioural OF Datapath_tb IS
    COMPONENT Datapath PORT (
        a_sel, b_sel, d_sel, f_sel : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        load_en, mb_sel, md_sel : IN STD_LOGIC;
        const_in, data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk : IN STD_LOGIC;
        data_out, add_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        v, c, n, z : OUT STD_LOGIC
        );
    END COMPONENT;
    SIGNAL a_sel, b_sel, d_sel, f_sel : STD_LOGIC_VECTOR(4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL load_en, mb_sel, md_sel : STD_LOGIC := '0';
    SIGNAL const_in, data_in : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL data_out, add_out : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v, c, n, z : STD_LOGIC := '0';

    CONSTANT period : TIME := 2ns;

BEGIN
    uut : Datapath PORT MAP(
        a_sel => a_sel,
        b_sel => b_sel,
        d_sel => d_sel,
        f_sel => f_sel,
        load_en => load_en,
        mb_sel => mb_sel,
        md_sel => md_sel,
        const_in => const_in,
        data_in => data_in,
        clk => clk,

        data_out => data_out,
        add_out => add_out,
        v => v,
        c => c,
        n => n,
        z => z
    );

    clk <= NOT clk AFTER period/2;

    md_sel <= '1'; -- select the data_in at the MD mux
    mb_sel <= '1'; -- select the const_in at the MB mux
    load_en <= '1'; -- allow writing to registers
    const_in <= x"20332400"; -- constant in is student id
    stim_proc : PROCESS
    BEGIN
        WAIT FOR 20ns;

        d_sel <= "00000";
        data_in <= x"20332400";
        WAIT FOR period;

        d_sel <= "00001";
        data_in <= x"203323ff";
        WAIT FOR period;

        d_sel <= "00010";
        data_in <= x"203323fe";
        WAIT FOR period;

        d_sel <= "00011";
        data_in <= x"203323fd";
        WAIT FOR period;

        d_sel <= "00100";
        data_in <= x"203323fc";
        WAIT FOR period;

        d_sel <= "00101";
        data_in <= x"203323fb";
        WAIT FOR period;

        d_sel <= "00110";
        data_in <= x"203323fa";
        WAIT FOR period;

        d_sel <= "00111";
        data_in <= x"203323f9";
        WAIT FOR period;

        d_sel <= "01000";
        data_in <= x"203323f8";
        WAIT FOR period;

        d_sel <= "01001";
        data_in <= x"203323f7";
        WAIT FOR period;

        d_sel <= "01010";
        data_in <= x"203323f6";
        WAIT FOR period;

        d_sel <= "01011";
        data_in <= x"203323f5";
        WAIT FOR period;

        d_sel <= "01100";
        data_in <= x"203323f4";
        WAIT FOR period;

        d_sel <= "01101";
        data_in <= x"203323f3";
        WAIT FOR period;

        d_sel <= "01110";
        data_in <= x"203323f2";
        WAIT FOR period;

        d_sel <= "01111";
        data_in <= x"203323f1";
        WAIT FOR period;

        d_sel <= "10000";
        data_in <= x"203323f0";
        WAIT FOR period;

        d_sel <= "10001";
        data_in <= x"203323ef";
        WAIT FOR period;

        d_sel <= "10010";
        data_in <= x"203323ee";
        WAIT FOR period;

        d_sel <= "10011";
        data_in <= x"203323ed";
        WAIT FOR period;

        d_sel <= "10100";
        data_in <= x"203323ec";
        WAIT FOR period;

        d_sel <= "10101";
        data_in <= x"203323eb";
        WAIT FOR period;

        d_sel <= "10110";
        data_in <= x"203323ea";
        WAIT FOR period;

        d_sel <= "10111";
        data_in <= x"203323e9";
        WAIT FOR period;

        d_sel <= "11000";
        data_in <= x"203323e8";
        WAIT FOR period;

        d_sel <= "11001";
        data_in <= x"203323e7";
        WAIT FOR period;

        d_sel <= "11010";
        data_in <= x"203323e6";
        WAIT FOR period;

        d_sel <= "11011";
        data_in <= x"203323e5";
        WAIT FOR period;

        d_sel <= "11100";
        data_in <= x"203323e4";
        WAIT FOR period;

        d_sel <= "11101";
        data_in <= x"203323e3";
        WAIT FOR period;

        d_sel <= "11110";
        data_in <= x"203323e2";
        WAIT FOR period;

        d_sel <= "11111";
        data_in <= x"203323e1";
        WAIT FOR period * 4;

        d_sel <= "00000";
        a_sel <= "00101";
        b_sel <= "01111";

        WAIT FOR period;
        f_sel <= "11000"; -- shift b left
        WAIT FOR period;
        f_sel <= "01000"; -- a and b
        WAIT FOR period;
        f_sel <= "00010"; -- a + b
        WAIT FOR period;
        f_sel <= "10000"; -- transfer b
        WAIT FOR period;
        f_sel <= "00011"; -- a + b + 1 (carry)
        WAIT FOR period;
        f_sel <= "10100"; -- shift b right
        WAIT FOR period;
        f_sel <= "00100"; -- a + not b
        WAIT FOR period;
        f_sel <= "01010"; -- a or b
        WAIT FOR period;
        f_sel <= "00101"; -- a + not b + 1 (a - b)
        WAIT FOR period;
        f_sel <= "01100"; -- a xor b
        WAIT;
    END PROCESS;
END Behavioural;