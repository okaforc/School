LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY register_file_tb IS
END register_file_tb;

ARCHITECTURE Behavioural OF register_file_tb IS
    COMPONENT register_file
        PORT (
            -- Inputs
            a_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- A data mux
            b_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- B data mux
            d_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- decoder 5 to 32
            load_wr : IN STD_LOGIC; -- signal
            data : IN STD_LOGIC_VECTOR (31 DOWNTO 0); -- input data
            clk : IN STD_LOGIC; -- clock for register

            -- Outputs
            data_a : OUT STD_LOGIC_VECTOR(31 DOWNTO 0); -- A data
            data_b : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) -- B data
        );
    END COMPONENT;

    SIGNAL a_add : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL b_add : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL d_add : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL load_wr : STD_LOGIC := '0';
    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL data : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL data_a : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL data_b : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');

    CONSTANT period : TIME := 4 ns;

BEGIN
    uut : register_file PORT MAP(
        a_add => a_add,
        b_add => b_add,
        d_add => d_add,
        clk => clk,
        load_wr => load_wr,
        data => data,
        data_a => data_a,
        data_b => data_b
    );

    clk <= NOT clk AFTER period/2; -- invert clock every 1ns
    load_wr <= '1';
    stim_proc : PROCESS
    BEGIN
        d_add <= "00000";
        data <= x"20332400";
        WAIT FOR period;

        d_add <= "00001";
        data <= x"203323ff";
        WAIT FOR period;

        d_add <= "00010";
        data <= x"203323fe";
        WAIT FOR period;

        d_add <= "00011";
        data <= x"203323fd";
        WAIT FOR period;

        d_add <= "00100";
        data <= x"203323fc";
        WAIT FOR period;

        d_add <= "00101";
        data <= x"203323fb";
        WAIT FOR period;

        d_add <= "00110";
        data <= x"203323fa";
        WAIT FOR period;

        d_add <= "00111";
        data <= x"203323f9";
        WAIT FOR period;

        d_add <= "01000";
        data <= x"203323f8";
        WAIT FOR period;

        d_add <= "01001";
        data <= x"203323f7";
        WAIT FOR period;

        d_add <= "01010";
        data <= x"203323f6";
        WAIT FOR period;

        d_add <= "01011";
        data <= x"203323f5";
        WAIT FOR period;

        d_add <= "01100";
        data <= x"203323f4";
        WAIT FOR period;

        d_add <= "01101";
        data <= x"203323f3";
        WAIT FOR period;

        d_add <= "01110";
        data <= x"203323f2";
        WAIT FOR period;

        d_add <= "01111";
        data <= x"203323f1";
        WAIT FOR period;

        d_add <= "10000";
        data <= x"203323f0";
        WAIT FOR period;

        d_add <= "10001";
        data <= x"203323ef";
        WAIT FOR period;

        d_add <= "10010";
        data <= x"203323ee";
        WAIT FOR period;

        d_add <= "10011";
        data <= x"203323ed";
        WAIT FOR period;

        d_add <= "10100";
        data <= x"203323ec";
        WAIT FOR period;

        d_add <= "10101";
        data <= x"203323eb";
        WAIT FOR period;

        d_add <= "10110";
        data <= x"203323ea";
        WAIT FOR period;

        d_add <= "10111";
        data <= x"203323e9";
        WAIT FOR period;

        d_add <= "11000";
        data <= x"203323e8";
        WAIT FOR period;

        d_add <= "11001";
        data <= x"203323e7";
        WAIT FOR period;

        d_add <= "11010";
        data <= x"203323e6";
        WAIT FOR period;

        d_add <= "11011";
        data <= x"203323e5";
        WAIT FOR period;

        d_add <= "11100";
        data <= x"203323e4";
        WAIT FOR period;

        d_add <= "11101";
        data <= x"203323e3";
        WAIT FOR period;

        d_add <= "11110";
        data <= x"203323e2";
        WAIT FOR period;

        d_add <= "11111";
        data <= x"203323e1";
        WAIT FOR period*4;

        a_add <= "00000";
        b_add <= "11010";
        wait for period;
        a_add <= "11111";
        b_add <= "00000";
        wait for period;
        a_add <= "10100";
        b_add <= "10001";
        wait for period;
        a_add <= "11101";
        b_add <= "00010";
        wait for period;
        a_add <= "10101";
        b_add <= "01010";
        wait for period;
        a_add <= "11111";
        b_add <= "11100";
        wait for period;
        a_add <= "00001";
        b_add <= "10000";
        wait for period;
        a_add <= "00110";
        b_add <= "11001";
        wait;
        
    END PROCESS;
END Behavioural;