LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux8_1bit_tb IS
END mux8_1bit_tb;

ARCHITECTURE Behavioral OF mux8_1bit_tb IS
    COMPONENT mux8_1bit
        PORT (
            MS : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
            bits_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
            Z : OUT STD_LOGIC
        );
    END COMPONENT;

    -- input
    SIGNAL MS : STD_LOGIC_VECTOR (2 DOWNTO 0) := (OTHERS => '0');
    SIGNAL bits_in : STD_LOGIC_VECTOR (7 DOWNTO 0);

    -- output
    SIGNAL Z : STD_LOGIC;
BEGIN
    uut : mux8_1bit PORT MAP(
        MS => MS,
        bits_in => bits_in,
        Z => Z
    );
    bits_in(0) <= '1';
    bits_in(1) <= '0';
    bits_in(2) <= '1';
    bits_in(3) <= '0';
    bits_in(4) <= '0';
    bits_in(5) <= '1';
    bits_in(6) <= '1';
    bits_in(7) <= '0';
    stim_proc : PROCESS
    BEGIN
        WAIT FOR 10ns;
        MS <= "000";
        WAIT FOR 10ns;
        MS <= "001";
        WAIT FOR 10ns;
        MS <= "010";
        WAIT FOR 10ns;
        MS <= "011";
        WAIT FOR 10ns;
        MS <= "100";
        WAIT FOR 10ns;
        MS <= "101";
        WAIT FOR 10ns;
        MS <= "110";
        WAIT FOR 10ns;
        MS <= "111";
        WAIT;
    END PROCESS;
END Behavioral;