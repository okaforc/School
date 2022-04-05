LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY logCircuit_tb IS
END logCircuit_tb;

ARCHITECTURE Behavioral OF logCircuit_tb IS
    COMPONENT logCircuit
        PORT (
            lc_a, lc_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            lc_s0, lc_s1 : STD_LOGIC;
            lc_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL lc_a, lc_b : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL lc_s0, lc_s1 : STD_LOGIC := '0';

    -- output
    SIGNAL lc_g : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    -- delay
    CONSTANT delay : TIME := 4ns;
BEGIN
    uut : logCircuit PORT MAP(lc_a => lc_a, lc_b => lc_b, lc_s0 => lc_s0, lc_s1 => lc_s1, lc_g => lc_g);
    lc_a <= x"20332400";
    lc_b <= x"55555555";
    stim_proc : PROCESS
    BEGIN
        WAIT FOR 2ns;
        lc_s0 <= '0';
        lc_s1 <= '0';
        WAIT FOR delay;
        lc_s0 <= '1';
        lc_s1 <= '0';
        WAIT FOR delay;
        lc_s0 <= '0';
        lc_s1 <= '1';
        WAIT FOR delay;
        lc_s0 <= '1';
        lc_s1 <= '1';
        WAIT;
    END PROCESS;
END Behavioral;