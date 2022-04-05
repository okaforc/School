LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY arCircuit_tb IS
END arCircuit_tb;

ARCHITECTURE Behavioral OF arCircuit_tb IS
    COMPONENT arCircuit
        PORT (
            ar_A, ar_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            ar_cin, ar_s0, ar_s1 : IN STD_LOGIC;
            ar_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
            ar_cout, ar_vout : OUT STD_LOGIC
        );
    END COMPONENT;

    -- input
    SIGNAL ar_A, ar_B : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL ar_cin, ar_s0, ar_s1 : STD_LOGIC := '0';

    -- output
    SIGNAL ar_g : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL ar_cout, ar_vout : STD_LOGIC := '0';
    -- delay
    CONSTANT delay : TIME := 2ns;

BEGIN
    uut : arCircuit PORT MAP(
        ar_A => ar_A,
        ar_B => ar_B,
        ar_cin => ar_cin,
        ar_s0 => ar_s0,
        ar_s1 => ar_s1,
        ar_g => ar_g,
        ar_cout => ar_cout,
        ar_vout => ar_vout
    );
    ar_A <= x"20332400";
    ar_B <= x"12345678";
    stim_proc : PROCESS
    BEGIN
        WAIT FOR delay;
        ar_s1 <= '0';
        ar_s0 <= '0';
        ar_cin <= '0';
        WAIT FOR delay;
        ar_s1 <= '0';
        ar_s0 <= '0';
        ar_cin <= '1';
        WAIT FOR delay;
        ar_s1 <= '0';
        ar_s0 <= '1';
        ar_cin <= '0';
        WAIT FOR delay;
        ar_s1 <= '0';
        ar_s0 <= '1';
        ar_cin <= '1';
        WAIT FOR delay;
        ar_s1 <= '1';
        ar_s0 <= '0';
        ar_cin <= '0';
        WAIT FOR delay;
        ar_s1 <= '1';
        ar_s0 <= '0';
        ar_cin <= '1';
        WAIT FOR delay;
        ar_s1 <= '1';
        ar_s0 <= '1';
        ar_cin <= '0';
        WAIT FOR delay;
        ar_s1 <= '1';
        ar_s0 <= '1';
        ar_cin <= '1';
    END PROCESS;
END Behavioral;