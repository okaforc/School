LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY fullAdder_tb IS
END fullAdder_tb;

ARCHITECTURE Behavioral OF fullAdder_tb IS
    COMPONENT full_adder
        PORT (
            x, y, z : IN STD_LOGIC;
            s, c : OUT STD_LOGIC
        );
    END COMPONENT;

    -- input
    SIGNAL x, y, z : STD_LOGIC := '0';

    -- -- output
    SIGNAL s, c : STD_LOGIC;

    -- delay
    constant delay : Time := 2ns;

BEGIN
    uut : full_adder PORT MAP(x => x, y => y, z => z, s => s, c => c);
    stim_proc : PROCESS
    BEGIN
        x <= '0';
        y <= '0';
        z <= '0';
        WAIT FOR delay;
        x <= '0';
        y <= '0';
        z <= '1';
        WAIT FOR delay;
        x <= '0';
        y <= '1';
        z <= '0';
        WAIT FOR delay;
        x <= '0';
        y <= '1';
        z <= '1';
        WAIT FOR delay;
        x <= '1';
        y <= '0';
        z <= '0';
        WAIT FOR delay;
        x <= '1';
        y <= '0';
        z <= '1';
        WAIT FOR delay;
        x <= '1';
        y <= '1';
        z <= '0';
        WAIT FOR delay;
        x <= '1';
        y <= '1';
        z <= '1';
        WAIT;
    END PROCESS;
END Behavioral;