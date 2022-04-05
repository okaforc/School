LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY logSlice_tb IS
END logSlice_tb;

ARCHITECTURE Behavioral OF logSlice_tb IS
    COMPONENT logSlice
        PORT (
            x, y : IN STD_LOGIC;
            s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
            g : OUT STD_LOGIC
            );
            END COMPONENT;

    -- input
    SIGNAL x, y : STD_LOGIC := '0';
    SIGNAL s : STD_LOGIC_VECTOR (1 DOWNTO 0) := (OTHERS => '0');

    -- output
    SIGNAL g : STD_LOGIC := '0';
    -- delay
    CONSTANT delay : TIME := 4ns;
BEGIN
    uut : logSlice PORT MAP(x => x, y => y, s => s, g => g);
    x <= '0';
    y <= '1';
    stim_proc : PROCESS
    BEGIN
        s <= "00";
        WAIT FOR delay;
        s <= "01";
        WAIT FOR delay;
        s <= "10";
        WAIT FOR delay;
        s <= "11";
        WAIT;
    END PROCESS;
END Behavioral;