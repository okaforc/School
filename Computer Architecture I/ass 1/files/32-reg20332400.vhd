LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
ENTITY reg32 IS
    PORT (
        D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        load, Clk : IN STD_LOGIC;
        Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END reg32;

ARCHITECTURE Behavioral OF reg32 IS
BEGIN
    PROCESS (Clk)
    BEGIN
        IF (rising_edge(Clk)) THEN
            IF load = '1' THEN
                Q <= D AFTER 5 ns;
            END IF;
        END IF;
    END PROCESS;
END Behavioral;