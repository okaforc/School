LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux8_1bit IS
    PORT (
        MS : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
        bits_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        Z : OUT STD_LOGIC
    );
END mux8_1bit;

ARCHITECTURE Behavioral OF mux8_1bit IS
BEGIN
    Z <= bits_in(0) AFTER 2 ns WHEN MS = "000" ELSE
        bits_in(1) AFTER 2 ns WHEN MS = "001" ELSE
        bits_in(2) AFTER 2 ns WHEN MS = "010" ELSE
        bits_in(3) AFTER 2 ns WHEN MS = "011" ELSE
        bits_in(4) AFTER 2 ns WHEN MS = "100" ELSE
        bits_in(5) AFTER 2 ns WHEN MS = "101" ELSE
        bits_in(6) AFTER 2 ns WHEN MS = "110" ELSE
        bits_in(7) AFTER 2 ns WHEN MS = "111" ELSE
        'X';
END Behavioral;