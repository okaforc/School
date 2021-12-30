LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY mux4_1bit IS
    PORT (
        mu4_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        mu4_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        mu4_Z : OUT STD_LOGIC
    );
END mux4_1bit;

ARCHITECTURE Behavioral OF mux4_1bit IS
BEGIN
    mu4_Z <= mu4_in(0) AFTER 2 ns WHEN mu4_s = "00" ELSE
        mu4_in(1) AFTER 2 ns WHEN mu4_s = "01" ELSE
        mu4_in(2) AFTER 2 ns WHEN mu4_s = "10" ELSE
        mu4_in(3) AFTER 2 ns WHEN mu4_s = "11" ELSE
        '0';
END Behavioral;