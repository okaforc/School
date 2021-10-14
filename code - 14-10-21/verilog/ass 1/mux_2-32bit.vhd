LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY mux2_32bit IS
    PORT (
        In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        s : IN STD_LOGIC;
        Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END mux2_32bit;

ARCHITECTURE Behavioral OF mux2_32bit IS
BEGIN
    Z <= In0 AFTER 5 ns WHEN S = '0' ELSE
        In1 AFTER 5 ns WHEN S = '1'ELSE
        x"00000000" AFTER 5 ns;
END Behavioral;