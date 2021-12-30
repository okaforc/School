LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux2_17bit IS
    PORT (
        In0 : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        In1 : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        s : IN STD_LOGIC;
        Z : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
END mux2_17bit;

ARCHITECTURE Behavioral OF mux2_17bit IS
BEGIN
    Z <= In0 AFTER 2 ns WHEN s = '0' ELSE
        In1 AFTER 2 ns WHEN s = '1'ELSE
        "00000000000000000";
END Behavioral;
