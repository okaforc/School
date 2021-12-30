LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY zero_fill IS
    PORT (
        data_in : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END zero_fill;

ARCHITECTURE Behavioral OF zero_fill IS
BEGIN
    data_out (4 DOWNTO 0) <= data_in;
    data_out (31 DOWNTO 5) <= "000000000000000000000000000";

END Behavioral;