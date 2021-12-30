LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY extend IS
    PORT (
        data_in : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END extend;

ARCHITECTURE Behavioral OF extend IS
BEGIN
    data_out (9 DOWNTO 0) <= data_in;
    data_out (31 DOWNTO 10) <= "0000000000000000000000";

END Behavioral;