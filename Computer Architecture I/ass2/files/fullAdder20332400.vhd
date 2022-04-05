LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY full_adder IS
    PORT (
        x, y, z : IN STD_LOGIC;
        s, c : OUT STD_LOGIC);
END full_adder;

ARCHITECTURE dataflow_2 OF full_adder IS
    SIGNAL s1, s2, s3 : STD_LOGIC := '0';
BEGIN
    s1 <= x XOR y AFTER 2 ns;
    s2 <= z AND s1 AFTER 2 ns;
    s3 <= x AND y AFTER 2 ns;
    s <= s1 XOR z AFTER 2 ns;
    c <= s2 OR s3 AFTER 2 ns;
END dataflow_2;