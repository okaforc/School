LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY BInput IS
    PORT (
        bi_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        bi_s0, bi_s1 : STD_LOGIC;
        bi_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END BInput;

ARCHITECTURE dataflow_2 OF BInput IS

BEGIN
    bi_g <= x"00000000" AFTER 2 ns WHEN bi_s1 = '0' AND bi_s0 = '0' ELSE
        bi_b AFTER 2 ns WHEN bi_s1 = '0' AND bi_s0 = '1' ELSE
        NOT (bi_b) AFTER 2 ns WHEN bi_s1 = '1' AND bi_s0 = '0' ELSE
        x"ffffffff" AFTER 2 ns WHEN bi_s1 = '1' AND bi_s0 = '1';

END dataflow_2; -- dataflow_2
-- 11010
-- 00101