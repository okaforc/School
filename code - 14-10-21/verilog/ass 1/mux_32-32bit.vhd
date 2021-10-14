LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY mux32 IS
    PORT (
        in0, in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11, in12,
        in13, in14, in15, in16, in17, in18, in19, in20, in21, in22, in23,
        in24, in25, in26, in27, in28, in29, in30, in31 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        s0, s1, s2, s3, s4 : IN STD_LOGIC;
        z : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
    );
END mux32;

ARCHITECTURE behavioural OF mux32 IS
BEGIN
    z <=
        in0 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '0' AND s3 = '0' AND s4 = '0' ELSE -- 00000
        in1 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '0' AND s3 = '0' AND s4 = '1' ELSE -- 00001
        in2 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '0' AND s3 = '1' AND s4 = '0' ELSE -- 00010
        in3 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '0' AND s3 = '1' AND s4 = '1' ELSE -- 00011
        in4 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '1' AND s3 = '0' AND s4 = '0' ELSE -- 00100
        in5 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '1' AND s3 = '0' AND s4 = '1' ELSE -- 00101
        in6 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '1' AND s3 = '1' AND s4 = '0' ELSE -- 00110
        in7 AFTER 5ns WHEN s0 = '0' AND s1 = '0' AND s2 = '1' AND s3 = '1' AND s4 = '1' ELSE -- 00111
        in8 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '0' AND s3 = '0' AND s4 = '0' ELSE -- 01000
        in9 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '0' AND s3 = '0' AND s4 = '1' ELSE -- 01001
        in10 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '0' AND s3 = '1' AND s4 = '0' ELSE -- 01010
        in11 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '0' AND s3 = '1' AND s4 = '1' ELSE -- 01011
        in12 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '1' AND s3 = '0' AND s4 = '0' ELSE -- 01100
        in13 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '1' AND s3 = '0' AND s4 = '1' ELSE -- 01101
        in14 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '1' AND s3 = '1' AND s4 = '0' ELSE -- 01110
        in15 AFTER 5ns WHEN s0 = '0' AND s1 = '1' AND s2 = '1' AND s3 = '1' AND s4 = '1' ELSE -- 01111
        in16 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '0' AND s3 = '0' AND s4 = '0' ELSE -- 10000
        in17 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '0' AND s3 = '0' AND s4 = '1' ELSE -- 10001
        in18 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '0' AND s3 = '1' AND s4 = '0' ELSE -- 10010
        in19 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '0' AND s3 = '1' AND s4 = '1' ELSE -- 10011
        in20 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '1' AND s3 = '0' AND s4 = '0' ELSE -- 10100
        in21 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '1' AND s3 = '0' AND s4 = '1' ELSE -- 10101
        in22 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '1' AND s3 = '1' AND s4 = '0' ELSE -- 10110
        in23 AFTER 5ns WHEN s0 = '1' AND s1 = '0' AND s2 = '1' AND s3 = '1' AND s4 = '1' ELSE -- 10111
        in24 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '0' AND s3 = '0' AND s4 = '0' ELSE -- 11000
        in25 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '0' AND s3 = '0' AND s4 = '1' ELSE -- 11001
        in26 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '0' AND s3 = '1' AND s4 = '0' ELSE -- 11010
        in27 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '0' AND s3 = '1' AND s4 = '1' ELSE -- 11011
        in28 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '1' AND s3 = '0' AND s4 = '0' ELSE -- 11100
        in29 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '1' AND s3 = '0' AND s4 = '1' ELSE -- 11101
        in30 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '1' AND s3 = '1' AND s4 = '0' ELSE -- 11110
        in31 AFTER 5ns WHEN s0 = '1' AND s1 = '1' AND s2 = '1' AND s3 = '1' AND s4 = '1' ELSE -- 11111
        "00000000000000000000000000000000" AFTER 5ns;

END behavioural; -- behavioural