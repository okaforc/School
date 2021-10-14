LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY mux4 IS
	PORT (
		in0, in1, in2, in3 : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		s0, s1 : IN STD_LOGIC;
		z : OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
	);
END mux4;

ARCHITECTURE behavioural OF mux4 IS
BEGIN
	z <= in0 AFTER 5ns WHEN s0 = '0' AND s1 = '0' ELSE
		in1 AFTER 5ns WHEN s0 = '0' AND s1 = '1' ELSE
		in2 AFTER 5ns WHEN s0 = '1' AND s1 = '0' ELSE
		in3 AFTER 5ns WHEN s0 = '1' AND s1 = '1' ELSE
		"00000000" AFTER 5ns;

END behavioural; -- behavioural