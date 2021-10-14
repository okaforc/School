--------------------------------------------------------------------------------
-- Company: Trinity College
-- Engineer: Dr. Michael Manzke
--
-- Create Date:   11:50:59 02/23/2012
-- Design Name:   
-- Module Name:   C:/Xilinx/12.4/ISE_DS/ISE/ISEexamples/MichaelsMultiplexer/multiplexer_tb.vhd
-- Project Name:  MichaelsMultiplexer
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: multiplexer
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;

ENTITY multiplexer_tb IS
END multiplexer_tb;

ARCHITECTURE behavior OF multiplexer_tb IS

	-- Component Declaration for the Unit Under Test (UUT)

	COMPONENT multiplexer
		PORT (
			s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
			in1 : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
			in2 : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
			in3 : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
			in4 : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
			z : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
		);
	END COMPONENT;
	--Inputs
	SIGNAL s : STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '0');
	SIGNAL in1 : STD_LOGIC_VECTOR(15 DOWNTO 0) := (OTHERS => '0');
	SIGNAL in2 : STD_LOGIC_VECTOR(15 DOWNTO 0) := (OTHERS => '0');
	SIGNAL in3 : STD_LOGIC_VECTOR(15 DOWNTO 0) := (OTHERS => '0');
	SIGNAL in4 : STD_LOGIC_VECTOR(15 DOWNTO 0) := (OTHERS => '0');

	--Outputs
	SIGNAL z : STD_LOGIC_VECTOR(15 DOWNTO 0);
	-- No clocks detected in port list. Replace <clock> below with 
	-- appropriate port name 

	--   constant Clk_period : time := 10 ns;

BEGIN

	-- Instantiate the Unit Under Test (UUT)
	uut : multiplexer PORT MAP(
		s => s,
		in1 => in1,
		in2 => in2,
		in3 => in3,
		in4 => in4,
		z => z
	);

	stim_proc : PROCESS
	BEGIN
		in1 <= "1010101010101010";
		in2 <= "1100110011001100";
		in3 <= "1111000011110000";
		in4 <= "1111111100000000";
		WAIT FOR 10 ns;
		s <= "00";

		WAIT FOR 10 ns;
		s <= "01";

		WAIT FOR 10 ns;
		s <= "10";

		WAIT FOR 10 ns;
		s <= "11";

		--     wait;
	END PROCESS;

END;