----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.10.2020 20:09:51
-- Design Name: 
-- Module Name: mult232_tb - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity mult232_tb is
end mult232_tb;

architecture Behavioral of mult232_tb is
    component mult2_32bit
    port (S : in STD_LOGIC;
          A0 : in STD_LOGIC_VECTOR (31 downto 0);
          A1 : in STD_LOGIC_VECTOR (31 downto 0);
          Q : out STD_LOGIC_VECTOR (31 downto 0));
    end component;
    
    signal S : std_logic := '0';
    signal A0 : std_logic_vector (31 downto 0) := (others => '0');
    signal A1 : std_logic_vector (31 downto 0) := (others => '0');
    signal Q : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
begin
    uut: mult2_32bit port map (S, A0, A1, Q);
    stim_proc: process
    begin
        A0 <= x"FFFF0000";
        A1 <= x"0000FFFF";
        wait for 10ns;
        S <= '1';
        wait for 10ns;
        S <= '0';
        wait for 10ns;
        A0 <= x"70707070";
        A1 <= x"07070707";
        S <= '1';
        wait for 10ns;
        S <= '0';
        wait for 10ns;
    end process;
end Behavioral;
