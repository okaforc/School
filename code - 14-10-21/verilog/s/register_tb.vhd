----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.10.2020 19:42:30
-- Design Name: 
-- Module Name: register_tb - Behavioral
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

entity register_tb is
end register_tb;

architecture Behavioral of register_tb is
    component register32bit
    port (
           load : in STD_LOGIC;
           clk : in STD_LOGIC;
           A : in STD_LOGIC_VECTOR (31 downto 0);
           Q : out STD_LOGIC_VECTOR (31 downto 0));
    end component;
    
    signal load : std_logic := '0';
    signal clk : std_logic := '0';
    signal A : std_logic_vector(31 downto 0) := (others => '0');
    signal Q : std_logic_vector(31 downto 0) := (others => '0');
begin
    uut: register32bit port map (load, clk, A, Q);
    stim_proc: process
    begin
        A <= x"0127065E";
        wait for 10ns;
        clk <= '1';
        wait for 10ns;
        clk <= '0';
        load <= '1';
        wait for 10ns;
        clk <= '1';
        wait for 10ns;
        clk <= '0';
        A <= x"00000000";
        load <= '0';
        wait for 10ns;
        clk <= '1';
        wait for 10ns;
        clk <= '0';
        wait for 10ns;
    end process;
    
    
end Behavioral;
