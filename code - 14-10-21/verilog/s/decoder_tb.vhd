----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.10.2020 15:38:30
-- Design Name: 
-- Module Name: decoder_tb - Behavioral
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

ENTITY decoder_tb IS
END decoder_tb;

architecture Behavioural of decoder_tb is
    COMPONENT decoder5to32
    PORT (
        A : IN std_logic_vector(4 downto 0);
        Q : OUT std_logic_vector(31 downto 0)
        );
    END COMPONENT;
    
    signal A : std_logic_vector(4 downto 0) := (others => '0');
    signal Q : std_logic_vector(31 downto 0);
    
BEGIN
    
    uut: decoder5to32 PORT MAP(A, Q);
    stim_proc: process
    BEGIN
        A <= "00000";
        wait for 10 ns;
        A <= "00010";
        wait for 10 ns;
        A <= "00111";
        wait for 10 ns;
        A <= "10101";
        wait for 10 ns;
        A <= "11111";
        wait for 10 ns;
    END process;
    
END;
