library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Microprocessor_tb is
end Microprocessor_tb; 

architecture Behavioural of Microprocessor_tb is
    component Microprocessor port (
        clk, resetCAR, resetPC : in std_logic
    );
    end component;
    
    signal clk, resetCAR, resetPC : std_logic := '0';
    constant delay : time := 10 ns;

begin
    uut : Microprocessor port map (
        clk => clk,
        resetPC => resetPC,
        resetCAR => resetCAR
    );

    clk <= not clk after 1 ns;

    stim_proc : process
    begin
        resetCAR <= '1';
        resetPC <= '1';
        wait for delay;
        resetCAR <= '0';
        resetPC <= '0';
        wait;
    end process; -- stim_proc
end Behavioural;