LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

entity PC_tb is
end PC_tb ;

architecture arch of PC_tb is
    component PC port (
        data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        PL, PI, reset, clk : IN STD_LOGIC;
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    -- input
    signal data_in : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');
    signal PL, PI, reset, clk : STD_LOGIC := '0';
    
    -- output
    signal data_out : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');

    -- delay
    CONSTANT delay : TIME := 2ns;

begin
    uut : PC port map (
        data_in => data_in,
        PL => PL,
        PI => PI,
        clk => clk,
        reset => reset,
        data_out => data_out
    );

    clk <= not clk after 1ns;

    stim_proc : process
    begin
        PL <= '1';
        data_in <= x"20332400";
        wait for delay;
        PI <= '1';
        PL <= '0';
        wait for delay;
        PI <= '0';
        PL <= '1';
        wait for delay;
        data_in <= x"f1415abc";
        wait for delay;
        PL <= '0';
        reset <= '1';
        wait for delay;
        reset <= '0';
        wait for delay;
        data_in <= x"1111ffff";
        wait;
    end process ; -- stim_proc


end architecture ; -- arch