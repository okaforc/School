LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY CAR_tb IS
END CAR_tb;

ARCHITECTURE Behavioural OF CAR_tb IS
    component CAR PORT (
        data_in : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        clk, load, reset : IN STD_LOGIC;
        ctrl : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
    end component;

    -- input
    signal data_in : STD_LOGIC_VECTOR(16 DOWNTO 0) := (others => '0');
    signal clk, load, reset : STD_LOGIC := '0';
    
    -- output
    signal ctrl : STD_LOGIC_VECTOR(16 DOWNTO 0) := (others => '0');

    -- delay
    CONSTANT delay : TIME := 4ns;
    
BEGIN
    uut: CAR port map (
        data_in => data_in,
        clk => clk,
        load => load,
        reset => reset,
        ctrl => ctrl
    );
    clk <= not clk after 1ns;
    stim_proc : process
    begin
        data_in <= "00001100001100001";
        wait for 5ns;
        reset <= '1';
        data_in <= "01101011010101100";
        wait for delay;
        reset <= '0';
        wait for delay;
        load <= '1';
        wait for delay;
        load <= '0';
        data_in <= "00000000000000111";
        wait;
    end process ;


END Behavioural; -- Behavioural