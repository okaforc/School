LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

entity extend_tb is
end extend_tb ;

architecture Behavioural of extend_tb is
    component extend port (
        data_in : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    -- input
    signal data_in : STD_LOGIC_VECTOR(9 DOWNTO 0) := (others => '0');
    
    -- output
    signal data_out : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');

    -- delay
    CONSTANT delay : TIME := 2ns;

begin
    uut : extend port map (
        data_in => data_in,
        data_out => data_out
    );

    stim_proc : process
    begin
        data_in <= "0110100110";
        wait for delay;
        data_in <= "1011010010";
        wait for delay;
        data_in <= "1111111101";
        wait;
    end process ; -- stim_proc

end Behavioural ;