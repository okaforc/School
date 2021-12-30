LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

entity zero_fill_tb is
end zero_fill_tb ;

architecture Behavioural of zero_fill_tb is
    component zero_fill port (
        data_in : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    -- input
    signal data_in : STD_LOGIC_VECTOR(4 DOWNTO 0) := (others => '0');
    
    -- output
    signal data_out : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');

    -- delay
    CONSTANT delay : TIME := 2ns;

begin
    uut : zero_fill port map (
        data_in => data_in,
        data_out => data_out
    );

    stim_proc : process
    begin
        data_in <= "00110";
        wait for delay;
        data_in <= "10010";
        wait for delay;
        data_in <= "11101";
        wait;
    end process ; -- stim_proc

end Behavioural ;