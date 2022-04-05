LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY rippleAdder_tb IS
END rippleAdder_tb;

architecture Behavioural of rippleAdder_tb is
    component rippleAdder port(
        ra_a, ra_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        ra_c : IN STD_LOGIC;
        ra_s : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        ra_cout, ra_vout : OUT STD_LOGIC
    );
    end component;
    signal ra_a, ra_b, ra_s : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    signal ra_c, ra_cout, ra_vout : std_logic := '0';
    -- delay
    constant delay : Time := 2ns;

begin
    uut : rippleAdder port map(ra_a => ra_a, ra_b => ra_b, ra_c => ra_c, ra_s => ra_s, ra_cout => ra_cout, ra_vout => ra_vout);
    stim_proc : process
    begin
        ra_a <= x"20332400";
        ra_b <= x"00000064";
        ra_c <= '0';
        wait for delay;
        ra_a <= x"20332400";
        ra_b <= x"ffffff9c";
        ra_c <= '0';
        wait for delay;
        ra_a <= x"DFCCDC00";
        ra_b <= x"00000064";
        ra_c <= '0';
        wait for delay;
        ra_a <= x"DFCCDC00";
        ra_b <= x"ffffff9c";
        ra_c <= '0';

        wait for 20ns;
        
        -- worst case propogation delay
        ra_a <= x"ffffffff";
        ra_b <= x"00000001";
        ra_c <= '0';
        wait;
    end process;
end Behavioural ; -- Behavioural