LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

entity IR_tb is
end IR_tb ;

architecture Behavioural of IR_tb is
    component InstReg PORT (
        IL, clk : IN STD_LOGIC;
        IR : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        opcode : OUT STD_LOGIC_VECTOR(16 DOWNTO 0);
        SA, SB, DR : OUT STD_LOGIC_VECTOR(4 DOWNTO 0)
    );
    end component;

    -- input
    signal IR : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');
    signal clk, IL : STD_LOGIC := '0';
    
    -- output
    signal opcode : STD_LOGIC_VECTOR(16 DOWNTO 0) := (others => '0');
    signal SA, SB, DR : std_logic_vector(4 downto 0) := (others => '0');

    -- delay
    CONSTANT delay : TIME := 8ns;
begin
    uut : InstReg port map (
        IL => IL,
        clk => clk,
        IR => IR,
        opcode => opcode,
        SA => SA,
        SB => SB,
        DR => DR
    );

    clk <= not clk after 1ns;

    stim_proc : process
    begin
        IL <= '0';
        wait for delay;
        IR <= x"55555555";
        wait for delay;
        IL <= '1';
        wait for delay;
        IR <= x"20332400";
        wait for delay;
        IL <= '0';
        IR <= x"12345678";
        wait;
    end process ; -- stim_proc


end Behavioural ; -- Behavioural