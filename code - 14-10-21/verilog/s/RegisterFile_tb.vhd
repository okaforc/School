library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity RegisterFile_tb is
end RegisterFile_tb;

architecture Behavioural of RegisterFile_tb is
    component RegisterFile32_32bit
    Port ( TA_SA : in STD_LOGIC_VECTOR (5 downto 0);
           TB_SB : in STD_LOGIC_VECTOR (5 downto 0);
           TD_DR : in STD_LOGIC_VECTOR (5 downto 0);
           RW : in STD_LOGIC;
           clk : in STD_LOGIC;
           D : in STD_LOGIC_VECTOR (31 downto 0);
           A : out STD_LOGIC_VECTOR (31 downto 0);
           B : out STD_LOGIC_VECTOR (31 downto 0));
    end component;
    
    signal TA_SA : STD_LOGIC_VECTOR (5 downto 0) := (others => '0');
    signal TB_SB : STD_LOGIC_VECTOR (5 downto 0) := (others => '0');
    signal TD_DR : STD_LOGIC_VECTOR (5 downto 0) := (others => '0');
    signal clk : STD_LOGIC := '0';
    signal RW : STD_LOGIC := '0';
    signal D : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
    signal A : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
    signal B : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
    
begin
    uut: RegisterFile32_32bit port map (TA_SA, TB_SB, TD_DR, RW, clk, D, A, B);
    clk <= not clk after 5ns;
    stim_proc: process
    begin
        
        TA_SA <= "000000";
        TB_SB <= "000000";
        TD_DR <= "000000";
        D <= x"F0F0F0F0";
        RW <= '1';
        wait for 10ns;
        TD_DR <= "000001";
        D <= x"0000FFFF";
        wait for 10ns;
        TD_DR <= "000010";
        D <= x"12345678";
        wait for 10ns;
        RW <= '0';
        wait for 10ns;
        TA_SA <= "000010";
        wait for 10ns;
        TB_SB <= "000001";
        wait for 10ns;
        TB_SB <= "100010";
        wait for 10ns;
        D <= x"98765432";
        TD_DR <= "100001";
        RW <= '1';
        wait for 10ns;
        RW <= '0';
        wait for 10ns;
        TA_SA <= "000000";
        wait for 10ns;
        TA_SA <= "000001";
        wait for 10ns;
        TA_SA <= "000010";
        wait for 10ns;
        TA_SA <= "100000";
        wait for 10ns;
        
    end process;
end Behavioural;