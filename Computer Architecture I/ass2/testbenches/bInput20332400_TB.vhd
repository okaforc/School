LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY BInput_tb IS
END BInput_tb;

ARCHITECTURE Behavioral OF BInput_tb IS
    COMPONENT BInput
        PORT (
            bi_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            bi_s0, bi_s1 : STD_LOGIC;
            bi_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL bi_b : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL bi_s0, bi_s1 : STD_LOGIC := '0';

    -- output
    SIGNAL bi_g : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    -- delay
    constant delay : Time := 2ns;
BEGIN
    uut : BInput PORT MAP(bi_b => bi_b, bi_s0 => bi_s0, bi_s1 => bi_s1, bi_g => bi_g);
    bi_b <= x"20332400";
    stim_proc : PROCESS
    BEGIN
        WAIT FOR delay;
        bi_s1 <= '0';
        bi_s0 <= '0';
        WAIT FOR delay;
        bi_s1 <= '0';
        bi_s0 <= '1';
        WAIT FOR delay;
        bi_s1 <= '1';
        bi_s0 <= '0';
        WAIT FOR delay;
        bi_s1 <= '1';
        bi_s0 <= '1';
    END PROCESS;
END Behavioral;