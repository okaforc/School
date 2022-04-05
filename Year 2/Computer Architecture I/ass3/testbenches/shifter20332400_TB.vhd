LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY shifter_tb IS
END shifter_tb;

ARCHITECTURE Behavioral OF shifter_tb IS
    COMPONENT shifter
        PORT (
            sh_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            sh_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
            sh_il, sh_ir : IN STD_LOGIC;
            sh_h : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL sh_B : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL sh_s : STD_LOGIC_VECTOR (1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL sh_il, sh_ir : STD_LOGIC := '0';
    
    -- output
    SIGNAL sh_h : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');

    -- delay
    CONSTANT delay : TIME := 4ns;
BEGIN
    uut : shifter PORT MAP(
        sh_b => sh_b,
        sh_s => sh_s,
        sh_il => sh_il,
        sh_ir => sh_ir,
        sh_h => sh_h
    );
    sh_b <= x"20332400";
    sh_il <= '0';
    sh_ir <= '0';
    stim_proc : PROCESS
    BEGIN
        sh_s <= "00";
        WAIT FOR delay;
        sh_s <= "01";
        WAIT FOR delay;
        sh_s <= "00";
        WAIT FOR delay;
        sh_s <= "10";
        WAIT FOR delay;
        WAIT;
    END PROCESS;
END Behavioral;