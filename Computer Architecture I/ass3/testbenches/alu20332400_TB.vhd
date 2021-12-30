LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY alu_tb IS
END alu_tb;

ARCHITECTURE Behavioral OF alu_tb IS
    COMPONENT alu
        PORT (
            al_A, al_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            al_gsel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            al_v, al_c : OUT STD_LOGIC;
            al_G : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL al_A, al_B : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL al_gsel : STD_LOGIC_VECTOR (3 DOWNTO 0) := (OTHERS => '0');

    -- output
    SIGNAL al_v, al_c : STD_LOGIC := '0';
    SIGNAL al_G : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');

    -- delay
    CONSTANT delay : TIME := 4ns;
BEGIN
    uut : alu PORT MAP(
        al_A => al_A,
        al_B => al_B,
        al_gsel => al_gsel,
        al_v => al_v,
        al_c => al_c,
        al_G => al_G
    );
    al_A <= x"20332400";
    al_B <= x"11111111";
    stim_proc : PROCESS
    BEGIN
        WAIT FOR delay/2;
        al_gsel <= "0000";
        WAIT FOR delay;
        al_gsel <= "0001";
        WAIT FOR delay;
        al_gsel <= "0010";
        WAIT FOR delay;
        al_gsel <= "0011";
        WAIT FOR delay;
        al_gsel <= "0100";
        WAIT FOR delay;
        al_gsel <= "0101";
        WAIT FOR delay;
        al_gsel <= "0110";
        WAIT FOR delay;
        al_gsel <= "0111";
        WAIT FOR delay;
        al_gsel <= "1000";
        WAIT FOR delay;
        al_gsel <= "1001";
        WAIT FOR delay;
        al_gsel <= "1010";
        WAIT FOR delay;
        al_gsel <= "1011";
        WAIT FOR delay;
        al_gsel <= "1100";
        WAIT FOR delay;
        al_gsel <= "1101";
        WAIT FOR delay;
        al_gsel <= "1110";
        WAIT FOR delay;
        al_gsel <= "1111";
        WAIT;
    END PROCESS;
END Behavioral;