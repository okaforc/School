LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY function_unit_tb IS
END function_unit_tb;

ARCHITECTURE Behavioral OF function_unit_tb IS
    COMPONENT function_unit
        PORT (
            f_A, f_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            f_FS : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            v, c, n, z : OUT STD_LOGIC;
            F : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL f_A, f_B : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL f_FS : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    
    -- output
    SIGNAL F : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v, c, n, z : STD_LOGIC := '0';

    -- delay
    CONSTANT delay : TIME := 2ns;
BEGIN
    uut : function_unit PORT MAP(
        f_A => f_A,
        f_B => f_B,
        f_FS => f_FS,
        v => v,
        c => c,
        n => n,
        z => z,
        F => F
    );
    f_A <= x"20332400";
    f_B <= x"20332400";
    stim_proc : PROCESS
    BEGIN
        f_FS <= "00000"; -- transfer a (v1)
        WAIT FOR delay;
        f_FS <= "11000"; -- shift b left
        WAIT FOR delay; 
        f_FS <= "00001"; -- increment a (a + 1)
        WAIT FOR delay;
        f_FS <= "01000"; -- a and b
        WAIT FOR delay;
        f_FS <= "00010"; -- a + b
        WAIT FOR delay;
        f_FS <= "10000"; -- transfer b
        WAIT FOR delay;
        f_FS <= "00011"; -- a + b + 1 (carry)
        WAIT FOR delay;
        f_FS <= "10100"; -- shift b right
        WAIT FOR delay;
        f_FS <= "00100"; -- a + not b
        WAIT FOR delay;
        f_FS <= "01110"; -- not a
        WAIT FOR delay;
        f_FS <= "00111"; -- transfer a (v2)
        WAIT FOR delay;
        f_FS <= "01010"; -- a or b
        WAIT FOR delay;
        f_FS <= "00110"; -- decrement a (a - 1)
        WAIT FOR delay;
        f_FS <= "00101"; -- a + not b + 1 (a - b)
        WAIT FOR delay;
        f_FS <= "01100"; -- a xor b
        WAIT;
    END PROCESS;
END Behavioral;