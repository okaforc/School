LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY zeroDetect_tb IS
END zeroDetect_tb;

ARCHITECTURE Behavioral OF zeroDetect_tb IS
    COMPONENT zeroDetect
        PORT (
            z_a : in std_logic_vector(31 downto 0);
            z_out : out std_logic
        );
    END COMPONENT;

    -- input
    SIGNAL z_a : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    
    -- output
    SIGNAL z_out : STD_LOGIC := '0';

    -- delay
    CONSTANT delay : TIME := 2ns;
BEGIN
    uut : zeroDetect PORT MAP(
        z_a => z_a,
        z_out => z_out
    );
    stim_proc : PROCESS
    BEGIN
        z_a <= x"12345453";
        WAIT FOR delay;
        z_a <= x"00000000";
        WAIT FOR delay;
        z_a <= x"ffffffff";
        WAIT FOR delay;
        z_a <= x"00000000";
        WAIT;
    END PROCESS;
END Behavioral;