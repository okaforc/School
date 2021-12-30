LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY register_tb IS
END register_tb;

ARCHITECTURE Behavioral OF register_tb IS
    COMPONENT reg32
        PORT (
            D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            load, clk : IN STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
    END COMPONENT;

    SIGNAL load : STD_LOGIC := '0';
    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL D : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL Q : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    CONSTANT period : TIME := 5ns;
BEGIN
    uut : reg32 PORT MAP(
        load => load,
        clk => clk,
        D => D,
        Q => Q
    );
    clk <= NOT clk AFTER period/2;
    stim_proc : PROCESS
    BEGIN
        WAIT UNTIL clk'event AND clk = '1';
        load <= '0';
        D <= x"12345678";
        WAIT UNTIL clk'event AND clk = '1';
        load <= '1';

        WAIT UNTIL clk'event AND clk = '1';
        load <= '0';
        D <= x"00110011";
        WAIT UNTIL clk'event AND clk = '1';
        load <= '1';

        WAIT UNTIL clk'event AND clk = '1';
        load <= '0';
        D <= x"87654321";
        WAIT UNTIL clk'event AND clk = '1';
        load <= '1';

        WAIT UNTIL clk'event AND clk = '1';
        load <= '0';
        D <= x"abcdef00";
        WAIT UNTIL clk'event AND clk = '1';
        load <= '1';

        WAIT UNTIL clk'event AND clk = '1';
        load <= '0';
        D <= x"20332400";
        WAIT UNTIL clk'event AND clk = '1';
        load <= '1';

        WAIT UNTIL clk'event AND clk = '1';
        load <= '0';
        D <= x"05050505";
        WAIT UNTIL clk'event AND clk = '1';
        load <= '1';
    END PROCESS;
END Behavioral;