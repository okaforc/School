LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY register_tb IS
END register_tb;

ARCHITECTURE Behavioral OF register_tb IS
    COMPONENT register32bit
        PORT (
            D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            load, Clk : IN STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
    END COMPONENT;

    SIGNAL load : STD_LOGIC := '0';
    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL A : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL Q : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
BEGIN
    uut : register32bit PORT MAP(
        load => load;
        clk => clk,
        A => A,
        Q => Q
    );

    stim_proc : PROCESS
    BEGIN
        A <= x"12345678";
        WAIT FOR 5ns;
        clk <= '1';
        WAIT FOR 5ns;
        clk <= '0';
        load <= '1';
        WAIT FOR 5ns;
        clk <= '1';
        WAIT FOR 5ns;
        clk <= '0';
        load <= '0';
        WAIT FOR 5ns;
        clk <= '1';

        A <= x"11111111";
        WAIT FOR 5ns;
        clk <= '1';
        WAIT FOR 5ns;
        clk <= '0';
        load <= '1';
        WAIT FOR 5ns;
        clk <= '1';
        WAIT FOR 5ns;
        clk <= '0';
        load <= '0';
        WAIT FOR 5ns;
        clk <= '1';

        A <= x"00000000";
        WAIT FOR 5ns;
        clk <= '1';
        WAIT FOR 5ns;
        clk <= '0';
        load <= '1';
        WAIT FOR 5ns;
        clk <= '1';
        WAIT FOR 5ns;
        clk <= '0';
        load <= '0';
        WAIT FOR 5ns;
        clk <= '1';
    END PROCESS;
END Behavioral;