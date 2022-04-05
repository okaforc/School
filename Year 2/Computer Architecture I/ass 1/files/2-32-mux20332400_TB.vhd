LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux232_tb IS
END mux232_tb;

ARCHITECTURE Behavioral OF mux232_tb IS
    COMPONENT mux2_32bit
        PORT (
            In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            s : IN STD_LOGIC;
            Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
    END COMPONENT;

    -- input
    SIGNAL In0 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL In1 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL s : STD_LOGIC := '0';

    -- output
    SIGNAL Z : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
BEGIN
    uut : mux2_32bit PORT MAP(In0=>In0, In1=>In1, s=>s, Z=>Z);
    In0 <= x"0000ffff";
    In1 <= x"ffff0000";
    stim_proc : PROCESS
    BEGIN
        WAIT FOR 5ns;
        -- outside
        s <= '0';
        WAIT FOR 5ns;
        s <= '1';
        -- wait;
    END PROCESS;
END Behavioral;