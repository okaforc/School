LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mult232_tb IS
END mult232_tb;

ARCHITECTURE Behavioral OF mult232_tb IS
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
    stim_proc : PROCESS
    BEGIN
        In0 <= x"12345678";
        In1 <= x"87654321";
        WAIT FOR 5ns;
        s <= '1';
        WAIT FOR 5ns;
        s <= '0';
        WAIT FOR 5ns;
        In0 <= x"a0a0a0a0";
        In1 <= x"0f0f0f0f";
        s <= '1';
        WAIT FOR 10ns;
        s <= '0';
        WAIT FOR 10ns;
    END PROCESS;
END Behavioral;