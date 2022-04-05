LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux232_tb IS
END mux232_tb;

ARCHITECTURE Behavioral OF mux232_tb IS
    COMPONENT mux2_32bit
        PORT (
        mu_In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_s : IN STD_LOGIC;
        mu_Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL mu_In0 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL mu_In1 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL mu_s : STD_LOGIC := '0';

    -- output
    SIGNAL mu_Z : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
BEGIN
    uut : mux2_32bit PORT MAP(mu_In0 => mu_In0, mu_In1 => mu_In1, mu_s => mu_s, mu_Z => mu_Z);
    mu_In0 <= x"0000ffff";
    mu_In1 <= x"ffff0000";
    stim_proc : PROCESS
    BEGIN
        WAIT FOR 2ns;
        -- outside
        mu_s <= '0';
        WAIT FOR 2ns;
        mu_s <= '1';
        -- wait;
    END PROCESS;
END Behavioral;