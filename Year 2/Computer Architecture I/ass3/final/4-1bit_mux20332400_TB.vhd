LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux4_1bit_tb IS
END mux4_1bit_tb;

ARCHITECTURE Behavioral OF mux4_1bit_tb IS
    COMPONENT mux4_1bit
        PORT (
        mu4_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        mu4_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        mu4_Z : OUT STD_LOGIC
        );
    END COMPONENT;

    -- input
    SIGNAL mu4_s : STD_LOGIC_VECTOR (1 DOWNTO 0) := (OTHERS => '0');
    SIGNAL mu4_in : STD_LOGIC_VECTOR (3 DOWNTO 0);
    
    -- output
    SIGNAL mu4_Z  : STD_LOGIC;
BEGIN
    uut : mux4_1bit PORT MAP(mu4_in => mu4_in, mu4_s => mu4_s, mu4_Z => mu4_Z);
    mu4_in(0) <= '1';
    mu4_in(1) <= '0';
    mu4_in(2) <= '1';
    mu4_in(3) <= '0';
    stim_proc : PROCESS
    BEGIN
        WAIT FOR 10ns;
        mu4_s <= "00";
        WAIT FOR 10ns;
        mu4_s <= "01";
        WAIT FOR 10ns;
        mu4_s <= "10";
        WAIT FOR 10ns;
        mu4_s <= "11";
    END PROCESS;
END Behavioral;