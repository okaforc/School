LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY InstReg IS
    PORT (
        IL, clk : IN STD_LOGIC;
        IR : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        opcode : OUT STD_LOGIC_VECTOR(16 DOWNTO 0);
        SA, SB, DR : OUT STD_LOGIC_VECTOR(4 DOWNTO 0)
    );
END InstReg;

ARCHITECTURE Behavioral OF InstReg IS
BEGIN
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (IL = '1') then
                SB <= IR(4 DOWNTO 0);
                SA <= IR(9 DOWNTO 5);
                DR <= IR(14 DOWNTO 10);
                opcode <= IR(31 DOWNTO 15);
            end if;
        end if;
    end process;
END Behavioral;