LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY PC IS
    PORT (
        data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        PL, PI, reset, clk : IN STD_LOGIC;
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END PC;

ARCHITECTURE Behavioral OF PC IS
signal final : std_logic_vector(31 downto 0) ;
BEGIN
    PROCESS (clk)
    BEGIN
        if (rising_edge(clk)) then
            if (reset = '1') then 
                final <= x"00000000"; -- reset data to 0
            elsif (PI = '1') then
                -- convert the signal into a signed integer, add 1, then turn back into a 32 bit vector
                final <= std_logic_vector(to_signed((to_integer(signed(final)) + 1), 32)); -- add 1 to data_out
            elsif (PL = '1') then
                final <= std_logic_vector(to_signed((to_integer(signed(final)) + to_integer(signed(data_in))), 32)); -- add data_in to data_out
            end if;
        end if;
    END PROCESS;
    data_out <= final;
END Behavioral;