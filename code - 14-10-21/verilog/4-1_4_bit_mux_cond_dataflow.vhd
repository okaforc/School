library ieee;
use ieee.std_logic_1164.all;

entity multiplexer_4_to_1 is
    port (S : in std_logic_vector(1 downto 0);
    D : in std_logic_vector(3 downto 0);
    Y : out std_logic);
end multiplexer_4_to_1;

architecture function_table of multiplexer_4_to_1 is
    begin
        Y <= D(0) when S = "00" else
        D(1) when S = "01" else
        D(2) when S = "10" else
        D(3) when S = "11" else
        'X';
    
    -- using a "switch" case
        -- with S select
        --     Y <= D(0) when "00",
        --     D(1) when "01",
        --     D(2) when "10",
        --     D(3) when "11",
        --     'X' when others;
end function_table;

