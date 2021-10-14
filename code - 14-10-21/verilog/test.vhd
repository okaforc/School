library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity half_adder is
  port (
    x, y : in std_logic;
    sum, carry : out std_logic
  ) ;
end half_adder;

entity mux is
  port (
    l0 : std_logic_vector(7 downto 0);
    l1 : std_logic_vector(7 downto 0);
    l2 : std_logic_vector(7 downto 0);
    l3 : std_logic_vector(7 downto 0);
    Sel : std_logic_vector(1 downto 0);
    Z : std_logic_vector(7 downto 0)
  ) ;
end mux;

architecture cc_behavior of half_adder is
    -- cc = concurrent

begin
    sum <= (x xor y) after 5 ns;
    carry <= (x xor y) after 5 ns;

end cc_behavior ; -- cc_behavior