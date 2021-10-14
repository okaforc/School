-- 4-to-1 Line Multiplexer: Structural VHDL Description
library ieee, lcdf_vhdl;
use ieee.std_logic_1164.all, lcdf_vhdl.func_prims.all;

entity multiplexer_4_to_1_st is
    port(S: in std_logic_vector(0 to 1);
    D: in std_logic_vector(0 to 3);
    Y: out std_logic);
end multiplexer_4_to_1_st;

architecture structural_2 of multiplexer_4_to_1_st is
    component NOT1
    port(in1: in std_logic;
    out1: out std_logic);
    end component;
    
    component AND3
    port(in1, in2, in3: in std_logic;
    out1: out std_logic);
    end component;

    component OR4
    port(in1, in2, in3, in4: in std_logic;
    out1: out std_logic);
    end component;

    signal not_S: std_logic_vector(0 to 1);
    signal N: std_logic_vector(0 to 3);
    begin
        g0: NOT1 port map (S(0), not_S(0));
        g1: NOT1 port map (S(1), not_S(1));
        g2: AND3 port map (not_S(1), not_S(0), D(0), N(0));
        g3: AND3 port map (not_S(1), S(0), D(1), N(1));
        g4: AND3 port map (S(1), not_S(0), D(2), N(2));
        g5: AND3 port map (S(1), S(0), D(3), N(3));
        g6: OR4 port map (N(0), N(1), N(2), N(3), Y);
end structural_2;