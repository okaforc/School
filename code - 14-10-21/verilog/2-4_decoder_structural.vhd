-- 2-to-4 Line Decoder: Structural VHDL Description
library ieee, lcdf_vhdl;
use ieee.std_logic_1164.all, lcdf_vhdl.func_prims.all;

entity decoder_2_to_4 is
    port(E, A0, A1: in std_logic;
        D0, D1, D2, D3: out std_logic);
    end decoder_2_to_4;
    
    architecture structural_1 of decoder_2_to_4 is
        component NOT1
        port(in1: in std_logic;
        out1: out std_logic);
end component;

component NAND3
    port(in1, in2, in3: in std_logic;
        out1: out std_logic);
    end component;

    signal not_A0, not_A1: std_logic;

    begin
        g0: NOT1 port map (in1 => A0, out1 => not_A0);
        g1: NOT1 port map (in1 => A1, out1 => not_A1);
        g2: NAND3 port map (in1 => not_A0, in2 => not_A1,
        in3 => E, out1 => D0);
        g3: NAND3 port map (in1 => A0, in2 => not_A1,
        in3 => E, out1 => D1);
        g4: NAND3 port map (in1 => not_A0, in2 => A1,
        in3 => E, out1 => D2);
        g5: NAND3 port map (in1 => A0, in2 => A1,
        in3 => E, out1 => D3);
end structural_1;

