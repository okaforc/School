-- 2-to-4 Line Decoder: Dataflow VHDL Description
LIBRARY ieee, lcdf_vhdl;
USE ieee.std_logic_1164.ALL, lcdf_vhdl.func_prims.ALL;

ENTITY decoder_2_to_4 IS
    PORT (
        E, A0, A1 : IN STD_LOGIC;
        D0, D1, D2, D3 : OUT STD_LOGIC);
END decoder_2_to_4;

ARCHITECTURE dataflow_1 OF decoder_2_to_4 IS
    SIGNAL not_A0, not_A1 : STD_LOGIC;
BEGIN
    not_A0 <= NOT A0;
    not_A1 <= NOT A1;
    D0 <= NOT (not_A0 AND not_A1 AND E);
    D1 <= NOT (A0 AND not_A1 AND E);
    D2 <= NOT (not_A0 AND A1 AND E);
    D3 <= NOT (A0 AND A1 AND E);
END dataflow_1;