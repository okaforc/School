-- 5-to-32 Line Decoder: Dataflow VHDL Description
LIBRARY ieee, lcdf_vhdl;
USE ieee.std_logic_1164.ALL, lcdf_vhdl.func_prims.ALL;

ENTITY decoder_5_to_32 IS
    PORT (
    A : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
    -- D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13,
    D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26,
    D27, D28, D29, D30, D31 : OUT STD_LOGIC);
END decoder_5_to_32;

ARCHITECTURE behavioural OF decoder_5_to_32 IS
BEGIN
    D0 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D1 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D2 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D3 <= (A(4) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D4 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D5 <= (A(4) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D6 <= ((NOT A(4)) AND A(3) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D7 <= (A(4) AND A(3) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 5ns;
    D8 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D9 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D10 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D11 <= (A(4) AND A(3) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D12 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D13 <= (A(4) AND (NOT A(3)) AND A(2) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D14 <= ((NOT A(4)) AND A(3) AND A(2) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D15 <= (A(4) AND A(3) AND A(2) AND A(1) AND (NOT A(0))) AFTER 5ns;
    D16 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D17 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D18 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D19 <= (A(4) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D20 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D21 <= (A(4) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D22 <= ((NOT A(4)) AND A(3) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D23 <= (A(4) AND A(3) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 5ns;
    D24 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 5ns;
    D25 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 5ns;
    D26 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 5ns;
    D27 <= (A(4) AND A(3) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 5ns;
    D28 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND A(1) AND A(0)) AFTER 5ns;
    D29 <= (A(4) AND (NOT A(3)) AND A(2) AND A(1) AND A(0)) AFTER 5ns;
    D30 <= ((NOT A(4)) AND A(3) AND A(2) AND A(1) AND A(0)) AFTER 5ns;
    D31 <= (A(4) AND A(3) AND A(2) AND A(1) AND A(0)) AFTER 5ns;

END behavioural;