-- 5-to-32 Line Decoder: Dataflow VHDL Description
LIBRARY ieee, lcdf_vhdl;
USE ieee.std_logic_1164.ALL;

ENTITY decoder_5_to_32 IS
    PORT (
        A : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13,
        D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26,
        D27, D28, D29, D30, D31 : OUT STD_LOGIC
    );
END decoder_5_to_32;

ARCHITECTURE behavioural OF decoder_5_to_32 IS
BEGIN
    D0 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D1 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D2 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D3 <= ((NOT A(4)) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 2 ns;
    D4 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D5 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D6 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D7 <= ((NOT A(4)) AND (NOT A(3)) AND A(2) AND A(1) AND A(0)) AFTER 2 ns;
    D8 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D9 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D10 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D11 <= ((NOT A(4)) AND A(3) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 2 ns;
    D12 <= ((NOT A(4)) AND A(3) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D13 <= ((NOT A(4)) AND A(3) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D14 <= ((NOT A(4)) AND A(3) AND A(2) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D15 <= ((NOT A(4)) AND A(3) AND A(2) AND A(1) AND A(0)) AFTER 2 ns;
    D16 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D17 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D18 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D19 <= (A(4) AND (NOT A(3)) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 2 ns;
    D20 <= (A(4) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D21 <= (A(4) AND (NOT A(3)) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D22 <= (A(4) AND (NOT A(3)) AND A(2) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D23 <= (A(4) AND (NOT A(3)) AND A(2) AND A(1) AND A(0)) AFTER 2 ns;
    D24 <= (A(4) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D25 <= (A(4) AND A(3) AND (NOT A(2)) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D26 <= (A(4) AND A(3) AND (NOT A(2)) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D27 <= (A(4) AND A(3) AND (NOT A(2)) AND A(1) AND A(0)) AFTER 2 ns;
    D28 <= (A(4) AND A(3) AND A(2) AND (NOT A(1)) AND (NOT A(0))) AFTER 2 ns;
    D29 <= (A(4) AND A(3) AND A(2) AND (NOT A(1)) AND A(0)) AFTER 2 ns;
    D30 <= (A(4) AND A(3) AND A(2) AND A(1) AND (NOT A(0))) AFTER 2 ns;
    D31 <= (A(4) AND A(3) AND A(2) AND A(1) AND A(0)) AFTER 2 ns;

END behavioural;