LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY rippleAdder IS
    PORT (
        ra_a, ra_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        ra_c : IN STD_LOGIC;
        ra_s : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        ra_cout, ra_vout : OUT STD_LOGIC
    );
END rippleAdder;

ARCHITECTURE structural_4 OF rippleAdder IS
    COMPONENT full_adder
        PORT (
            x, y, z : IN STD_LOGIC;
            s, c : OUT STD_LOGIC);
    END COMPONENT;
    SIGNAL C : STD_LOGIC_VECTOR(31 DOWNTO 1);
    SIGNAL v_temp, c_temp : STD_LOGIC;
BEGIN
    Bit0 : full_adder

    PORT MAP(
        x => ra_b(0),
        y => ra_a(0),
        z => ra_c,
        s => ra_s(0),
        c => C(1)
    );
    Bit1 : full_adder
    PORT MAP(
        x => ra_b(1),
        y => ra_a(1),
        z => C(1),
        s => ra_s(1),
        c => C(2)
    );
    Bit2 : full_adder
    PORT MAP(
        x => ra_b(2),
        y => ra_a(2),
        z => C(2),
        s => ra_s(2),
        c => C(3)
    );
    Bit3 : full_adder
    PORT MAP(
        x => ra_b(3),
        y => ra_a(3),
        z => C(3),
        s => ra_s(3),
        c => C(4)
    );
    Bit4 : full_adder
    PORT MAP(
        x => ra_b(4),
        y => ra_a(4),
        z => C(4),
        s => ra_s(4),
        c => C(5)
    );
    Bit5 : full_adder
    PORT MAP(
        x => ra_b(5),
        y => ra_a(5),
        z => C(5),
        s => ra_s(5),
        c => C(6)
    );
    Bit6 : full_adder
    PORT MAP(
        x => ra_b(6),
        y => ra_a(6),
        z => C(6),
        s => ra_s(6),
        c => C(7)
    );
    Bit7 : full_adder
    PORT MAP(
        x => ra_b(7),
        y => ra_a(7),
        z => C(7),
        s => ra_s(7),
        c => C(8)
    );
    Bit8 : full_adder
    PORT MAP(
        x => ra_b(8),
        y => ra_a(8),
        z => C(8),
        s => ra_s(8),
        c => C(9)
    );
    Bit9 : full_adder
    PORT MAP(
        x => ra_b(9),
        y => ra_a(9),
        z => C(9),
        s => ra_s(9),
        c => C(10)
    );
    Bit10 : full_adder
    PORT MAP(
        x => ra_b(10),
        y => ra_a(10),
        z => C(10),
        s => ra_s(10),
        c => C(11)
    );
    Bit11 : full_adder
    PORT MAP(
        x => ra_b(11),
        y => ra_a(11),
        z => C(11),
        s => ra_s(11),
        c => C(12)
    );
    Bit12 : full_adder
    PORT MAP(
        x => ra_b(12),
        y => ra_a(12),
        z => C(12),
        s => ra_s(12),
        c => C(13)
    );
    Bit13 : full_adder
    PORT MAP(
        x => ra_b(13),
        y => ra_a(13),
        z => C(13),
        s => ra_s(13),
        c => C(14)
    );
    Bit14 : full_adder
    PORT MAP(
        x => ra_b(14),
        y => ra_a(14),
        z => C(14),
        s => ra_s(14),
        c => C(15)
    );
    Bit15 : full_adder
    PORT MAP(
        x => ra_b(15),
        y => ra_a(15),
        z => C(15),
        s => ra_s(15),
        c => C(16)
    );
    Bit16 : full_adder
    PORT MAP(
        x => ra_b(16),
        y => ra_a(16),
        z => C(16),
        s => ra_s(16),
        c => C(17)
    );
    Bit17 : full_adder
    PORT MAP(
        x => ra_b(17),
        y => ra_a(17),
        z => C(17),
        s => ra_s(17),
        c => C(18)
    );
    Bit18 : full_adder
    PORT MAP(
        x => ra_b(18),
        y => ra_a(18),
        z => C(18),
        s => ra_s(18),
        c => C(19)
    );
    Bit19 : full_adder
    PORT MAP(
        x => ra_b(19),
        y => ra_a(19),
        z => C(19),
        s => ra_s(19),
        c => C(20)
    );
    Bit20 : full_adder
    PORT MAP(
        x => ra_b(20),
        y => ra_a(20),
        z => C(20),
        s => ra_s(20),
        c => C(21)
    );
    Bit21 : full_adder
    PORT MAP(
        x => ra_b(21),
        y => ra_a(21),
        z => C(21),
        s => ra_s(21),
        c => C(22)
    );
    Bit22 : full_adder
    PORT MAP(
        x => ra_b(22),
        y => ra_a(22),
        z => C(22),
        s => ra_s(22),
        c => C(23)
    );
    Bit23 : full_adder
    PORT MAP(
        x => ra_b(23),
        y => ra_a(23),
        z => C(23),
        s => ra_s(23),
        c => C(24)
    );
    Bit24 : full_adder
    PORT MAP(
        x => ra_b(24),
        y => ra_a(24),
        z => C(24),
        s => ra_s(24),
        c => C(25)
    );
    Bit25 : full_adder
    PORT MAP(
        x => ra_b(25),
        y => ra_a(25),
        z => C(25),
        s => ra_s(25),
        c => C(26)
    );
    Bit26 : full_adder
    PORT MAP(
        x => ra_b(26),
        y => ra_a(26),
        z => C(26),
        s => ra_s(26),
        c => C(27)
    );
    Bit27 : full_adder
    PORT MAP(
        x => ra_b(27),
        y => ra_a(27),
        z => C(27),
        s => ra_s(27),
        c => C(28)
    );
    Bit28 : full_adder
    PORT MAP(
        x => ra_b(28),
        y => ra_a(28),
        z => C(28),
        s => ra_s(28),
        c => C(29)
    );
    Bit29 : full_adder
    PORT MAP(
        x => ra_b(29),
        y => ra_a(29),
        z => C(29),
        s => ra_s(29),
        c => C(30)
    );
    Bit30 : full_adder
    PORT MAP(
        x => ra_b(30),
        y => ra_a(30),
        z => C(30),
        s => ra_s(30),
        c => C(31)
    );
    Bit31 : full_adder
    PORT MAP(
        x => ra_b(31),
        y => ra_a(31),
        z => C(31),
        s => ra_s(31),
        c => c_temp
    );

    v_temp <= c_temp XOR C(31) AFTER 2 ns;
    ra_cout <= c_temp;
    ra_vout <= v_temp;
END structural_4;