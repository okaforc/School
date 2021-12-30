LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY logCircuit IS
    PORT (
        lc_a, lc_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        lc_s0, lc_s1 : IN STD_LOGIC;
        lc_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END logCircuit;

ARCHITECTURE dataflow_2 OF logCircuit IS
    COMPONENT logSlice PORT (
        x, y : IN STD_LOGIC;
        s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        g : OUT STD_LOGIC
        );
    END COMPONENT;
BEGIN

    slice0 : logSlice PORT MAP(
        x => lc_a(0),
        y => lc_b(0),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(0)
    );
    slice1 : logSlice PORT MAP(
        x => lc_a(1),
        y => lc_b(1),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(1)
    );
    slice2 : logSlice PORT MAP(
        x => lc_a(2),
        y => lc_b(2),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(2)
    );
    slice3 : logSlice PORT MAP(
        x => lc_a(3),
        y => lc_b(3),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(3)
    );
    slice4 : logSlice PORT MAP(
        x => lc_a(4),
        y => lc_b(4),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(4)
    );
    slice5 : logSlice PORT MAP(
        x => lc_a(5),
        y => lc_b(5),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(5)
    );
    slice6 : logSlice PORT MAP(
        x => lc_a(6),
        y => lc_b(6),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(6)
    );
    slice7 : logSlice PORT MAP(
        x => lc_a(7),
        y => lc_b(7),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(7)
    );
    slice8 : logSlice PORT MAP(
        x => lc_a(8),
        y => lc_b(8),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(8)
    );
    slice9 : logSlice PORT MAP(
        x => lc_a(9),
        y => lc_b(9),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(9)
    );
    slice10 : logSlice PORT MAP(
        x => lc_a(10),
        y => lc_b(10),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(10)
    );
    slice11 : logSlice PORT MAP(
        x => lc_a(11),
        y => lc_b(11),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(11)
    );
    slice12 : logSlice PORT MAP(
        x => lc_a(12),
        y => lc_b(12),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(12)
    );
    slice13 : logSlice PORT MAP(
        x => lc_a(13),
        y => lc_b(13),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(13)
    );
    slice14 : logSlice PORT MAP(
        x => lc_a(14),
        y => lc_b(14),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(14)
    );
    slice15 : logSlice PORT MAP(
        x => lc_a(15),
        y => lc_b(15),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(15)
    );
    slice16 : logSlice PORT MAP(
        x => lc_a(16),
        y => lc_b(16),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(16)
    );
    slice17 : logSlice PORT MAP(
        x => lc_a(17),
        y => lc_b(17),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(17)
    );
    slice18 : logSlice PORT MAP(
        x => lc_a(18),
        y => lc_b(18),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(18)
    );
    slice19 : logSlice PORT MAP(
        x => lc_a(19),
        y => lc_b(19),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(19)
    );
    slice20 : logSlice PORT MAP(
        x => lc_a(20),
        y => lc_b(20),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(20)
    );
    slice21 : logSlice PORT MAP(
        x => lc_a(21),
        y => lc_b(21),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(21)
    );
    slice22 : logSlice PORT MAP(
        x => lc_a(22),
        y => lc_b(22),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(22)
    );
    slice23 : logSlice PORT MAP(
        x => lc_a(23),
        y => lc_b(23),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(23)
    );
    slice24 : logSlice PORT MAP(
        x => lc_a(24),
        y => lc_b(24),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(24)
    );
    slice25 : logSlice PORT MAP(
        x => lc_a(25),
        y => lc_b(25),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(25)
    );
    slice26 : logSlice PORT MAP(
        x => lc_a(26),
        y => lc_b(26),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(26)
    );
    slice27 : logSlice PORT MAP(
        x => lc_a(27),
        y => lc_b(27),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(27)
    );
    slice28 : logSlice PORT MAP(
        x => lc_a(28),
        y => lc_b(28),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(28)
    );
    slice29 : logSlice PORT MAP(
        x => lc_a(29),
        y => lc_b(29),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(29)
    );
    slice30 : logSlice PORT MAP(
        x => lc_a(30),
        y => lc_b(30),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(30)
    );
    slice31 : logSlice PORT MAP(
        x => lc_a(31),
        y => lc_b(31),
        s(0) => lc_s0,
        s(1) => lc_s1,
        g => lc_g(31)
    );

END dataflow_2; -- dataflow_2