LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY shifter IS
	PORT (
		sh_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		sh_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
		sh_il, sh_ir : IN STD_LOGIC;
		sh_h : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
	);
END shifter;

ARCHITECTURE dataflow OF shifter IS
	COMPONENT mux4_1bit PORT (
		mu4_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
		mu4_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
		mu4_Z : OUT STD_LOGIC
		);
	END COMPONENT;
	SIGNAL ignore : STD_LOGIC;

BEGIN
	ignore <= '0';
	m0 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(0),
		mu4_in(1) => sh_B(1),
		mu4_in(2) => sh_il,
		mu4_in(3) => ignore,
		mu4_z => sh_h(0)
	);
	m1 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(1),
		mu4_in(1) => sh_B(2),
		mu4_in(2) => sh_B(0),
		mu4_in(3) => ignore,
		mu4_z => sh_h(1)
	);
	m2 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(2),
		mu4_in(1) => sh_B(3),
		mu4_in(2) => sh_B(1),
		mu4_in(3) => ignore,
		mu4_z => sh_h(2)
	);
	m3 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(3),
		mu4_in(1) => sh_B(4),
		mu4_in(2) => sh_B(2),
		mu4_in(3) => ignore,
		mu4_z => sh_h(3)
	);
	m4 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(4),
		mu4_in(1) => sh_B(5),
		mu4_in(2) => sh_B(3),
		mu4_in(3) => ignore,
		mu4_z => sh_h(4)
	);
	m5 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(5),
		mu4_in(1) => sh_B(6),
		mu4_in(2) => sh_B(4),
		mu4_in(3) => ignore,
		mu4_z => sh_h(5)
	);
	m6 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(6),
		mu4_in(1) => sh_B(7),
		mu4_in(2) => sh_B(5),
		mu4_in(3) => ignore,
		mu4_z => sh_h(6)
	);
	m7 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(7),
		mu4_in(1) => sh_B(8),
		mu4_in(2) => sh_B(6),
		mu4_in(3) => ignore,
		mu4_z => sh_h(7)
	);
	m8 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(8),
		mu4_in(1) => sh_B(9),
		mu4_in(2) => sh_B(7),
		mu4_in(3) => ignore,
		mu4_z => sh_h(8)
	);
	m9 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(9),
		mu4_in(1) => sh_B(10),
		mu4_in(2) => sh_B(8),
		mu4_in(3) => ignore,
		mu4_z => sh_h(9)
	);
	m10 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(10),
		mu4_in(1) => sh_B(11),
		mu4_in(2) => sh_B(9),
		mu4_in(3) => ignore,
		mu4_z => sh_h(10)
	);
	m11 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(11),
		mu4_in(1) => sh_B(12),
		mu4_in(2) => sh_B(10),
		mu4_in(3) => ignore,
		mu4_z => sh_h(11)
	);
	m12 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(12),
		mu4_in(1) => sh_B(13),
		mu4_in(2) => sh_B(11),
		mu4_in(3) => ignore,
		mu4_z => sh_h(12)
	);
	m13 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(13),
		mu4_in(1) => sh_B(14),
		mu4_in(2) => sh_B(12),
		mu4_in(3) => ignore,
		mu4_z => sh_h(13)
	);
	m14 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(14),
		mu4_in(1) => sh_B(15),
		mu4_in(2) => sh_B(13),
		mu4_in(3) => ignore,
		mu4_z => sh_h(14)
	);
	m15 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(15),
		mu4_in(1) => sh_B(16),
		mu4_in(2) => sh_B(14),
		mu4_in(3) => ignore,
		mu4_z => sh_h(15)
	);
	m16 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(16),
		mu4_in(1) => sh_B(17),
		mu4_in(2) => sh_B(15),
		mu4_in(3) => ignore,
		mu4_z => sh_h(16)
	);
	m17 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(17),
		mu4_in(1) => sh_B(18),
		mu4_in(2) => sh_B(16),
		mu4_in(3) => ignore,
		mu4_z => sh_h(17)
	);
	m18 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(18),
		mu4_in(1) => sh_B(19),
		mu4_in(2) => sh_B(17),
		mu4_in(3) => ignore,
		mu4_z => sh_h(18)
	);
	m19 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(19),
		mu4_in(1) => sh_B(20),
		mu4_in(2) => sh_B(18),
		mu4_in(3) => ignore,
		mu4_z => sh_h(19)
	);
	m20 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(20),
		mu4_in(1) => sh_B(21),
		mu4_in(2) => sh_B(19),
		mu4_in(3) => ignore,
		mu4_z => sh_h(20)
	);
	m21 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(21),
		mu4_in(1) => sh_B(22),
		mu4_in(2) => sh_B(20),
		mu4_in(3) => ignore,
		mu4_z => sh_h(21)
	);
	m22 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(22),
		mu4_in(1) => sh_B(23),
		mu4_in(2) => sh_B(21),
		mu4_in(3) => ignore,
		mu4_z => sh_h(22)
	);
	m23 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(23),
		mu4_in(1) => sh_B(24),
		mu4_in(2) => sh_B(22),
		mu4_in(3) => ignore,
		mu4_z => sh_h(23)
	);
	m24 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(24),
		mu4_in(1) => sh_B(25),
		mu4_in(2) => sh_B(23),
		mu4_in(3) => ignore,
		mu4_z => sh_h(24)
	);
	m25 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(25),
		mu4_in(1) => sh_B(26),
		mu4_in(2) => sh_B(24),
		mu4_in(3) => ignore,
		mu4_z => sh_h(25)
	);
	m26 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(26),
		mu4_in(1) => sh_B(27),
		mu4_in(2) => sh_B(25),
		mu4_in(3) => ignore,
		mu4_z => sh_h(26)
	);
	m27 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(27),
		mu4_in(1) => sh_B(28),
		mu4_in(2) => sh_B(26),
		mu4_in(3) => ignore,
		mu4_z => sh_h(27)
	);
	m28 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(28),
		mu4_in(1) => sh_B(29),
		mu4_in(2) => sh_B(27),
		mu4_in(3) => ignore,
		mu4_z => sh_h(28)
	);
	m29 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(29),
		mu4_in(1) => sh_B(30),
		mu4_in(2) => sh_B(28),
		mu4_in(3) => ignore,
		mu4_z => sh_h(29)
	);
	m30 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(30),
		mu4_in(1) => sh_B(31),
		mu4_in(2) => sh_B(29),
		mu4_in(3) => ignore,
		mu4_z => sh_h(30)
	);
	m31 : mux4_1bit PORT MAP(
		mu4_s => sh_s,
		mu4_in(0) => sh_B(31),
		mu4_in(1) => sh_ir,
		mu4_in(2) => sh_B(30),
		mu4_in(3) => ignore,
		mu4_z => sh_h(31)
	);
END dataflow; -- dataflow