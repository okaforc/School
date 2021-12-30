LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY arCircuit IS
    PORT (
        ar_A, ar_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        ar_cin, ar_s0, ar_s1 : IN STD_LOGIC;
        ar_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        ar_cout, ar_vout : OUT STD_LOGIC
    );
END arCircuit;

ARCHITECTURE structural OF arCircuit IS
    COMPONENT BInput PORT (
        bi_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        bi_s0, bi_s1 : STD_LOGIC;
        bi_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT rippleAdder PORT (
        ra_a, ra_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        ra_c : IN STD_LOGIC;
        ra_s : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        ra_cout, ra_vout : OUT STD_LOGIC
        );
    END COMPONENT;
    SIGNAL ar_y : STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN
    binputlogic : BInput PORT MAP (
        bi_b => ar_B,
        bi_s0 => ar_s0,
        bi_s1 => ar_s1,
        bi_g => ar_y
    );
    radder : rippleAdder PORT MAP (
        ra_a => ar_A,
        ra_b => ar_y,
        ra_c => ar_cin,
        ra_s => ar_g,
        ra_cout => ar_cout,
        ra_vout => ar_vout
    );

END structural;