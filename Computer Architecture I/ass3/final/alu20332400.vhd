LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY alu IS
    PORT (
        al_A, al_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        al_gsel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        al_v, al_c : OUT STD_LOGIC;
        al_G : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END alu;
ARCHITECTURE dataflow_2 OF alu IS
    COMPONENT logCircuit PORT (
        lc_a, lc_b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        lc_s0, lc_s1 : STD_LOGIC;
        lc_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    COMPONENT arCircuit PORT (
        ar_A, ar_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        ar_cin, ar_s0, ar_s1 : IN STD_LOGIC;
        ar_g : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        ar_cout, ar_vout : OUT STD_LOGIC
        );
    END COMPONENT;
    COMPONENT mux2_32bit PORT (
        mu_In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_s : IN STD_LOGIC;
        mu_Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    SIGNAL lc : STD_LOGIC_VECTOR(31 DOWNTO 0); -- logCircuit output
    SIGNAL ar : STD_LOGIC_VECTOR(31 DOWNTO 0); -- arCircuit output

BEGIN

    logic : logCircuit PORT MAP(
        lc_a => al_A,
        lc_b => al_B,
        lc_s0 => al_gsel(1),
        lc_s1 => al_gsel(2),
        lc_g => lc
    );

    arithmetic : arCircuit PORT MAP(
        ar_a => al_A,
        ar_b => al_B,
        ar_cin => al_gsel(0),
        ar_s0 => al_gsel(1),
        ar_s1 => al_gsel(2),
        ar_g => ar,
        ar_cout => al_c,
        ar_vout => al_v
    );

    multiplexer : mux2_32bit PORT MAP(
        mu_in0 => ar,
        mu_in1 => lc,
        mu_s => al_gsel(3),
        mu_Z => al_G
    );

END dataflow_2;