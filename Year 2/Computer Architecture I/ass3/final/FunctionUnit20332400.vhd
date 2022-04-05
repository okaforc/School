LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY function_unit IS
    PORT (
        f_A, f_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        f_FS : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        v, c, n, z : OUT STD_LOGIC;
        F : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
END function_unit;
ARCHITECTURE dataflow_2 OF function_unit IS
    COMPONENT alu PORT (
        al_A, al_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        al_gsel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        al_v, al_c : OUT STD_LOGIC;
        al_G : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT shifter PORT (
        sh_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        sh_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        sh_il, sh_ir : IN STD_LOGIC;
        sh_h : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT mux2_32bit PORT (
        mu_In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_s : IN STD_LOGIC;
        mu_Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT zeroDetect PORT (
        z_a : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        z_out : OUT STD_LOGIC
        );
    END COMPONENT;

    SIGNAL alu_out, shift_out, m_out : STD_LOGIC_VECTOR(31 DOWNTO 0); -- various outputs
    SIGNAL z_detect : STD_LOGIC := '0'; -- zero detection

BEGIN

    alu_unit : alu PORT MAP(
        al_A => f_A,
        al_B => f_B,
        al_gsel => f_FS(3 DOWNTO 0),
        al_v => v,
        al_c => c,
        al_G => alu_out
    );

    shift_unit : shifter PORT MAP(
        sh_B => f_B,
        sh_s(0) => f_FS(2),
        sh_s(1) => f_FS(3),
        sh_il => '0',
        sh_ir => '0',
        sh_h => shift_out
    );

    multiplexer : mux2_32bit PORT MAP(
        mu_in0 => alu_out,
        mu_in1 => shift_out,
        mu_s => f_FS(4),
        mu_Z => m_out
    );

    zDetect : zeroDetect PORT MAP(
        z_a => alu_out,
        z_out => z_detect
    );

    F <= m_out;
    n <= alu_out(31);
    z <= z_detect;

END dataflow_2;