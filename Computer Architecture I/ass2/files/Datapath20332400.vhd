LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY Datapath IS
    PORT (
        a_sel, b_sel, d_sel, f_sel : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        load_en, mb_sel, md_sel : IN STD_LOGIC;
        const_in, data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk : IN STD_LOGIC;
        data_out, add_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        v, c, n, z : OUT STD_LOGIC
    );
END Datapath;

ARCHITECTURE datapath_0 OF Datapath IS
    COMPONENT register_file PORT (
        a_add, b_add, d_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0);
        data : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk, load_wr : IN STD_LOGIC;
        data_a, data_b : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT function_unit PORT (
        f_A, f_B : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        f_FS : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        v, c, n, z : OUT STD_LOGIC;
        F : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    COMPONENT mux2_32bit PORT (
        mu_In0, mu_In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_s : IN STD_LOGIC;
        mu_Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    SIGNAL a_data, b_data, b_mux_data, f_out, d_bus : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN
    regFile : register_file PORT MAP(
        data => d_bus,
        a_add => a_sel,
        b_add => b_sel, 
        d_add => d_sel, 
        clk => clk, 
        load_wr => load_en, 
        data_a => a_data,
        data_b => b_data
    );
    mux_0 : mux2_32bit PORT MAP(
        mu_In0 => const_in, 
        mu_In1 => b_data, 
        mu_s => mb_sel, 
        mu_Z => b_mux_data
    );
    funit : function_unit PORT MAP(
        f_A => a_data, 
        f_B => b_mux_data, 
        f_FS => f_sel, 
        v => v, 
        c => c, 
        n => n, 
        z => z,
        F => f_out
    );
    mux_1 : mux2_32bit PORT MAP(
        mu_In0 => f_out, 
        mu_In1 => data_in, 
        mu_s => md_sel, 
        mu_Z => d_bus
    );

    add_out <= a_data;
    data_out <= b_data;
END datapath_0; -- datapath_0