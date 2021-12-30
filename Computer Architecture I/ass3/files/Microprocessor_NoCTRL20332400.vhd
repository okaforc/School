LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY MP_noControl IS
    PORT (
        const_in, inst_addr_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        DR, SA, SB, FS : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        RW, TD, TA, TB, MW, MB, MM, MD, clk : IN STD_LOGIC;
        v, c, n, z : OUT STD_LOGIC;
        mem_data_out : out std_logic_vector(31 downto 0)
    );
END MP_noControl;

architecture Behavioural of MP_noControl is
    signal mux_m : STD_LOGIC_VECTOR(31 downto 0); -- mux m output
    signal mem_data_out_dp, mem_data_out_inst, bus_a, bus_b : STD_LOGIC_VECTOR(31 DOWNTO 0); -- datapath output, memory input (buses); memory output, datapath input (data)

    component Datapath PORT (
        a_sel, b_sel, d_sel, f_sel : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        load_en, mb_sel, md_sel : IN STD_LOGIC;
        const_in, data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk, TA, TB, TD: IN STD_LOGIC;
        data_out, add_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
        v, c, n, z : OUT STD_LOGIC
    );
    end component;
    
    component memory_512 port (
        mem_address : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        write_enable, clk : IN STD_LOGIC;
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    component mux2_32bit port (
        mu_In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        mu_s : IN STD_LOGIC;
        mu_Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;
    
    begin
        dp : Datapath PORT map(
            a_sel => SA,
            b_sel => SB,
            d_sel => DR,
            f_sel => FS,
            load_en => RW,
            mb_sel => MB,
            md_sel => MD,
            const_in => const_in,
            data_in => mem_data_out_dp,
            clk => clk,
            TA => TA,
            TB => TB,
            TD => TD,
            data_out => bus_a,
            add_out => bus_b,
            v => v,
            c => c,
            n => n,
            z => z
        );
        
        memory : memory_512 port map(
            mem_address => mux_m,
            data_in => bus_b,
            clk => clk,
            write_enable => MW,
            data_out => mem_data_out_dp
        );
        
    
        multi_m : mux2_32bit port map(
            mu_In0 => bus_a,
            mu_In1 => inst_addr_in,
            mu_s => MM,
            mu_Z => mux_m
        );
        
        mem_data_out_inst <= mem_data_out_dp;
        mem_data_out <= mem_data_out_inst;
end Behavioural ; -- Behavioural