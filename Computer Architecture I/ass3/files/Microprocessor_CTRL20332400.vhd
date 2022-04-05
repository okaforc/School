LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY MP_wControl IS
    PORT (
        const_in, inst_addr_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        DR, SA, SB : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        IN_CAR : in std_logic_vector(16 downto 0);
        clk : IN STD_LOGIC; 
        FL, RZ, RN, RC, RV, PL, PI, IL, MC, v, c, n, z : out std_logic;
        MS : out std_logic_vector(2 downto 0);
        NA : out std_logic_vector(16 downto 0);
        mem_data_out : out std_logic_vector(31 downto 0)
    );
END MP_wControl;

architecture Behavioural of MP_wControl is
    signal MW, MM, RW, MD, MB, TB, TA, TD : std_logic; -- control memory outputs
    signal FSout : std_logic_vector(4 downto 0);
    -- signal discarded : std_logic_vector(8 downto 0) ; -- unused control memory outputs as a vector
    -- mem_data_out, bus_a, bus_b : INOUT STD_LOGIC_VECTOR(31 DOWNTO 0); -- datapath output, memory input (buses); memory output, datapath input (data)
    
    component ctrl_mem port (
        IN_CAR : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        FL : OUT STD_LOGIC; -- 0
        RZ : OUT STD_LOGIC; -- 1
        RN : OUT STD_LOGIC; -- 2
        RC : OUT STD_LOGIC; -- 3
        RV : OUT STD_LOGIC; -- 4
        MW : OUT STD_LOGIC; -- 5
        MM : OUT STD_LOGIC; -- 6
        RW : OUT STD_LOGIC; -- 7
        MD : OUT STD_LOGIC; -- 8
        FS : OUT STD_LOGIC_VECTOR(4 DOWNTO 0); -- 9 to 13
        MB : OUT STD_LOGIC; -- 14
        TB : OUT STD_LOGIC; -- 15
        TA : OUT STD_LOGIC; -- 16
        TD : OUT STD_LOGIC; -- 17
        PL : OUT STD_LOGIC; -- 18
        PI : OUT STD_LOGIC; -- 19
        IL : OUT STD_LOGIC; -- 20
        MC : OUT STD_LOGIC; -- 21
        MS : OUT STD_LOGIC_VECTOR(2 DOWNTO 0); -- 22 to 24
        NA : OUT STD_LOGIC_VECTOR(16 DOWNTO 0) -- 25 to 41
    );
    end component;

    component MP_noControl PORT (
        const_in, inst_addr_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        DR, SA, SB, FS : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        RW, TD, TA, TB, MW, MB, MM, MD, clk : IN STD_LOGIC;
        mem_data_out: out STD_LOGIC_VECTOR(31 DOWNTO 0); -- datapath output, memory input (buses); memory output, datapath input (data)
        v, c, n, z : OUT STD_LOGIC
    );
    end component;

begin
    mp_nctrl : MP_noControl PORT map(
        const_in => const_in,
        inst_addr_in => inst_addr_in,
        DR => DR,
        SA => SA,
        SB => SB,
        FS => FSout,
        RW => RW,
        TD => TD,
        TA => TA,
        TB => TB,
        MW => MW,
        MB => MB,
        MM => MM,
        MD => MD,
        clk => clk,
        v => v,
        c => c,
        n => n,
        z => z,
        mem_data_out => mem_data_out
    );
    
    control_memory : ctrl_mem port map(
        IN_CAR => IN_CAR,
        FL => FL,
        RZ => RZ,
        RN => RN,
        RC => RC,
        RV => RV,
        MW => MW,
        MM => MM,
        RW => RW,
        MD => MD,
        FS => FSout,
        MB => MB,
        TB => TB,
        TA => TA,
        TD => TD,
        PL => PL,
        PI => PI,
        IL => IL,
        MC => MC,
        MS => MS,
        NA => NA
    );
        

end Behavioural ; -- Behavioural