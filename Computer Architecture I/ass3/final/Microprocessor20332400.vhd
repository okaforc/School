LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

entity Microprocessor is
    port (
        clk, resetCAR, resetPC : in std_logic
    );
end Microprocessor;

architecture Behavioural of Microprocessor is
    component ControlUnit port (
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk, resetPC, resetCAR, v, c, n, z : IN STD_LOGIC;
        FL, RZ, RN, RC, RV, PL, PI, IL, MC : in std_logic;
        NA : in std_logic_vector(16 downto 0);
        MS : in STD_LOGIC_VECTOR(2 DOWNTO 0);
        DR, SA, SB : out STD_LOGIC_VECTOR(4 DOWNTO 0);
        car_out : out std_logic_vector(16 downto 0);
        zerofilled_out, inst_addr_out : out std_logic_vector(31 downto 0)
    );
    end component;

    component MP_wControl port (
        const_in, inst_addr_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        DR, SA, SB : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        IN_CAR : in std_logic_vector(16 downto 0);
        clk : IN STD_LOGIC; 
        FL, RZ, RN, RC, RV, PL, PI, IL, MC, v, c, n, z : out std_logic;
        MS : out std_logic_vector(2 downto 0);
        NA : out std_logic_vector(16 downto 0);
        mem_data_out : out std_logic_vector(31 downto 0)
    );
    end component;

    signal DR, SA, SB : STD_LOGIC_VECTOR(4 DOWNTO 0);
    signal car_out : std_logic_vector(16 downto 0);
    signal zerofilled_out, inst_addr_out : std_logic_vector(31 downto 0);
    signal FL, RZ, RN, RC, RV, PL, PI, IL, MC, v, c, n, z : std_logic;
    signal MS : std_logic_vector(2 downto 0);
    signal NA : std_logic_vector(16 downto 0);
    signal mem_data_in : std_logic_vector(31 downto 0);

begin
    mpwc : MP_wControl port map(
        clk => clk,
        -- inputs
        const_in => zerofilled_out,
        inst_addr_in => inst_addr_out,
        IN_CAR => car_out,
        DR => DR,
        SA => SA,
        SB => SB,
        -- outputs
        FL => FL,
        RV => RV,
        RC => RC,
        RN => RN,
        RZ => RZ,
        PL => PL,
        PI => PI,
        IL => IL,
        MC => MC,
        v => v,
        c => c,
        n => n,
        z => z,
        MS => MS,
        NA => NA,
        mem_data_out => mem_data_in
    );

    ctrlunit : ControlUnit port map(
        clk => clk,
        -- inputs
        instruction => mem_data_in,
        resetPC => resetPC,
        resetCAR => resetCAR,
        v => v,
        c => c,
        n => n,
        z => z,
        FL => FL,
        RZ => RZ,
        RN => RN,
        RC => RC,
        RV => RV,
        PL => PL,
        PI => PI,
        IL => IL,
        MC => MC,
        MS => MS,
        NA => NA,
        -- outputs
        DR => DR,
        SA => SA,
        SB => SB,
        car_out => car_out,
        zerofilled_out => zerofilled_out,
        inst_addr_out => inst_addr_out
    );
end Behavioural;