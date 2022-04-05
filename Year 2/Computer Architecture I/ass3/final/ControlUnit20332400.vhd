LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

entity ControlUnit is
    port (
        -- in - takes in instruction into IR, control memory controls, clock, reset
        instruction : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        clk, resetPC, resetCAR, v, c, n, z : IN STD_LOGIC;
        FL, RZ, RN, RC, RV, PL, PI, IL, MC : in std_logic;
        NA : in std_logic_vector(16 downto 0);
        MS : in STD_LOGIC_VECTOR(2 DOWNTO 0);

        -- out - gives out register file inputs, zero fill output, instruction address for memory
        DR, SA, SB : out STD_LOGIC_VECTOR(4 DOWNTO 0);
        car_out : out std_logic_vector(16 downto 0);
        zerofilled_out, inst_addr_out : out std_logic_vector(31 downto 0)
    );
end ControlUnit ;

architecture Behavioural of ControlUnit is
    component extend port (
        data_in : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    component PC port (
        data_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        PL, PI, reset, clk : IN STD_LOGIC;
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    component InstReg port (
        IR : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        IL, clk : IN STD_LOGIC;
        opcode : OUT STD_LOGIC_VECTOR(16 DOWNTO 0);
        SA, SB, DR : OUT STD_LOGIC_VECTOR(4 DOWNTO 0)
    );
    end component;

    component mux8_1bit port (
        MS : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
        bits_in : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        Z : OUT STD_LOGIC
    );
    end component;

    component mux2_17bit port (
        In0 : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        In1 : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        s : IN STD_LOGIC;
        Z : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
    end component;

    component CAR port (
        data_in : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        clk, load, reset : IN STD_LOGIC;
        ctrl : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
    end component;

    component zero_fill port(
        data_in : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        data_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
    );
    end component;

    component flipflops port(
        clk, FL, v, c, n, z : IN STD_LOGIC;
        reset : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        Q : OUT STD_LOGIC_VECTOR (3 DOWNTO 0)
    );
    end component;
    
    signal muxS_out, NC, NZ : std_logic;
    signal VCNZ : std_logic_vector(3 downto 0);
    signal IR_SA : std_logic_vector(4 downto 0);
    signal IR_SBDR : std_logic_vector(9 downto 0);
    signal muxC_out, IR_opcode : std_logic_vector(16 downto 0);
    signal zero_out, extend_out : std_logic_vector(31 downto 0);



begin   

    NC <= not (VCNZ(1));
    NZ <= not (VCNZ(3));

    ext : extend port map(
        data_in => IR_SBDR,
        data_out => extend_out
    );

    ProgCount : PC port map(
        data_in => extend_out,
        PL => PL,
        PI => PI,
        reset => resetPC,
        clk => clk,
        data_out => inst_addr_out -- final output
    );

    IReg : InstReg port map(
        IL => IL,
        clk => clk,
        IR => instruction,
        opcode => IR_opcode,
        SA => IR_SA,
        SB => IR_SBDR(4 downto 0),
        DR => IR_SBDR(9 downto 5)
    );

    SA <= IR_SA; -- final output
    SB <= IR_SBDR(4 downto 0); -- final output
    DR <= IR_SBDR(9 downto 5); -- final output

    muxS : mux8_1bit port map(
        MS => MS,
        bits_in(0) => '0',
        bits_in(1) => '1',
        bits_in(2) => VCNZ(1),
        bits_in(3) => VCNZ(0),
        bits_in(4) => VCNZ(3),
        bits_in(5) => VCNZ(2),
        bits_in(6) => NC,
        bits_in(7) => NZ,
        Z => muxS_out
    );

    muxC : mux2_17bit port map(
        In0 => NA,
        In1 => IR_opcode,
        s => MC,
        Z => muxC_out
    );

    CtrlAddReg : CAR port map(
        data_in => muxC_out,
        clk => clk,
        reset => resetCAR,
        load => muxS_out,
        ctrl => car_out
    );

    z_fill : zero_fill port map(
        data_in => IR_SBDR(4 downto 0),
        data_out => zero_out
    );
    
    zerofilled_out <= zero_out; -- final output

    dff : flipflops port map(
        clk => clk,
        FL => FL,
        v => v,
        c => c,
        n => n,
        z => z,
        reset(0) => RV,
        reset(1) => RC,
        reset(2) => RN,
        reset(3) => RZ,
        Q => VCNZ
    );

end Behavioural ; -- Behavioural