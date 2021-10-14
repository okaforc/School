LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY register_file IS
    PORT (
        src_s0 : IN STD_LOGIC;
        src_s1 : IN STD_LOGIC;
        des_A0 : IN STD_LOGIC;
        des_A1 : IN STD_LOGIC;
        Clk : IN STD_LOGIC;
        data_src : IN STD_LOGIC;
        data : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        reg0 : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        reg1 : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        reg2 : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
        reg3 : OUT STD_LOGIC_VECTOR(3 DOWNTO 0));
END register_file;

ARCHITECTURE Behavioral OF register_file IS
    -- components
    -- 4 bit Register for register file
    COMPONENT reg4
        PORT (
            D : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            load : IN STD_LOGIC;
            Clk : IN STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
        );
    END COMPONENT;

    -- 2 to 4 Decoder
    COMPONENT decoder_2to4
        PORT (
            A0 : IN STD_LOGIC;
            A1 : IN STD_LOGIC;
            Q0 : OUT STD_LOGIC;
            Q1 : OUT STD_LOGIC;
            Q2 : OUT STD_LOGIC;
            Q3 : OUT STD_LOGIC
        );
    END COMPONENT;
    -- 2 to 1 line multiplexer
    COMPONENT mux2_4bit
        PORT (
            In0 : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            In1 : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            s : IN STD_LOGIC;
            Z : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
        );
    END COMPONENT;
    -- 4 to 1 line multiplexer
    COMPONENT mux4_4bit
        PORT (
            In0 : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            In1 : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            In2 : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            In3 : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
            S0 : IN STD_LOGIC;
            S1 : IN STD_LOGIC;
            Z : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
        );
    END COMPONENT;
    -- signals
    SIGNAL load_reg0, load_reg1, load_reg2, load_reg3 : STD_LOGIC;
    SIGNAL reg0_q, reg1_q, reg2_q, reg3_q,
    data_src_mux_out, src_reg : STD_LOGIC_VECTOR(3 DOWNTO 0);

BEGIN
    -- port maps ;-)
    -- register 0
    reg00 : reg4 PORT MAP(
        D => data_src_mux_out,
        load => load_reg0,
        Clk => Clk,
        Q => reg0_q
    );
    -- register 1
    reg01 : reg4 PORT MAP(
        D => data_src_mux_out,
        load => load_reg1,
        Clk => Clk,
        Q => reg1_q
    );
    -- register 2
    reg02 : reg4 PORT MAP(
        D => data_src_mux_out,
        load => load_reg2,
        Clk => Clk,
        Q => reg2_q
    );
    -- register 3
    reg03 : reg4 PORT MAP(
        D => data_src_mux_out,
        load => load_reg3,
        Clk => Clk,
        Q => reg3_q
    );
    -- Destination register decoder
    des_decoder_2to4 : decoder_2to4 PORT MAP(
        A0 => des_A0,
        A1 => des_A1,
        Q0 => load_reg0,
        Q1 => load_reg1,
        Q2 => load_reg2,
        Q3 => load_reg3
    );
    -- 2 to 1 Data source multiplexer
    data_src_mux2_4bit : mux2_4bit PORT MAP(
        In0 => data,
        In1 => src_reg,
        s => data_src,
        Z => data_src_mux_out
    );
    -- 4 to 1 source register multiplexer
    Inst_mux4_4bit : mux4_4bit PORT MAP(
        In0 => reg0_q,
        In1 => reg1_q,
        In2 => reg2_q,
        In3 => reg3_q,
        S0 => src_s0,
        S1 => src_s1,
        Z => src_reg
    );
    reg0 <= reg0_q;
    reg1 <= reg1_q;
    reg2 <= reg2_q;
    reg3 <= reg3_q;
END Behavioral;