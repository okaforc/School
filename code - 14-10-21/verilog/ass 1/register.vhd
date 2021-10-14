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
        reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11, reg12, reg13,
        reg14, reg15, reg16, reg17, reg18, reg19, reg20, reg21, reg22, reg23, reg24, reg25, reg26,
        reg27, reg28, reg29, reg30, reg31 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END register_file;

ARCHITECTURE Behavioral OF register_file IS
    -- components
    -- 4 bit Register for register file
    COMPONENT reg32
        PORT (
            D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            load : IN STD_LOGIC;
            Clk : IN STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- 5 to 32 Decoder
    COMPONENT decoder_5to32
        PORT (
            A0, A1, A2, A3, A4 : IN STD_LOGIC;
            Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17,
            Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28, Q29, Q30,
            Q31 : OUT STD_LOGIC);
    END COMPONENT;
    -- 2 to 1 line multiplexer
    COMPONENT mux2_32bit
        PORT (
            In0 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            In1 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            s : IN STD_LOGIC;
            Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    -- 32 to 1 line multiplexer
    COMPONENT mux4_4bit
        PORT (
            in0, in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11, in12,
            in13, in14, in15, in16, in17, in18, in19, in20, in21, in22, in23,
            in24, in25, in26, in27, in28, in29, in30, in31 : STD_LOGIC_VECTOR(31 DOWNTO 0);
            S0 : IN STD_LOGIC;
            S1 : IN STD_LOGIC;
            Z : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;
    -- signals
    SIGNAL load_reg0, load_reg1, load_reg2, load_reg3, load_reg4, load_reg5, load_reg6, load_reg7, load_reg8, load_reg9,
    load_reg10, load_reg11, load_reg12, load_reg13, load_reg14, load_reg15, load_reg16, load_reg17, load_reg18, load_reg19,
    load_reg20, load_reg21, load_reg22, load_reg23, load_reg24, load_reg25, load_reg26, load_reg27, load_reg28, load_reg29,
    load_reg30, load_reg31 : STD_LOGIC;
    SIGNAL reg0_q, reg1_q, reg2_q, reg3_q, reg4_q, reg5_q, reg6_q, reg7_q, reg8_q, reg9_q, reg10_q, reg11_q, reg12_q, 
    reg13_q, reg14_q, reg15_q, reg16_q, reg17_q, reg18_q, reg19_q, reg20_q, reg21_q, reg22_q, reg23_q, reg24_q, reg25_q, 
    reg26_q, reg27_q, reg28_q, reg29_q, reg30_q, reg31_q, 
    data_src_mux_out, src_reg : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN
    -- port maps ;-)
    -- register 0
    reg00 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg0,
        Clk => Clk,
        Q => reg0_q
    );
    -- register 1
    reg01 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg1,
        Clk => Clk,
        Q => reg1_q
    );
    -- register 2
    reg02 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg2,
        Clk => Clk,
        Q => reg2_q
    );
    -- register 3
    reg03 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg3,
        Clk => Clk,
        Q => reg3_q
    );
    -- register 4
    reg04 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg4,
        Clk => Clk,
        Q => reg4_q
    );
    -- register 5
    reg05 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg5,
        Clk => Clk,
        Q => reg5_q
    );
    -- register 6
    reg06 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg6,
        Clk => Clk,
        Q => reg6_q
    );
    -- register 7
    reg07 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg7,
        Clk => Clk,
        Q => reg7_q
    );
    -- register 8
    reg08 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg8,
        Clk => Clk,
        Q => reg8_q
    );
    -- register 9
    reg09 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg9,
        Clk => Clk,
        Q => reg9_q
    );
    -- register 10
    reg10 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg10,
        Clk => Clk,
        Q => reg10_q
    );
    -- register 11
    reg11 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg11,
        Clk => Clk,
        Q => reg11_q
    );
    -- register 12
    reg12 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg12,
        Clk => Clk,
        Q => reg12_q
    );
    -- register 13
    reg13 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg13,
        Clk => Clk,
        Q => reg13_q
    );
    -- register 14
    reg14 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg14,
        Clk => Clk,
        Q => reg14_q
    );
    -- register 15
    reg15 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg15,
        Clk => Clk,
        Q => reg15_q
    );
    -- register 16
    reg16 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg16,
        Clk => Clk,
        Q => reg16_q
    );
    -- register 17
    reg17 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg17,
        Clk => Clk,
        Q => reg17_q
    );
    -- register 18
    reg18 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg18,
        Clk => Clk,
        Q => reg18_q
    );
    -- register 19
    reg19 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg19,
        Clk => Clk,
        Q => reg19_q
    );
    -- register 20
    reg20 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg20,
        Clk => Clk,
        Q => reg20_q
    );
    -- register 21
    reg21 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg21,
        Clk => Clk,
        Q => reg21_q
    );
    -- register 22
    reg22 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg22,
        Clk => Clk,
        Q => reg22_q
    );
    -- register 23
    reg23 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg23,
        Clk => Clk,
        Q => reg23_q
    );
    -- register 24
    reg24 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg24,
        Clk => Clk,
        Q => reg24_q
    );
    -- register 25
    reg25 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg25,
        Clk => Clk,
        Q => reg25_q
    );
    -- register 26
    reg26 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg26,
        Clk => Clk,
        Q => reg26_q
    );
    -- register 27
    reg27 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg27,
        Clk => Clk,
        Q => reg27_q
    );
    -- register 28
    reg28 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg28,
        Clk => Clk,
        Q => reg28_q
    );
    -- register 29
    reg29 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg29,
        Clk => Clk,
        Q => reg29_q
    );
    -- register 30
    reg30 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg30,
        Clk => Clk,
        Q => reg30_q
    );
    -- register 31
    reg31 : reg32 PORT MAP(
        D => data_src_mux_out,
        load => load_reg31,
        Clk => Clk,
        Q => reg31_q
    );
    -- Destination register decoder
    des_decoder_2to32 : decoder_2to4 PORT MAP(
        A0 => des_A0,
        A1 => des_A1,
        Q0 => load_reg0,
        Q1 => load_reg1,
        Q2 => load_reg2,
        Q3 => load_reg3
    );
    -- 2 to 1 Data source multiplexer
    data_src_mux2_32bit : mux2_4bit PORT MAP(
        In0 => data,
        In1 => src_reg,
        s => data_src,
        Z => data_src_mux_out
    );
    -- 4 to 1 source register multiplexer
    Inst_mux32_32bit : mux32_32bit PORT MAP(
        in0 => reg0_q,
        in1 => reg1_q,
        in2 => reg2_q,
        in3 => reg3_q,
        in4 => reg4_q,
        in5 => reg5_q,
        in6 => reg6_q,
        in7 => reg7_q,
        in8 => reg8_q,
        in9 => reg9_q,
        in10 => reg10_q,
        in11 => reg11_q,
        in12 => reg12_q,
        in13 => reg13_q,
        in14 => reg14_q,
        in15 => reg15_q,
        in16 => reg16_q,
        in17 => reg17_q,
        in18 => reg18_q,
        in19 => reg19_q,
        in20 => reg20_q,
        in21 => reg21_q,
        in22 => reg22_q,
        in23 => reg23_q,
        in24 => reg24_q,
        in25 => reg25_q,
        in26 => reg26_q,
        in27 => reg27_q,
        in28 => reg28_q,
        in29 => reg29_q,
        in30 => reg30_q,
        in31 => reg31_q,
        S0 => src_s0,
        S1 => src_s1,
        Z => src_reg
    );
    reg0 <= reg0_q;
    reg1 <= reg1_q;
    reg2 <= reg2_q;
    reg3 <= reg3_q;
    reg4 <= reg4_q;
    reg5 <= reg5_q;
    reg6 <= reg6_q;
    reg7 <= reg7_q;
    reg8 <= reg8_q;
    reg9 <= reg9_q;
    reg10 <= reg10_q;
    reg11 <= reg11_q;
    reg12 <= reg12_q;
    reg13 <= reg13_q;
    reg14 <= reg14_q;
    reg15 <= reg15_q;
    reg16 <= reg16_q;
    reg17 <= reg17_q;
    reg18 <= reg18_q;
    reg19 <= reg19_q;
    reg20 <= reg20_q;
    reg21 <= reg21_q;
    reg22 <= reg22_q;
    reg23 <= reg23_q;
    reg24 <= reg24_q;
    reg25 <= reg25_q;
    reg26 <= reg26_q;
    reg27 <= reg27_q;
    reg28 <= reg28_q;
    reg29 <= reg29_q;
    reg30 <= reg30_q;
    reg31 <= reg31_q;
END Behavioral;