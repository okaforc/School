LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY register_file IS
    PORT (
        -- Inputs
        a_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- A data mux
        b_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- B data mux
        d_add : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- decoder 5 to 32
        load_wr : IN STD_LOGIC; -- write signal
        data : IN STD_LOGIC_VECTOR (31 DOWNTO 0); -- input data
        clk : IN STD_LOGIC; -- clock for register

        -- Outputs
        data_a : OUT STD_LOGIC_VECTOR(31 DOWNTO 0); -- A data
        data_b : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) -- B data
    );
END register_file;

ARCHITECTURE Behavioral OF register_file IS
    -- components
    -- 32 bit Register for register file
    COMPONENT reg32
        PORT (
            D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            load : IN STD_LOGIC;
            Clk : IN STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    -- 5 to 32 Decoder
    COMPONENT decoder_5_to_32
        PORT (
            A : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13,
            D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26,
            D27, D28, D29, D30, D31 : OUT STD_LOGIC
        );
    END COMPONENT;

    -- 32 to 1 line multiplexer
    COMPONENT mux32
        PORT (
            in0, in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11, in12,
            in13, in14, in15, in16, in17, in18, in19, in20, in21, in22, in23,
            in24, in25, in26, in27, in28, in29, in30, in31 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            s0, s1, s2, s3, s4 : IN STD_LOGIC;
            z : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
        );
    END COMPONENT;
    -- signals
    SIGNAL load_reg0, load_reg1, load_reg2, load_reg3, load_reg4, load_reg5, load_reg6, load_reg7, load_reg8, load_reg9,
    load_reg10, load_reg11, load_reg12, load_reg13, load_reg14, load_reg15, load_reg16, load_reg17, load_reg18, load_reg19,
    load_reg20, load_reg21, load_reg22, load_reg23, load_reg24, load_reg25, load_reg26, load_reg27, load_reg28, load_reg29,
    load_reg30, load_reg31 : STD_LOGIC;
    SIGNAL dec_out : STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL reg0_q, reg1_q, reg2_q, reg3_q, reg4_q, reg5_q, reg6_q, reg7_q, reg8_q, reg9_q, reg10_q, reg11_q, reg12_q,
    reg13_q, reg14_q, reg15_q, reg16_q, reg17_q, reg18_q, reg19_q, reg20_q, reg21_q, reg22_q, reg23_q, reg24_q, reg25_q,
    reg26_q, reg27_q, reg28_q, reg29_q, reg30_q, reg31_q : STD_LOGIC_VECTOR(31 DOWNTO 0);

BEGIN
    -- port maps ;-)
    -- register 0
    reg_00 : reg32 PORT MAP(
        D => data,
        load => load_reg0,
        Clk => Clk,
        Q => reg0_q
    );
    -- register 1
    reg_01 : reg32 PORT MAP(
        D => data,
        load => load_reg1,
        Clk => Clk,
        Q => reg1_q
    );
    -- register 2
    reg_02 : reg32 PORT MAP(
        D => data,
        load => load_reg2,
        Clk => Clk,
        Q => reg2_q
    );
    -- register 3
    reg_03 : reg32 PORT MAP(
        D => data,
        load => load_reg3,
        Clk => Clk,
        Q => reg3_q
    );
    -- register 4
    reg_04 : reg32 PORT MAP(
        D => data,
        load => load_reg4,
        Clk => Clk,
        Q => reg4_q
    );
    -- register 5
    reg_05 : reg32 PORT MAP(
        D => data,
        load => load_reg5,
        Clk => Clk,
        Q => reg5_q
    );
    -- register 6
    reg_06 : reg32 PORT MAP(
        D => data,
        load => load_reg6,
        Clk => Clk,
        Q => reg6_q
    );
    -- register 7
    reg_07 : reg32 PORT MAP(
        D => data,
        load => load_reg7,
        Clk => Clk,
        Q => reg7_q
    );
    -- register 8
    reg_08 : reg32 PORT MAP(
        D => data,
        load => load_reg8,
        Clk => Clk,
        Q => reg8_q
    );
    -- register 9
    reg_09 : reg32 PORT MAP(
        D => data,
        load => load_reg9,
        Clk => Clk,
        Q => reg9_q
    );
    -- register 10
    reg_10 : reg32 PORT MAP(
        D => data,
        load => load_reg10,
        Clk => Clk,
        Q => reg10_q
    );
    -- register 11
    reg_11 : reg32 PORT MAP(
        D => data,
        load => load_reg11,
        Clk => Clk,
        Q => reg11_q
    );
    -- register 12
    reg_12 : reg32 PORT MAP(
        D => data,
        load => load_reg12,
        Clk => Clk,
        Q => reg12_q
    );
    -- register 13
    reg_13 : reg32 PORT MAP(
        D => data,
        load => load_reg13,
        Clk => Clk,
        Q => reg13_q
    );
    -- register 14
    reg_14 : reg32 PORT MAP(
        D => data,
        load => load_reg14,
        Clk => Clk,
        Q => reg14_q
    );
    -- register 15
    reg_15 : reg32 PORT MAP(
        D => data,
        load => load_reg15,
        Clk => Clk,
        Q => reg15_q
    );
    -- register 16
    reg_16 : reg32 PORT MAP(
        D => data,
        load => load_reg16,
        Clk => Clk,
        Q => reg16_q
    );
    -- register 17
    reg_17 : reg32 PORT MAP(
        D => data,
        load => load_reg17,
        Clk => Clk,
        Q => reg17_q
    );
    -- register 18
    reg_18 : reg32 PORT MAP(
        D => data,
        load => load_reg18,
        Clk => Clk,
        Q => reg18_q
    );
    -- register 19
    reg_19 : reg32 PORT MAP(
        D => data,
        load => load_reg19,
        Clk => Clk,
        Q => reg19_q
    );
    -- register 20
    reg_20 : reg32 PORT MAP(
        D => data,
        load => load_reg20,
        Clk => Clk,
        Q => reg20_q
    );
    -- register 21
    reg_21 : reg32 PORT MAP(
        D => data,
        load => load_reg21,
        Clk => Clk,
        Q => reg21_q
    );
    -- register 22
    reg_22 : reg32 PORT MAP(
        D => data,
        load => load_reg22,
        Clk => Clk,
        Q => reg22_q
    );
    -- register 23
    reg_23 : reg32 PORT MAP(
        D => data,
        load => load_reg23,
        Clk => Clk,
        Q => reg23_q
    );
    -- register 24
    reg_24 : reg32 PORT MAP(
        D => data,
        load => load_reg24,
        Clk => Clk,
        Q => reg24_q
    );
    -- register 25
    reg_25 : reg32 PORT MAP(
        D => data,
        load => load_reg25,
        Clk => Clk,
        Q => reg25_q
    );
    -- register 26
    reg_26 : reg32 PORT MAP(
        D => data,
        load => load_reg26,
        Clk => Clk,
        Q => reg26_q
    );
    -- register 27
    reg_27 : reg32 PORT MAP(
        D => data,
        load => load_reg27,
        Clk => Clk,
        Q => reg27_q
    );
    -- register 28
    reg_28 : reg32 PORT MAP(
        D => data,
        load => load_reg28,
        Clk => Clk,
        Q => reg28_q
    );
    -- register 29
    reg_29 : reg32 PORT MAP(
        D => data,
        load => load_reg29,
        Clk => Clk,
        Q => reg29_q
    );
    -- register 30
    reg_30 : reg32 PORT MAP(
        D => data,
        load => load_reg30,
        Clk => Clk,
        Q => reg30_q
    );
    -- register 31
    reg_31 : reg32 PORT MAP(
        D => data,
        load => load_reg31,
        Clk => Clk,
        Q => reg31_q
    );
    -- Destination register decoder
    des_decoder_5to32 : decoder_5_to_32 PORT MAP(
        A(0) => d_add(0),
        A(1) => d_add(1),
        A(2) => d_add(2),
        A(3) => d_add(3),
        A(4) => d_add(4),

        D0 => dec_out(0),
        D1 => dec_out(1),
        D2 => dec_out(2),
        D3 => dec_out(3),
        D4 => dec_out(4),
        D5 => dec_out(5),
        D6 => dec_out(6),
        D7 => dec_out(7),
        D8 => dec_out(8),
        D9 => dec_out(9),
        D10 => dec_out(10),
        D11 => dec_out(11),
        D12 => dec_out(12),
        D13 => dec_out(13),
        D14 => dec_out(14),
        D15 => dec_out(15),
        D16 => dec_out(16),
        D17 => dec_out(17),
        D18 => dec_out(18),
        D19 => dec_out(19),
        D20 => dec_out(20),
        D21 => dec_out(21),
        D22 => dec_out(22),
        D23 => dec_out(23),
        D24 => dec_out(24),
        D25 => dec_out(25),
        D26 => dec_out(26),
        D27 => dec_out(27),
        D28 => dec_out(28),
        D29 => dec_out(29),
        D30 => dec_out(30),
        D31 => dec_out(31)
    );

    load_reg0 <= dec_out(0) AND load_wr AFTER 2 ns;
    load_reg1 <= dec_out(1) AND load_wr AFTER 2 ns;
    load_reg2 <= dec_out(2) AND load_wr AFTER 2 ns;
    load_reg3 <= dec_out(3) AND load_wr AFTER 2 ns;
    load_reg4 <= dec_out(4) AND load_wr AFTER 2 ns;
    load_reg5 <= dec_out(5) AND load_wr AFTER 2 ns;
    load_reg6 <= dec_out(6) AND load_wr AFTER 2 ns;
    load_reg7 <= dec_out(7) AND load_wr AFTER 2 ns;
    load_reg8 <= dec_out(8) AND load_wr AFTER 2 ns;
    load_reg9 <= dec_out(9) AND load_wr AFTER 2 ns;
    load_reg10 <= dec_out(10) AND load_wr AFTER 2 ns;
    load_reg11 <= dec_out(11) AND load_wr AFTER 2 ns;
    load_reg12 <= dec_out(12) AND load_wr AFTER 2 ns;
    load_reg13 <= dec_out(13) AND load_wr AFTER 2 ns;
    load_reg14 <= dec_out(14) AND load_wr AFTER 2 ns;
    load_reg15 <= dec_out(15) AND load_wr AFTER 2 ns;
    load_reg16 <= dec_out(16) AND load_wr AFTER 2 ns;
    load_reg17 <= dec_out(17) AND load_wr AFTER 2 ns;
    load_reg18 <= dec_out(18) AND load_wr AFTER 2 ns;
    load_reg19 <= dec_out(19) AND load_wr AFTER 2 ns;
    load_reg20 <= dec_out(20) AND load_wr AFTER 2 ns;
    load_reg21 <= dec_out(21) AND load_wr AFTER 2 ns;
    load_reg22 <= dec_out(22) AND load_wr AFTER 2 ns;
    load_reg23 <= dec_out(23) AND load_wr AFTER 2 ns;
    load_reg24 <= dec_out(24) AND load_wr AFTER 2 ns;
    load_reg25 <= dec_out(25) AND load_wr AFTER 2 ns;
    load_reg26 <= dec_out(26) AND load_wr AFTER 2 ns;
    load_reg27 <= dec_out(27) AND load_wr AFTER 2 ns;
    load_reg28 <= dec_out(28) AND load_wr AFTER 2 ns;
    load_reg29 <= dec_out(29) AND load_wr AFTER 2 ns;
    load_reg30 <= dec_out(30) AND load_wr AFTER 2 ns;
    load_reg31 <= dec_out(31) AND load_wr AFTER 2 ns;

    -- 32 to 1 source register multiplexer
    mux32_1 : mux32 PORT MAP(
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
        s0 => a_add(0),
        s1 => a_add(1),
        s2 => a_add(2),
        s3 => a_add(3),
        s4 => a_add(4),
        Z => data_a
    );
    -- 32 to 1 source register multiplexer
    mux32_2 : mux32 PORT MAP(
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
        s0 => b_add(0),
        s1 => b_add(1),
        s2 => b_add(2),
        s3 => b_add(3),
        s4 => b_add(4),
        Z => data_b
    );
END Behavioral;