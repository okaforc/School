LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY register_file_tb IS
END register_file_tb;

ARCHITECTURE Behavioural OF register_file_tb IS
    COMPONENT register_file
        PORT (
            -- Inputs
            src_s : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- mux 32 to 32 bit
            des_a : IN STD_LOGIC_VECTOR (4 DOWNTO 0); -- decoder 5 to 32
            clk : IN STD_LOGIC; -- clock for register
            data_src : IN STD_LOGIC; -- mux 2 to 32 bit
            data : IN STD_LOGIC_VECTOR (31 DOWNTO 0); -- mux 2 to 32 bit

            -- Outputs
            reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11, reg12,
            reg13, reg14, reg15, reg16, reg17, reg18, reg19, reg20, reg21, reg22, reg23,
            reg24, reg25, reg26, reg27, reg28, reg29, reg30, reg31 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    SIGNAL src_s : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL des_a : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL data_src : STD_LOGIC := '0';
    SIGNAL data : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11, reg12, reg13, reg14, reg15,
    reg16, reg17, reg18, reg19, reg20, reg21, reg22, reg23, reg24, reg25, reg26, reg27, reg28,
    reg29, reg30, reg31 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');

BEGIN
    uut : register_file PORT MAP(
        reg0 => reg0, reg1 => reg1, reg2 => reg2, reg3 => reg3, reg4 => reg4, reg5 => reg5, reg6 => reg6, reg7 => reg7,
        reg8 => reg8, reg9 => reg9, reg10 => reg10, reg11 => reg11, reg12 => reg12, reg13 => reg13, reg14 => reg14, reg15 => reg15,
        reg16 => reg16, reg17 => reg17, reg18 => reg18, reg19 => reg19, reg20 => reg20, reg21 => reg21, reg22 => reg22, reg23 => reg23,
        reg24 => reg24, reg25 => reg25, reg26 => reg26, reg27 => reg27, reg28 => reg28, reg29 => reg29, reg30 => reg30, reg31 => reg31,
        src_s => src_s, des_a => des_a, clk => clk, data_src => data_src, data => data
    );
    clk <= NOT clk AFTER 5ns; -- invert clock every 5ns
    stim_proc : PROCESS
    BEGIN
        src_s <= "00000";
        des_a <= "00000";
        data <= x"20332400";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00001";
        des_a <= "00001";
        data <= x"203323ff";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00010";
        des_a <= "00010";
        data <= x"203323fe";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00011";
        des_a <= "00011";
        data <= x"203323fd";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00100";
        des_a <= "00100";
        data <= x"203323fc";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00101";
        des_a <= "00101";
        data <= x"203323fb";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00110";
        des_a <= "00110";
        data <= x"203323fa";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "00111";
        des_a <= "00111";
        data <= x"203323f9";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01000";
        des_a <= "01000";
        data <= x"203323f8";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01001";
        des_a <= "01001";
        data <= x"203323f7";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01010";
        des_a <= "01010";
        data <= x"203323f6";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01011";
        des_a <= "01011";
        data <= x"203323f5";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01100";
        des_a <= "01100";
        data <= x"203323f4";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01101";
        des_a <= "01101";
        data <= x"203323f3";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01110";
        des_a <= "01110";
        data <= x"203323f2";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "01111";
        des_a <= "01111";
        data <= x"203323f1";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10000";
        des_a <= "10000";
        data <= x"203323f0";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10001";
        des_a <= "10001";
        data <= x"203323ef";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10010";
        des_a <= "10010";
        data <= x"203323ee";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10011";
        des_a <= "10011";
        data <= x"203323ed";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10100";
        des_a <= "10100";
        data <= x"203323ec";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10101";
        des_a <= "10101";
        data <= x"203323eb";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10110";
        des_a <= "10110";
        data <= x"203323ea";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "10111";
        des_a <= "10111";
        data <= x"203323e9";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11000";
        des_a <= "11000";
        data <= x"203323e8";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11001";
        des_a <= "11001";
        data <= x"203323e7";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11010";
        des_a <= "11010";
        data <= x"203323e6";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11011";
        des_a <= "11011";
        data <= x"203323e5";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11100";
        des_a <= "11100";
        data <= x"203323e4";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11101";
        des_a <= "11101";
        data <= x"203323e3";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11110";
        des_a <= "11110";
        data <= x"203323e2";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;

        src_s <= "11111";
        des_a <= "11111";
        data <= x"203323e1";
        WAIT FOR 5ns;
        data_src <= '1';
        WAIT FOR 5ns;
        data_src <= '0';
        WAIT FOR 10ns;
    END PROCESS;
END Behavioural;