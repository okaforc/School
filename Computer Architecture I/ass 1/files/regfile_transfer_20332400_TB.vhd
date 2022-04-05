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
            data : IN STD_LOGIC_VECTOR (31 DOWNTO 0) -- mux 2 to 32 bit
        );
    END COMPONENT;

    SIGNAL src_s : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL des_a : STD_LOGIC_VECTOR (4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL clk : STD_LOGIC := '0';
    SIGNAL data_src : STD_LOGIC := '0';
    SIGNAL data : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');

    CONSTANT period : TIME := 10ns;

BEGIN
    uut : register_file PORT MAP(
        src_s => src_s,
        des_a => des_a,
        clk => clk,
        data_src => data_src,
        data => data
    );

    clk <= NOT clk AFTER period/2; -- invert clock every period/2 seconds
    stim_proc : PROCESS
    BEGIN

        -- switch to outside data
        data_src <= '0';

        -- destination register 0
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00000";
        data <= x"20332400";

        -- destination register 1
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00001";
        data <= x"203323ff";

        -- destination register 2
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00010";
        data <= x"203323fe";

        -- destination register 3
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00011";
        data <= x"203323fd";

        -- destination register 4
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00100";
        data <= x"203323fc";

        -- destination register 5
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00101";
        data <= x"203323fb";

        -- destination register 6
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00110";
        data <= x"203323fa";

        -- destination register 7
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "00111";
        data <= x"203323f9";

        -- destination register 8
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01000";
        data <= x"203323f8";

        -- destination register 9
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01001";
        data <= x"203323f7";

        -- destination register 10
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01010";
        data <= x"203323f6";

        -- destination register 11
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01011";
        data <= x"203323f5";

        -- destination register 12
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01100";
        data <= x"203323f4";

        -- destination register 13
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01101";
        data <= x"203323f3";

        -- destination register 14
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01110";
        data <= x"203323f2";

        -- destination register 15
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "01111";
        data <= x"203323f1";

        -- destination register 16
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10000";
        data <= x"203323f0";

        -- destination register 17
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10001";
        data <= x"203323ef";

        -- destination register 18
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10010";
        data <= x"203323ee";

        -- destination register 19
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10011";
        data <= x"203323ed";

        -- destination register 20
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10100";
        data <= x"203323ec";

        -- destination register 21
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10101";
        data <= x"203323eb";

        -- destination register 22
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10110";
        data <= x"203323ea";

        -- destination register 23
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "10111";
        data <= x"203323e9";

        -- destination register 24
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11000";
        data <= x"203323e8";

        -- destination register 25
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11001";
        data <= x"203323e7";

        -- destination register 26
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11010";
        data <= x"203323e6";

        -- destination register 27
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11011";
        data <= x"203323e5";

        -- destination register 28
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11100";
        data <= x"203323e4";

        -- destination register 29
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11101";
        data <= x"203323e3";

        -- destination register 30
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11110";
        data <= x"203323e2";

        -- destination register 31
        WAIT UNTIL clk'event AND clk = '1';
        des_a <= "11111";
        data <= x"203323e1";

        -- Transfering data between source and destination registers

        -- switch to 32-32 bit multiplexer output
        data_src <= '1';

        -- 0 to 4
        -- 1 to 8
        -- 2 to 12
        -- 3 to 16
        -- 4 to 20
        -- 5 to 24
        -- 6 to 28
        -- 7 to 32
        -- 8 to 0
        -- 9 to 1
        -- 10 to 2

        -- source register 0 and destination register 4 
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00000";
        des_a <= "00100";

        -- source register 1 and destination register 8 
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00001";
        des_a <= "01000";

        -- source register 2 and destination register 12
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00010";
        des_a <= "01100";

        -- source register 3 and destination register 16
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00011";
        des_a <= "10000";

        -- source register 4 and destination register 20
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00100";
        des_a <= "10100";

        -- source register 5 and destination register 24
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00101";
        des_a <= "11000";

        -- source register 6 and destination register 28
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00110";
        des_a <= "11100";

        -- source register 7 and destination register 31
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "00111";
        des_a <= "11111";

        -- source register 8 and destination register 0
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "01000";
        des_a <= "00000";

        -- source register 9 and destination register 1
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "01001";
        des_a <= "00001";

        -- source register 10 and destination register 2
        WAIT UNTIL clk'event AND clk = '1';
        src_s <= "01010";
        des_a <= "00010";

        WAIT;
    END PROCESS;
END Behavioural;