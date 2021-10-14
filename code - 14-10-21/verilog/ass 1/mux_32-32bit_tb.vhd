LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY mux32_tb IS
END mux32_tb;

ARCHITECTURE Behavioral OF mux32_tb IS
    COMPONENT mux32
        PORT (
        in0, in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, in11, in12,
        in13, in14, in15, in16, in17, in18, in19, in20, in21, in22, in23,
        in24, in25, in26, in27, in28, in29, in30, in31 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        s0, s1, s2, s3, s4 : IN STD_LOGIC;
        z : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
        );
    END COMPONENT;

    -- input
    SIGNAL in0 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in1 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in2 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in3 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in4 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in5 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in6 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in7 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in8 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in9 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in10 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in11 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in12 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in13 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in14 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in15 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in16 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in17 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in18 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in19 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in20 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in21 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in22 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in23 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in24 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in25 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in26 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in27 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in28 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in29 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in30 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL in31 : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');

    SIGNAL s0 : STD_LOGIC := '0';
    SIGNAL s1 : STD_LOGIC := '0';
    SIGNAL s2 : STD_LOGIC := '0';
    SIGNAL s3 : STD_LOGIC := '0';
    SIGNAL s4 : STD_LOGIC := '0';

    -- output
    SIGNAL Z : STD_LOGIC_VECTOR (31 DOWNTO 0) := (OTHERS => '0');
BEGIN
    uut : mux32 PORT MAP(
        in0 => in0, in1 => in1, in2 => in2, in3 => in3, in4 => in4, in5 => in5, in6 => in6, in7 => in7, in8 => in8, in9 => in9, in10 => in10, in11 => in11,
        in12 => in12, in13 => in13, in14 => in14, in15 => in15, in16 => in16, in17 => in17, in18 => in18, in19 => in19, in20 => in20, in21 => in21, in22 => in22,
        in23 => in23, in24 => in24, in25 => in25, in26 => in26, in27 => in27, in28 => in28, in29 => in29, in30 => in30, in31 => in31,
        s0 => s0, s1 => s1, s2 => s2, s3 => s3, s4 => s4,
        Z => Z
    );
    stim_proc : PROCESS
    BEGIN
        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';

        in0 <= "10110110001111101101100101001000";
        WAIT FOR 5ns;
        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        in1 <= "11111100000111111011001000111100";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        in2 <= "11100001001001100100000011110100";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';

        in3 <= "10111010011101001110010010000000";
        WAIT FOR 5ns;
        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';
        in4 <= "10011010110100001001000001011100";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';
        in5 <= "11101101101101110110011000100000";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';
        in6 <= "10110101010111101000110101100010";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        in7 <= "11011001011100100111110001100111";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';
        in8 <= "10111101000010000000110010110000";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        in9 <= "10101101000001000001101000110001";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        in10 <= "10001011100000000011001011100010";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';
        in11 <= "10101100011110001001010001000011";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';

        in12 <= "10100001100111011000001000011101";
        WAIT FOR 5ns;
        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';
        in13 <= "11100010100110001101011110100000";
        WAIT FOR 5ns;

        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';

        in14 <= "11111111101001100111011101100010";
        WAIT FOR 5ns;
        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        in15 <= "11010111011111100101011101100101";
        WAIT FOR 5ns;

        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';
        in16 <= "10110000101000111110100000000000";
        WAIT FOR 5ns;

        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        in17 <= "11100011011010010010010011110111";
        WAIT FOR 5ns;

        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        in18 <= "10001000111110000100001000010001";
        WAIT FOR 5ns;

        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';

        in19 <= "10101101000000100000001110001101";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';
        in20 <= "11110010010000110001010110010010";
        WAIT FOR 5ns;

        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';

        in21 <= "11100101101011000100101000110001";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';

        in22 <= "10011001001000000111001010100011";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';

        in23 <= "10111110101000101100111001110011";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';

        in24 <= "11001110010001111000110010101011";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';

        in25 <= "11000011010110001000000100010010";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';

        in26 <= "10000010111101110100010000011101";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';

        in27 <= "11111101011001100101111011011100";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';

        in28 <= "11110010111100101110111111001000";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';

        in29 <= "10010000001000010001111111110110";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';

        in30 <= "10100100101100100111110111010011";
        WAIT FOR 5ns;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        in31 <= "01000110001100011010000011101001";
        WAIT FOR 5ns;

    END PROCESS;
END Behavioral;