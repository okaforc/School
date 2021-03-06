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

    -- delay
    constant delay : Time := 2ns;
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
        in0 <= x"20332400";
        in1 <= x"203323ff";
        in2 <= x"203323fe";
        in3 <= x"203323fd";
        in4 <= x"203323fc";
        in5 <= x"203323fb";
        in6 <= x"203323fa";
        in7 <= x"203323f9";
        in8 <= x"203323f8";
        in9 <= x"203323f7";
        in10 <= x"203323f6";
        in11 <= x"203323f5";
        in12 <= x"203323f4";
        in13 <= x"203323f3";
        in14 <= x"203323f2";
        in15 <= x"203323f1";
        in16 <= x"203323f0";
        in17 <= x"203323ef";
        in18 <= x"203323ee";
        in19 <= x"203323ed";
        in20 <= x"203323ec";
        in21 <= x"203323eb";
        in22 <= x"203323ea";
        in23 <= x"203323e9";
        in24 <= x"203323e8";
        in25 <= x"203323e7";
        in26 <= x"203323e6";
        in27 <= x"203323e5";
        in28 <= x"203323e4";
        in29 <= x"203323e3";
        in30 <= x"203323e2";
        in31 <= x"203323e1";

        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '0';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '0';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '0';
        s3 <= '1';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '0';
        s4 <= '1';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '0';
        WAIT FOR delay;
        s0 <= '1';
        s1 <= '1';
        s2 <= '1';
        s3 <= '1';
        s4 <= '1';
        WAIT;
    END PROCESS;
END Behavioral;