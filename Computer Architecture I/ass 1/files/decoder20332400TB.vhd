LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY decoder_tb IS
END decoder_tb;

ARCHITECTURE Behavioural OF decoder_tb IS
    COMPONENT decoder_5_to_32
        PORT (
            A : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
            D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13,
            D14, D15, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26,
            D27, D28, D29, D30, D31 : OUT STD_LOGIC
        );
    END COMPONENT;

    SIGNAL A : STD_LOGIC_VECTOR(4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15,
    D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26, D27, D28, D29, D30
    D31 : STD_LOGIC;

BEGIN

    uut : decoder_5_to_32 PORT MAP(
        A => A,
        D0 => D0, D1 => D1, D2 => D2, D3 => D3, D4 => D4, D5 => D5, D6 => D6, D7 => D7, D8 => D8, D9 => D9, D10 => D10, D11 => D11,
        D12 => D12, D13 => D13, D14 => D14, D15 => D15, D16 => D16, D17 => D17, D18 => D18, D19 => D19, D20 => D20, D21 => D21, D22 => D22,
        D23 => D23, D24 => D24, D25 => D25, D26 => D26, D27 => D27, D28 => D28, D29 => D29, D30 => D30, D31 => D31
    );
    stim_proc : PROCESS
    BEGIN

        A <= "00000";
        WAIT FOR 5 ns;
        A <= "00001";
        WAIT FOR 5 ns;
        A <= "00010";
        WAIT FOR 5 ns;
        A <= "00011";
        WAIT FOR 5 ns;
        A <= "00100";
        WAIT FOR 5 ns;
        A <= "00101";
        WAIT FOR 5 ns;
        A <= "00110";
        WAIT FOR 5 ns;
        A <= "00111";
        WAIT FOR 5 ns;
        A <= "01000";
        WAIT FOR 5 ns;
        A <= "01001";
        WAIT FOR 5 ns;
        A <= "01010";
        WAIT FOR 5 ns;
        A <= "01011";
        WAIT FOR 5 ns;
        A <= "01100";
        WAIT FOR 5 ns;
        A <= "01101";
        WAIT FOR 5 ns;
        A <= "01110";
        WAIT FOR 5 ns;
        A <= "01111";
        WAIT FOR 5 ns;
        A <= "10000";
        WAIT FOR 5 ns;
        A <= "10001";
        WAIT FOR 5 ns;
        A <= "10010";
        WAIT FOR 5 ns;
        A <= "10011";
        WAIT FOR 5 ns;
        A <= "10100";
        WAIT FOR 5 ns;
        A <= "10101";
        WAIT FOR 5 ns;
        A <= "10110";
        WAIT FOR 5 ns;
        A <= "10111";
        WAIT FOR 5 ns;
        A <= "11000";
        WAIT FOR 5 ns;
        A <= "11001";
        WAIT FOR 5 ns;
        A <= "11010";
        WAIT FOR 5 ns;
        A <= "11011";
        WAIT FOR 5 ns;
        A <= "11100";
        WAIT FOR 5 ns;
        A <= "11101";
        WAIT FOR 5 ns;
        A <= "11110";
        WAIT FOR 5 ns;
        A <= "11111";
        WAIT;
    END PROCESS;

END;