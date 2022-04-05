LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY MP_noControl_tb IS
END MP_noControl_tb;

ARCHITECTURE Behavioural OF MP_noControl_tb IS
    COMPONENT MP_noControl PORT (
        const_in, inst_addr_in : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        DR, SA, SB, FS : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
        RW, TD, TA, TB, MW, MB, MM, MD, clk : IN STD_LOGIC;
        mem_data_out, bus_a, bus_b : INOUT STD_LOGIC_VECTOR(31 DOWNTO 0); -- datapath output, memory input (buses); memory output, datapath input (data)
        v, c, n, z : OUT STD_LOGIC
        );
    END COMPONENT;
    SIGNAL const_in, inst_addr_in : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL DR, SA, SB, FS : STD_LOGIC_VECTOR(4 DOWNTO 0) := (OTHERS => '0');
    SIGNAL RW, TD, TA, TB, MW, MB, MM, MD, clk : STD_LOGIC := '0';
    SIGNAL mem_data_out, bus_a, bus_b : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    SIGNAL v, c, n, z : STD_LOGIC := '0';

    CONSTANT period : TIME := 6 ns;

BEGIN
    uut : MP_noControl PORT MAP(
        const_in => const_in,
        inst_addr_in => inst_addr_in,
        DR => DR,
        SA => SA,
        SB => SB,
        FS => FS,
        RW => RW,
        TD => TD,
        TA => TA,
        TB => TB,
        MW => MW,
        MB => MB,
        MM => MM,
        MD => MD,
        clk => clk,
        mem_data_out => mem_data_out,
        bus_a => bus_a,
        bus_b => bus_b,
        v => v,
        c => c,
        n => n,
        z => z
    );

    clk <= NOT clk AFTER 1 ns;
    stim_proc : PROCESS
    BEGIN
        MW <= '0'; -- unset memory write bit to read
        RW <= '1'; -- set register file write bit
        TD <= '0'; -- unset TD to choose a regular register
        TA <= '0'; -- unset TA to choose a regular register
        TB <= '0'; -- unset TB to choose a regular register
        const_in <= x"00001111"; -- random constant in. this value is not used.
        SA <= "00000"; -- use first mux input for a address
        SB <= "00000"; -- take in the same value as SA. SA will later be incremented, while SB is the data to be inputted.
        MM <= '1'; -- use inst_addr_in instead of bus_a
        MB <= '0'; -- use mux b input 0
        FS <= "00000"; -- set function select to increment data input and feed back into register file
        MD <= '1'; -- take output from memory to put back into register file
        -- wait for period;

        DR <= "00000"; -- use first register
        SA <= "00000";
        inst_addr_in <= x"00000000"; -- 0
        wait for period;
        DR <= "00001";
        SA <= "00001";
        inst_addr_in <= x"00000001"; -- 1
        wait for period;
        DR <= "00010";
        SA <= "00010";
        inst_addr_in <= x"00000002"; -- 2
        wait for period;
        DR <= "00011";
        SA <= "00011";
        inst_addr_in <= x"00000003"; -- 3
        wait for period;
        DR <= "00100";
        SA <= "00100";
        inst_addr_in <= x"00000004"; -- 4
        wait for period;
        DR <= "00101";
        SA <= "00101";
        inst_addr_in <= x"00000005"; -- 5
        wait for period;
        DR <= "00110";
        SA <= "00110";
        inst_addr_in <= x"00000006"; -- 6
        wait for period;
        DR <= "00111";
        SA <= "00111";
        inst_addr_in <= x"00000007"; -- 7
        wait for period;
        DR <= "01000";
        SA <= "01000";
        inst_addr_in <= x"00000008"; -- 8
        wait for period;
        DR <= "01001";
        SA <= "01001";
        inst_addr_in <= x"00000009"; -- 9
        wait for period;
        DR <= "01010";
        SA <= "01010";
        inst_addr_in <= x"0000000a"; -- 10
        wait for period;
        DR <= "01011";
        SA <= "01011";
        inst_addr_in <= x"0000000b"; -- 11
        wait for period;
        DR <= "01100";
        SA <= "01100";
        inst_addr_in <= x"0000000c"; -- 12
        wait for period;
        DR <= "01101";
        SA <= "01101";
        inst_addr_in <= x"0000000d"; -- 13
        wait for period;
        DR <= "01110";
        SA <= "01110";
        inst_addr_in <= x"0000000e"; -- 14
        wait for period;
        DR <= "01111";
        SA <= "01111";
        inst_addr_in <= x"0000000f"; -- 15
        wait for period;
        DR <= "10000";
        SA <= "10000";
        inst_addr_in <= x"00000010"; -- 16
        wait for period;
        DR <= "10001";
        SA <= "10001";
        inst_addr_in <= x"00000011"; -- 17
        wait for period;
        DR <= "10010";
        SA <= "10010";
        inst_addr_in <= x"00000012"; -- 18
        wait for period;
        DR <= "10011";
        SA <= "10011";
        inst_addr_in <= x"00000013"; -- 19
        wait for period;
        DR <= "10100";
        SA <= "10100";
        inst_addr_in <= x"00000014"; -- 20
        wait for period;
        DR <= "10101";
        SA <= "10101";
        inst_addr_in <= x"00000015"; -- 21
        wait for period;
        DR <= "10110";
        SA <= "10110";
        inst_addr_in <= x"00000016"; -- 22
        wait for period;
        DR <= "10111";
        SA <= "10111";
        inst_addr_in <= x"00000017"; -- 23
        wait for period;
        DR <= "11000";
        SA <= "11000";
        inst_addr_in <= x"00000018"; -- 24
        wait for period;
        DR <= "11001";
        SA <= "11001";
        inst_addr_in <= x"00000019"; -- 25
        wait for period;
        DR <= "11010";
        SA <= "11010";
        inst_addr_in <= x"0000001a"; -- 26
        wait for period;
        DR <= "11011";
        SA <= "11011";
        inst_addr_in <= x"0000001b"; -- 27
        wait for period;
        DR <= "11100";
        SA <= "11100";
        inst_addr_in <= x"0000001c"; -- 28
        wait for period;
        DR <= "11101";
        SA <= "11101";
        inst_addr_in <= x"0000001d"; -- 29
        wait for period;
        DR <= "11110";
        SA <= "11110";
        inst_addr_in <= x"0000001e"; -- 30
        wait for period;
        DR <= "11111";
        SA <= "11111";
        inst_addr_in <= x"0000001f"; -- 31
        wait for period;
        TD <= '1'; -- choose register 32 (33rd register)
        TA <= '1'; -- output register 32 to the b bus
        inst_addr_in <= x"00000020"; -- 32

        WAIT FOR 16 ns;

        MD <= '0'; -- take output from function unit to put back into register file
        TD <= '0'; -- choose regular register again (0 - 31)
        TA <= '0';
        FS <= "01110"; -- NOT current value

        DR <= "00000"; -- use first register
        SA <= "00000";
        inst_addr_in <= x"00000000"; -- 0
        wait for period;
        DR <= "00001";
        SA <= "00001";
        inst_addr_in <= x"00000001"; -- 1
        wait for period;
        DR <= "00010";
        SA <= "00010";
        inst_addr_in <= x"00000002"; -- 2
        wait for period;
        DR <= "00011";
        SA <= "00011";
        inst_addr_in <= x"00000003"; -- 3
        wait for period;
        DR <= "00100";
        SA <= "00100";
        inst_addr_in <= x"00000004"; -- 4
        wait for period;
        DR <= "00101";
        SA <= "00101";
        inst_addr_in <= x"00000005"; -- 5
        wait for period;
        DR <= "00110";
        SA <= "00110";
        inst_addr_in <= x"00000006"; -- 6
        wait for period;
        DR <= "00111";
        SA <= "00111";
        inst_addr_in <= x"00000007"; -- 7
        wait for period;
        DR <= "01000";
        SA <= "01000";
        inst_addr_in <= x"00000008"; -- 8
        wait for period;
        DR <= "01001";
        SA <= "01001";
        inst_addr_in <= x"00000009"; -- 9
        wait for period;
        DR <= "01010";
        SA <= "01010";
        inst_addr_in <= x"0000000a"; -- 10
        wait for period;
        DR <= "01011";
        SA <= "01011";
        inst_addr_in <= x"0000000b"; -- 11
        wait for period;
        DR <= "01100";
        SA <= "01100";
        inst_addr_in <= x"0000000c"; -- 12
        wait for period;
        DR <= "01101";
        SA <= "01101";
        inst_addr_in <= x"0000000d"; -- 13
        wait for period;
        DR <= "01110";
        SA <= "01110";
        inst_addr_in <= x"0000000e"; -- 14
        wait for period;
        DR <= "01111";
        SA <= "01111";
        inst_addr_in <= x"0000000f"; -- 15
        wait for period;
        DR <= "10000";
        SA <= "10000";
        inst_addr_in <= x"00000010"; -- 16
        wait for period;
        DR <= "10001";
        SA <= "10001";
        inst_addr_in <= x"00000011"; -- 17
        wait for period;
        DR <= "10010";
        SA <= "10010";
        inst_addr_in <= x"00000012"; -- 18
        wait for period;
        DR <= "10011";
        SA <= "10011";
        inst_addr_in <= x"00000013"; -- 19
        wait for period;
        DR <= "10100";
        SA <= "10100";
        inst_addr_in <= x"00000014"; -- 20
        wait for period;
        DR <= "10101";
        SA <= "10101";
        inst_addr_in <= x"00000015"; -- 21
        wait for period;
        DR <= "10110";
        SA <= "10110";
        inst_addr_in <= x"00000016"; -- 22
        wait for period;
        DR <= "10111";
        SA <= "10111";
        inst_addr_in <= x"00000017"; -- 23
        wait for period;
        DR <= "11000";
        SA <= "11000";
        inst_addr_in <= x"00000018"; -- 24
        wait for period;
        DR <= "11001";
        SA <= "11001";
        inst_addr_in <= x"00000019"; -- 25
        wait for period;
        DR <= "11010";
        SA <= "11010";
        inst_addr_in <= x"0000001a"; -- 26
        wait for period;
        DR <= "11011";
        SA <= "11011";
        inst_addr_in <= x"0000001b"; -- 27
        wait for period;
        DR <= "11100";
        SA <= "11100";
        inst_addr_in <= x"0000001c"; -- 28
        wait for period;
        DR <= "11101";
        SA <= "11101";
        inst_addr_in <= x"0000001d"; -- 29
        wait for period;
        DR <= "11110";
        SA <= "11110";
        inst_addr_in <= x"0000001e"; -- 30
        wait for period;
        DR <= "11111";
        SA <= "11111";
        inst_addr_in <= x"0000001f"; -- 31
        wait for period;
        TD <= '1'; -- choose register 32 (33rd register)
        TA <= '1'; -- output register 32 to the b bus
        inst_addr_in <= x"00000020"; -- 32

        WAIT FOR 16 ns;

        TA <= '0';
        TD <= '0'; -- choose regular register again (0 - 31)
        FS <= "00001"; -- increment 1 to the NOT value to get the 2's complement

        DR <= "00000"; -- use first register
        SA <= "00000";
        inst_addr_in <= x"00000000"; -- 0
        wait for period;
        DR <= "00001";
        SA <= "00001";
        inst_addr_in <= x"00000001"; -- 1
        wait for period;
        DR <= "00010";
        SA <= "00010";
        inst_addr_in <= x"00000002"; -- 2
        wait for period;
        DR <= "00011";
        SA <= "00011";
        inst_addr_in <= x"00000003"; -- 3
        wait for period;
        DR <= "00100";
        SA <= "00100";
        inst_addr_in <= x"00000004"; -- 4
        wait for period;
        DR <= "00101";
        SA <= "00101";
        inst_addr_in <= x"00000005"; -- 5
        wait for period;
        DR <= "00110";
        SA <= "00110";
        inst_addr_in <= x"00000006"; -- 6
        wait for period;
        DR <= "00111";
        SA <= "00111";
        inst_addr_in <= x"00000007"; -- 7
        wait for period;
        DR <= "01000";
        SA <= "01000";
        inst_addr_in <= x"00000008"; -- 8
        wait for period;
        DR <= "01001";
        SA <= "01001";
        inst_addr_in <= x"00000009"; -- 9
        wait for period;
        DR <= "01010";
        SA <= "01010";
        inst_addr_in <= x"0000000a"; -- 10
        wait for period;
        DR <= "01011";
        SA <= "01011";
        inst_addr_in <= x"0000000b"; -- 11
        wait for period;
        DR <= "01100";
        SA <= "01100";
        inst_addr_in <= x"0000000c"; -- 12
        wait for period;
        DR <= "01101";
        SA <= "01101";
        inst_addr_in <= x"0000000d"; -- 13
        wait for period;
        DR <= "01110";
        SA <= "01110";
        inst_addr_in <= x"0000000e"; -- 14
        wait for period;
        DR <= "01111";
        SA <= "01111";
        inst_addr_in <= x"0000000f"; -- 15
        wait for period;
        DR <= "10000";
        SA <= "10000";
        inst_addr_in <= x"00000010"; -- 16
        wait for period;
        DR <= "10001";
        SA <= "10001";
        inst_addr_in <= x"00000011"; -- 17
        wait for period;
        DR <= "10010";
        SA <= "10010";
        inst_addr_in <= x"00000012"; -- 18
        wait for period;
        DR <= "10011";
        SA <= "10011";
        inst_addr_in <= x"00000013"; -- 19
        wait for period;
        DR <= "10100";
        SA <= "10100";
        inst_addr_in <= x"00000014"; -- 20
        wait for period;
        DR <= "10101";
        SA <= "10101";
        inst_addr_in <= x"00000015"; -- 21
        wait for period;
        DR <= "10110";
        SA <= "10110";
        inst_addr_in <= x"00000016"; -- 22
        wait for period;
        DR <= "10111";
        SA <= "10111";
        inst_addr_in <= x"00000017"; -- 23
        wait for period;
        DR <= "11000";
        SA <= "11000";
        inst_addr_in <= x"00000018"; -- 24
        wait for period;
        DR <= "11001";
        SA <= "11001";
        inst_addr_in <= x"00000019"; -- 25
        wait for period;
        DR <= "11010";
        SA <= "11010";
        inst_addr_in <= x"0000001a"; -- 26
        wait for period;
        DR <= "11011";
        SA <= "11011";
        inst_addr_in <= x"0000001b"; -- 27
        wait for period;
        DR <= "11100";
        SA <= "11100";
        inst_addr_in <= x"0000001c"; -- 28
        wait for period;
        DR <= "11101";
        SA <= "11101";
        inst_addr_in <= x"0000001d"; -- 29
        wait for period;
        DR <= "11110";
        SA <= "11110";
        inst_addr_in <= x"0000001e"; -- 30
        wait for period;
        DR <= "11111";
        SA <= "11111";
        inst_addr_in <= x"0000001f"; -- 31
        wait for period;
        TD <= '1'; -- choose register 32 (33rd register)
        TA <= '1'; -- output register 32 to the b bus
        inst_addr_in <= x"00000020"; -- 32

        WAIT FOR 16 ns;

        TB <= '0';
        TD <= '0';
        RW <= '0'; -- read from registers
        MW <= '1'; -- write from registers into memory
        FS <= "00000"; -- return the register value from the register with no changes 

        DR <= "00000"; -- use first register
        inst_addr_in <= x"00000000"; -- 0
        wait for period;
        DR <= "00001";
        inst_addr_in <= x"00000001"; -- 1
        wait for period;
        DR <= "00010";
        inst_addr_in <= x"00000002"; -- 2
        wait for period;
        DR <= "00011";
        inst_addr_in <= x"00000003"; -- 3
        wait for period;
        DR <= "00100";
        inst_addr_in <= x"00000004"; -- 4
        wait for period;
        DR <= "00101";
        inst_addr_in <= x"00000005"; -- 5
        wait for period;
        DR <= "00110";
        inst_addr_in <= x"00000006"; -- 6
        wait for period;
        DR <= "00111";
        inst_addr_in <= x"00000007"; -- 7
        wait for period;
        DR <= "01000";
        inst_addr_in <= x"00000008"; -- 8
        wait for period;
        DR <= "01001";
        inst_addr_in <= x"00000009"; -- 9
        wait for period;
        DR <= "01010";
        inst_addr_in <= x"0000000a"; -- 10
        wait for period;
        DR <= "01011";
        inst_addr_in <= x"0000000b"; -- 11
        wait for period;
        DR <= "01100";
        inst_addr_in <= x"0000000c"; -- 12
        wait for period;
        DR <= "01101";
        inst_addr_in <= x"0000000d"; -- 13
        wait for period;
        DR <= "01110";
        inst_addr_in <= x"0000000e"; -- 14
        wait for period;
        DR <= "01111";
        inst_addr_in <= x"0000000f"; -- 15
        wait for period;
        DR <= "10000";
        inst_addr_in <= x"00000010"; -- 16
        wait for period;
        DR <= "10001";
        inst_addr_in <= x"00000011"; -- 17
        wait for period;
        DR <= "10010";
        inst_addr_in <= x"00000012"; -- 18
        wait for period;
        DR <= "10011";
        inst_addr_in <= x"00000013"; -- 19
        wait for period;
        DR <= "10100";
        inst_addr_in <= x"00000014"; -- 20
        wait for period;
        DR <= "10101";
        inst_addr_in <= x"00000015"; -- 21
        wait for period;
        DR <= "10110";
        inst_addr_in <= x"00000016"; -- 22
        wait for period;
        DR <= "10111";
        inst_addr_in <= x"00000017"; -- 23
        wait for period;
        DR <= "11000";
        inst_addr_in <= x"00000018"; -- 24
        wait for period;
        DR <= "11001";
        inst_addr_in <= x"00000019"; -- 25
        wait for period;
        DR <= "11010";
        inst_addr_in <= x"0000001a"; -- 26
        wait for period;
        DR <= "11011";
        inst_addr_in <= x"0000001b"; -- 27
        wait for period;
        DR <= "11100";
        inst_addr_in <= x"0000001c"; -- 28
        wait for period;
        DR <= "11101";
        inst_addr_in <= x"0000001d"; -- 29
        wait for period;
        DR <= "11110";
        inst_addr_in <= x"0000001e"; -- 30
        wait for period;
        DR <= "11111";
        inst_addr_in <= x"0000001f"; -- 31
        wait for period;
        TB <= '1';
        TD <= '1';
        inst_addr_in <= x"00000020"; -- 32

        WAIT FOR 16ns;
        MD <= '1';
        TB <= '0';
        TD <= '0';
        RW <= '0'; -- read from registers
        MW <= '0'; -- write from registers into memory
        FS <= "00000"; -- return the register value from the register with no changes 

        DR <= "00000"; -- use first register
        inst_addr_in <= x"00000000"; -- 0
        wait for period;
        DR <= "00001";
        inst_addr_in <= x"00000001"; -- 1
        wait for period;
        DR <= "00010";
        inst_addr_in <= x"00000002"; -- 2
        wait for period;
        DR <= "00011";
        inst_addr_in <= x"00000003"; -- 3
        wait for period;
        DR <= "00100";
        inst_addr_in <= x"00000004"; -- 4
        wait for period;
        DR <= "00101";
        inst_addr_in <= x"00000005"; -- 5
        wait for period;
        DR <= "00110";
        inst_addr_in <= x"00000006"; -- 6
        wait for period;
        DR <= "00111";
        inst_addr_in <= x"00000007"; -- 7
        wait for period;
        DR <= "01000";
        inst_addr_in <= x"00000008"; -- 8
        wait for period;
        DR <= "01001";
        inst_addr_in <= x"00000009"; -- 9
        wait for period;
        DR <= "01010";
        inst_addr_in <= x"0000000a"; -- 10
        wait for period;
        DR <= "01011";
        inst_addr_in <= x"0000000b"; -- 11
        wait for period;
        DR <= "01100";
        inst_addr_in <= x"0000000c"; -- 12
        wait for period;
        DR <= "01101";
        inst_addr_in <= x"0000000d"; -- 13
        wait for period;
        DR <= "01110";
        inst_addr_in <= x"0000000e"; -- 14
        wait for period;
        DR <= "01111";
        inst_addr_in <= x"0000000f"; -- 15
        wait for period;
        DR <= "10000";
        inst_addr_in <= x"00000010"; -- 16
        wait for period;
        DR <= "10001";
        inst_addr_in <= x"00000011"; -- 17
        wait for period;
        DR <= "10010";
        inst_addr_in <= x"00000012"; -- 18
        wait for period;
        DR <= "10011";
        inst_addr_in <= x"00000013"; -- 19
        wait for period;
        DR <= "10100";
        inst_addr_in <= x"00000014"; -- 20
        wait for period;
        DR <= "10101";
        inst_addr_in <= x"00000015"; -- 21
        wait for period;
        DR <= "10110";
        inst_addr_in <= x"00000016"; -- 22
        wait for period;
        DR <= "10111";
        inst_addr_in <= x"00000017"; -- 23
        wait for period;
        DR <= "11000";
        inst_addr_in <= x"00000018"; -- 24
        wait for period;
        DR <= "11001";
        inst_addr_in <= x"00000019"; -- 25
        wait for period;
        DR <= "11010";
        inst_addr_in <= x"0000001a"; -- 26
        wait for period;
        DR <= "11011";
        inst_addr_in <= x"0000001b"; -- 27
        wait for period;
        DR <= "11100";
        inst_addr_in <= x"0000001c"; -- 28
        wait for period;
        DR <= "11101";
        inst_addr_in <= x"0000001d"; -- 29
        wait for period;
        DR <= "11110";
        inst_addr_in <= x"0000001e"; -- 30
        wait for period;
        DR <= "11111";
        inst_addr_in <= x"0000001f"; -- 31
        wait for period;
        TB <= '1';
        TD <= '1';
        inst_addr_in <= x"00000020"; -- 32

        WAIT;

    END PROCESS;
END Behavioural;