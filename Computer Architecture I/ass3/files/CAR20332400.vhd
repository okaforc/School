LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY CAR IS
    PORT (
        data_in : IN STD_LOGIC_VECTOR(16 DOWNTO 0);
        clk, load, reset : IN STD_LOGIC;
        ctrl : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
END CAR;

ARCHITECTURE Behavioral OF CAR IS
    COMPONENT reg32
        PORT (
            D : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            load, Clk : IN STD_LOGIC;
            Q : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
    END COMPONENT;

    SIGNAL inc_data : STD_LOGIC_VECTOR(16 DOWNTO 0); -- incremented data
    SIGNAL reg_in : STD_LOGIC_VECTOR(31 DOWNTO 0); -- register input signal
    SIGNAL reg_data : STD_LOGIC_VECTOR(16 DOWNTO 0); -- register output signal. only used first 17 bits
    signal unused : std_logic_vector(14 downto 0) ; -- unused 14 bits of register output
BEGIN

    reg : reg32
    PORT MAP(
        D => reg_in, -- register data input
        load => '1', -- output this value when the clock is rising
        clk => clk,
        Q (16 DOWNTO 0) => reg_data, -- only want first 17 bits of register output
        Q (31 downto 17) => unused
    );

    inc_data <= std_logic_vector(signed(reg_data) + 1); -- add 1 to the register data
    reg_in (16 DOWNTO 0) <= inc_data WHEN load = '1' ELSE
    data_in (16 DOWNTO 0); -- load the incremented data when the load bit is set. otherwise, load the regular data.

    reg_in(31 DOWNTO 17) <= "000000000000000"; -- fill in rest of register input with zeroes

    ctrl (16 DOWNTO 0) <= reg_data WHEN reset = '0' ELSE
    "00000000000000000"; -- reset data to 0 when the reset bit is set
END Behavioral;