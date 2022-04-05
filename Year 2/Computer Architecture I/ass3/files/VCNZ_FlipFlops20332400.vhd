-- Positive Edge-Triggered D Flip-Flop with Reset:
-- VHDL Process Description
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY dff IS
    PORT (
        CLK, RESET, D, load : IN STD_LOGIC;
        Q : OUT STD_LOGIC);
END dff;

ARCHITECTURE pet_pr OF dff IS
    -- Implements positive edge-triggered bit state storage
    -- with asynchronous reset.
    SIGNAL state : STD_LOGIC;
BEGIN
    Q <= state;
    PROCESS (CLK, RESET, load, D)
    BEGIN
        IF (RESET = '1') THEN
            state <= '0';
        ELSE
            IF (CLK'event AND ClK = '1' AND load = '1') THEN
                state <= D;
            END IF;
        END IF;
    END PROCESS;
END;

-- 4-bit positive edge-triggered D flip-flop with reset
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY flipflops IS
    PORT (
        clk, FL, v, c, n, z : IN STD_LOGIC;
        reset : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        Q : OUT STD_LOGIC_VECTOR (3 DOWNTO 0)
    );
END flipflops;

ARCHITECTURE Behavioural OF flipflops IS
    -- Implements positive edge-triggered 4-bit state storage
    -- with asynchronous reset.

    COMPONENT dff PORT (
        CLK, RESET, D, load : IN STD_LOGIC;
        Q : OUT STD_LOGIC
        );
    END COMPONENT;

BEGIN

    dff_v : dff PORT MAP(
        clk => clk,
        reset => reset(0),
        D => v,
        load => FL,
        Q => Q(0)
    );

    dff_c : dff PORT MAP(
        clk => clk,
        reset => reset(1),
        D => c,
        load => FL,
        Q => Q(1)
    );

    dff_n : dff PORT MAP(
        clk => clk,
        reset => reset(2),
        D => n,
        load => FL,
        Q => Q(2)
    );

    dff_z : dff PORT MAP(
        clk => clk,
        reset => reset(3),
        D => z,
        load => FL,
        Q => Q(3)
    );
END;