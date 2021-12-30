LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY logSlice IS
    PORT (
        x, y : IN STD_LOGIC;
        s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        g : OUT STD_LOGIC
    );
END logSlice;
ARCHITECTURE dataflow_1 OF logSlice IS
    COMPONENT mux4_1bit PORT (
        mu4_s : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        mu4_in : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        mu4_Z : OUT STD_LOGIC
        );
    END COMPONENT;
    SIGNAL t_i, t_j, t_k, t_l, t_g : STD_LOGIC;
BEGIN
    t_i <= x AND y AFTER 2 ns;
    t_j <= x OR y AFTER 2 ns;
    t_k <= x XOR y AFTER 2 ns;
    t_l <= NOT x AFTER 2 ns;

    mux4 : mux4_1bit PORT MAP(
        mu4_s => s,
        mu4_in(0) => t_i,
        mu4_in(1) => t_j,
        mu4_in(2) => t_k,
        mu4_in(3) => t_l,
        mu4_Z => t_g
    );

    g <= t_g;
END dataflow_1;