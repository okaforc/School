LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

ENTITY zeroDetect IS
    PORT (
        z_a : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
        z_out : OUT STD_LOGIC
    );
END zeroDetect;

ARCHITECTURE Behavioral OF zeroDetect IS
BEGIN
    z_out <= NOT (z_a(0) OR z_a(1) OR z_a(2) OR z_a(3) OR z_a(4) OR z_a(5) OR z_a(6) OR z_a(7) OR
        z_a(8) OR z_a(9) OR z_a(10) OR z_a(11) OR z_a(12) OR z_a(13) OR z_a(14) OR z_a(15) OR z_a(16) OR z_a(17) OR
        z_a(18) OR z_a(19) OR z_a(20) OR z_a(21) OR z_a(22) OR z_a(23) OR z_a(24) OR z_a(25) OR
        z_a(26) OR z_a(27) OR z_a(28) OR z_a(29) OR z_a(30) OR z_a(31)) AFTER 2 ns;
END Behavioral;