library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
entity memory is -- use unsigned for memory address
    Port ( address : in unsigned std_logic_vector(31 downto 0);
    write_data : in std_logic_vector(31 downto 0);
    MemWrite, MemRead : in std_logic;
    read_data : out std_logic_vector(31 downto 0));
end memory;

architecture Behavioral of memory is
    type mem_array is array(0 to 7) of std_logic_vector(31 downto 0);
    -- define type, for memory arrays
    begin
    mem_process: process (address, write_data)
    -- initialize data memory, X denotes hexadecimal number
    variable data_mem : mem_array := (
    X"00000000", X"00000000", X"00000000",X"00000000",
    X"00000000", X"00000000", X"00000000",X"00000000");
    variable addr:integer
    
    begin -- the following type conversion function is in std_logic_arith
        addr:=conv_integer(address(2 downto 0));
        if MemWrite ='1' then
            data_mem(addr):= write_data;
        elsif MemRead='1' then
            read_data <= data_mem(addr) after 10 ns;
        end if;
    end process;
end Behavioral;