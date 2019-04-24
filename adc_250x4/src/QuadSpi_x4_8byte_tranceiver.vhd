----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.04.2019 10:50:01
-- Design Name: 
-- Module Name: QuadSpi_x4_8byte_tranceiver - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity QuadSpi_x4_8byte_tranceiver is
    generic(
      C_CPHA            : integer := 0;
      C_CPOL            : integer := 0;
      C_LSB_FIRST       : integer := 0
      );
    Port (
      cs                : in std_logic;
      sck               : in std_logic;
      miso              : out std_logic;
      mosi              : out std_logic;
      sio2              : out std_logic;
      sio3              : out std_logic;

      clk               : in std_logic;
      rst               : in std_logic;
      data_8byte        : in std_logic_vector(63 downto 0);
      start             : in std_logic;
      ready             : out std_logic
    );
end QuadSpi_x4_8byte_tranceiver;

architecture Behavioral of QuadSpi_x4_8byte_tranceiver is
    signal spi_byte_shift_counter       : std_logic_vector(4 downto 0):= (others => '0');
    signal spi_byte_shift_reg           : std_logic_vector(63 downto 0):= (others => '0');
    signal spi_cmlt_sync_vect           : std_logic_vector(2 downto 0);
    signal spi_cmlt_sync                : std_logic;

begin

sck_rising_edge_process :
    process(SCK, CS, rst, start)
    begin
      if (CS = '1') or (rst = '1') then
        spi_byte_shift_counter <= (others => '0');
        spi_byte_shift_reg <= (others => '0');
      elsif (start = '1') then
        spi_byte_shift_reg <= data_8byte;
      elsif falling_edge(SCK) then
        if spi_byte_shift_counter = "10000" then
          spi_byte_shift_counter <= ( 0 => '1', others => '0');
        else
        spi_byte_shift_counter <= spi_byte_shift_counter + 1;
        end if;
        spi_byte_shift_reg(59 downto 0) <= spi_byte_shift_reg(63 downto 4);
      end if;
    end process;

    miso <= spi_byte_shift_reg(0);
    mosi <= spi_byte_shift_reg(1);
    sio2 <= spi_byte_shift_reg(2);
    sio3 <= spi_byte_shift_reg(3);


spi_cmlt_sync_vect_proc :
    process(clk)
    begin
      if rising_edge(clk) then
        if (rst = '1') then
          spi_cmlt_sync_vect <= (others => '0');
        else
          spi_cmlt_sync_vect(0) <= spi_byte_shift_counter(4);
          spi_cmlt_sync_vect(2 downto 1) <= spi_cmlt_sync_vect(1 downto 0);
        end if;
      end if;
    end process;

spi_cmlt_sync <= (not spi_cmlt_sync_vect(2)) and spi_cmlt_sync_vect(1);
ready <= spi_cmlt_sync;

end Behavioral;
