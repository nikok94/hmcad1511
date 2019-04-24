----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.04.2019 10:22:02
-- Design Name: 
-- Module Name: QuadSPI_adc_250x4_module - Behavioral
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

library work;
use work.QuadSpi_x4_8byte_tranceiver;
use work.spi_byte_receiver;
--use work.ila_qspi;
--use work.icon_0;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity QuadSPI_adc_250x4_module is
    generic (
      C_CPHA            : integer := 0;
      C_CPOL            : integer := 0;
      C_LSB_FIRST       : integer := 0
      );
    Port (
      spifi_cs          : in std_logic;
      spifi_sck         : in std_logic;
      spifi_miso        : inout std_logic;
      spifi_mosi        : inout std_logic;
      spifi_sio2        : inout std_logic;
      spifi_sio3        : inout std_logic;
      
      s_strm_clk        : in std_logic;
      s_strm_rst        : in std_logic;
      s_strm_data       : in std_logic_vector(63 downto 0);
      s_strm_valid      : in std_logic;
      s_strm_ready      : out std_logic;
      compleat          : out std_logic
    );
end QuadSPI_adc_250x4_module;

architecture Behavioral of QuadSPI_adc_250x4_module is
    type state_machine is (idle, rd_start_byte, cmd_decode, s_strm_read, qspi_trans_start, qspi_trans, qspi_trans_compleat);
    signal state, next_state    : state_machine;
    signal spi_rec_byte         : std_logic_vector(7 downto 0);
    signal spi_rec_valid        : std_logic;
    signal spifi_tri_state      : std_logic:= '1';
    signal mosi_i               : std_logic;
    signal mosi_o               : std_logic;
    signal miso_i               : std_logic;
    signal miso_o               : std_logic;
    signal sio2_i               : std_logic;
    signal sio2_o               : std_logic;
    signal sio3_i               : std_logic;
    signal sio3_o               : std_logic;
    signal data_8byte           : std_logic_vector(63 downto 0);
    signal start                : std_logic;
    signal quad_tr_ready        : std_logic;
    signal ready                : std_Logic;
    signal ila_control_0        : std_logic_vector(35 downto 0);

begin

s_strm_ready <= ready;

MOSI_IOBUF_inst : IOBUF
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => mosi_i,     -- Buffer output
      IO => spifi_mosi,   -- Buffer inout port (connect directly to top-level port)
      I => mosi_o,     -- Buffer input
      T => spifi_tri_state      -- 3-state enable input, high=input, low=output 
   );

MISO_IOBUF_inst : IOBUF
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => miso_i,     -- Buffer output
      IO => spifi_miso,   -- Buffer inout port (connect directly to top-level port)
      I => miso_o,     -- Buffer input
      T => spifi_tri_state      -- 3-state enable input, high=input, low=output 
   );

SIO2_IOBUF_inst : IOBUF
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => sio2_i,     -- Buffer output
      IO => spifi_sio2,   -- Buffer inout port (connect directly to top-level port)
      I => sio2_o,     -- Buffer input
      T => spifi_tri_state      -- 3-state enable input, high=input, low=output 
   );

SIO3_IOBUF_inst : IOBUF
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => sio3_i,     -- Buffer output
      IO => spifi_sio3,   -- Buffer inout port (connect directly to top-level port)
      I => sio3_o,     -- Buffer input
      T => spifi_tri_state      -- 3-state enable input, high=input, low=output 
   );

spi_byte_receiver_inst : entity spi_byte_receiver
    Generic map(
      C_CPHA        => C_CPHA     ,
      C_CPOL        => C_CPOL     ,
      C_LSB_FIRST   => C_LSB_FIRST
    )
    Port map(
      SCK          => spifi_sck ,
      CS           => spifi_cs,
      MOSI         => mosi_i,
      
      clk          => s_strm_clk,
      rst          => s_strm_rst,
      byte         => spi_rec_byte,
      valid        => spi_rec_valid
    );
    
QuadSpi_x4_8byte_tranceiver_inst : entity QuadSpi_x4_8byte_tranceiver
    generic map(
      C_CPHA            => 0,
      C_CPOL            => 0,
      C_LSB_FIRST       => 0
      )
    Port map(
      cs                => spifi_cs ,
      sck               => spifi_sck,
      miso              => miso_o,
      mosi              => mosi_o,
      sio2              => sio2_o,
      sio3              => sio3_o,

      clk               => s_strm_clk,
      rst               => s_strm_rst,
      data_8byte        => data_8byte,
      start             => start     ,
      ready             => quad_tr_ready     
    );

state_sync_proc :
  process(s_strm_clk) 
  begin
    if rising_edge(s_strm_clk) then
      if (spifi_cs = '1' or s_strm_rst = '1')then
        state <= idle;
      else 
        state <= next_state;
      end if;
    end if;
  end process;

state_data_proc:
  process(state) 
  begin
    ready <= '0';
    spifi_tri_state <= '1';
    start <= '0';
    compleat <= '0';
      case state is
        when idle =>
        when rd_start_byte =>
        when cmd_decode =>
        when s_strm_read =>
          ready <= '1';
          spifi_tri_state <= '0';
          data_8byte <= s_strm_data;
        when qspi_trans_start =>
          start <= '1';
          spifi_tri_state <= '0';
        when qspi_trans => 
          spifi_tri_state <= '0';
        when qspi_trans_compleat => 
          compleat <= '1';
        when others =>
      end case;
  end process;

next_state_proc:
  process(state, spi_rec_valid, quad_tr_ready, s_strm_valid, ready, spi_rec_byte) 
  begin
    next_state <= state;
      case state is
      when idle =>
        next_state <= rd_start_byte;
      when rd_start_byte =>
        if (spi_rec_valid  = '1') then
          next_state <= cmd_decode;
        end if;
      when cmd_decode =>
        if (spi_rec_byte(0) = '0') then
          next_state <= s_strm_read;
        else 
          next_state <= idle;
        end if;
      when s_strm_read =>
        if (s_strm_valid = '1') and (ready = '1') then
          next_state <= qspi_trans_start;
        end if;
      when qspi_trans_start =>
          next_state <= qspi_trans;
      when qspi_trans => 
        if (quad_tr_ready = '1') then
          if s_strm_valid = '1' then 
            next_state <= s_strm_read;
          else
            next_state <= qspi_trans_compleat;
          end if;
        end if;
      when qspi_trans_compleat => 
        next_state <= idle;
      when others =>
         next_state <= idle;
      end case;
  end process;

--ila_reg_inst : entity ila_qspi 
--  port map (
--    CONTROL     => ila_control_0,
--    CLK         => s_strm_clk,
--    DATA        => sio3_o & sio2_o & mosi_o & mosi_i &  miso_o & spifi_sck & spifi_cs & quad_tr_ready & start & data_8byte,
--    TRIG0       => sio3_o & sio2_o & mosi_o & mosi_i & miso_o & spifi_sck & spifi_cs & quad_tr_ready & start 
--    );
--icon_inst : ENTITY icon_0
--  port map (
--    CONTROL0 => ila_control_0
--    );

end Behavioral;
