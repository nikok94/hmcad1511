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
use IEEE.STD_LOGIC_unsigned.ALL;

library work;
use work.fifo_64_8;
--use work.chipscope_icon_qspi;
--use work.chipscope_ila_qspi;

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
      clk_250MHz_in     : in std_logic;
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
      fast_adc_valid    : out std_logic
    );
end QuadSPI_adc_250x4_module;

architecture Behavioral of QuadSPI_adc_250x4_module is
    type state_machine is (idle, rd_start_byte, nibble_1, nibble_2);
    signal state, next_state    : state_machine;
    signal command_bit_counter  : std_logic_vector(3 downto 0);
    signal command_byte         : std_logic_vector(7 downto 0);
    signal command_valid        : std_logic;
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
    signal quad_compleat        : std_logic;
    signal ready                : std_Logic;
    signal ila_control_0        : std_logic_vector(35 downto 0);
    signal fifo_rst             : STD_LOGIC;
    signal fifo_wr_en           : STD_LOGIC;
    signal fifo_rd_en           : STD_LOGIC;
    signal fifo_dout            : STD_LOGIC_VECTOR(7 DOWNTO 0);
    signal fifo_full            : STD_LOGIC;
    signal fifo_empty           : STD_LOGIC;
    signal fifo_valid           : STD_LOGIC;
    signal spifi_sck_d          : std_logic;
    signal spifi_sck_d_1        : std_logic;
    signal spifi_cs_d           : std_logic;
    signal spifi_cs_down        : std_logic;
    signal spifi_cs_up          : std_logic;
    signal spifi_sck_edge       : std_Logic;
    signal spifi_sck_fall       : std_logic;
    signal nibble               : std_logic_vector(3 downto 0);
    signal next_byte            : std_logic;
    signal next_byte_d1         : std_logic;
    signal next_byte_d2         : std_logic;
    signal next_byte_d3         : std_logic;
    
begin

--s_strm_ready <= ready;

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

delay_process:
    process(s_strm_clk)
    begin
      if rising_edge(s_strm_clk) then
        spifi_cs_d  <= spifi_cs;
      end if;
    end process;
spifi_cs_up     <= (not spifi_cs_d) and spifi_cs;

command_byte_proc:
  process(spifi_sck, spifi_cs)
  begin
    if (spifi_cs = '1') then
      command_bit_counter <= (others => '0');
      command_byte  <= (others => '0');
    elsif rising_edge(spifi_sck) then
      if command_valid = '0' then 
        command_bit_counter <= command_bit_counter + 1;
        command_byte(7 downto 1) <= command_byte( 6 downto 0);
        command_byte(0) <= mosi_i;
      end if;
    end if;
  end process;

command_valid <= command_bit_counter(3);

sck_delay_process:
    process(clk_250MHz_in)
    begin
      if rising_edge(clk_250MHz_in) then
        next_byte_d1 <= next_byte;
        next_byte_d2 <= next_byte_d1;
        next_byte_d3 <= next_byte_d2;
      end if;
    end process;

fifo_rd_en <= (not next_byte_d3) and next_byte_d2;

process(spifi_sck)
begin
  if rising_edge(spifi_sck) then
    if (state = nibble_1) then 
      next_byte <= '1';
    else
      next_byte <= '0';
    end if;
  end if;
end process;

fifo_inst : ENTITY fifo_64_8
  PORT MAP(
    rst     => fifo_rst,
    wr_clk  => s_strm_clk,
    rd_clk  => clk_250MHz_in,
    din     => s_strm_data,
    wr_en   => fifo_wr_en,
    rd_en   => fifo_rd_en,
    dout    => fifo_dout ,
    full    => fifo_full ,
    empty   => fifo_empty,
    valid   => fifo_valid
  );
fast_adc_valid <= fifo_valid;
fifo_wr_en <= s_strm_valid and (not fifo_full);
s_strm_ready <= not fifo_full;

fifo_rst <= spifi_cs_up;

nibble <= fifo_dout(3 downto 0) when (state = nibble_2) else fifo_dout(7 downto 4);

mosi_o <= nibble(0);
miso_o <= nibble(1);
sio2_o <= nibble(2);
sio3_o <= nibble(3);

state_sync_proc :
  process(spifi_sck, spifi_cs, s_strm_rst) 
  begin
    if (spifi_cs = '1' or s_strm_rst = '1') then
      state <= idle;
    elsif rising_edge(spifi_sck) then
      state <= next_state;
    end if;
  end process;

state_data_proc:
  process(state) 
  begin
    spifi_tri_state <= '1';
      case state is
        when idle =>
        when rd_start_byte =>
        when nibble_1 =>
            spifi_tri_state <= '0';
        when nibble_2 =>
            spifi_tri_state <= '0';
        when others =>
      end case;
  end process;

next_state_proc:
  process(state, command_valid) 
  begin
    next_state <= state;
      case state is
      when idle =>
        next_state <= rd_start_byte;
      when rd_start_byte =>
        if (command_valid  = '1') then
          case command_byte is 
            when x"00" => next_state <= nibble_1;
            when others => next_state <= idle;
          end case;
        end if;
      when nibble_1 =>
          next_state <= nibble_2;
      when nibble_2 =>
          next_state <= nibble_1;
      when others =>
         next_state <= idle;
      end case;
  end process;

--ila_reg_inst : entity chipscope_ila_qspi 
--  port map (
--    CONTROL     => ila_control_0,
--    CLK         => clk_250MHz_in,
--    DATA        => sio3_o & sio2_o & mosi_o & mosi_i &  miso_o & spifi_sck & spifi_cs & quad_compleat & ready & s_strm_valid & s_strm_data,
--    TRIG0       => sio3_o & sio2_o & mosi_o & mosi_i &  miso_o & spifi_sck & spifi_cs & quad_compleat & ready & s_strm_valid
--    );
--icon_inst : ENTITY chipscope_icon_qspi
--  port map (
--    CONTROL0 => ila_control_0
--    );

end Behavioral;
