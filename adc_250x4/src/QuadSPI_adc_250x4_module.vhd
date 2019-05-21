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
--use work.fifo_16_8;
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
      spifi_cs          : in std_logic;
      spifi_sck         : in std_logic;
      spifi_miso        : inout std_logic;
      spifi_mosi        : inout std_logic;
      spifi_sio2        : inout std_logic;
      spifi_sio3        : inout std_logic;
      
      clk               : in std_logic;
      rst               : in std_logic;
      s_strm_data       : in std_logic_vector(63 downto 0);
      s_strm_valid      : in std_logic;
      s_strm_ready      : out std_logic;
      fast_adc_valid    : out std_logic;
      
      low_channel_activ     : out std_logic;
      low_adc_m_strm_data   : in std_logic_vector(7 downto 0);
      low_adc_m_strm_valid  : in std_logic;
      low_adc_m_strm_ready  : out std_logic
      
    );
end QuadSPI_adc_250x4_module;

architecture Behavioral of QuadSPI_adc_250x4_module is
    type state_machine is (idle, rd_start_byte, nibble_1, nibble_2);
    signal state, next_state    : state_machine;
    signal command_bit_counter  : std_logic_vector(2 downto 0);
    signal command_byte         : std_logic_vector(7 downto 0):= (others => '0');
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
    signal fast_fifo_rst        : STD_LOGIC;
    signal fast_fifo_wr_en      : STD_LOGIC;
    signal fast_fifo_rd_en      : STD_LOGIC;
    signal fast_fifo_dout       : STD_LOGIC_VECTOR(7 DOWNTO 0);
    signal fast_fifo_full       : STD_LOGIC;
    signal fast_fifo_empty      : STD_LOGIC;
    signal fast_fifo_valid      : STD_LOGIC;
    
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
    signal low_activ            : std_logic;
    
begin

low_channel_activ <= low_activ;

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
    process(clk)
    begin
      if rising_edge(clk) then
        spifi_cs_d  <= spifi_cs;
      end if;
    end process;
spifi_cs_up     <= (not spifi_cs_d) and spifi_cs;

command_byte_proc:
  process(spifi_sck, spifi_cs)
  begin
    if (spifi_cs = '1') then
      command_bit_counter <= (others => '0');
    elsif rising_edge(spifi_sck) then
      if (state = rd_start_byte) then
        command_bit_counter <= command_bit_counter + 1;
        command_byte(7 downto 1) <= command_byte(6 downto 0);
        command_byte(0) <= mosi_i;
      end if;
    end if;
  end process;

low_activ <= command_byte(0);

sck_delay_process:
    process(clk)
    begin
      if rising_edge(clk) then
        next_byte_d1 <= next_byte;
        next_byte_d2 <= next_byte_d1;
        next_byte_d3 <= next_byte_d2;
      end if;
    end process;

fast_fifo_rd_en <= (not next_byte_d3) and next_byte_d2 and (not low_activ);

low_adc_m_strm_ready <= (not next_byte_d3) and next_byte_d2 and low_activ;



process(spifi_sck)
begin
  if rising_edge(spifi_sck) then
    if (next_state = nibble_2) then 
      next_byte <= '1';
    else
      next_byte <= '0';
    end if;
  end if;
end process;


fast_adc_fifo_inst : ENTITY fifo_64_8
  PORT MAP(
    rst     => fast_fifo_rst,
    wr_clk  => clk,
    rd_clk  => clk,
    din     => s_strm_data,
    wr_en   => fast_fifo_wr_en,
    rd_en   => fast_fifo_rd_en,
    dout    => fast_fifo_dout ,
    full    => fast_fifo_full ,
    empty   => fast_fifo_empty,
    valid   => fast_fifo_valid
  );
fast_adc_valid <= fast_fifo_valid;
fast_fifo_wr_en <= s_strm_valid and (not fast_fifo_full);
s_strm_ready <= not fast_fifo_full;

fast_fifo_rst <= spifi_cs_up and (not low_activ);

nibble <= fast_fifo_dout(3 downto 0) when (next_state = nibble_1) and (low_activ = '0') else 
          fast_fifo_dout(7 downto 4) when (next_state = nibble_2) and (low_activ = '0') else
          low_adc_m_strm_data(3 downto 0) when (next_state = nibble_1) and (low_activ = '1') else 
          low_adc_m_strm_data(7 downto 4) when (next_state = nibble_2) and (low_activ = '1') else
          (others => '0');

  mosi_o <= nibble(0);
  miso_o <= nibble(1);
  sio2_o <= nibble(2);
  sio3_o <= nibble(3);

state_sync_proc :
  process(spifi_sck, spifi_cs) 
  begin
    if (spifi_cs = '1') then
      state <= idle;
    elsif rising_edge(spifi_sck) then
      state <= next_state;
    end if;
  end process;


next_state_proc:
  process(state, command_bit_counter) 
  begin
    spifi_tri_state <= '1';
    next_state <= state;
      case state is
      when idle =>
        next_state <= rd_start_byte;
      when rd_start_byte =>
        if (command_bit_counter  = b"111") then
          next_state <= nibble_1;
        end if;
      when nibble_1 =>
          spifi_tri_state <= '0';
          next_state <= nibble_2;
      when nibble_2 =>
          spifi_tri_state <= '0';
          next_state <= nibble_1;
      when others =>
         next_state <= idle;
      end case;
  end process;

--ila_reg_inst : entity chipscope_ila_qspi 
--  port map (
--    CONTROL     => ila_control_0,
--    CLK         => clk,
--    DATA        => sio3_o & sio2_o & mosi_o & mosi_i &  miso_o & spifi_sck & spifi_cs & nibble & next_byte & fast_fifo_rd_en & fast_fifo_dout & '0',
--    TRIG0       => sio3_o & sio2_o & mosi_o & mosi_i &  miso_o & spifi_sck & spifi_cs & next_byte & fast_fifo_rd_en
--    );
--icon_inst : ENTITY chipscope_icon_qspi
--  port map (
--    CONTROL0 => ila_control_0
--    );

end Behavioral;
