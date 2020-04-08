----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11.04.2019 12:34:53
-- Design Name: 
-- Module Name: HMCAD1511_v1_01 - Behavioral
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
use work.data_deserializer;
--use work.serdes_1_to_n_data_ddr_s8_diff;
use work.high_speed_clock_to_serdes;
--use work.async_fifo_32;

--use work.chipscope_vio_3;
--use work.ila;
--use work.chipscope_icon;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity HMCAD1511_v1_01 is
    Port (
      FCLKp                 : in std_logic;
      FCLKn                 : in std_logic;
      LCLKp                 : in std_logic;
      LCLKn                 : in std_logic;
      DxXAp                 : in std_logic_vector(3 downto 0);
      DxXAn                 : in std_logic_vector(3 downto 0);
      DxXBp                 : in std_logic_vector(3 downto 0);
      DxXBn                 : in std_logic_vector(3 downto 0);
      
      ARESET                : in std_logic;

      m_strm_aclk           : in std_logic;
      M_STRM_VALID          : out std_logic;
      M_STRM_DATA           : out std_logic_vector(63 downto 0)
    );
end HMCAD1511_v1_01;

architecture Behavioral of HMCAD1511_v1_01 is
    constant frame_pattern  : std_logic_vector(7 downto 0):= x"0f";
    type   adc_data is array(3 downto 0) of std_logic_vector(7 downto 0);
    signal adc_data_a_8bit                  : adc_data;
    signal adc_data_b_8bit                  : adc_data;
    signal lclk_ibufg_out                   : std_logic;
    signal IOCLK0_0                         : std_logic;
    signal IOCLK1_0                         : std_logic;
    signal serdesstrobe_0                   : std_logic;
    signal IOCLK0_1                         : std_logic;
    signal IOCLK1_1                         : std_logic;
    signal serdesstrobe_1                   : std_logic;
    signal div_clk_bufg_0                   : std_logic;
    signal div_clk_bufg_1                   : std_logic;
    signal frame_calib_valid                : std_logic;
    signal rst                              : std_logic;
    signal bitslip                          : std_logic;
    signal frame_data                       : std_logic_vector(7 downto 0);
    signal rst_counter                      : std_logic_vector(3 downto 0);
    signal counter                          : std_logic_vector(4 downto 0);
    signal data_calib_valid_vect            : std_logic_vector(7 downto 0);
    type state_machine is (idle, frame_st, bitslip_st, counter_st, ready_st, rst_st, rst_cont_st);
    signal state, next_state : state_machine;
    signal valid                            : std_logic;
    signal bitslip_counter                  : std_logic_vector(3 downto 0);
    signal fifo_32_0_wr_en                  : std_logic;
    signal fifo_32_0_rd_en                  : std_logic;
    signal fifo_32_0_dout                   : std_logic_vector(31 downto 0);
    signal fifo_32_0_full                   : std_logic;
    signal fifo_32_0_empty                  : std_logic;
    signal fifo_32_0_valid                  : std_logic;
    signal fifo_32_1_wr_en                  : std_logic;
    signal fifo_32_1_rd_en                  : std_logic;
    signal fifo_32_1_dout                   : std_logic_vector(31 downto 0);
    signal fifo_32_1_full                   : std_logic;
    signal fifo_32_1_empty                  : std_logic;
    signal fifo_32_1_valid                  : std_logic;
    signal debug_frame                      : std_logic_vector(8 downto 0);
    

begin

M_STRM_VALID <= valid;--'1' when data_calib_valid_vect = x"ff" else '0';



IBUFGDS_LCLK_inst : IBUFGDS
   generic map (
      IBUF_LOW_PWR => TRUE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
      IOSTANDARD => "DEFAULT")
   port map (
      O => lclk_ibufg_out,  -- Clock buffer output
      I => LCLKp,  -- Diff_p clock buffer input
      IB => LCLKn -- Diff_n clock buffer input
   );

high_speed_clock_to_serdes_1 : entity high_speed_clock_to_serdes
    Port map( 
        clkin_ibufg         => lclk_ibufg_out,
        gclk                => div_clk_bufg_0,
        serdesclk0          => IOCLK0_0,
        serdesclk1          => IOCLK1_0,
        serdesstrobe        => serdesstrobe_0
    );

high_speed_clock_to_serdes_0 : entity high_speed_clock_to_serdes

    Port map( 
        clkin_ibufg         => lclk_ibufg_out,
        gclk                => div_clk_bufg_1,
        serdesclk0          => IOCLK0_1,
        serdesclk1          => IOCLK1_1,
        serdesstrobe        => serdesstrobe_1
    );


frame_deserializer_a_inst : entity data_deserializer
    generic map(
      DIFF_TERM         => true
    )
    Port map(
      serdes_clk0       => IOCLK0_1,
      serdes_clk1       => IOCLK1_1,
      serdes_divclk     => div_clk_bufg_1,
      serdes_strobe     => serdesstrobe_1,
      data_p            => FCLKp,
      data_n            => FCLKn,
      reset             => rst,
      bitslip           => bitslip,
      clk               => m_strm_aclk,
      data_out          => frame_data,
      valid             => frame_calib_valid
    );
--frame_deserializer_a_inst : entity serdes_1_to_n_data_ddr_s8_diff
--    generic map(
--      S                     => 8,
--      D                     => 1,
--      DIFF_TERM             => "TRUE"
--    )
--    Port map(
--      use_phase_detector    => '1',
--      rxioclkp              => IOCLK0_1,
--      rxioclkn              => IOCLK1_1,
--      gclk                  => div_clk_bufg_1,
--      rxserdesstrobe        => serdesstrobe_1,
--      datain_p(0)           => FCLKp,
--      datain_n(0)           => FCLKn,
--      reset                 => rst,
--      bitslip               => bitslip,
--      debug_in              => "00",
--      data_out              => frame_data,
--      debug                 => debug_frame,
--      stat_out              => open
--    );
--    frame_calib_valid <= debug_frame(1);

adc_deserializer_gen1 : for i in 0 to 1 generate

lvds_deserializer_a_inst : entity data_deserializer
    generic map(
      DIFF_TERM         => true
    )
    Port map(
      serdes_clk0       => IOCLK0_0,
      serdes_clk1       => IOCLK1_0,
      serdes_divclk     => div_clk_bufg_0,
      serdes_strobe     => serdesstrobe_0,
      data_p            => DxXAp(i),
      data_n            => DxXAn(i),
      reset             => rst,
      bitslip           => bitslip,
      clk               => m_strm_aclk,
      data_out          => adc_data_a_8bit(i),
      valid             => data_calib_valid_vect(i)
    );

lvds_deserializer_b_inst : entity data_deserializer
    generic map(
      DIFF_TERM         => true
    )
    Port map(
      serdes_clk0       => IOCLK0_0,
      serdes_clk1       => IOCLK1_0,
      serdes_divclk     => div_clk_bufg_0,
      serdes_strobe     => serdesstrobe_0,
      data_p            => DxXBp(i),
      data_n            => DxXBn(i),
      reset             => rst,
      bitslip           => bitslip,
      clk               => m_strm_aclk,
      data_out          => adc_data_b_8bit(i),
      valid             => data_calib_valid_vect(4 + i)
    );
--lvds_deserializer_a_inst : entity serdes_1_to_n_data_ddr_s8_diff
--    generic map(
--      S                     => 8,
--      D                     => 1,
--      DIFF_TERM             => "TRUE"
--    )
--    Port map(
--      use_phase_detector    => '1',
--      rxioclkp              => IOCLK0_0,
--      rxioclkn              => IOCLK1_0,
--      gclk                  => div_clk_bufg_0,
--      rxserdesstrobe        => serdesstrobe_0,
--      datain_p(0)           => DxXAp(i),
--      datain_n(0)           => DxXAn(i),
--      reset                 => rst,
--      bitslip               => bitslip,
--      debug_in              => "00",
--      data_out              => adc_data_a_8bit(i),
--     -- debug(1)              => data_calib_valid_vect(i),
--     -- debug(0)              => open,
--     -- debug(8 downto 2)     => open,
--      debug                 => open,
--      stat_out              => open
--    );
--
--lvds_deserializer_b_inst : entity serdes_1_to_n_data_ddr_s8_diff
--    generic map(
--      S                     => 8,
--      D                     => 1,
--      DIFF_TERM             => "TRUE"
--    )
--    Port map(
--      use_phase_detector    => '1',
--      rxioclkp              => IOCLK0_0,
--      rxioclkn              => IOCLK1_0,
--      gclk                  => div_clk_bufg_0,
--      rxserdesstrobe        => serdesstrobe_0,
--      datain_p(0)           => DxXBp(i),
--      datain_n(0)           => DxXBn(i),
--      reset                 => rst,
--      bitslip               => bitslip,
--      debug_in              => "00",
--      data_out              => adc_data_b_8bit(i),
--      --debug(1)              => data_calib_valid_vect(4 + i),
--      --debug(0)              => open,
--      --debug(8 downto 2)     => open,
--      debug                 => open,
--      stat_out              => open
--    );
end generate;

adc_deserializer_gen2 : for i in 2 to 3 generate

--lvds_deserializer_a_inst : entity serdes_1_to_n_data_ddr_s8_diff
--    generic map(
--      S                     => 8,
--      D                     => 1,
--      DIFF_TERM             => "TRUE"
--    )
--    Port map(
--      use_phase_detector    => '1',
--      rxioclkp              => IOCLK0_1,
--      rxioclkn              => IOCLK1_1,
--      gclk                  => div_clk_bufg_1,
--      rxserdesstrobe        => serdesstrobe_1,
--      datain_p(0)           => DxXAp(i),
--      datain_n(0)           => DxXAn(i),
--      reset                 => rst,
--      bitslip               => bitslip,
--      debug_in              => "00",
--      data_out              => adc_data_a_8bit(i),
--      --debug(1)              => data_calib_valid_vect(i),
--      --debug(0)              => open,
--      --debug(8 downto 2)     => open,
--      debug                 => open,
--      stat_out              => open
--    );
--
--lvds_deserializer_b_inst : entity serdes_1_to_n_data_ddr_s8_diff
--    generic map(
--      S                     => 8,
--      D                     => 1,
--      DIFF_TERM             => "TRUE"
--    )
--    Port map(
--      use_phase_detector    => '1',
--      rxioclkp              => IOCLK0_1,
--      rxioclkn              => IOCLK1_1,
--      gclk                  => div_clk_bufg_1,
--      rxserdesstrobe        => serdesstrobe_1,
--      datain_p(0)           => DxXBp(i),
--      datain_n(0)           => DxXBn(i),
--      reset                 => rst,
--      bitslip               => bitslip,
--      debug_in              => "00",
--      data_out              => adc_data_b_8bit(i),
--      --debug(1)              => data_calib_valid_vect(4 + i),
--      --debug(0)              => open,
--      --debug(8 downto 2)     => open,
--      debug                 => open,
--      stat_out              => open
--    );

lvds_deserializer_a_inst: entity data_deserializer
    generic map(
      DIFF_TERM         => true
    )
    Port map(
      serdes_clk0       => IOCLK0_1,
      serdes_clk1       => IOCLK1_1,
      serdes_divclk     => div_clk_bufg_1,
      serdes_strobe     => serdesstrobe_1,
      data_p            => DxXAp(i),
      data_n            => DxXAn(i),
      reset             => rst,
      bitslip           => bitslip,
      clk               => m_strm_aclk,
      data_out          => adc_data_a_8bit(i),
      valid             => data_calib_valid_vect(i)
    );

lvds_deserializer_b_inst: entity data_deserializer
    generic map(
      DIFF_TERM         => true
    )
    Port map(
      serdes_clk0       => IOCLK0_1,
      serdes_clk1       => IOCLK1_1,
      serdes_divclk     => div_clk_bufg_1,
      serdes_strobe     => serdesstrobe_1,
      data_p            => DxXBp(i),
      data_n            => DxXBn(i),
      reset             => rst,
      bitslip           => bitslip,
      clk               => m_strm_aclk,
      data_out          => adc_data_b_8bit(i),
      valid             => data_calib_valid_vect(4 + i)
    );
end generate;

M_STRM_DATA <= adc_data_b_8bit(3) & adc_data_a_8bit(3) & adc_data_b_8bit(2) & adc_data_a_8bit(2) & adc_data_b_8bit(1) & adc_data_a_8bit(1) & adc_data_b_8bit(0) & adc_data_a_8bit(0);

counter_proc :
process(m_strm_aclk)
begin
  if rising_edge(m_strm_aclk) then
    if (state = counter_st) then
      counter <= counter + 1;
    else
      counter <= (others => '0');
    end if;
  end if;
end process;

rst_counter_proc :
process(m_strm_aclk, state)
begin
  if (state /= rst_st) then
    rst_counter     <= (others => '0');
  else
    if rising_edge(m_strm_aclk) then
      rst_counter <= rst_counter + 1;
    end if;
  end if;
end process;

bitslip_counter_proc :
process(m_strm_aclk, state)
begin
  if (state = rst_st) then
    bitslip_counter     <= (others => '0');
  else
    if rising_edge(m_strm_aclk) then
      if (state = bitslip_st) then 
        bitslip_counter <= bitslip_counter + 1;
      end if;
    end if;
  end if;
end process;

sync_proc :
process(ARESET, m_strm_aclk)
begin
  if (ARESET = '1') then
    state <= idle;
  elsif rising_edge(m_strm_aclk) then
    state <= next_state;
  end if;
end process;

next_state_proc :
process(state, frame_calib_valid, counter(counter'length - 1), frame_data, data_calib_valid_vect, rst_counter(rst_counter'length - 1), bitslip_counter)
begin
  next_state <= state;
    case state is
      when idle =>
        next_state <= rst_st;
      when frame_st =>
        if (frame_calib_valid = '1') then
            if (frame_data = frame_pattern) then
              next_state <= ready_st;
            else
              next_state <= bitslip_st;
            end if;
        end if;
      when bitslip_st =>
          next_state <= counter_st;
      when counter_st => 
        if (counter(counter'length - 1) = '1') then
          next_state <= frame_st;
        end if;
      when ready_st =>
        if (frame_data /= frame_pattern) or (frame_calib_valid /= '1') or (data_calib_valid_vect /= "11111111")then
          next_state <= idle;
        end if;
      when rst_st =>
        if rst_counter(rst_counter'length - 1) = '1' then
          next_state <= rst_cont_st;
        end if;
      when rst_cont_st =>
        if ((frame_calib_valid = '1') and (data_calib_valid_vect = "11111111")) then
          next_state <= frame_st;
        end if;
      when others =>
        next_state <= idle;
    end case;
end process;

out_proc :
process(state)
begin
  bitslip <= '0';
  valid <= '0';
  rst <= '0';
    case state is
      when idle => 
        rst <= '1';
      when bitslip_st =>
        bitslip <= '1';
      when ready_st =>
        valid <= '1';
      when rst_st =>
        rst <= '1';
      when others =>
    end case;
end process;

end Behavioral;
