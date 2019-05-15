----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05.04.2019 11:06:45
-- Design Name: 
-- Module Name: ADC1511_250MHzX4_Top - Behavioral
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
library UNISIM;
use UNISIM.VComponents.all;

library work;
--use work.ila_spi_reg;
--use work.icon_0;
--use work.chipscope_vio;
--use work.chipscope_vio_out16;
use work.HMCAD1511_v1_01;
use work.data_capture_module;
use work.trigger_capture;
use work.infrastructure_module;
use work.fifo_sream;
use work.spi_adc_250x4_master;
use work.QuadSPI_adc_250x4_module;


entity ADC1511_250MHzX4_Top is
    Port (
        in_clk_20MHz            : in std_logic;

        adc_lck_p               : in std_logic;
        adc_lck_n               : in std_logic;
        adc_dx_a_p              : in std_logic_vector(3 downto 0);
        adc_dx_a_n              : in std_logic_vector(3 downto 0);
        adc_dx_b_p              : in std_logic_vector(3 downto 0);
        adc_dx_b_n              : in std_logic_vector(3 downto 0);
        
        spifi_cs                : in std_logic;
        spifi_sck               : in std_logic;
        spifi_miso              : inout std_logic;
        spifi_mosi              : inout std_logic;
        spifi_sio2              : inout std_logic;
        spifi_sio3              : inout std_logic;

        fpga_sck                : in std_logic;
        fpga_cs                 : in std_logic;
        fpga_miso               : out std_logic;
        fpga_mosi               : in std_logic;
        
        ext_trig                : in std_logic;
        fast_adc_data_valid     : out std_logic;
        fast_adc_calib_done     : out std_logic;
        low_adc_data_valid      : out std_logic
        );
end ADC1511_250MHzX4_Top;

architecture Behavioral of ADC1511_250MHzX4_Top is
    signal adc_data_out                             : std_logic_vector(63 downto 0);

    signal ila_control_0                            : std_logic_vector(35 downto 0);
    signal ila_control_1                            : std_logic_vector(35 downto 0);
    signal adc_clk_div8                             : std_logic;
    signal div_clk_bufg_1                           : std_logic;
    
    signal vio_vector_in                            : std_logic_vector(50 downto 0);
    signal vio_vector_out                           : std_logic_vector(50 downto 0);
    signal vio_control                              : std_logic_vector(35 downto 0);
    signal vio_window                               : std_logic_vector(35 downto 0);
    signal vio_position                             : std_logic_vector(35 downto 0);
    signal vio_reg_control                          : std_logic_vector(35 downto 0);

    signal adc_lck_p_ibufg_df                       : std_logic;
    signal adc_lck_n_ibufg_df                       : std_logic;
    
    signal adc_lck_bufg                             : std_logic;
    
    signal CLKFBIN                                  : std_logic;
    signal CLKFBOUT                                 : std_logic;
    
    signal CLK_100MHz                               : std_logic;
    signal pll_clk0                                 : std_logic;
    
    signal bitslip_sync_vect                        : std_logic_vector(3 downto 0);
    signal bitslip                                  : std_logic;
    signal adc_calib_vector                         : std_logic_vector(3 downto 0);
    signal adc_calib                                : std_logic;
    signal ce_sync_vect                             : std_logic_vector(3 downto 0);
    signal iodelay_ce                               : std_logic;
    signal iodelay_inc                              : std_logic;
    
    signal adc_fclk_bufg                            : std_logic;
    signal fclk_bufio                               : std_logic;
    signal rst                                      : std_logic:= '1';
    signal rst_vector                               : std_logic_vector(7 downto 0) := (others => '1');
    signal adc_data_valid                           : std_logic;
    
    signal trigger_start_channel_2                  : std_logic;
    signal trigger_start_channel_1                  : std_logic;
    signal trigger_start                            : std_logic;
    signal channel_control                          : std_logic_vector(1 downto 0);

    signal m_strm_data                              : std_logic_vector(63 downto 0);
    signal m_strm_valid                             : std_logic;
    signal m_strm_ready                             : std_logic;
    
    signal clk_125MHz                               : std_logic;
    signal clk_250MHz                               : std_logic;
    signal stream_fifo_rst                          : std_logic;
    signal m_stream_valid                           : std_logic;
    signal m_stream_data                            : std_logic_vector(63 downto 0);
    signal error                                    : std_logic;

    signal vio_wr_en                                : std_logic;
    signal fifo_64_4_full                           : STD_LOGIC;
    signal fifo_64_4_almost_full                    : STD_LOGIC;
    signal fifo_64_4_empty                          : STD_LOGIC;
    signal fifo_64_4_almost_empty                   : STD_LOGIC;
    signal fifo_64_4_valid                          : STD_LOGIC;
    signal MISO_I                                   : std_logic;
    signal MISO_O                                   : std_logic;
    signal MISO_T                                   : std_logic;
    signal MOSI_I                                   : std_logic;
    signal MOSI_O                                   : std_logic;
    signal MOSI_T                                   : std_logic;
    signal m_fcb_aresetn                            : std_logic;
    signal m_fcb_addr                               : std_logic_vector(8 - 1 downto 0);
    signal m_fcb_wrdata                             : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_wrreq                              : std_logic;
    signal m_fcb_wrack                              : std_logic;
    signal m_fcb_rddata                             : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_rdreq                              : std_logic;
    signal m_fcb_rdack                              : std_logic;

    signal trig_set_up_reg                          : std_logic_vector(15 downto 0):= x"7f00";
    signal trig_window_width_reg                    : std_logic_vector(15 downto 0):= x"1000";
    signal trig_position_reg                        : std_logic_vector(15 downto 0):= x"0800";
    signal control_reg                              : std_logic_vector(15 downto 0):= (others => '0');
    signal calib_pattern_reg                        : std_logic_vector(15 downto 0):= x"55AA";
    signal wr_req_vec                               : std_logic_vector(5 downto 0);
    signal reg_address_int                          : integer;
    signal pll_lock                                 : std_logic;
    signal qspi_compleat                            : std_logic;
    signal infrst_rst_out                           : std_logic;
    signal control_reg_0_d                          : std_logic;
    signal spifi_cs_d                               : std_logic;
    signal spifi_cs_up                              : std_logic;
    signal capture_module_rst                       : std_logic;

begin
-- модуль infrastructure_module использует поделенную тактовую частоту генерируемую АЦП 
-- поделенная тактовая частота используется для тактирования pll 

infr_inst : entity infrastructure_module
    Port map( 
      clk_in        => adc_clk_div8,
      rst_in        => '0',
      pll_lock      => pll_lock,
      clk_out_125MHz=> clk_125MHz,
      clk_out_250MHz=> clk_250MHz,
      rst_out       => infrst_rst_out
    );

rst <= infrst_rst_out or control_reg(1);

adc_data_receiver : entity HMCAD1511_v1_01
    generic map(
      C_IDELAY_VALUE      => 12,
      C_IODELAY_FIXED     => TRUE
    )
    Port map(
      LCLKp             => adc_lck_p,
      LCLKn             => adc_lck_n,
      DxXAp             => adc_dx_a_p,
      DxXAn             => adc_dx_a_n,
      DxXBp             => adc_dx_b_p,
      DxXBn             => adc_dx_b_n,
      CAL_DUAL_PATTERN  => calib_pattern_reg,
      CAL               => adc_calib,
      ARESET            => rst,
      CAL_DONE          => open,
      CLK               => clk_125MHz,
      DIVCLK_OUT        => adc_clk_div8,
      M_STRM_VALID      => adc_data_valid,
      M_STRM_DATA       => adc_data_out
    );
stream_fifo_rst <= adc_data_valid;

-- fifo_sream предназначена для перехода с тактовой частоты АЦП на частоту PLL
-- выходная тактовая частота от PLL используется для тактирования оснвного дезайна
-- т.е. модулями захвата данных

Stream_fifo_inst : ENTITY fifo_sream
  PORT map(
    m_aclk          => clk_125MHz,
    s_aclk          => adc_clk_div8,
    s_aresetn       => stream_fifo_rst,
    s_axis_tvalid   => adc_data_valid,
    s_axis_tready   => open,
    s_axis_tdata    => adc_data_out,
    m_axis_tvalid   => m_stream_valid,
    m_axis_tready   => '1',
    m_axis_tdata    => m_stream_data
  );

fast_adc_calib_done <= m_stream_valid;

--capture_valid_proc :
--    process(clk_125MHz)
--    begin
--      if rising_edge(clk_125MHz) then
--        if (rst = '1') or (spifi_cs_up = '1') then
--          fast_adc_data_valid <= '0';
--        elsif m_strm_valid = '1' then
--          fast_adc_data_valid <= '1';
--        end if;
--        spifi_cs_d <= spifi_cs;
--      end if;
--    end process;
--
--spifi_cs_up <= spifi_cs and (not spifi_cs_d);

trigger_capture_channel_1 : entity trigger_capture
    generic map(
        c_data_width    => 32
    )
    Port map( 
      clk               => clk_125MHz,
      rst               => rst,
      control_reg       => trig_set_up_reg,
      control_reg_wr_en => wr_req_vec(5),

      data              => m_stream_data(31 downto 0),   -- входные значения данных от АЦП
      ext_trig          => ext_trig,        -- внешний триггер
      
      trigger_start     => trigger_start_channel_1    -- выходной сигнал управляет модулем захвата данных
    );
    
trigger_capture_channel_2 : entity trigger_capture
    generic map(
        c_data_width    => 32
    )
    Port map( 
      clk               => clk_125MHz,
      rst               => rst,
      control_reg       => trig_set_up_reg,
      control_reg_wr_en => wr_req_vec(5),

      data              => m_stream_data(63 downto 32),   -- входные значения данных от АЦП
      ext_trig          => ext_trig,        -- внешний триггер
      
      trigger_start     => trigger_start_channel_2    -- выходной сигнал управляет модулем захвата данных
    );


-- выбор управляющего канала для захвата триггера

channel_control <= trig_set_up_reg(3 downto 2);

trigger_start_mux_process:
    process (channel_control, trigger_start_channel_1, trigger_start_channel_2)
    begin
      case channel_control is 
         when "00" => trigger_start <= trigger_start_channel_1 or trigger_start_channel_2;
         when "01" => trigger_start <= trigger_start_channel_1;
         when "10" => trigger_start <= trigger_start_channel_2;
         when others => trigger_start<= trigger_start_channel_1 or trigger_start_channel_2;
      end case;
    end process;
    

capture_module_rst <= rst or spifi_cs_up;

stream_data_capture_inst    : entity data_capture_module
    generic map (
      c_max_window_size_width   => 16,
      c_strm_data_width         => 64,
      c_trig_delay              => 0
    )
    Port map(
      clk                   => clk_125MHz,
      rst                   => capture_module_rst,
      trigger_start         => trigger_start,
      window_size           => trig_window_width_reg,
      trig_position         => trig_position_reg,

      s_strm_data           => m_stream_data,
      s_strm_valid          => m_stream_valid,
      s_strm_ready          => open,

      m_strm_data           => m_strm_data,
      m_strm_valid          => m_strm_valid,
      m_strm_ready          => m_strm_ready 
    );

-- m_strm_ready <= '1';

QuadSPI_adc_250x4_module_inst : entity QuadSPI_adc_250x4_module
    Port map(
      clk_250MHz_in     => clk_250MHz,
      spifi_cs          => spifi_cs  ,
      spifi_sck         => spifi_sck ,
      spifi_miso        => spifi_miso,
      spifi_mosi        => spifi_mosi,
      spifi_sio2        => spifi_sio2,
      spifi_sio3        => spifi_sio3,
      
      s_strm_clk        => clk_125MHz,
      s_strm_rst        => rst,
      s_strm_data       => m_strm_data,
      s_strm_valid      => m_strm_valid,
      s_strm_ready      => m_strm_ready,
      fast_adc_valid    => fast_adc_data_valid
    );

-- Модуль SPI для прогрузки управляющих регистров 
-- размер адреса 1 байт
-- размер данных 2 байта

spi_fcb_master_inst : entity spi_adc_250x4_master
    generic map(
      C_CPHA            => 1,
      C_CPOL            => 1,
      C_LSB_FIRST       => 0
    )
    Port map( 
      SCK               => fpga_sck,
      CS                => fpga_cs,

      MISO_I            => MISO_I,
      MISO_O            => MISO_O,
      MISO_T            => MISO_T,
      MOSI_I            => MOSI_I,
      MOSI_O            => MOSI_O,
      MOSI_T            => MOSI_T,

      m_fcb_clk         => clk_125MHz,
      m_fcb_areset      => rst,
      m_fcb_addr        => m_fcb_addr   ,
      m_fcb_wrdata      => m_fcb_wrdata ,
      m_fcb_wrreq       => m_fcb_wrreq  ,
      m_fcb_wrack       => m_fcb_wrack  ,
      m_fcb_rddata      => m_fcb_rddata ,
      m_fcb_rdreq       => m_fcb_rdreq  ,
      m_fcb_rdack       => m_fcb_rdack  
    );

OBUFT_inst : OBUFT
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => fpga_miso,     -- Buffer output (connect directly to top-level port)
      I => MISO_O,     -- Buffer input
      T => MISO_T      -- 3-state enable input 
   );

MOSI_I <= fpga_mosi;
-------------------------------------------------
-- управляющие регистры 
-------------------------------------------------
-- процесс записи/чтения регистров управления
reg_address_int <= conv_integer(m_fcb_addr(6 downto 0));

m_fcb_wr_process :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        if (infrst_rst_out = '1') then
          wr_req_vec <= (others => '0');
          control_reg(1 downto 0) <= (others => '0');
        elsif (m_fcb_wrreq = '1') then
          m_fcb_wrack <= '1';
          case reg_address_int is
            when 0 => 
              wr_req_vec(0) <= '1';
              trig_set_up_reg(15 downto 2) <= m_fcb_wrdata(15 downto 2);
            when 1 => 
              wr_req_vec(1) <= '1';
              trig_window_width_reg <= m_fcb_wrdata;
            when 2 =>
              wr_req_vec(2) <= '1';
              trig_position_reg <= m_fcb_wrdata;
            when 3 =>
              wr_req_vec(3) <= '1';
              control_reg(0) <= m_fcb_wrdata(0);
              control_reg(1) <= m_fcb_wrdata(1);
            when 4 =>
              wr_req_vec(4) <= '1';
              calib_pattern_reg <= m_fcb_wrdata;
            when 5 =>
              wr_req_vec(5) <= '1';
              trig_set_up_reg(1 downto 0) <= m_fcb_wrdata(1 downto 0);
            when others =>
          end case;
        else 
          m_fcb_wrack <= '0';
          wr_req_vec <= (others => '0');
          control_reg(1 downto 0) <= (others => '0');
        end if;
        control_reg_0_d <= control_reg(0);
        adc_calib <= (not control_reg_0_d) and control_reg(0);
      end if;
    end process;
    
    control_reg(2) <= pll_lock;

m_fcb_rd_process :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        if (infrst_rst_out = '1') then
        elsif (m_fcb_rdreq = '1') then
          m_fcb_rdack <= '1';
          case reg_address_int is
            when 0 => 
              m_fcb_rddata <= trig_set_up_reg;
            when 1 => 
              m_fcb_rddata <= trig_window_width_reg;
            when 2 =>
              m_fcb_rddata <= trig_position_reg;
            when 3 =>
              m_fcb_rddata(2 downto 0) <= control_reg(2 downto 0);
              m_fcb_rddata(15 downto 3)<= (others => '0');
            when 4 =>
              m_fcb_rddata <= calib_pattern_reg;
            when others =>
          end case;
        else 
          m_fcb_rdack <= '0';
        end if;
      end if;
    end process;


--ila_reg_inst : entity ila_spi_reg 
--  port map (
--    CONTROL     => ila_control_0,
--    CLK         => clk_125MHz,
--    DATA        => control_reg(1) & adc_calib & fpga_mosi & MISO_O & fpga_cs & fpga_sck & m_fcb_rdreq & m_fcb_rddata & m_fcb_wrreq & m_fcb_wrdata & m_fcb_addr,
--    TRIG0       => control_reg(1) & adc_calib & fpga_mosi & MISO_O & fpga_cs & fpga_sck & m_fcb_rdreq & m_fcb_wrreq
--    );
--icon_inst : ENTITY icon_0
--  port map (
--    CONTROL0 => ila_control_0
--    );

end Behavioral;
