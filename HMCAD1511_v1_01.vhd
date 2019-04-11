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
use work.lvds_deserializer;
use work.high_speed_clock_to_serdes;

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
    generic (
      C_IDELAY_VALUE      : integer := 16;
      C_DUAL_PATTERN      : std_logic_vector(15 downto 0):= x"55AA";
      C_IODELAY_FIXED     : boolean := FALSE
    );
    Port (
      LCLKp         : in std_logic;
      LCLKn         : in std_logic;
      DxXAp         : in std_logic_vector(3 downto 0);
      DxXAn         : in std_logic_vector(3 downto 0);
      DxXBp         : in std_logic_vector(3 downto 0);
      DxXBn         : in std_logic_vector(3 downto 0);
      CAL           : in std_logic;
      CAL_DONE      : out std_logic;
      CLK           : in std_logic;
      ARESET        : in std_logic;

      M_STRM_VALID  : out std_logic;
      M_STRM_DATA   : out std_logic_vector(63 downto 0);
      DIVCLK_OUT    : out std_logic
    );
end HMCAD1511_v1_01;

architecture Behavioral of HMCAD1511_v1_01 is
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
    signal pll_clkfbout                     : std_logic;
    signal pll_clkfbout_bufg                : std_logic;
    signal pll_clkfbin                      : std_logic;
    signal pll_locked                       : std_logic;
    signal pll_clkout0_125MHz               : std_logic;
    signal lvds_deserializers_busy_vec      : std_logic_vector(7 downto 0);
    signal lvds_deserializers_busy          : std_logic;
    signal lvds_deserializers_busy_d        : std_logic;
    signal lvds_deserializers_busy_edge     : std_logic;
    signal lvds_deserializers_busy_fall     : std_logic;
    signal cal_in                           : std_logic;
    signal cal_in_d1                        : std_logic;
    signal cal_in_d2                        : std_logic;
    signal cal_in_d3                        : std_logic;
    signal cal_in_sync                      : std_logic;
    signal data_out_valid                   : std_logic;
    signal data_out_valid_d1                : std_logic;
    signal data_out_valid_d2                : std_logic;
    signal data_out_valid_d3                : std_logic;
    signal cal_done_sync                    : std_logic;
    signal iodelay_clk                      : std_logic;
    signal iodelay_inc                      : std_logic;
    signal iodelay_ce                       : std_logic;
    signal iodelay_cal                      : std_logic;
    signal iodelay_rst                      : std_logic;
    signal iodelay_calib                    : std_logic;
    signal vio_calib_control                : std_logic_vector(35 downto 0);
    signal ila_control                      : std_logic_vector(35 downto 0);
    signal vio_calib_vector                 : std_logic_vector(3 downto 0);
    signal vio_calib_vector_d               : std_logic_vector(3 downto 0);
    signal data_out                         : std_logic_vector(66 downto 0);
    

begin
DIVCLK_OUT <= div_clk_bufg_1;
M_STRM_VALID <= data_out_valid;
CAL_DONE <= cal_done_sync;

cal_capture_proc :
   process(CLK)
   begin
     if ARESET = '1' then
      cal_in <= '0';
      data_out_valid_d1 <= '0';
      data_out_valid_d2 <= '0';
      data_out_valid_d3 <= '0';
     elsif rising_edge(clk) then
       if (CAL = '1') then
         cal_in <= '1';
       elsif data_out_valid_d2 = '1' then
         cal_in <= '0';
       end if;
       data_out_valid_d1 <= data_out_valid;
       data_out_valid_d2 <= data_out_valid_d1;
       data_out_valid_d3 <= data_out_valid_d2;
       cal_done_sync <= (not data_out_valid_d3) and data_out_valid_d2;
     end if;
   end process;

sync_cal_in_proc :
   process(div_clk_bufg_1)
   begin
     if ARESET = '1' then
      cal_in_d1 <= '0';
      cal_in_d2 <= '0';
      cal_in_d3 <= '0';
     elsif rising_edge(div_clk_bufg_1) then
      cal_in_d1 <= cal_in;
      cal_in_d2 <= cal_in_d1;
      cal_in_d3 <= cal_in_d2;
      cal_in_sync <= (not cal_in_d3) and cal_in_d2;
     end if;
   end process;


lvds_deserializers_busy <=  lvds_deserializers_busy_vec(7) or lvds_deserializers_busy_vec(6) or 
                            lvds_deserializers_busy_vec(5) or lvds_deserializers_busy_vec(4) or 
                            lvds_deserializers_busy_vec(3) or lvds_deserializers_busy_vec(2) or 
                            lvds_deserializers_busy_vec(1) or lvds_deserializers_busy_vec(0);
valid_process :
   process(div_clk_bufg_1, ARESET)
   begin
     if (ARESET = '1') then
       data_out_valid <= '0';
       lvds_deserializers_busy_edge <= '0';
       lvds_deserializers_busy_fall <= '0';
     elsif rising_edge(div_clk_bufg_1) then
       lvds_deserializers_busy_d <= lvds_deserializers_busy;
       lvds_deserializers_busy_edge <= not lvds_deserializers_busy_d and lvds_deserializers_busy;
       lvds_deserializers_busy_fall <= not lvds_deserializers_busy and lvds_deserializers_busy_d;
         if lvds_deserializers_busy_edge = '1' then
           data_out_valid <= '0';
         elsif (lvds_deserializers_busy_fall = '1') then
           data_out_valid <= '1';
         end if;
       end if;
   end process;

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
        in_clk_from_bufg    => lclk_ibufg_out,
        div_clk_bufg        => div_clk_bufg_0,
        serdesclk0          => IOCLK0_0,
        serdesclk1          => IOCLK1_0,
        serdesstrobe        => serdesstrobe_0
    );

high_speed_clock_to_serdes_0 : entity high_speed_clock_to_serdes

    Port map( 
        in_clk_from_bufg    => lclk_ibufg_out,
        div_clk_bufg        => div_clk_bufg_1,
        serdesclk0          => IOCLK0_1,
        serdesclk1          => IOCLK1_1,
        serdesstrobe        => serdesstrobe_1
    );
adc_deserializer_gen1 : for i in 0 to 1 generate
lvds_deserializer_a_inst: entity lvds_deserializer
    generic map(
      C_IDELAY_VALUE        => C_IDELAY_VALUE,
      C_DUAL_PATTERN        => C_DUAL_PATTERN,
      C_IODELAY_FIXED       => C_IODELAY_FIXED
    )
    Port map( 
      data_in_p             => DxXAp(i),
      data_in_n             => DxXAn(i),
      ioclk0          		=> IOCLK0_0,
      ioclk1          		=> IOCLK1_0,
      clkdiv         		=> div_clk_bufg_1,
      serdesstrobe   		=> serdesstrobe_0,
      
      iodelay_clk           => iodelay_clk,
      iodelay_inc           => iodelay_inc,
      iodelay_ce            => iodelay_ce ,
      iodelay_cal           => iodelay_cal,
      iodelay_rst           => iodelay_rst,
      
      
      data_8bit_out         => adc_data_a_8bit(i),
	  start_calib           => cal_in_sync,
      calib_busy            => lvds_deserializers_busy_vec(2*i),
	  rst					=> ARESET
    );

lvds_deserializer_b_inst: entity lvds_deserializer
    generic map(
      C_IDELAY_VALUE        => C_IDELAY_VALUE,
      C_DUAL_PATTERN        => C_DUAL_PATTERN,
      C_IODELAY_FIXED       => C_IODELAY_FIXED
    )
    Port map( 
      data_in_p             => DxXBp(i),
      data_in_n             => DxXBn(i),
      ioclk0         		=> IOCLK0_0,
      ioclk1         		=> IOCLK1_0,
      clkdiv         		=> div_clk_bufg_1,
      serdesstrobe   		=> serdesstrobe_0,
      
      iodelay_clk           => iodelay_clk,
      iodelay_inc           => iodelay_inc,
      iodelay_ce            => iodelay_ce ,
      iodelay_cal           => iodelay_cal,
      iodelay_rst           => iodelay_rst,
      
      data_8bit_out         => adc_data_b_8bit(i),
	  start_calib           => cal_in_sync,
      calib_busy            => lvds_deserializers_busy_vec(2*i + 1),
	  rst					=> ARESET
    );
end generate;

adc_deserializer_gen2 : for i in 2 to 3 generate
lvds_deserializer_a_inst: entity lvds_deserializer
    generic map(
      C_IDELAY_VALUE        => C_IDELAY_VALUE,
      C_DUAL_PATTERN        => C_DUAL_PATTERN,
      C_IODELAY_FIXED       => C_IODELAY_FIXED
    )
    Port map( 
      data_in_p             => DxXAp(i),
      data_in_n             => DxXAn(i),
      ioclk0        		=> IOCLK0_1,
      ioclk1        		=> IOCLK1_1,
      clkdiv        		=> div_clk_bufg_1,
      serdesstrobe  		=> serdesstrobe_1,
      
      iodelay_clk           => iodelay_clk,
      iodelay_inc           => iodelay_inc,
      iodelay_ce            => iodelay_ce ,
      iodelay_cal           => iodelay_cal,
      iodelay_rst           => iodelay_rst,
      
      data_8bit_out         => adc_data_a_8bit(i),
	  start_calib           => cal_in_sync,
      calib_busy            => lvds_deserializers_busy_vec(2*i),
	  rst					=> ARESET
    );

lvds_deserializer_b_inst: entity lvds_deserializer
    generic map(
       C_IDELAY_VALUE        => C_IDELAY_VALUE,
       C_DUAL_PATTERN        => C_DUAL_PATTERN,
       C_IODELAY_FIXED       => C_IODELAY_FIXED
    )
    Port map( 
      data_in_p             => DxXBp(i),
      data_in_n             => DxXBn(i),
      ioclk0         		=> IOCLK0_1,
      ioclk1         		=> IOCLK1_1,
      clkdiv         		=> div_clk_bufg_1,
      serdesstrobe   		=> serdesstrobe_1,
      
      iodelay_clk           => iodelay_clk,
      iodelay_inc           => iodelay_inc,
      iodelay_ce            => iodelay_ce ,
      iodelay_cal           => iodelay_cal,
      iodelay_rst           => iodelay_rst,
      
      data_8bit_out         => adc_data_b_8bit(i),
	  start_calib           => cal_in_sync,
      calib_busy            => lvds_deserializers_busy_vec(2*i + 1),
	  rst					=> ARESET
    );
end generate;

    M_STRM_DATA <= adc_data_b_8bit(3) & adc_data_a_8bit(3) & 
                   adc_data_b_8bit(2) & adc_data_a_8bit(2) & 
                   adc_data_b_8bit(1) & adc_data_a_8bit(1) & 
                   adc_data_b_8bit(0) & adc_data_a_8bit(0);
    
    
--    iodelay_clk <= div_clk_bufg_1;
--    iodelay_inc <= '1';
--    
--icon_inst : ENTITY chipscope_icon
--  port map(
--    CONTROL0    => vio_calib_control,
--    CONTROL1    => ila_control
--    );
--
--vio_calib_inst :ENTITY chipscope_vio_3
--  port map(
--    CONTROL		=> vio_calib_control,
--    CLK			=> div_clk_bufg_1,
--    SYNC_OUT	=> vio_calib_vector
--    );
--
--ila_inst : ENTITY ila
--  port map(
--    CONTROL => ila_control,
--    CLK     => div_clk_bufg_1,
--    DATA    => DATA_out(63 downto 0),
--    TRIG0(0)   => iodelay_calib
--    );
--
--    DATA_out(7 downto 0)        <= adc_data_a_8bit(0);
--    DATA_out(15 downto 8)       <= adc_data_b_8bit(0);
--    DATA_out(23 downto 16)      <= adc_data_a_8bit(1);
--    DATA_out(31 downto 24)      <= adc_data_b_8bit(1);
--    DATA_out(39 downto 32)      <= adc_data_a_8bit(2);
--    DATA_out(47 downto 40)      <= adc_data_b_8bit(2);
--    DATA_out(55 downto 48)      <= adc_data_a_8bit(3);
--    DATA_out(63 downto 56)      <= adc_data_b_8bit(3);
--    DATA_out(66 downto 64)      <= "000";
--
--
--
--process(div_clk_bufg_1)
--begin
--  if rising_edge(div_clk_bufg_1) then
--    vio_calib_vector_d <= vio_calib_vector;
--    iodelay_calib<= (not vio_calib_vector_d(3)) and vio_calib_vector(3);
--    iodelay_ce  <= (not vio_calib_vector_d(2)) and vio_calib_vector(2);
--    iodelay_cal <= (not vio_calib_vector_d(1)) and vio_calib_vector(1);
--    iodelay_rst <= (not vio_calib_vector_d(0)) and vio_calib_vector(0);
--  end if;
--end process;

end Behavioral;
