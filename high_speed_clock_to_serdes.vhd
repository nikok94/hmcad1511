----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02.04.2019 19:37:43
-- Design Name: 
-- Module Name: high_speed_clock_to_serdes - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity high_speed_clock_to_serdes is
    Port ( 
        in_clk_from_bufg_p  : in std_logic;
        in_clk_from_bufg_n  : in std_logic;

        rst                 : in std_logic;

        clk                 : in std_logic;
        cal                 : in std_logic;

        iodelay_busy        : out std_logic;

        div_clk_bufg        : out std_logic;
        serdesclk0          : out std_logic;
        serdesclk1          : out std_logic;
        serdesstrobe        : out std_logic 
    );
end high_speed_clock_to_serdes;

architecture Behavioral of high_speed_clock_to_serdes is
    type calib_state_machine is (idle, rst, calib, ready, calib_done);
    signal state, next_state : calib_state_machine;
    signal div_clk          : std_logic;
    signal mstr_dly         : std_logic;
    signal slve_dly         : std_logic;
    signal master_dly_busy  : std_logic;
    signal slave_dly_busy   : std_logic;
    signal busy             : std_logic;
    signal iodelay_rst      : std_logic;
    signal iodelay_cal      : std_logic;
    signal calib_ok         : std_logic;

begin

div_clk_bufg_ins : BUFG port map ( I => div_clk, O => div_clk_bufg );

busy <= master_dly_busy or slave_dly_busy;

iodelay_busy <= busy;

sync_proc :
    process(iodelay_clk)
    begin
      if rising_edge(clk) then
        if rst = '1' then 
          state <= idle;
        else
          state <= next_state;
        end if;
      end if;
    end process;
    
data_proc:
    process(state)
    begin
    iodelay_rst <= '0';
    iodelay_cal <= '0';
      case
        when idle =>
          calib_ok <= '0';
        when calib =>
          iodelay_cal <= '1';
        when rst =>
          calib_ok <= '1';
          iodelay_rst <= '1';
        when others =>
      end case;
    end process;

next_state_machine_process:
    process(state, cal, calib_ok, busy)
    begin
      next_state <= state;
        case state is
          when idle =>
            if cal = '1' then
              next_state <= calib;
            end if;
          when calib then
            next_state <= ready;
          when ready then
            if (busy = '0') then
              if (calib_ok = '1') then
                next_state <= calib_done;
              else
                next_state <= rst;
              end if;
            end if;
          when rst => 
            next_state <= calib_done;
          when calib_done => 
            if cal = '1' then
              next_state <= calib;
            end if;
          when others =>
            next_state <= idle;
        end case;
    end process;

MASTER_IODELAY2_inst : IODELAY2
   generic map (
      COUNTER_WRAPAROUND => "WRAPAROUND", -- "STAY_AT_LIMIT" or "WRAPAROUND" 
      DATA_RATE => "DDR",                 -- "SDR" or "DDR" 
      DELAY_SRC => "IDATAIN",                  -- "IO", "ODATAIN" or "IDATAIN" 
      IDELAY2_VALUE => 0,                 -- Delay value when IDELAY_MODE="PCI" (0-255)
      IDELAY_MODE => "NORMAL",            -- "NORMAL" or "PCI" 
      IDELAY_TYPE => "DEFAULT",           -- "FIXED", "DEFAULT", "VARIABLE_FROM_ZERO", "VARIABLE_FROM_HALF_MAX" 
                                          -- or "DIFF_PHASE_DETECTOR" 
      IDELAY_VALUE => 0,                  -- Amount of taps for fixed input delay (0-255)
      ODELAY_VALUE => 0,                  -- Amount of taps fixed output delay (0-255)
      SERDES_MODE => "MASTER",              -- "NONE", "MASTER" or "SLAVE" 
      SIM_TAPDELAY_VALUE => 75            -- Per tap delay used for simulation in ps
   )
   port map (
      BUSY => master_dly_busy,         -- 1-bit output: Busy output after CAL
      DATAOUT => mstr_dly,   -- 1-bit output: Delayed data output to ISERDES/input register
      DATAOUT2 => open, -- 1-bit output: Delayed data output to general FPGA fabric
      DOUT => open,         -- 1-bit output: Delayed data output
      TOUT => open,         -- 1-bit output: Delayed 3-state output
      CAL => '0',           -- 1-bit input: Initiate calibration input
      CE => '0',             -- 1-bit input: Enable INC input
      CLK => clk,           -- 1-bit input: Clock input
      IDATAIN => in_clk_from_bufg_p,   -- 1-bit input: Data input (connect to top-level port or I/O buffer)
      INC => '0',           -- 1-bit input: Increment / decrement input
      IOCLK0 => in_clk_from_bufg_p,     -- 1-bit input: Input from the I/O clock network
      IOCLK1 => in_clk_from_bufg_n,     -- 1-bit input: Input from the I/O clock network
      ODATAIN => '0',   -- 1-bit input: Output data input from output register or OSERDES2.
      RST => iodelay_rst,           -- 1-bit input: Reset to zero or 1/2 of total delay period
      T => '1'                -- 1-bit input: 3-state input signal
   );
   
SLAVE_IODELAY2_inst : IODELAY2
   generic map (
      COUNTER_WRAPAROUND => "WRAPAROUND", -- "STAY_AT_LIMIT" or "WRAPAROUND" 
      DATA_RATE => "DDR",                 -- "SDR" or "DDR" 
      DELAY_SRC => "IDATAIN",                  -- "IO", "ODATAIN" or "IDATAIN" 
      IDELAY2_VALUE => 0,                 -- Delay value when IDELAY_MODE="PCI" (0-255)
      IDELAY_MODE => "NORMAL",            -- "NORMAL" or "PCI" 
      IDELAY_TYPE => "DEFAULT",           -- "FIXED", "DEFAULT", "VARIABLE_FROM_ZERO", "VARIABLE_FROM_HALF_MAX" 
                                          -- or "DIFF_PHASE_DETECTOR" 
      IDELAY_VALUE => 0,                  -- Amount of taps for fixed input delay (0-255)
      ODELAY_VALUE => 0,                  -- Amount of taps fixed output delay (0-255)
      SERDES_MODE => "MASTER",              -- "NONE", "MASTER" or "SLAVE" 
      SIM_TAPDELAY_VALUE => 75            -- Per tap delay used for simulation in ps
   )
   port map (
      BUSY => slave_dly_busy,         -- 1-bit output: Busy output after CAL
      DATAOUT => slve_dly,   -- 1-bit output: Delayed data output to ISERDES/input register
      DATAOUT2 => open, -- 1-bit output: Delayed data output to general FPGA fabric
      DOUT => open,         -- 1-bit output: Delayed data output
      TOUT => open,         -- 1-bit output: Delayed 3-state output
      CAL => iodelay_cal,           -- 1-bit input: Initiate calibration input
      CE => '0',             -- 1-bit input: Enable INC input
      CLK => clk,           -- 1-bit input: Clock input
      IDATAIN => in_clk_from_bufg_n,   -- 1-bit input: Data input (connect to top-level port or I/O buffer)
      INC => '0',           -- 1-bit input: Increment / decrement input
      IOCLK0 => in_clk_from_bufg_p,     -- 1-bit input: Input from the I/O clock network
      IOCLK1 => in_clk_from_bufg_n,     -- 1-bit input: Input from the I/O clock network
      ODATAIN => '0',   -- 1-bit input: Output data input from output register or OSERDES2.
      RST => iodelay_rst,           -- 1-bit input: Reset to zero or 1/2 of total delay period
      T => '1'                -- 1-bit input: 3-state input signal
   );



   BUFIO2_2CLK_inst : BUFIO2_2CLK
   generic map (
      DIVIDE => 3  -- DIVCLK divider (3-8)
   )
   port map (
      DIVCLK => div_clk,             -- 1-bit output: Divided clock output
      IOCLK => serdesclk0,               -- 1-bit output: I/O output clock
      SERDESSTROBE => serdesstrobe, -- 1-bit output: Output SERDES strobe (connect to ISERDES2/OSERDES2)
      I => mstr_dly,                       -- 1-bit input: Clock input (connect to IBUFG)
      IB => slve_dly                      -- 1-bit input: Secondary clock input
   );



BUFIO2_clk1_inst : BUFIO2
   generic map (
      DIVIDE => 8,           -- DIVCLK divider (1,3-8)
      DIVIDE_BYPASS => FALSE, -- Bypass the divider circuitry (TRUE/FALSE)
      I_INVERT => TRUE,     -- Invert clock (TRUE/FALSE)
      USE_DOUBLER => FALSE   -- Use doubler circuitry (TRUE/FALSE)
   )
   port map (
      DIVCLK => open,             -- 1-bit output: Divided clock output
      IOCLK => serdesclk1,               -- 1-bit output: I/O output clock
      SERDESSTROBE => open, -- 1-bit output: Output SERDES strobe (connect to ISERDES2/OSERDES2)
      I => slve_dly                        -- 1-bit input: Clock input (connect to IBUFG)
   );

end Behavioral;
