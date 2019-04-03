----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02.04.2019 15:47:42
-- Design Name: 
-- Module Name: pll_clk_to_serial_clk - Behavioral
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

entity pll_clk_to_serial_clk is
    Port ( 
		in_diff_clk_p	: in std_logic;
		in_diff_clk_n	: in std_logic;
		rst				: in std_logic;
		lock			: out std_logic;
		div_clk			: out std_logic;
		ioclk0			: out std_logic;
		ioclk1			: out std_logic;
		serdesstrobe    : out std_logic	
	);
end pll_clk_to_serial_clk;

architecture Behavioral of pll_clk_to_serial_clk is
	signal in_clk_bufg				: std_logic;
	signal in_clk_bufio_div8		: std_logic;
	signal pll_base_1GHz			: std_logic;
	signal pll_base_1GHz_180    	: std_logic;
	signal pll_base_1GHz_div8   	: std_logic;
	signal pll_base_1GHz_div8_180 	: std_logic;
	signal CLKFBOUT					: std_logic;			
	signal CLKFBIN					: std_logic;
	signal pll_locked				: std_logic;
	signal buff_pll0_lock			: std_logic;
	signal buff_pll1_lock			: std_logic;
	signal buff_pll180_lock			: std_logic;
	signal div_clk0_bufg			: std_logic;
	signal div_clk180_bufg			: std_logic;

begin

IBUFGDS_inst : IBUFGDS
   generic map (
      IBUF_LOW_PWR => FALSE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
      IOSTANDARD => "DEFAULT")
   port map (
      O => in_clk_bufg,  -- Clock buffer output
      I => in_diff_clk_p,  -- Diff_p clock buffer input
      IB => in_diff_clk_n -- Diff_n clock buffer input
   );

BUFIO2_inst : BUFIO2
   generic map (
      DIVIDE => 8,           -- DIVCLK divider (1,3-8)
      DIVIDE_BYPASS => FALSE, -- Bypass the divider circuitry (TRUE/FALSE)
      I_INVERT => FALSE,     -- Invert clock (TRUE/FALSE)
      USE_DOUBLER => TRUE   -- Use doubler circuitry (TRUE/FALSE)
   )
   port map (
      DIVCLK => in_clk_bufio_div8,             -- 1-bit output: Divided clock output
      IOCLK => open,               -- 1-bit output: I/O output clock
      SERDESSTROBE => open, -- 1-bit output: Output SERDES strobe (connect to ISERDES2/OSERDES2)
      I => in_clk_bufg                        -- 1-bit input: Clock input (connect to IBUFG)
   );

BUFPLL0_inst : BUFPLL
   generic map (
      DIVIDE => 8,         -- DIVCLK divider (1-8)
      ENABLE_SYNC => TRUE  -- Enable synchrnonization between PLL and GCLK (TRUE/FALSE)
   )
   port map (
      IOCLK => ioclk0,               -- 1-bit output: Output I/O clock
      LOCK => buff_pll0_lock,                 -- 1-bit output: Synchronized LOCK output
      SERDESSTROBE => SERDESSTROBE, -- 1-bit output: Output SERDES strobe (connect to ISERDES2/OSERDES2)
      GCLK => div_clk0_bufg,                 -- 1-bit input: BUFG clock input
      LOCKED => pll_locked,             -- 1-bit input: LOCKED input from PLL
      PLLIN => pll_base_1GHz                -- 1-bit input: Clock input from PLL
   );

BUFPLL1_inst : BUFPLL
   generic map (
      DIVIDE => 8,         -- DIVCLK divider (1-8)
      ENABLE_SYNC => TRUE  -- Enable synchrnonization between PLL and GCLK (TRUE/FALSE)
   )
   port map (
      IOCLK => ioclk1,               -- 1-bit output: Output I/O clock
      LOCK => buff_pll1_lock,                 -- 1-bit output: Synchronized LOCK output
      SERDESSTROBE => open, -- 1-bit output: Output SERDES strobe (connect to ISERDES2/OSERDES2)
      GCLK => div_clk180_bufg,                 -- 1-bit input: BUFG clock input
      LOCKED => pll_locked,             -- 1-bit input: LOCKED input from PLL
      PLLIN => pll_base_1GHz_180                -- 1-bit input: Clock input from PLL
   );



PLL_BASE_inst : PLL_BASE
   generic map (
      BANDWIDTH => "OPTIMIZED",             -- "HIGH", "LOW" or "OPTIMIZED" 
      CLKFBOUT_MULT => 8,                   -- Multiply value for all CLKOUT clock outputs (1-64)
      CLKFBOUT_PHASE => 0.0,                -- Phase offset in degrees of the clock feedback output
                                            -- (0.0-360.0).
      CLKIN_PERIOD => 8.0,                  -- Input clock period in ns to ps resolution (i.e. 33.333 is 30
                                            -- MHz).
      -- CLKOUT0_DIVIDE - CLKOUT5_DIVIDE: Divide amount for CLKOUT# clock output (1-128)
      CLKOUT0_DIVIDE => 1,
      CLKOUT1_DIVIDE => 1,
      CLKOUT2_DIVIDE => 8,
      CLKOUT3_DIVIDE => 1,
      CLKOUT4_DIVIDE => 1,
      CLKOUT5_DIVIDE => 1,
      -- CLKOUT0_DUTY_CYCLE - CLKOUT5_DUTY_CYCLE: Duty cycle for CLKOUT# clock output (0.01-0.99).
      CLKOUT0_DUTY_CYCLE => 0.5,
      CLKOUT1_DUTY_CYCLE => 0.5,
      CLKOUT2_DUTY_CYCLE => 0.5,
      CLKOUT3_DUTY_CYCLE => 0.5,
      CLKOUT4_DUTY_CYCLE => 0.5,
      CLKOUT5_DUTY_CYCLE => 0.5,
      -- CLKOUT0_PHASE - CLKOUT5_PHASE: Output phase relationship for CLKOUT# clock output (-360.0-360.0).
      CLKOUT0_PHASE => 0.0,
      CLKOUT1_PHASE => 180.0,
      CLKOUT2_PHASE => 0.0,
      CLKOUT3_PHASE => 180.0,
      CLKOUT4_PHASE => 0.0,
      CLKOUT5_PHASE => 0.0,
      CLK_FEEDBACK => "CLKFBOUT",           -- Clock source to drive CLKFBIN ("CLKFBOUT" or "CLKOUT0")
      COMPENSATION => "SYSTEM_SYNCHRONOUS", -- "SYSTEM_SYNCHRONOUS", "SOURCE_SYNCHRONOUS", "EXTERNAL" 
      DIVCLK_DIVIDE => 1,                   -- Division value for all output clocks (1-52)
      REF_JITTER => 0.1,                    -- Reference Clock Jitter in UI (0.000-0.999).
      RESET_ON_LOSS_OF_LOCK => FALSE        -- Must be set to FALSE
   )
   port map (
      CLKFBOUT => CLKFBOUT, -- 1-bit output: PLL_BASE feedback output
      -- CLKOUT0 - CLKOUT5: 1-bit (each) output: Clock outputs
      CLKOUT0 => pll_base_1GHz,
      CLKOUT1 => pll_base_1GHz_180,
      CLKOUT2 => pll_base_1GHz_div8,
      CLKOUT3 => pll_base_1GHz_div8_180,
      CLKOUT4 => open,
      CLKOUT5 => open,
      LOCKED => pll_locked,     -- 1-bit output: PLL_BASE lock status output
      CLKFBIN => CLKFBIN,   -- 1-bit input: Feedback clock input
      CLKIN => in_clk_bufio_div8,       -- 1-bit input: Clock input
      RST => RST            -- 1-bit input: Reset input
   );
   
BUFG0_inst : BUFG port map ( O => div_clk0_bufg, I => pll_base_1GHz_div8);
BUFG180_inst : BUFG port map ( O => div_clk180_bufg, I => pll_base_1GHz_div8_180);
   
	CLKFBIN <= CLKFBOUT;
	div_clk <= div_clk0_bufg;
	lock <= pll_locked and buff_pll1_lock and buff_pll0_lock;

end Behavioral;
