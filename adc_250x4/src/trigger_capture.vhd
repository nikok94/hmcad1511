----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09.04.2019 16:15:17
-- Design Name: 
-- Module Name: trigger_capture - Behavioral
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

entity trigger_capture is
    generic (
        c_data_width    : integer := 64
    );
    Port ( 
      clk               : in std_logic;
      rst               : in std_logic;
      control_reg       : in std_logic_vector(15 downto 0);
      control_reg_wr_en : in std_logic;

      data              : in std_logic_vector(c_data_width-1 downto 0); -- входные значения данных от АЦП
      ext_trig          : in std_logic; -- внешний триггер
      
      trigger_start     : out std_logic -- выходной сигнал управляет модулем захвата данных
    );
end trigger_capture;

architecture Behavioral of trigger_capture is
    signal capture_mode     : std_logic_vector(1 downto 0); -- определяет режим работы захвата 00 - захват по записи в контрольный регистр, 01 - захват по переднему фронту при значении входных данных больше capture_level
                                                            -- 10 - захват по заднему фронту при значении входных данных меньше capture_level, 11 - захват по внешнему триггеру ext_trig
    signal capture_level    : std_logic_vector(7 downto 0); -- определяет уровень срабатывания триггера при capture_mode = 01 и capture_mode = 10
    signal control_reg_setup: std_logic_vector(3 downto 0);
    
    signal control_start    : std_logic;
    signal ext_start        : std_logic;
    signal ext_start_sync_vec : std_logic_vector(3 downto 0);
    signal level_up_start   : std_logic;
    signal level_up         : std_logic:= '0';
    signal level_up_d       : std_logic:= '0';
    signal level_down_start : std_logic;
    signal level_down       : std_logic:= '0';
    signal level_down_d     : std_logic:= '0';
    signal old_data         : std_logic_vector(c_data_width-1 downto 0);
    signal level_up_vect    : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal level_down_vect  : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal capture_enable   : std_logic;
    signal capture_start    : std_logic;

begin

trigger_start <= capture_start when capture_enable = '1' else '0';

capture_enable_proc :
  process(clk)
  begin
    if rising_edge(clk) then
      if (rst = '1' or capture_start = '1') then 
        capture_enable <= '0';
      elsif (control_reg_wr_en = '1') then
        capture_enable <= '1';
      end if;
    end if;
  end process;

control_reg_process :
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then 
        capture_mode <= "00";
        capture_level <= x"7f";
        control_reg_setup <= (others => '0');
      elsif (control_reg_wr_en = '1') then
        capture_mode <= control_reg(1 downto 0);
        capture_level <= control_reg(15 downto 8);
        control_reg_setup(0) <= '1';
      elsif (capture_mode = "00") then
        control_reg_setup(0) <= '0';
        control_reg_setup(3 downto 1) <= control_reg_setup(2 downto 0);
        control_start <= (not control_reg_setup(3)) and control_reg_setup(2);
      else 
        control_start <= '0';
      end if;
    end if;
  end process;

ext_start_proc :
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then 
        ext_start_sync_vec <= (others => '0');
      elsif (capture_mode = "11") then
        ext_start_sync_vec(0) <= ext_trig;
        ext_start_sync_vec(3 downto 1) <= ext_start_sync_vec(2 downto 0);
        ext_start <= (not ext_start_sync_vec(3)) and ext_start_sync_vec(2);
      else 
        ext_start <= '0';
      end if;
    end if;
  end process;

trigger_start_out_proc :
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        capture_start <= '0';
      else
        case capture_mode is
          when "00" => capture_start <= control_start;
          when "01" => capture_start <= level_up_start;
          when "10" => capture_start <= level_down_start;
          when "11" => capture_start <= ext_start;
          when others => capture_start <= '0';
        end case;
      end if;
    end if;
  end process;

old_data_process :
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then 
        old_data <= (others => '0');
      else 
        old_data <= data;
      end if;
    end if;
  end process;

generate_process : for i in 0 to c_data_width/8 - 1 generate
  level_up_vect(i) <= '1' when ((old_data(8*i + 7 downto 8*i) < data(8*i + 7 downto 8*i)) and (old_data(8*i + 7 downto 8*i) >= capture_level)) else '0';
  level_down_vect(i) <= '1' when ((old_data(8*i + 7 downto 8*i) > data(8*i + 7 downto 8*i)) and (old_data(8*i + 7 downto 8*i) <= capture_level)) else '0';
end generate generate_process;

level_up_process :
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        level_up   <= '0';
      elsif (level_up_vect /= 0) then 
        level_up <= '1';
      elsif (control_reg_wr_en = '1') then
        level_up <= '0';
      end if;
      level_up_d <= level_up;
      level_up_start <= (not level_up_d) and level_up;
    end if;
  end process;

level_down_process :
    process(clk)
    begin
      if rising_edge(clk) then
        if rst = '1' then
          level_down   <= '0';
        elsif (level_down_vect /= 0) then 
          level_down <= '1';
        elsif (control_reg_wr_en = '1') then
          level_down <= '0';
        end if;
        level_down_d <= level_down;
        level_down_start <= (not level_down_d) and level_down;
      end if;
    end process;

end Behavioral;