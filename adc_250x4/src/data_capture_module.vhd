----------------------------------------------------------------------------------
-- Company: ИСЭ
-- Engineer: МНС КОКИН Д.С.
-- 
-- Create Date: 12.04.2019 09:47:15
-- Design Name: Проект для платы гигагерцового АЦП на базе HMCAD1511
-- Module Name: data_capture_module - Behavioral
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

entity data_capture_module is
    generic (
      c_max_window_size_width   : integer := 16;
      c_strm_data_width         : integer := 64;
      c_trig_delay              : integer := 4
    );
    Port ( 
      clk               : in std_logic;
      rst               : in std_logic;
      trigger_start     : in std_logic;
      window_size       : in std_logic_vector(c_max_window_size_width - 1 downto 0);
      trig_position     : in std_logic_vector(c_max_window_size_width - 1 downto 0);

      s_strm_data       : in std_logic_vector(c_strm_data_width - 1 downto 0);
      s_strm_valid      : in std_logic;
      s_strm_ready      : out std_logic;

      m_strm_data       : out std_logic_vector(c_strm_data_width - 1 downto 0);
      m_strm_valid      : out std_logic;
      m_strm_ready      : in std_logic
    );
end data_capture_module;

architecture Behavioral of data_capture_module is
    component  mem_64_4096 IS
    PORT (
        clka : IN STD_LOGIC;
        wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
        addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
        dina : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
        clkb : IN STD_LOGIC;
        addrb : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
        doutb : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
    );
    END component mem_64_4096;
    
    type state_machine is (edle, wait_trigger_start, capture, send_word, buff_state, rd_addr_edge);
    signal state, next_state    : state_machine;
    constant c_memory_max_width : std_logic_vector(c_max_window_size_width - 1 downto 0):= x"1000";
    signal wr_addr              : std_logic_vector(c_max_window_size_width - 1 downto 0);
    signal rd_addr              : std_logic_vector(c_max_window_size_width - 1 downto 0);
    signal s_ready              : std_logic;
    signal m_valid              : std_logic;
    signal wr_en                : std_logic;
    signal addr_start_position  : std_logic_vector(c_max_window_size_width - 1 downto 0);
    signal addr_end_position    : std_logic_vector(c_max_window_size_width - 1 downto 0);

begin
m_strm_valid <= m_valid;

trigger_setting_proc :
   process(clk)
   begin
     if rising_edge(clk) then
       if (state = edle) then
          addr_start_position    <= (others => '0');
          addr_end_position      <= (others => '0');
        elsif ((trigger_start = '1') and (state = wait_trigger_start)) then
          
          if (wr_addr >= trig_position + c_trig_delay ) then
            addr_start_position <= wr_addr - (trig_position + c_trig_delay);
          else
            addr_start_position <= c_memory_max_width - (trig_position + c_trig_delay) + wr_addr + 1;
          end if;
          
          if wr_addr <= (c_memory_max_width - window_size + trig_position + c_trig_delay) then
            addr_end_position <= (wr_addr + window_size - trig_position - c_trig_delay );
          else
            addr_end_position <= (wr_addr + window_size - trig_position - c_trig_delay -1) - c_memory_max_width;
          end if;
          
        end if;
      end if;
   end process;

s_strm_ready <= s_ready;

wr_en <= s_strm_valid and s_ready;

wr_addr_process  : 
  process(clk)
  begin
    if rising_edge(clk) then
      if (state = edle) then
        wr_addr <= (others => '0');
      elsif (state = capture) or (state = wait_trigger_start) then
        if (s_ready = '1' and s_strm_valid = '1') then
          if (wr_addr > c_memory_max_width - 1) then 
            wr_addr <= (others => '0');
          else
            wr_addr <= wr_addr + 1;
          end if;
        end if;
      end if;
    end if;
  end process;
  
rd_addr_process  : 
  process(clk)
  begin
    if rising_edge(clk) then
      if (state = edle) then
        rd_addr <= (others => '0');
      elsif (state = capture) then
        rd_addr <= addr_start_position;
      else
        if (state = rd_addr_edge) then
          if (rd_addr > c_memory_max_width-1) then 
            rd_addr <= (others => '0');
          else
            rd_addr <= rd_addr + 1;
          end if;
        end if;
      end if;
    end if;
  end process;


sync_state_machine_proc :
    process(clk)
    begin
      if rising_edge(clk) then
        if (rst = '1') then
          state <= edle;
        else
          state <= next_state;
        end if;
      end if;
    end process;

output_state_machine_proc :
    process(state, wr_addr)
    begin
      s_ready <= '0';
      m_valid <= '0';
      case state is
        when edle => 
        when wait_trigger_start =>
          s_ready <= '1';
        when capture =>
          s_ready <= '1';
        when send_word =>
          m_valid <= '1';
        when others =>
      end case;
    end process;

next_state_machine_proc :
    process(state, trigger_start, wr_addr, rd_addr, addr_end_position, m_strm_ready, window_size)
    begin
      next_state <= state;
      case state is
        when edle => 
          next_state <= wait_trigger_start;
        when wait_trigger_start =>
          if ((trigger_start = '1') and (window_size < c_memory_max_width)) then
            next_state <= capture;
          end if;
        when capture =>
          if (wr_addr = addr_end_position) then
            next_state <= send_word;
          end if;
        when send_word =>
          if (m_strm_ready = '1') then
            next_state <= buff_state;
          end if;
        when rd_addr_edge => 
          next_state <= send_word;
        when buff_state =>
            if (rd_addr = addr_end_position) then
              next_state <= edle;
            else 
              next_state <= rd_addr_edge;
          end if;
        when others =>
          next_state <= edle;
      end case;
    end process;

memory_inst : mem_64_4096
  PORT MAP(
    clka    => clk,
    wea(0)  => wr_en,
    addra   => wr_addr(11 downto 0),
    dina    => s_strm_data,
    clkb    => clk,
    addrb   => rd_addr(11 downto 0),
    doutb   => m_strm_data
  );

end Behavioral;
