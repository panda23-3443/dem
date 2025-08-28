-- KC705 + AD3552 (584-EVALAD3552RFMC1Z) VHDL project
-- Dosyalar:
-- 1) constraints.xdc
-- 2) spi_master.vhd
-- 3) ad3552_ctrl.vhd
-- 4) tb_spi.vhd
-- Aşağıda her dosya bir başlıkla ayrılmıştır. Vivado'ya eklemeden önce XDC'deki pinleri KC705 kartınızdaki uygun pinlerle değiştirin.

----------------------------------------------------------------
-- File: constraints.xdc
-- Not: KC705 üzerinde hangi header/fmc pini kullanıldığına göre pinleri güncelleyin.

# Clock input (örnek: FPGA board system clock)
set_property PACKAGE_PIN W5 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
create_clock -period 10.0 -name sys_clk -waveform {0 5} [get_ports clk]

# SPI signals (örnek placeholder pin isimleri - LÜTFEN KENDİ KARTINIZA GÖRE GÜNCELLEYİN)
set_property PACKAGE_PIN H17 [get_ports spi_sclk]    # replace H17
set_property IOSTANDARD LVCMOS33 [get_ports spi_sclk]

set_property PACKAGE_PIN G17 [get_ports spi_mosi]    # replace G17
set_property IOSTANDARD LVCMOS33 [get_ports spi_mosi]

set_property PACKAGE_PIN K15 [get_ports spi_cs_n]    # replace K15
set_property IOSTANDARD LVCMOS33 [get_ports spi_cs_n]

set_property PACKAGE_PIN J16 [get_ports spi_miso]    # optional, replace J16
set_property IOSTANDARD LVCMOS33 [get_ports spi_miso]

# LEDs / Debug outputs
set_property PACKAGE_PIN N14 [get_ports led_busy]
set_property IOSTANDARD LVCMOS33 [get_ports led_busy]

set_property PACKAGE_PIN P14 [get_ports led_done]
set_property IOSTANDARD LVCMOS33 [get_ports led_done]

# Constraints notes:
# - "PACKAGE_PIN" değerlerini kendi KC705 pinout tablonuzdan güncelleyin.
# - AD3552 RFMC kartı hangi FMC sinyallerini kullandığına bakıp uygun pinleri atayın.

----------------------------------------------------------------
-- File: spi_master.vhd
-- Simple SPI master (mode 0), MSB first. Configurable data width and SCLK divider.
-- This SPI master sends the frame on request (start). Outputs busy while sending.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_master is
  generic (
    DATA_WIDTH : integer := 16; -- payload width
    CLK_DIV    : integer := 4   -- SCLK toggles every CLK_DIV system clocks (must be >=2)
  );
  port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    start    : in  std_logic;                 -- pulse to start transfer
    data_in  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    sclk     : out std_logic;                 -- SPI clock (idles low)
    mosi     : out std_logic;
    miso     : in  std_logic := '0';          -- optional
    cs_n     : out std_logic;                 -- active low chip select
    busy     : out std_logic;
    done     : out std_logic
  );
end entity;

architecture rtl of spi_master is
  type state_type is (IDLE, ASSERT_CS, TRANSFER, FINISH);
  signal state      : state_type := IDLE;
  signal bit_cnt    : integer range 0 to DATA_WIDTH := 0;
  signal shift_reg  : std_logic_vector(DATA_WIDTH-1 downto 0) := (others=>'0');
  signal clk_divcnt : integer range 0 to CLK_DIV-1 := 0;
  signal sclk_r     : std_logic := '0';
  signal sclk_edge  : std_logic := '0';
  signal done_r     : std_logic := '0';
begin
  sclk <= sclk_r;
  done <= done_r;
  busy <= '1' when state /= IDLE else '0';

  process(clk, rst)
  begin
    if rst = '1' then
      state <= IDLE;
      sclk_r <= '0';
      cs_n <= '1';
      bit_cnt <= 0;
      shift_reg <= (others=>'0');
      clk_divcnt <= 0;
      done_r <= '0';
      mosi <= '0';
    elsif rising_edge(clk) then
      done_r <= '0';
      case state is
        when IDLE =>
          sclk_r <= '0';
          cs_n <= '1';
          clk_divcnt <= 0;
          if start = '1' then
            shift_reg <= data_in;
            bit_cnt <= DATA_WIDTH;
            state <= ASSERT_CS;
          end if;

        when ASSERT_CS =>
          cs_n <= '0';
          -- wait one sclk period before first edge
          clk_divcnt <= 0;
          sclk_r <= '0';
          state <= TRANSFER;

        when TRANSFER =>
          -- SCLK generation
          if clk_divcnt = CLK_DIV-1 then
            clk_divcnt <= 0;
            sclk_r <= not sclk_r;
            sclk_edge <= '1';
          else
            clk_divcnt <= clk_divcnt + 1;
            sclk_edge <= '0';
          end if;

          -- On falling edge -> output next MOSI bit (Mode 0: sample on rising, change on falling)
          if sclk_edge = '1' and sclk_r = '1' then -- just after toggled to '1' means we finished rising edge
            -- sample miso could be read here if needed
            null;
          elsif sclk_edge = '1' and sclk_r = '0' then -- just after toggled to '0' => falling edge occurred
            if bit_cnt > 0 then
              mosi <= shift_reg(DATA_WIDTH-1);
              shift_reg <= shift_reg(DATA_WIDTH-2 downto 0) & '0';
              bit_cnt <= bit_cnt - 1;
            end if;
          end if;

          -- transfer complete when all bits shifted and we have completed full clock cycle
          if bit_cnt = 0 and clk_divcnt = 0 and sclk_r = '0' then
            state <= FINISH;
          end if;

        when FINISH =>
          cs_n <= '1';
          done_r <= '1';
          state <= IDLE;
      end case;
    end if;
  end process;
end architecture;

----------------------------------------------------------------
-- File: ad3552_ctrl.vhd
-- Top-level controller: up-down counter -> formats data for DAC and triggers spi_master.
-- Assumes DAC accepts 16-bit data (MSB first). Eğer DAC farklı bir protokol istiyorsa frame formatını değiştirin.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ad3552_ctrl is
  port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    up       : in  std_logic; -- control for up-down counter
    down     : in  std_logic;
    load     : in  std_logic; -- optional load into DAC immediately

    -- SPI pins to DAC (to be constrained in XDC)
    spi_sclk : out std_logic;
    spi_mosi : out std_logic;
    spi_cs_n : out std_logic;
    spi_miso : in  std_logic := '0';

    -- LEDs for debug
    led_busy : out std_logic;
    led_done : out std_logic
  );
end entity;

architecture rtl of ad3552_ctrl is
  constant WIDTH : integer := 16; -- DAC resolution (assumed)

  signal counter : std_logic_vector(WIDTH-1 downto 0) := (others=>'0');
  signal start_sp : std_logic := '0';
  signal busy_sp  : std_logic;
  signal done_sp  : std_logic;
  signal data_frame : std_logic_vector(WIDTH-1 downto 0);

  -- clock divider for spi_master (tune for your board)
  constant CLK_DIV : integer := 8; -- adjust to set SCLK frequency

begin
  -- up-down counter: simple saturating or wrapping behavior
  process(clk, rst)
  begin
    if rst = '1' then
      counter <= (others=>'0');
    elsif rising_edge(clk) then
      if up = '1' and down = '0' then
        counter <= std_logic_vector(unsigned(counter) + 1);
      elsif down = '1' and up = '0' then
        counter <= std_logic_vector(unsigned(counter) - 1);
      elsif load = '1' then
        null; -- keep current value but trigger send below
      end if;
    end if;
  end process;

  -- Format data for DAC: direct mapping 0..2^16-1 => 0..Vref (we'll assume external scaling to achieve 0..5V)
  data_frame <= counter;

  -- Trigger SPI send whenever counter changes or load asserted
  process(clk, rst)
    variable prev : std_logic_vector(WIDTH-1 downto 0) := (others=>'0');
  begin
    if rst = '1' then
      start_sp <= '0';
      prev := (others=>'0');
    elsif rising_edge(clk) then
      if prev /= data_frame then
        start_sp <= '1';
        prev := data_frame;
      else
        start_sp <= '0';
      end if;
      if load = '1' then
        start_sp <= '1';
      end if;
    end if;
  end process;

  -- instantiate SPI master
  spi_inst : entity work.spi_master
    generic map (
      DATA_WIDTH => WIDTH,
      CLK_DIV    => CLK_DIV
    )
    port map (
      clk     => clk,
      rst     => rst,
      start   => start_sp,
      data_in => data_frame,
      sclk    => spi_sclk,
      mosi    => spi_mosi,
      miso    => spi_miso,
      cs_n    => spi_cs_n,
      busy    => busy_sp,
      done    => done_sp
    );

  led_busy <= busy_sp;
  led_done <= done_sp;

end architecture;

----------------------------------------------------------------
-- File: tb_spi.vhd
-- Testbench: simulates ad3552_ctrl with SPI master. Generates clock and toggles up/down to exercise code.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_spi is
end entity;

architecture sim of tb_spi is
  signal clk_tb   : std_logic := '0';
  signal rst_tb   : std_logic := '1';
  signal up_tb    : std_logic := '0';
  signal down_tb  : std_logic := '0';
  signal load_tb  : std_logic := '0';

  signal spi_sclk_tb : std_logic;
  signal spi_mosi_tb : std_logic;
  signal spi_cs_n_tb : std_logic;
  signal spi_miso_tb : std_logic := '0';

  signal led_busy_tb : std_logic;
  signal led_done_tb : std_logic;

  constant CLK_PERIOD : time := 10 ns; -- 100 MHz test clock

begin
  -- DUT instantiation
  dut: entity work.ad3552_ctrl
    port map (
      clk => clk_tb,
      rst => rst_tb,
      up  => up_tb,
      down => down_tb,
      load => load_tb,
      spi_sclk => spi_sclk_tb,
      spi_mosi => spi_mosi_tb,
      spi_cs_n => spi_cs_n_tb,
      spi_miso => spi_miso_tb,
      led_busy => led_busy_tb,
      led_done => led_done_tb
    );

  -- clock generation
  clk_proc : process
  begin
    while now < 5 ms loop
      clk_tb <= '0';
      wait for CLK_PERIOD/2;
      clk_tb <= '1';
      wait for CLK_PERIOD/2;
    end loop;
    wait;
  end process;

  -- stimulus
  stim_proc: process
  begin
    -- reset
    rst_tb <= '1';
    wait for 100 ns;
    rst_tb <= '0';
    wait for 200 ns;

    -- increment a few times
    for i in 0 to 15 loop
      up_tb <= '1';
      wait for 100 ns;
      up_tb <= '0';
      wait for 400 ns;
    end loop;

    -- decrement a few times
    for i in 0 to 7 loop
      down_tb <= '1';
      wait for 100 ns;
      down_tb <= '0';
      wait for 400 ns;
    end loop;

    -- load current value explicitly
    load_tb <= '1';
    wait for 20 ns;
    load_tb <= '0';

    wait for 2 ms;
    assert false report "End of simulation" severity failure;
  end process;

end architecture;

-- End of project files
-- Kullanım notları:
-- 1) XDC dosyasındaki pin atamalarını KC705 pinout'unuza göre güncelleyin (özellikle FMC/PMOD pinleri).
-- 2) AD3552 kartının veri formatı (komut bitleri, adres, MSB/LSB) farklı olabilir; datasheet'e göre frame yapısını ayarlayın.
-- 3) SPI clock frekansını CLK_DIV parametresi ile ayarlayın. KC705 üzerinde hız sınırlarına dikkat edin.
-- 4) Gerçek donanımda Vref ve çıkış ölçeklemesi ile 0..5V aralığını elde edin (bazı DAC'lar harici op-amp gerektirir).
