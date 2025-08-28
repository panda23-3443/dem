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
  constant WIDTH : integer := 16; -- DAC data width assumed
  signal counter : std_logic_vector(WIDTH-1 downto 0) := (others=>'0');

  -- start pulse generation (edge detect)
  signal prev_counter : std_logic_vector(WIDTH-1 downto 0) := (others=>'0');
  signal start_sp : std_logic := '0';

  -- SPI status
  signal busy_sp  : std_logic;
  signal done_sp  : std_logic;

  -- clock divider for spi_master
  constant CLK_DIV : integer := 8; -- tune to desired SCLK frequency (clk/(2*CLK_DIV))
begin

  -- Up/down counter (wrap-around behavior)
  process(clk, rst)
  begin
    if rst = '1' then
      counter <= (others=>'0');
    elsif rising_edge(clk) then
      if up = '1' and down = '0' then
        counter <= std_logic_vector(unsigned(counter) + 1);
      elsif down = '1' and up = '0' then
        counter <= std_logic_vector(unsigned(counter) - 1);
      end if;
    end if;
  end process;

  -- generate start pulse when counter changes or load asserted
  process(clk, rst)
  begin
    if rst = '1' then
      prev_counter <= (others=>'0');
      start_sp <= '0';
    elsif rising_edge(clk) then
      if load = '1' then
        start_sp <= '1';
      elsif prev_counter /= counter then
        start_sp <= '1';
        prev_counter <= counter;
      else
        start_sp <= '0';
      end if;
    end if;
  end process;

  -- instantiate SPI master (MSB first)
  spi_inst : entity work.spi_master
    generic map (
      DATA_WIDTH => WIDTH,
      CLK_DIV    => CLK_DIV
    )
    port map (
      clk     => clk,
      rst     => rst,
      start   => start_sp,
      data_in => counter,
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
