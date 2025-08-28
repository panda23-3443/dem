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
    while now < 20 ms loop
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

    -- increment a few times (each up pulse is 1clk)
    for i in 0 to 7 loop
      up_tb <= '1';
      wait for CLK_PERIOD; -- one clock pulse
      up_tb <= '0';
      wait for 50 * CLK_PERIOD; -- wait to allow transfer finish
    end loop;

    -- decrement a few times
    for i in 0 to 3 loop
      down_tb <= '1';
      wait for CLK_PERIOD;
      down_tb <= '0';
      wait for 50 * CLK_PERIOD;
    end loop;

    -- explicit load pulse
    load_tb <= '1';
    wait for CLK_PERIOD;
    load_tb <= '0';
    wait for 2000 * CLK_PERIOD;

    assert false report "End of simulation" severity failure;
  end process;

end architecture;
