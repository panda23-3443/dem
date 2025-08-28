library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity spi_master is
  generic (
    DATA_WIDTH : integer := 16; -- payload width
    CLK_DIV    : integer := 4   -- SCLK toggles every CLK_DIV system clocks (must be >=1)
  );
  port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    start    : in  std_logic;                 -- pulse (1 clk) to start transfer
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

  signal bit_index  : integer range -1 to DATA_WIDTH-1 := -1; -- index of current bit to output
  signal shift_reg  : std_logic_vector(DATA_WIDTH-1 downto 0) := (others=>'0');

  signal clk_divcnt : integer range 0 to 65535 := 0; -- wide enough
  signal sclk_r     : std_logic := '0';
  signal sclk_r_prev: std_logic := '0';

  signal start_r    : std_logic := '0';
  signal done_r     : std_logic := '0';
  signal mosi_r     : std_logic := '0';
begin
  sclk <= sclk_r;
  mosi <= mosi_r;
  done <= done_r;
  busy <= '1' when state /= IDLE else '0';

  -- sample start (rising edge detection will be in logic below)
  process(clk, rst)
  begin
    if rst = '1' then
      start_r <= '0';
    elsif rising_edge(clk) then
      start_r <= start;
    end if;
  end process;

  process(clk, rst)
  begin
    if rst = '1' then
      state <= IDLE;
      sclk_r <= '0';
      sclk_r_prev <= '0';
      cs_n <= '1';
      bit_index <= -1;
      shift_reg <= (others=>'0');
      clk_divcnt <= 0;
      done_r <= '0';
      mosi_r <= '0';
    elsif rising_edge(clk) then
      sclk_r_prev <= sclk_r;
      done_r <= '0';

      case state is
        when IDLE =>
          cs_n <= '1';
          sclk_r <= '0';
          clk_divcnt <= 0;
          if (start = '1' and start_r = '0') then  -- detect rising edge of start
            shift_reg <= data_in;
            bit_index <= DATA_WIDTH-1;            -- prepare to send MSB first
            -- set first MOSI so it is valid before the first rising edge of sclk
            mosi_r <= data_in(DATA_WIDTH-1);
            state <= ASSERT_CS;
          end if;

        when ASSERT_CS =>
          cs_n <= '0';
          -- start generating SCLK. Ensure at least one idle (sclk = '0') before first rising.
          clk_divcnt <= 0;
          sclk_r <= '0';
          state <= TRANSFER;

        when TRANSFER =>
          -- clock divider for toggling sclk
          if clk_divcnt >= (CLK_DIV-1) then
            clk_divcnt <= 0;
            sclk_r <= not sclk_r;
          else
            clk_divcnt <= clk_divcnt + 1;
          end if;

          -- detect edges of sclk (using previous value)
          -- On falling edge (previous='1' and current='0') -> change MOSI to next bit (Mode 0)
          if sclk_r_prev = '1' and sclk_r = '0' then
            -- we've just gone through a falling edge => next MOSI bit should be prepared
            if bit_index > 0 then
              bit_index <= bit_index - 1;
              mosi_r <= shift_reg(bit_index - 1); -- next bit
            else
              -- no more bits left to prepare; keep mosi stable (last bit was already output)
              bit_index <= -1;
            end if;
          end if;

          -- On rising edge (previous='0' and current='1') we can sample MISO if needed (not used here)
          -- Check termination: transfer complete when we have toggled SCLK enough times to clock all bits:
          -- A safe condition: bit_index = -1 AND sclk is low (idle) AND we have completed at least one CS active period.
          if bit_index = -1 and sclk_r = '0' and clk_divcnt = 0 then
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
