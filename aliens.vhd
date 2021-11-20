-- Aliens for the defender game

------------- Aliens Spawning on RNG -------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.all;

entity AlienRNG is 
	port(
		max10_clk 			: in std_logic;
		RNG 					: in std_logic_vector(9 downto 0);
		alive					: inout std_logic := '0';
		size 					: inout integer;
		color					: out std_logic_vector(11 downto 0);
		x_pos					: inout integer := 641;
		y_pos					: inout integer := 240;
		min_period			: in integer := 5;
		RNG_bit_map			: in std_logic_vector(9 downto 0);
		ship_x				: in integer;
		ship_y 				: in integer;
		collision			: inout std_logic := '0';
		spawn					: inout std_logic := '0'
	);
end entity;

architecture beh of AlienRNG is

	signal RNG_instance 			: std_logic_vector(9 downto 0);
	signal movement_clock 		: std_logic;
	signal size_unsigned			: unsigned(2 downto 0);
	
begin
	initialize : process(alive)
	begin
		if(rising_edge(alive)) then		-- save RNG instance			
			RNG_instance <= RNG;
			size_unsigned(2 downto 0) <= unsigned(RNG_instance(3 downto 1));
			color <= "110000001111";
			size <= to_integer(size_unsigned);
		end if;
	end process;
		
	move_clock : process (max10_clk)
	variable movement_counter : integer := 0;
	begin
		if(rising_edge(max10_clk)) then
			movement_counter := movement_counter + 1;		
		end if;
		
		if (movement_counter <= 100000) then
			movement_clock <= NOT movement_clock;
			movement_counter := 0;
		end if;

	end process;
	
	movement : process (movement_clock)
	variable floaty : integer := 0;
	variable fast : integer := 0;
	variable slow : integer := 0;
	variable diagonalSpeed : integer := 0;
	variable diagonalDir : integer := 0;
	
	variable y_max : INTEGER := 67;
	variable y_min : INTEGER := 413;
	variable x_max : INTEGER := 640;
	variable x_min : INTEGER := 25;
	
	begin
		if(spawn = '1') then
			x_pos <= 641;
			y_pos <= 240;
		elsif(rising_edge(movement_clock) AND alive = '1') then
			case size_unsigned is
				when "011"|"000" =>	-- floaty
					if(floaty > 500) then
						if(RNG(8) = '1') then
							x_pos <= x_pos - 1;
						end if;
						
						if (y_pos <= y_max) then
							y_pos <= y_pos + 1;
						elsif (y_pos >= y_min) then
							y_pos <= y_pos - 1;
						elsif (RNG(0) = '1') then
							y_pos <= y_pos + 1;
						else
							y_pos <= y_pos - 1;
						end if;
						floaty := 0;
					else
						floaty := floaty + 1;
					end if;
					
					
				when "100"|"101" =>	-- diagonal
					if(diagonalSpeed > 10) then
						if(diagonalDir = 0) then	--up left
							if(x_pos <= x_min) then
								diagonalDir := 3;
							else
								x_pos <= x_pos - 1;
							end if;
							
							if (y_pos <= y_max - ((size+1)*8)) then
								diagonalDir := 1;
							else
								y_pos <= y_pos - 1;
							end if;
							
						elsif(diagonalDir = 1) then	-- down left
							if(x_pos <= x_min) then
								diagonalDir := 2;
							else
								x_pos <= x_pos - 1;
							end if;
							
							if(y_pos >= y_min) then
								diagonalDir := 0;
							else
								y_pos <= y_pos + 1;
							end if;
							
							
						elsif(diagonalDir = 2) then	-- down right
							if(x_pos >= x_max - ((size+1)*8)) then
								diagonalDir := 1;
							else
								x_pos <= x_pos + 1;
							end if;	
								
							if(y_pos >= y_min) then
								diagonalDir := 3;
							else
								y_pos <= y_pos + 1;
							end if;
							
						else									-- up right
							if(x_pos >= x_max- ((size+1)*8)) then
								diagonalDir := 0;
							else
								x_pos <= x_pos + 1;
							end if;	
								
							if(y_pos <= y_max + (size+1)*8) then
								diagonalDir := 2;
							else
								y_pos <= y_pos - 1;
							end if;
						end if;
						diagonalSpeed := 0;
					else
						diagonalSpeed := diagonalSpeed + 1;	
					end if;
					
					
					
				when "001"|"010" =>	-- horizontal fast
					if(fast > 5) then
						x_pos <= x_pos - 1;
						fast := 0;
					else
						fast := fast + 1;
					end if;
					
				when "110"|"111" =>	-- slowly approach player
					if (slow > 20) then
						if (ship_x > x_pos) then
							x_pos <= x_pos + 1;
						else
							x_pos <= x_pos - 1;
						end if;
						
						if (ship_y > y_pos) then
							y_pos <= y_pos + 1;
						else
							y_pos <= y_pos - 1;
						end if;						
						slow := 0;
					else
						slow := slow + 1;
					end if;
			end case;
		end if;
	end process;
	
	spawning : process (max10_clk, alive)
	variable timeSinceLastSpawn : unsigned(63 downto 0) := (OTHERS => '0');
	begin
		timeSinceLastSpawn := timeSinceLastSpawn + 1;
		if( (RNG AND RNG_bit_map) = RNG_bit_map AND alive = '0' AND timeSinceLastSpawn > to_unsigned(50000000*min_period,63) ) then
			spawn <= '1';
			timeSinceLastSpawn := (OTHERS => '0');
		elsif ( rising_edge(alive) ) then
			spawn <= '0';			
		else
			spawn <= spawn;
		end if;
	end process;
	
	-- alive flag
	alienAliveFlag : process (max10_clk, collision, spawn)
	begin
		if(spawn = '1') then
			alive <= '1';
		elsif( collision = '1' ) then
			alive <= '0';
		else
			alive <= alive;
		end if;
	end process;
	
end architecture;

------------- Aliens Spawning on Constant Timer -----------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.all;

entity AlienTimer is 
	port(
		max10_clk 			: in std_logic;
		RNG 					: in std_logic_vector(9 downto 0);
		alive					: inout std_logic := '0';
		size 					: inout integer;
		color					: out std_logic_vector(11 downto 0);
		x_pos					: inout integer := 641;
		y_pos					: inout integer := 153;
		period_seconds 	: in integer;
		ship_x				: in integer;
		ship_y 				: in integer;
		collision			: inout std_logic := '0';
		spawn					: inout std_logic := '0'
	);
end entity;

architecture beh of AlienTimer is

	signal RNG_instance 			: std_logic_vector(9 downto 0);
	signal i 						: std_logic;
	signal movement_clock 		: std_logic;
	signal size_unsigned			: unsigned(2 downto 0);
	
begin
	initialize : process(alive)
	begin
		if(rising_edge(alive)) then		-- save RNG instance			
			RNG_instance <= RNG;
			size_unsigned(2 downto 0) <= unsigned(RNG_instance(3 downto 1));
			color <= "000011110000";
			size <= to_integer(size_unsigned);
		end if;
	end process;	

	move_clock : process (max10_clk)
	variable movement_counter : integer := 0;
	begin
		if(rising_edge(max10_clk)) then
			movement_counter := movement_counter + 1;		
		end if;
		
		if (movement_counter <= 100000) then
			movement_clock <= NOT movement_clock;
			movement_counter := 0;
		end if;
	end process;
	
	movement : process (movement_clock)
	variable floaty : integer := 0;
	variable fast : integer := 0;
	variable slow : integer := 0;
	variable diagonalSpeed : integer := 0;
	variable diagonalDir : integer := 0;
	
	variable y_max : INTEGER := 67;
	variable y_min : INTEGER := 413;
	variable x_max : INTEGER := 640;
	variable x_min : INTEGER := 25;
	
	begin
		if(spawn = '1') then
			x_pos <= 641;
			y_pos <= 153;
		elsif(rising_edge(movement_clock) AND alive = '1') then
			case size_unsigned is
				when "011"|"000" =>	-- floaty
					if(floaty > 500) then
						if(RNG(8) = '1') then
							x_pos <= x_pos - 1;
						end if;
						
						if (y_pos <= y_max) then
							y_pos <= y_pos + 1;
						elsif (y_pos >= y_min) then
							y_pos <= y_pos - 1;
						elsif (RNG(0) = '1') then
							y_pos <= y_pos + 1;
						else
							y_pos <= y_pos - 1;
						end if;
						floaty := 0;
					else
						floaty := floaty + 1;
					end if;
					
					
				when "100"|"101" =>	-- diagonal
					if(diagonalSpeed > 10) then
						if(diagonalDir = 0) then	--up left
							if(x_pos <= x_min) then
								diagonalDir := 3;
							else
								x_pos <= x_pos - 1;
							end if;
							
							if (y_pos <= y_max - ((size+1)*8)) then
								diagonalDir := 1;
							else
								y_pos <= y_pos - 1;
							end if;
							
						elsif(diagonalDir = 1) then	-- down left
							if(x_pos <= x_min) then
								diagonalDir := 2;
							else
								x_pos <= x_pos - 1;
							end if;
							
							if(y_pos >= y_min) then
								diagonalDir := 0;
							else
								y_pos <= y_pos + 1;
							end if;
							
							
						elsif(diagonalDir = 2) then	-- down right
							if(x_pos >= x_max - ((size+1)*8)) then
								diagonalDir := 1;
							else
								x_pos <= x_pos + 1;
							end if;	
								
							if(y_pos >= y_min) then
								diagonalDir := 3;
							else
								y_pos <= y_pos + 1;
							end if;
							
						else									-- up right
							if(x_pos >= x_max- ((size+1)*8)) then
								diagonalDir := 0;
							else
								x_pos <= x_pos + 1;
							end if;	
								
							if(y_pos <= y_max + (size+1)*8) then
								diagonalDir := 2;
							else
								y_pos <= y_pos - 1;
							end if;
						end if;
						diagonalSpeed := 0;
					else
						diagonalSpeed := diagonalSpeed + 1;	
					end if;
					
					
					
				when "001"|"010" =>	-- horizontal fast
					if(fast > 5) then
						x_pos <= x_pos - 1;
						fast := 0;
					else
						fast := fast + 1;
					end if;
					
				when "110"|"111" =>	-- slowly approach player
					if (slow > 20) then
						if (ship_x > x_pos) then
							x_pos <= x_pos + 1;
						else
							x_pos <= x_pos - 1;
						end if;
						
						if (ship_y > y_pos) then
							y_pos <= y_pos + 1;
						else
							y_pos <= y_pos - 1;
						end if;						
						slow := 0;
					else
						slow := slow + 1;
					end if;
			end case;
		end if;
	end process;
	spawning : process (max10_clk, alive)
	variable timeSinceLastSpawn : unsigned(63 downto 0)  := (OTHERS => '0');
	begin
		timeSinceLastSpawn := timeSinceLastSpawn + 1;
		if(alive = '0' AND timeSinceLastSpawn > to_unsigned(50000000*period_seconds, 63)) then
			spawn <= '1';
			timeSinceLastSpawn := (OTHERS => '0');
		elsif ( rising_edge(alive) ) then
			spawn <= '0';
		else
			spawn <= spawn;
		end if;	
	end process;
	
	-- alive flag
	alienAliveFlag : process (max10_clk, collision, spawn)
	begin
		if(spawn = '1') then
			alive <= '1';
		elsif( collision = '1' ) then
			alive <= '0';
		else
			alive <= alive;
		end if;
	end process;
end architecture;

------------- Aliens Spawning on Score based timer -------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.all;

entity AlienScoreTimer is 
	port(
		max10_clk 				: in std_logic;
		RNG 						: in std_logic_vector(9 downto 0);
		alive						: inout std_logic := '0';
		size 						: inout integer;
		color						: out std_logic_vector(11 downto 0);
		x_pos						: inout integer := 641; 
		y_pos						: inout integer := 326;
		max_period_seconds	: in integer;
		min_period_seconds	: in integer;
		current_score			: in integer;
		ship_x				: in integer;
		ship_y 				: in integer;
		collision			: inout std_logic := '0';
		spawn					: inout std_logic := '0'
	);
end entity;

architecture beh of AlienScoreTimer is

	signal RNG_instance 			: std_logic_vector(9 downto 0);
	signal i 						: std_logic;
	signal movement_clock 		: std_logic;
	signal size_unsigned			: unsigned(2 downto 0);
	
begin
	initialize : process(alive)
	begin
		if(rising_edge(alive)) then		-- save RNG instance			
			RNG_instance <= RNG;
			size_unsigned(2 downto 0) <= unsigned(RNG_instance(3 downto 1));
			color <= "000000001111";
			size <= to_integer(size_unsigned);
		end if;
	end process;
	
	move_clock : process (max10_clk)
	variable movement_counter : integer := 0;
	begin
		if(rising_edge(max10_clk)) then
			movement_counter := movement_counter + 1;		
		end if;
		
		if (movement_counter <= 10000000) then
			movement_clock <= NOT movement_clock;
			movement_counter := 0;			
		end if;
	end process;
	
	movement : process (movement_clock)
	variable floaty : integer := 0;
	variable fast : integer := 0;
	variable slow : integer := 0;
	variable diagonalSpeed : integer := 0;
	variable diagonalDir : integer := 0;
	
	variable y_max : INTEGER := 67;
	variable y_min : INTEGER := 413;
	variable x_max : INTEGER := 640;
	variable x_min : INTEGER := 25;
	
	begin
		if(spawn = '1') then
			x_pos <= 641;
			y_pos <= 326;
		elsif(rising_edge(movement_clock) AND alive = '1') then
			case size_unsigned is
				when "011"|"000" =>	-- floaty
					if(floaty > 500) then
						if(RNG(8) = '1') then
							x_pos <= x_pos - 1;
						end if;
						
						if (y_pos <= y_max) then
							y_pos <= y_pos + 1;
						elsif (y_pos >= y_min) then
							y_pos <= y_pos - 1;
						elsif (RNG(0) = '1') then
							y_pos <= y_pos + 1;
						else
							y_pos <= y_pos - 1;
						end if;
						floaty := 0;
					else
						floaty := floaty + 1;
					end if;
					
					
				when "100"|"101" =>	-- diagonal
					if(diagonalSpeed > 10) then
						if(diagonalDir = 0) then	--up left
							if(x_pos <= x_min) then
								diagonalDir := 3;
							else
								x_pos <= x_pos - 1;
							end if;
							
							if (y_pos <= y_max - ((size+1)*8)) then
								diagonalDir := 1;
							else
								y_pos <= y_pos - 1;
							end if;
							
						elsif(diagonalDir = 1) then	-- down left
							if(x_pos <= x_min) then
								diagonalDir := 2;
							else
								x_pos <= x_pos - 1;
							end if;
							
							if(y_pos >= y_min) then
								diagonalDir := 0;
							else
								y_pos <= y_pos + 1;
							end if;
							
							
						elsif(diagonalDir = 2) then	-- down right
							if(x_pos >= x_max - ((size+1)*8)) then
								diagonalDir := 1;
							else
								x_pos <= x_pos + 1;
							end if;	
								
							if(y_pos >= y_min) then
								diagonalDir := 3;
							else
								y_pos <= y_pos + 1;
							end if;
							
						else									-- up right
							if(x_pos >= x_max- ((size+1)*8)) then
								diagonalDir := 0;
							else
								x_pos <= x_pos + 1;
							end if;	
								
							if(y_pos <= y_max + (size+1)*8) then
								diagonalDir := 2;
							else
								y_pos <= y_pos - 1;
							end if;
						end if;
						diagonalSpeed := 0;
					else
						diagonalSpeed := diagonalSpeed + 1;	
					end if;
					
					
					
				when "001"|"010" =>	-- horizontal fast
					if(fast > 5) then
						x_pos <= x_pos - 1;
						fast := 0;
					else
						fast := fast + 1;
					end if;
					
				when "110"|"111" =>	-- slowly approach player
					if (slow > 20) then
						if (ship_x > x_pos) then
							x_pos <= x_pos + 1;
						else
							x_pos <= x_pos - 1;
						end if;
						
						if (ship_y > y_pos) then
							y_pos <= y_pos + 1;
						else
							y_pos <= y_pos - 1;
						end if;						
						slow := 0;
					else
						slow := slow + 1;
					end if;
			end case;
		end if;
	end process;
	--spawn flag
	spawning : process (max10_clk, alive)
	variable timeSinceLastSpawn : unsigned(31 downto 0)  := (OTHERS => '0');
	variable numSpawns : integer := 0;
	variable period : integer := max_period_seconds;
	begin
		timeSinceLastSpawn := timeSinceLastSpawn + 1;
		if(period > min_period_seconds) then
			period := max_period_seconds - numSpawns;
		else
			period := min_period_seconds;
		end if;
		
		if(alive = '0' AND timeSinceLastSpawn > (50000000*period) ) then
			spawn <= '1';
			timeSinceLastSpawn := (OTHERS => '0');
			numSpawns := numSpawns + 1;
		elsif ( rising_edge(alive) ) then
			spawn <= '0';		
		else
			spawn <= spawn;
		end if;
	end process;
	
	-- alive flag
	alienAliveFlag : process (max10_clk, collision, spawn)
	begin
		if(spawn = '1') then
			alive <= '1';
		elsif( collision = '1' ) then
			alive <= '0';
		else
			alive <= alive;
		end if;
	end process;
end architecture;
