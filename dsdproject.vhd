LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

package custom_types is
	type int_array is array (integer range <>) of integer;

	type alien_t is record
		alive : STD_LOGIC;
		size : INTEGER;
		color : STD_LOGIC_VECTOR(11 downto 0);
		x : INTEGER;
		y : INTEGER;
		collision : STD_LOGIC;
	end record alien_t;
	
	type ship_t is record
		alive : STD_LOGIC;
		x : INTEGER;
		y : INTEGER;
		collision : STD_LOGIC;
	end record ship_t;
	
	type player_proj is record
		x : int_array(19 downto 0); --Y POS
		y : int_array(19 downto 0); --X POS
		hs1 : STD_LOGIC_VECTOR(19 downto 0);	--Entity Handshake 1
		hs2 : STD_LOGIC_VECTOR(19 downto 0);	--Entity Handshake 2
		e : STD_LOGIC_VECTOR(19 downto 0); --Entity bit
	end record player_proj;
	
	type alien_array is array (integer range <>) of alien_t;
	
end package;

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;
USE work.custom_types.all;

ENTITY dsdproject IS
  GENERIC(
		
		y_max : INTEGER := 67;
		y_min : INTEGER := 413;
		x_max : INTEGER := 320;
		x_min : INTEGER := 25;
		
		bar_thickness : INTEGER := 5;
		ship_height : INTEGER := 18;
		ship_length : INTEGER := 36;
		
		--X AND Y FOR SCORE ARE BOTTOM RIGHT COORD
		score_x : INTEGER := 630;
		score_y : INTEGER := 48;
		
		max_digits : INTEGER := 6;
		digit_height : INTEGER := 19;
		digit_spacing : INTEGER := 4;
		digit_thickness : INTEGER := 3;
		
		ss_x : int_array(0 to 2) := (25, 70, 115);
		ss_y : INTEGER := 57 --(y_max - bar_thickness - 5)
	);

  PORT(
    disp_ena :  IN   STD_LOGIC;  --display enable ('1' = display time, '0' = blanking time)
    row      :  IN   INTEGER;    --row pixel coordinate
    column   :  IN   INTEGER;    --column pixel coordinate
    red      :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
    green    :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
    blue     :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0'); --blue magnitude output to DAC
	 
	max10_clk : inout std_logic;
	
	--ports to run the accelerometer
	GSENSOR_CS_N : OUT	STD_LOGIC;
	GSENSOR_SCLK : OUT	STD_LOGIC;
	GSENSOR_SDI  : INOUT	STD_LOGIC;
	GSENSOR_SDO  : INOUT	STD_LOGIC;
	reset_accel : in std_logic := '1';
	
	reset_RNG : IN STD_LOGIC;
	
	pause_toggle	: in std_logic;
	shoot			: in std_logic
	
	);
END entity;

ARCHITECTURE behavior OF dsdproject IS
	--FOR DRAWING COLOR W/ ONE VECTOR--
	signal colorconcat : STD_LOGIC_VECTOR(11 downto 0);

	--PLAYER DATA--
	signal ship : ship_t := (alive => '1', x => x_min, y => (240 + ship_height/2), collision => '0');
	signal spare_ships : INTEGER := 3;

	--SCORE AND SCOREBOARD--
	signal score : INTEGER := 0;

	--ENTITIES--
	signal alien : alien_array(11 downto 0);
	signal p_proj : player_proj := (x => (OTHERS => 0), y => (OTHERS => 0), hs1 => (OTHERS => '0'), hs2 => (OTHERS => '0'), e => (OTHERS => '0'));

	--CLOCK RELATED DATA--
	signal clock_x, clock_y : STD_LOGIC := '0';
	signal data_x_magnitude, data_y_magnitude : std_logic_vector(7 downto 0);
	signal countX : integer := 1;
	signal countY : integer := 1;
	signal data_x, data_y, data_z : STD_LOGIC_VECTOR(15 downto 0);	
	signal clockWithPause 		: std_logic := '0';
	signal projectile_clock : std_logic := '0';

	--OTHER--
	signal RNG : std_logic_vector(9 downto 0);
	signal pause 	: std_logic := '0';
	
	-- Accelerometer component
	component ADXL345_controller is port(	
		reset_n     : IN STD_LOGIC;
		clk         : IN STD_LOGIC;
		data_valid  : OUT STD_LOGIC;
		data_x      : OUT STD_LOGIC_VECTOR(15 downto 0);
		data_y      : OUT STD_LOGIC_VECTOR(15 downto 0);
		data_z      : OUT STD_LOGIC_VECTOR(15 downto 0);
		SPI_SDI     : OUT STD_LOGIC;
		SPI_SDO     : IN STD_LOGIC;
		SPI_CSN     : OUT STD_LOGIC;
		SPI_CLK     : OUT STD_LOGIC	
    );	
    end component;
	 
	 -- 10 Bit RNG, the LSB repeats more often than the MSB
	component RNG10 is
		port (
			set, clkToggle, clk10Mhz : in std_logic;
			PRNG10 : buffer std_logic_vector(9 downto 0)
		);			
	end component;

	-- Alien that spawns when timer and RNG conditions are met
	component AlienRNG is 
		port(
			max10_clk 			: in std_logic := '0';
			RNG 					: in std_logic_vector(9 downto 0) := "0000000000";
			alive					: inout std_logic := '0';
			size 					: inout integer;
			color					: out std_logic_vector(11 downto 0);
			x_pos					: inout integer := 640;
			y_pos					: inout integer := 240;
			min_period			: in integer := 5;
			RNG_bit_map			: in std_logic_vector(9 downto 0) := "1111111111";
			ship_x				: in integer := 0;
			ship_y 				: in integer := 0
		);
	end component;
	
	-- Alien that spawns on a constant timer
	component AlienTimer is 
		port(
			max10_clk 			: in std_logic := '0';
			RNG 					: in std_logic_vector(9 downto 0) := "0000000000";
			alive					: inout std_logic := '0';
			size 					: inout integer;
			color					: out std_logic_vector(11 downto 0);
			x_pos					: inout integer;
			y_pos					: inout integer;
			period_seconds 	: in integer;
			ship_x				: in integer := 0;
			ship_y 				: in integer := 0
		);
	end component;
	
	-- Alien that spawns on a timer that gets faster as the game progresses
	component AlienScoreTimer is 
		port(
			max10_clk 				: in std_logic := '0';
			RNG 						: in std_logic_vector(9 downto 0) := "0000000000";
			alive						: inout std_logic := '0';
			size 						: inout integer;
			color						: out std_logic_vector(11 downto 0);
			x_pos						: inout integer;
			y_pos						: inout integer;
			max_period_seconds	: in integer;
			min_period_seconds	: in integer;
			current_score			: in integer;
			ship_x					: in integer;
			ship_y 					: in integer
		);
	end component;	
	
	
	BEGIN

	U0 : ADXL345_controller port map('1', max10_clk, open, data_x, data_y, data_z, GSENSOR_SDI, GSENSOR_SDO, GSENSOR_CS_N, GSENSOR_SCLK);
	U1 : RNG10 port map(reset_RNG, '0', max10_clk, RNG);
	
-------- Alien AIs ---------------------------------------------------------------------------------	
	U02 : AlienRNG port map(clockWithPause, RNG, alien(0).alive, alien(0).size, alien(0).color, alien(0).x, alien(0).y, 11, "1101111010", ship.x, ship.y);
	U03 : AlienRNG port map(clockWithPause, RNG, alien(1).alive, alien(1).size, alien(1).color, alien(1).x, alien(1).y, 20, "0110110111", ship.x, ship.y);
	U04 : AlienRNG port map(clockWithPause, RNG, alien(2).alive, alien(2).size, alien(2).color, alien(2).x, alien(2).y, 29, "1011101101", ship.x, ship.y);
	U05 : AlienRNG port map(clockWithPause, RNG, alien(3).alive, alien(3).size, alien(3).color, alien(3).x, alien(3).y, 35, "0011011111", ship.x, ship.y);
	U06 : AlienTimer port map(clockWithPause, RNG, alien(4).alive, alien(4).size, alien(4).color, alien(4).x, alien(4).y, 10, ship.x, ship.y);
	U07 : AlienTimer port map(clockWithPause, RNG, alien(5).alive, alien(5).size, alien(5).color, alien(5).x, alien(5).y, 17, ship.x, ship.y);
	U08 : AlienTimer port map(clockWithPause, RNG, alien(6).alive, alien(6).size, alien(6).color, alien(6).x, alien(6).y, 23, ship.x, ship.y);
	U09 : AlienTimer port map(clockWithPause, RNG, alien(7).alive, alien(7).size, alien(7).color, alien(7).x, alien(7).y, 13, ship.x, ship.y);
	U10 : AlienScoreTimer port map(clockWithPause, RNG, alien(8).alive, alien(8).size, alien(8).color, alien(8).x, alien(8).y, 30, 4, score, ship.x, ship.y);
	U11 : AlienScoreTimer port map(clockWithPause, RNG, alien(9).alive, alien(9).size, alien(9).color, alien(9).x, alien(9).y, 45, 5, score, ship.x, ship.y);
	U12 : AlienScoreTimer port map(clockWithPause, RNG, alien(10).alive, alien(10).size, alien(10).color, alien(10).x, alien(10).y, 50, 3, score, ship.x, ship.y);
	U13 : AlienScoreTimer port map(clockWithPause, RNG, alien(11).alive, alien(11).size, alien(11).color, alien(11).x, alien(11).y, 60, 7, score, ship.x, ship.y);
	

	PROCESS(disp_ena, row, column)
		variable calcA : INTEGER;
		variable calcB : INTEGER;
		variable calcC : INTEGER;
		
	BEGIN

    IF(disp_ena = '1') THEN        --display time
	 
------DRAWS THE HORIZONTAL BARS THAT DEFINE PLAY REGION--------------------------------------
		IF( ((row < y_max) AND (row > (y_max - bar_thickness))) OR ((row > y_min) AND (row < (y_min + bar_thickness)))  ) THEN
			colorconcat <= "000000000000";
		ELSE
			colorconcat <= "111111111111";
		END IF;
		
------DRAWS THE PLAYER SHIP ON THE SCREEN----------------------------------------------------
		calcA := column - ship.x;		--Relative X position
		calcB := ship.y - row;			--Relative Y position
		calcC := -(ship_height * calcA)/ship_length + ship_height;	--Check if in area

		IF ((calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0)) THEN
			IF ((calcA = 1 OR calcA = ship_length) OR (calcB = 1 OR calcB = calcC)) THEN
				colorconcat <= "000000000000";
			ELSE
				colorconcat <= "111100000000";
			END IF;
		END IF;
		
------DRAWS THE REMAINING LIVES ON THE SCREEN------------------------------------------------
		FOR i in 0 to 2 LOOP
			IF (spare_ships > i) THEN
				calcA := column - ss_x(i);		--Relative X position
				calcB := ss_y - row;			--Relative Y position
				calcC := -(ship_height * calcA)/ship_length + ship_height;	--Check if in area
				
				IF ((calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0)) THEN
					IF ((calcA = 1 OR calcA = ship_length) OR (calcB = 1 OR calcB = calcC)) THEN
						colorconcat <= "000000000000";
					ELSE
						colorconcat <= "111100000000";
					END IF;
				END IF;
			END IF;
		END LOOP;

------DRAWS THE ENEMIES ON THE SCREEN--------------------------------------------------------
		FOR i in 0 to 11 LOOP
			IF (alien(i).alive = '1') THEN
				calcA := column - alien(i).x;	--Relative X position
				calcB := alien(i).y - row;		--Relative Y position
				calcC := (alien(i).size+1) * 8;			--Calc adjusted size
				
				IF ((calcB <= calcC AND calcB >= 0) AND (calcA <= calcC AND calcA >= 0)) THEN
					IF ((calcB = calcC OR calcB = 0) OR (calcA = calcC OR calcA = 0)) THEN
						colorconcat <= "000000000000";
					ELSE
						colorconcat <= alien(i).color;
					END IF;
				END IF;
			END IF;
		END LOOP;
------DRAWS THE PLAYER PROJECTILES ON THE SCREEN---------------------------------------------
		FOR i in 0 to 19 LOOP
			IF (p_proj.e(i) = '1') THEN
				IF (row = p_proj.y(i) AND column >= p_proj.x(i) AND column <= (p_proj.x(i) + 20)) THEN
					colorconcat <= "111100000000";
				END IF;
			END IF;
		END LOOP;


------OUTPUTS THE RESULTING COLORS TO THE SCREEN---------------------------------------------
		red <= "0000" & colorconcat(11 downto 8);
		green <= "0000" & colorconcat(7 downto 4);
		blue <= "0000" & colorconcat(3 downto 0);
		
    ELSE                           --blanking time
      red <= (OTHERS => '0');
      green <= (OTHERS => '0');
      blue <= (OTHERS => '0');
    END IF;
  
  END PROCESS;
  
 ------ Pause ------------------------------------------ -----------------------------------------------------------------------------
	pauseProcess : process ( max10_clk )
	begin	
		 if(falling_edge(pause_toggle)) then
			pause <= not pause;
		 end if;
		 
		 clockWithPause <= max10_clk AND NOT pause;
	
	end process;
 
 ------Clock for X Axis Movement-----------------------------------------------------------------------------------------------------------

  xAxisClock : process ( max10_clk, pause )	
	variable clockDivX : natural := 255;
	begin
		if(rising_edge(max10_clk) and pause = '0') then
			for i in 0 to 7 loop
				data_x_magnitude(i) <= data_x(i);
			end loop;
			if(data_x_magnitude(7 downto 4) = "0000" or data_x_magnitude(7 downto 4) = "1111") then
				clock_x <= clock_x;
			else
				if(data_x(11) = '0') then -- tilt left starts with 000
					clockDivX := 255 - to_integer(unsigned(data_x_magnitude));
				else -- tilt right starts at FF
					clockDivX := to_integer(unsigned(data_x_magnitude));
				end if;
				
				if (clockDivX = 0) then
					clockDivX := 255;
				else
					clockDivX := clockDivX;
				end if;
				
				countX <= countX+1;
				if (countX > ( 10000 * clockDivX ) ) then
					clock_x <= NOT clock_x;
					countX <= 1;
				end if;
			end if;
		end if;	
	end process;

------Clock for Y Axis Movement--------------------------------------------------------------
	
	yAxisClock : process ( max10_clk, pause )	
	variable clockDivY : natural := 255;
	begin
		if(rising_edge(max10_clk) and pause = '0') then
			for i in 0 to 7 loop
				data_y_magnitude(i) <= data_y(i);
			end loop;
			if (data_y_magnitude(7 downto 4) = "0000" or data_y_magnitude(7 downto 4) = "1111") then
				clock_y <= clock_y;
			else
				if(data_y(11) = '0') then 
					clockDivY := 255 - to_integer(unsigned(data_y_magnitude));
				else 
					clockDivY := to_integer(unsigned(data_y_magnitude));
				end if;
				
				if (clockDivY = 0) then
					clockDivY := 255;
				else
					clockDivY := clockDivY;
				end if;
				
				countY <= countY+1;
				if (countY > ( 20000 * clockDivY ) ) then
					clock_y <= NOT clock_y;
					countY <= 1;
				end if;
			end if;	
		end if;	
	end process;

------X Axis Movement------------------------------------------------------------------------
	
	xLocationAdjust : process (clock_x)
	begin
		if(reset_accel = '0') then
			ship.x <= ship.x;
		else
			if(rising_edge(clock_x)) then
				if(data_x(11) = '1') then		--RIGHT
					if (ship.x = x_max-ship_length) then
						ship.x <= x_max-ship_length;
					else
						ship.x <= ship.x+1;
					end if;
				else 									--LEFT
					if (ship.x = x_min) then
						ship.x <= x_min;
					else	
						ship.x <= ship.x-1;
					end if;
				end if;
			end if;
		end if;
	end process;

------Y Axis Movement------------------------------------------------------------------------
	
	yLocationAdjust : process (clock_y)
	begin
		if(reset_accel = '0') then
			ship.y <= ship.y;
		else
			if(rising_edge(clock_y)) then
				if(data_y(11) = '1') then --forward/up
					if (ship.y = y_max + ship_height) then
						ship.y <= y_max + ship_height;
					else
						ship.y <= ship.y-1;
					end if;
				else 							--backward/down
					if (ship.y = y_min+1) then
						ship.y <= y_min+1;
					else
						ship.y <= ship.y+1;
					end if;
				end if;
			end if;
		end if;
	end process;
	
------Player Laser Data----------------------------------------------------------------------
	
	projectileMoveClock : process (max10_clk, pause)
	variable proj_clock_counter : integer := 0;
	begin
		if(rising_edge(max10_clk) AND pause = '0') then
			proj_clock_counter := proj_clock_counter + 1;		
		end if;
		
		if (proj_clock_counter > 70000) then
			projectile_clock <= NOT projectile_clock;
			proj_clock_counter := 0;
		end if;

	end process;

	hndl_Projectile : PROCESS (shoot)
	VARIABLE ei : INTEGER; --Entity Index
	BEGIN
		IF (pause = '0' AND rising_edge(shoot)) THEN
			p_proj.e(ei) <= '1';
			p_proj.hs1(ei) <= '1';
			ei := ((ei + 1) mod 20);
		END IF;
		FOR i in 0 to 19 LOOP
			IF (p_proj.hs2(i) = '1') THEN
				p_proj.hs1(i) <= '0';
			END IF;
		END LOOP;
	END PROCESS;
  
	move_Projectile : PROCESS (projectile_clock)
	BEGIN
		IF (rising_edge(projectile_clock)) THEN	
			FOR i in 0 to 19 LOOP
				IF (p_proj.hs1(i) = '1') THEN
					p_proj.hs2(i) <= '1';
					p_proj.x(i) <= ship.x + ship_length;
					p_proj.y(i) <= ship.y;
				ELSE
					p_proj.x(i) <= p_proj.x(i) + 1;
					p_proj.hs2(i) <= '0';
				END IF;
			END LOOP;
		END IF;
	END PROCESS;
END architecture;