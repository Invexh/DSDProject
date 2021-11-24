LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

package custom_types is
	type int_array is array (integer range <>) of integer;

	type alien_t is record
		color : STD_LOGIC_VECTOR(11 downto 0);
		collision : STD_LOGIC;
		numSpawns : INTEGER;
		alive : STD_LOGIC;
		min_p : INTEGER;
		max_p : INTEGER;
		hs1 : STD_LOGIC;
		hs2 : STD_LOGIC;
		size : INTEGER;
		tsls : INTEGER;
		x : INTEGER;
		y : INTEGER;
		p : INTEGER;
	end record alien_t;
	
	type ship_t is record
		alive : STD_LOGIC;
		x : INTEGER;
		y : INTEGER;
		collision : STD_LOGIC;
		right : STD_LOGIC;
	end record ship_t;

	type seg_digit is record
		s : STD_LOGIC_VECTOR(0 to 6);
	end record seg_digit;

	type player_proj is record
		x : INTEGER; --Y POS
		y : INTEGER; --X POS
		hs1 : STD_LOGIC;	--Entity Handshake 1
		hs2 : STD_LOGIC;	--Entity Handshake 2
		e : STD_LOGIC; --Entity bit
		right : std_logic;
	end record player_proj;
	
	type alien_proj is record
		x : INTEGER; --Y POS
		y : INTEGER; --X POS
		hs1 : STD_LOGIC;	--Entity Handshake 1
		hs2 : STD_LOGIC;	--Entity Handshake 2
		e : STD_LOGIC; --Entity bit
		parent : integer; --alien the projectile spawned from
	end record alien_proj;
	
	type player_proj_array is array (integer range <>) of player_proj;
	type alien_proj_array is array (integer range <>) of alien_proj;
	type alien_array is array (integer range <>) of alien_t;
	type seg_array is array (integer range <>) of seg_digit;
	
end package;

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;
USE work.custom_types.all;

ENTITY dsdproject IS
  GENERIC(
		
		--Play area bounds
		y_max : INTEGER := 67;
		y_min : INTEGER := 413;
		x_max : INTEGER := 320;
		x_min : INTEGER := 25;
		
		--Horizontal Bar
		bar_thickness : INTEGER := 5;

		--Player ship data
		ship_height : INTEGER := 18;
		ship_length : INTEGER := 36;

		--Projectiles data
		max_pproj : INTEGER := 15;
		max_aproj : INTEGER := 5;
		
		--Scoreboard data
		max_digits : INTEGER := 6;
		digit_height : INTEGER := 30;
		digit_spacing : INTEGER := 4;
		digit_thickness : INTEGER := 3;
		score_x : INTEGER := 500;
		score_y : INTEGER := 48;
		
		--Spare ship data
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
	GSENSOR_CS_N 	: OUT	STD_LOGIC;
	GSENSOR_SCLK 	: OUT	STD_LOGIC;
	GSENSOR_SDI  	: INOUT	STD_LOGIC;
	GSENSOR_SDO  	: INOUT	STD_LOGIC;
	reset_accel 	: in std_logic := '1';
	
	reset_RNG 		: IN STD_LOGIC;
	
	pause_toggle	: in std_logic;
	shoot				: in std_logic;
	
	buzzer1			: inout std_logic;
	buzzer2 			: inout std_logic
	
	);
END entity;

ARCHITECTURE behavior OF dsdproject IS
	--FOR DRAWING COLOR W/ ONE VECTOR--
	signal colorconcat : STD_LOGIC_VECTOR(11 downto 0);

	--PLAYER DATA--
	signal ship : ship_t := (alive => '1', x => x_min, y => (240 + ship_height/2), collision => '0', right => '1');
	signal spare_ships : INTEGER := 3;

	--SCORE AND SCOREBOARD--
	signal score : INTEGER := 0;
	signal digit : seg_array(max_digits-1 downto 0);

	--ENTITIES--
	signal alien : alien_array(11 downto 0) := (
		0 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 00, min_p => 11, p => 00, hs1 => '0', hs2 => '0'),
		1 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 00, min_p => 20, p => 00, hs1 => '0', hs2 => '0'),
		2 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 00, min_p => 29, p => 00, hs1 => '0', hs2 => '0'),
		3 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 00, min_p => 35, p => 00, hs1 => '0', hs2 => '0'),
		4 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 30, min_p => 15, p => 10, hs1 => '0', hs2 => '0'),
		5 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 30, min_p => 21, p => 17, hs1 => '0', hs2 => '0'),
		6 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 30, min_p => 12, p => 23, hs1 => '0', hs2 => '0'),
		7 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 30, min_p => 17, p => 13, hs1 => '0', hs2 => '0'),
		8 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 30, min_p => 04, p => 00, hs1 => '0', hs2 => '0'),
		9 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 45, min_p => 05, p => 00, hs1 => '0', hs2 => '0'),
	   10 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 50, min_p => 03, p => 00, hs1 => '0', hs2 => '0'),
	   11 => (color => "000000000000", numSpawns => 0, alive => '0', collision => '0', size => 1, tsls => 0, x => 640, y => 240, max_p => 60, min_p => 07, p => 00, hs1 => '0', hs2 => '0')
	);
	signal p_proj : player_proj_array(max_pproj downto 0);
	--signal a_proj : alien_proj_array(max_aproj downto 0);

	--CLOCK RELATED DATA--
	signal clock_x, clock_y : STD_LOGIC := '0';
	signal data_x_magnitude, data_y_magnitude : std_logic_vector(7 downto 0);
	signal countX 						: integer := 1;
	signal countY 						: integer := 1;
	signal data_x, data_y, data_z : STD_LOGIC_VECTOR(15 downto 0);	
	signal clockWithPause 			: std_logic := '0';
	signal projectile_clock 		: std_logic := '0';
	signal movement_clock			: std_logic := '0';
	signal alien_projectile_clock : std_logic := '0';
	signal mountain_clk				: std_logic := '0';

	--OTHER--
	signal RNG 				: std_logic_vector(9 downto 0);
	signal pause 			: std_logic := '1';
	
	--AUDIO--
	signal bz1_clk : 		std_logic := '0';
	signal pew_sound :	std_logic := '0';
	signal exp_sound :	std_logic := '0';
	
	--FLAGS--
	signal startOfGameFlag : std_logic := '1';
	
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

	BEGIN

	U0 : ADXL345_controller port map('1', max10_clk, open, data_x, data_y, data_z, GSENSOR_SDI, GSENSOR_SDO, GSENSOR_CS_N, GSENSOR_SCLK);
	U1 : RNG10 port map(reset_RNG, '0', max10_clk, RNG);
	
	PROCESS(disp_ena, row, column)
		variable calcA : INTEGER;
		variable calcB : INTEGER;
		variable calcC : INTEGER;
		variable calcD : INTEGER;
		
		variable up_downNot					: boolean 	:= true;
		variable mountain_height 			: integer 	:= 0;
		variable mountain_counter  		: integer 	:= 0;
		variable mountain_clk_counter		: integer 	:= 0;
		
	BEGIN

    IF(disp_ena = '1') THEN        --display time
	 
------DRAWS THE HORIZONTAL BARS THAT DEFINE PLAY REGION--------------------------------------
		IF( ((row < y_max) AND (row > (y_max - bar_thickness))) OR ((row > y_min) AND (row < (y_min + bar_thickness)))  ) THEN
			colorconcat <= "111111110000";
		ELSE
			colorconcat <= "000000000000";
		END IF;
		
		
------DRAWS THE COLLISION-LESS BACKGROUND "MOUNTAINS" (TRIANGLES)----------------------------	
		IF( (ROW > (Y_MIN + 1 - MOUNTAIN_HEIGHT)) AND  (ROW < Y_MIN + 1) ) THEN -- - (MOUNTAIN_HEIGHT + 5)
			COLORCONCAT <= "101010101010";		
		END IF;
		
		if ( ((column + mountain_counter) rem 100) < 50 ) then
			mountain_height := 2*((column + mountain_counter) rem 50);
		else
			mountain_height := 2*(50 - ((column + mountain_counter) rem 50));
		end if;
		
		--mountain sliding clock
		if( rising_edge(max10_clk) AND pause = '0' ) then
			if (mountain_clk_counter > 750000) then
				mountain_clk <= not mountain_clk;
				mountain_clk_counter := 0;
			else
				mountain_clk_counter := mountain_clk_counter + 1;
			end if;
		end if;
			
		--variable to slide mountains
		if(rising_edge(mountain_clk)) then	
			mountain_counter := ((mountain_counter + 1) rem 100);
		else
			mountain_counter := mountain_counter;
		end if;

		
------DRAWS THE REMAINING LIVES ON THE SCREEN------------------------------------------------
		FOR i in 0 to 2 LOOP
			IF (spare_ships > i) THEN
				calcA := column - ss_x(i);		--Relative X position
				calcB := ss_y - row;			--Relative Y position
				calcC := -(ship_height * calcA)/ship_length + ship_height;	--Check if in area
				
				IF ((calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0)) THEN
					IF ((calcA = 1 OR calcA = ship_length) OR (calcB = 1 OR calcB = calcC)) THEN
						colorconcat <= "111111111111";
					ELSE
						colorconcat <= "111100000000";
					END IF;
				END IF;
			END IF;
		END LOOP;

------DRAWS THE ENEMIES ON THE SCREEN--------------------------------------------------------
		FOR i in 0 to 11 LOOP
			IF (alien(i).alive = '1') THEN
				calcA := alien(i).x - column;	--Relative X position
				calcB := alien(i).y - row;		--Relative Y position
				calcC := (alien(i).size) * 6;			--Calc adjusted size
				
				IF ((calcB <= calcC AND calcB >= 0) AND (calcA <= calcC AND calcA >= 0)) THEN
					IF ((calcB = calcC OR calcB = 0) OR (calcA = calcC OR calcA = 0)) THEN
						colorconcat <= "111111111111";
					ELSE
						colorconcat <= alien(i).color;
					END IF;
				END IF;
			END IF;
		END LOOP;
	
------DRAWS THE PLAYER SHIP ON THE SCREEN----------------------------------------------------
		calcA := column - ship.x;		--Relative X position
		calcB := ship.y - row;			--Relative Y position
		calcC := -(ship_height * calcA)/ship_length + ship_height;	--Check if in area

		IF (ship.right = '1' AND (calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0)) THEN
			IF ((calcA = 1 OR calcA = ship_length) OR (calcB = 1 OR calcB = calcC)) THEN
				colorconcat <= "111111111111";
			ELSE
				colorconcat <= "111100000000";
			END IF;
		END IF;

		calcA := column - ship.x;		--Relative X position
		calcB := ship.y - row;			--Relative Y position
		calcC := (ship_height * calcA)/ship_length;	--Check if in area

		IF (ship.right = '0' AND (calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0)) THEN
			IF ((calcA > 1 AND calcA < ship_length) AND (calcB < calcC AND calcB > 1)) THEN
				colorconcat <= "111100000000";
			ELSE
				colorconcat <= "111111111111";
			END IF;
		END IF;

------DRAWS THE PLAYER SHIPS EXHAUST ON THE SCREEN-------------------------------------------
		

------DRAWS THE PLAYER PROJECTILES ON THE SCREEN---------------------------------------------
		FOR i in 0 to (max_pproj - 1) LOOP
			IF (p_proj(i).e = '1') THEN
				IF (row = p_proj(i).y AND column >= p_proj(i).x AND column <= (p_proj(i).x + 20)) THEN
					colorconcat <= "111100000000";
				END IF;
			END IF;
		END LOOP;
		
------DRAWS THE ALIEN PROJECTILES ON THE SCREEN---------------------------------------------
	--		FOR i in 0 to (max_aproj - 1) LOOP
	--			IF (a_proj(i).e = '1') THEN
	--				IF (row = a_proj(i).y AND column <= a_proj(i).x AND column >= (a_proj(i).x - 20)) THEN
	--					colorconcat <= "110000001100";
	--				END IF;
	--			END IF;
	--		END LOOP;
			
		
------DRAWS THE SCOREBOARD TO THE SCREEN-----------------------------------------------------
		calcA := (digit_thickness - 1)/2;	--Onesided thickness of digit
		calcB := (digit_height - 3)/2;		--Segment Length 
		FOR i in 0 to (max_digits - 1) LOOP
			calcC := column - (score_x + i*(digit_spacing + 2*calcA + calcB));	--Relative x position
			calcD := score_y - row; --Relative y position

			IF (digit(i).s(0) = '1' AND (calcC > 0 AND calcC <= (calcB + 2*calcA)) AND (calcD >= 2*(calcB + calcA) AND calcD <= (2*calcB + 1 + 3*calcA))) THEN
				colorconcat <= "111100000000";
			END IF;

			IF (digit(i).s(1) = '1' AND (calcC >= (calcB + calcA) AND calcC <= (calcB + 2*calcA + 1)) AND (calcD > (calcB + 2*calcA) AND calcD <= (2*calcB + 1 + 2*calcA))) THEN
				colorconcat <= "111100000000";
			END IF;

			IF (digit(i).s(2) = '1' AND (calcC >= (calcB + calcA) AND calcC <= (calcB + 2*calcA + 1)) AND (calcD > 0 AND calcD <= (calcB + 1))) THEN
				colorconcat <= "111100000000";
			END IF;

			IF (digit(i).s(3) = '1' AND (calcC > 0 AND calcC <= (calcB + 2*calcA)) AND (calcD >= 0 AND calcD <= (1 + calcA))) THEN
				colorconcat <= "111100000000";
			END IF;

			IF (digit(i).s(4) = '1' AND (calcC >= 0 AND calcC <= (1 + calcA)) AND (calcD > 0 AND calcD <= (calcB + 1))) THEN
				colorconcat <= "111100000000";
			END IF;

			IF (digit(i).s(5) = '1' AND (calcC >= 0 AND calcC <= (1 + calcA)) AND (calcD > (calcB + calcA + 1) AND calcD <= (2*calcB + 1 + 2*calcA))) THEN
				colorconcat <= "111100000000";
			END IF;

			IF (digit(i).s(6) = '1' AND (calcC > 0 AND calcC <= (calcB + 2*calcA)) AND (calcD >= (calcB + calcA) AND calcD <= (calcB + 1 + 3*calcA))) THEN
				colorconcat <= "111100000000";
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
  
------Pause----------------------------------------------------------------------------------
	pauseProcess : process ( max10_clk, pause_toggle, startOfGameFlag )
	variable hs3 : boolean := false;
	variable hs4 : boolean := false;
	begin	

		if(startOfGameFlag = '1') then
			if(shoot = '0') then
				pause <= '0';
			else
				pause <= '1';
			end if;
		elsif(falling_edge(pause_toggle)) then
			pause <= not pause;
		else
			pause <= pause;
		end if;
		
		clockWithPause <= max10_clk AND NOT pause;
	
	end process;
	
	start_proc : process(shoot)
	begin
		if(falling_edge(shoot) AND pause = '1') then
			startOfGameFlag <= '0';
		end if;
	end process;
 
------Clock for X Axis Movement--------------------------------------------------------------

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
				if (countX > ( 1000 * clockDivX ) ) then
					clock_x <= NOT clock_x;
					countX <= 1;
				end if;
			end if;
		end if;	

		score <= 255 - to_integer(unsigned(data_x));
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
				if (countY > ( 2000 * clockDivY ) ) then
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
				ship.right <= data_x(11);
			end if;
		end if;
	end process;	
	
	--	ship_LeftorRight : process (clock_x)
	--	begin
	--		ship.right <= data_x(11);		
	--	end process;

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
	
------PLAYER LASER DATA----------------------------------------------------------------------
	
	projectileMoveClock : process (max10_clk, pause)
	variable proj_clock_counter : integer := 0;
	begin
		if(rising_edge(max10_clk) AND pause = '0') then
			proj_clock_counter := proj_clock_counter + 1;		
		end if;
		
		if (proj_clock_counter > 90000) then
			projectile_clock <= NOT projectile_clock;
			proj_clock_counter := 0;
		end if;

	end process;

	hndl_Projectile : PROCESS (shoot)
	VARIABLE ei : INTEGER; --Entity Index
	BEGIN
		IF (pause = '0' AND falling_edge(shoot)) THEN
			p_proj(ei).e <= '1';
			p_proj(ei).hs1 <= '1';
			ei := ((ei + 1) mod max_pproj);
		END IF;
		FOR i in 0 to (max_pproj - 1) LOOP
			IF (p_proj(i).hs2 = '1') THEN
				p_proj(i).hs1 <= '0';
			END IF;
		END LOOP;
	END PROCESS;
  
	move_Projectile : PROCESS (projectile_clock)
	BEGIN
		IF (rising_edge(projectile_clock)) THEN	
			FOR i in 0 to (max_pproj - 1) LOOP
				IF (p_proj(i).hs1 = '1') THEN
					p_proj(i).hs2 <= '1';
					p_proj(i).y <= ship.y - 2;
					p_proj(i).right <= ship.right;
					if(ship.right = '1') then
						p_proj(i).x <= ship.x + ship_length;
					else
						p_proj(i).x <= ship.x;
					end if;
				ELSE
					if(p_proj(i).right = '1') then
						p_proj(i).x <= p_proj(i).x + 1;
					else
						p_proj(i).x <= p_proj(i).x - 1;
					end if;
					p_proj(i).hs2 <= '0';
				END IF;
			END LOOP;
		END IF;
	END PROCESS;
	
------Alien LASER DATA----------------------------------------------------------------------
	
	--	alienProjectileMoveClock : process (max10_clk, pause)
	--	variable alien_proj_clock_counter : integer := 0;
	--	begin
	--		if(rising_edge(max10_clk) AND pause = '0') then
	--			alien_proj_clock_counter := alien_proj_clock_counter + 1;		
	--		end if;
	--	
	--		if (alien_proj_clock_counter > 90000) then
	--			alien_projectile_clock <= NOT alien_projectile_clock;
	--			alien_proj_clock_counter := 0;
	--		end if;
	--
	--	end process;
	--
	--	alien_hndl_Projectile : PROCESS (max10_clk)
	--	VARIABLE ei 					: INTEGER; --Entity Index
	--	variable shoot_counter 		: integer := 0;
	--	variable num_shots			: integer := 0;
	--	
	--	BEGIN
	--		if(rising_edge(max10_clk) AND pause = '0') then
	--			for j in 0 to 11 loop
	--				if(alien(j).size = 5 OR alien(j).size = 7) then -- floatys
	--					IF ( shoot_counter > 25000000 - num_shots ) THEN
	--						a_proj(ei).e <= '1';
	--						a_proj(ei).hs1 <= '1';
	--						ei := ((ei + 1) mod max_aproj);
	--						a_proj(ei).parent <= j;
	--						shoot_counter := 0;
	--						if(num_shots < 20000000) then
	--							num_shots := num_shots + 1;
	--						else
	--							num_shots := 20000000;
	--						end if;
	--					else
	--						shoot_counter := shoot_counter + 1;
	--					END IF;
	--				end if;
	--	 		end loop;
	--	 		FOR i in 0 to (max_aproj - 1) LOOP
	--	 			IF (a_proj(i).hs2 = '1') THEN
	--	 				a_proj(i).hs1 <= '0';
	--	 			END IF;
	--	 		END LOOP;
	--	 	end if;
	--	END PROCESS;
	--  
	--	alien_move_Projectile : PROCESS (alien_projectile_clock)
	--	BEGIN
	--	 	IF (rising_edge(alien_projectile_clock)) THEN	
	--	 		FOR i in 0 to (max_aproj - 1) LOOP
	--	 			IF (a_proj(i).hs1 = '1') THEN
	--	 				a_proj(i).hs2 <= '1';
	--	 				a_proj(i).x <= alien(a_proj(i).parent).x - 20;
	--	 				a_proj(i).y <= alien(a_proj(i).parent).y - 2;
	--	 			ELSE
	--	 				a_proj(i).x <= a_proj(i).x - 1;
	--	 				a_proj(i).hs2 <= '0';
	--	 			END IF;
	--	 		END LOOP;
	--	 	END IF;
	--	END PROCESS;


------UPDATE DIGTIS WITH SCORE VALUE---------------------------------------------------------
	hndl_Digits : process(score)
	VARIABLE valu : INTEGER;
	VARIABLE modC : INTEGER;
	begin
		FOR i in 0 to (max_digits - 1) LOOP
			valu := (score/(10 ** i)) mod 10;
			
			case valu is
				when 9 => digit(max_digits-i-1).s <= "1110011";
				when 8 => digit(max_digits-i-1).s <= "1111111";
				when 7 => digit(max_digits-i-1).s <= "1110000";
				when 6 => digit(max_digits-i-1).s <= "1011111";
				when 5 => digit(max_digits-i-1).s <= "1011011";
				when 4 => digit(max_digits-i-1).s <= "0110011";
				when 3 => digit(max_digits-i-1).s <= "1111001";
				when 2 => digit(max_digits-i-1).s <= "1101101";
				when 1 => digit(max_digits-i-1).s <= "0110000";
				when 0 => digit(max_digits-i-1).s <= "1111110";
				when others => digit(max_digits-i-1).s <= "1000111";
			end case;
		END LOOP;
	END PROCESS;
	
------BUZZER1 -------------------------------------------------
	buzzer1_clock : process(clockWithPause)
	variable C3_counter : integer := 0;
	variable exp_clk_counter : integer := 0;
	variable RNG_instance : integer := 7500;		--generates random-ish clock for white noise	
	begin
		if(rising_edge(clockWithPause)) then	
			if(exp_sound = '1') then
				if (exp_clk_counter > RNG_instance ) then
					bz1_clk <= not bz1_clk;
					RNG_instance := 300*(to_integer(unsigned(RNG(7 downto 3)))+1);
					exp_clk_counter := 0;
				else
					bz1_clk <= bz1_clk;
					exp_clk_counter := exp_clk_counter + 1;
				end if;	
			elsif(pew_sound = '1') then		
				if (C3_counter > 38225 ) 	then	-- cycles to get frequency of 130.813 (C3) 
					bz1_clk <= not bz1_clk;
					C3_counter := 0;
				else
					bz1_clk <= bz1_clk;
					C3_counter := C3_counter + 1;					
				end if;
			else
				bz1_clk <= bz1_clk;
			end if;
		end if;	
	end process;
	
	buzzer1_process : process(bz1_clk, clockWithPause, shoot )
	variable pew_counter : integer := 0;
	variable exp_counter : integer := 0;
	variable sound_done  : std_logic := '0';
	variable hs5 : boolean := false;
	variable hs6 : boolean := false;
	
	
	begin
		if(rising_edge(bz1_clk)) then	
			if(exp_counter > 0) then		-- explosion
				if(exp_counter < 1000) then
					buzzer1 <= not buzzer1;
					exp_counter := exp_counter + 1;
				else
					exp_counter := 0;
					sound_done := '1';
				end if;			
			elsif (pew_counter > 0) then					-- pew
				if (pew_counter < 50) then
					buzzer1 <= not buzzer1;
					pew_counter := pew_counter + 1;
				else
					sound_done := '1';
					pew_counter := 0;
				end if;
			elsif(exp_sound = '1') then
				exp_counter := 1;
			elsif(pew_sound = '1') then
				pew_counter := 1;
			else
				buzzer1 <= buzzer1;
				sound_done := '0';
			end if;
		end if;
		
		-- flags for sound creation
		if(rising_edge(clockWithPause)) then
			for i in 0 to 11 loop
				if(alien(i).collision = '1') then
					exp_sound <= '1';
				elsif (sound_done = '1') then
					exp_sound <= '0';	
				else
					exp_sound <= exp_sound;
				end if;
			end loop;
			
			if(shoot = '0') then
				pew_sound <='1';
				hs6 := true;
			elsif( sound_done = '1') then
				pew_sound <= '0';
				hs6 := false;
			else
				pew_sound <= pew_sound;
			end if;
		end if;			
			
	end process;

------ALIEN PROCESSING----------------------------------------------------

	Move_CLK : process (max10_clk, pause)
	variable movement_counter : integer := 0;
	begin
		if(rising_edge(max10_clk) AND pause = '0') then
			movement_counter := movement_counter + 1;
			if (movement_counter >= 200000) then
				movement_clock <= NOT movement_clock;
				movement_counter := 0;
			end if;
		end if;
	end process;

	hndl_Alien : process (clockWithPause)
	begin
		FOR i in 0 to 11 LOOP
			IF(rising_edge(clockWithPause)) THEN
				IF (alien(i).alive = '0') THEN
					alien(i).tsls <= alien(i).tsls + 1;
				END IF;

				IF (alien(i).hs2 = '1') THEN
					alien(i).hs1 <= '0';
				END IF;

				IF (i < 4 AND alien(i).alive = '0' AND alien(i).tsls >= (alien(i).min_p * 50000000)) THEN
					alien(i).alive <= '1';
					alien(i).size <= to_integer(unsigned(RNG(2 downto 0))) + 3;
					alien(i).color <= "110000001100";
					alien(i).hs1 <= '1';
					alien(i).tsls <= 0;

				ELSIF ( (i < 8 AND alien(i).alive = '0' AND alien(i).tsls >= (alien(i).min_p * 50000000)) ) THEN
					alien(i).alive <= '1';
					alien(i).size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(2 downto 0)) + 3);
					alien(i).color <= "000011000100";
					alien(i).hs1 <= '1';
					alien(i).tsls <= 0;

					IF (score > 1000 AND RNG(1) = '1') THEN
						alien(i).min_p <= alien(i).min_p - 2;
					END IF;
					
				ELSIF ( (i < 12 AND alien(i).alive = '0' AND alien(i).tsls >= (alien(i).min_p * 50000000))) THEN
					alien(i).alive <= '1';
					alien(i).size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(9 downto 7)) + 3);
					alien(i).color <= "000000001100";
					alien(i).hs1 <= '1';
					alien(i).tsls <= 0;

					IF (score > 2000) THEN
						alien(i).min_p <= alien(i).min_p - 2;
					END IF;

				ELSIF ((alien(i).x > 60000) OR (alien(i).x <= 0)) THEN
					alien(i).alive <= '0';
				END IF;
			END IF;
		END LOOP;
	END PROCESS;

	move_Alien : process (movement_clock)
	VARIABLE randomValue : INTEGER;
	begin
		FOR i in 0 to 11 LOOP
			IF (rising_edge(movement_clock) AND alien(i).alive = '1') THEN
				IF (alien(i).hs1 = '1') THEN
					randomValue := to_integer(unsigned(RNG( 8 downto (i rem 3) ))) * 8;
					alien(i).x <= 750 + randomValue/4;
					alien(i).y <= ((randomValue + y_max + 6*alien(i).size) rem (y_min - (y_max + 6*alien(i).size)) + (y_max + 6*alien(i).size) + 8);
					alien(i).hs2 <= '1';
				ELSE
					alien(i).hs2 <= '0';
					alien(i).x <= alien(i).x - 1;
				END IF;
			END IF;
			
			IF (alien(i).alive = '0') THEN
				alien(i).x <= 750;
				alien(i).y <= 240;
			END IF;
		END LOOP;
	END PROCESS;
	
END architecture;