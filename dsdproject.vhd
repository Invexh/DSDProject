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
		max_pproj : INTEGER := 16;
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
	shoot			: in std_logic;
	
	buzzer1			: inout std_logic;
	buzzer2 		: inout std_logic
	
	);
END entity;

ARCHITECTURE behavior OF dsdproject IS
------SIGNAL DECLARATIONS--------------------------------------------------------------------
	--FOR DRAWING COLOR W/ ONE VECTOR--
	signal colorconcat : STD_LOGIC_VECTOR(11 downto 0);

	--Player--
	signal ship : ship_t := (alive => '1', x => x_min, y => (240 + ship_height/2), collision => '0', right => '1', exhaust => 0, dead => '0');
	signal p_proj : player_proj_array((max_pproj - 1) downto 0);

	--Score--
	signal score : INTEGER range 0 to 999999 := 1;
	signal digit : seg_array((max_digits - 1) downto 0);

	--Lives--
	signal spare_ships : INTEGER range 0 to 7 := 3;
	
	--Aliens--
	signal aliens : alien_array(11 downto 0) := (
		0 => (color => "000000000000", collision => '0', alive => '0', min_p => 11, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		1 => (color => "000000000000", collision => '0', alive => '0', min_p => 20, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		2 => (color => "000000000000", collision => '0', alive => '0', min_p => 29, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		3 => (color => "000000000000", collision => '0', alive => '0', min_p => 35, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		4 => (color => "000000000000", collision => '0', alive => '0', min_p => 15, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		5 => (color => "000000000000", collision => '0', alive => '0', min_p => 21, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		6 => (color => "000000000000", collision => '0', alive => '0', min_p => 12, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		7 => (color => "000000000000", collision => '0', alive => '0', min_p => 17, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		8 => (color => "000000000000", collision => '0', alive => '0', min_p => 04, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
		9 => (color => "000000000000", collision => '0', alive => '0', min_p => 05, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
	   10 => (color => "000000000000", collision => '0', alive => '0', min_p => 03, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240),
	   11 => (color => "000000000000", collision => '0', alive => '0', min_p => 07, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240)
	);

	--Timing Related Signals--
	signal paused		  	: STD_LOGIC;
	signal pauseClock     	: STD_LOGIC;
	signal mountain_clk   	: STD_LOGIC;
	signal movement_clock   : STD_LOGIC;
	signal projectile_clock : STD_LOGIC;

	--Accelerometer Signals--
	signal data_x, data_y, data_z : STD_LOGIC_VECTOR(15 downto 0);

	--Other Signals--
	signal startOfGame : STD_LOGIC := '1';
	signal RNG : STD_LOGIC_VECTOR(9 downto 0);
	signal selProj : INTEGER range 0 to 31;

------COMPONENTS-----------------------------------------------------------------------------
    -- Accelerometer component
	component ADXL345_controller is 
		port(	
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

	-- Movement controller component
	COMPONENT controller IS
		GENERIC(
			--Play area bounds
			y_max : INTEGER := 67;
			y_min : INTEGER := 413;
			x_max : INTEGER := 320;
			x_min : INTEGER := 25;

			x_start : INTEGER := 25;
			y_start : INTEGER := 249;

			--Player ship data
			ship_height : INTEGER := 18;
			ship_length : INTEGER := 36
		);
		PORT(
			dataX : IN STD_LOGIC_VECTOR(15 downto 0);
			dataY : IN STD_LOGIC_VECTOR(15 downto 0);
			Xpos  : OUT INTEGER range 0 to 640;
			Ypos  : OUT INTEGER range 0 to 480;
			Exha  : OUT INTEGER range 0 to 7;
			R     : OUT STD_LOGIC;
			CLK   : IN STD_LOGIC
		);
	END COMPONENT;

	COMPONENT pause IS
		PORT(
			start : OUT STD_LOGIC;
			clock : IN STD_LOGIC;
			btn_0 : IN STD_LOGIC;
			btn_1 : IN STD_LOGIC;
			pauseClk : OUT STD_LOGIC;
			paused	 : OUT STD_LOGIC
		);
	END COMPONENT;

	COMPONENT scoreboard IS
		PORT(
			score : IN INTEGER range 0 to 999999;
			index : IN INTEGER range 0 to 5;
			digit : OUT seg_digit
		);
	END COMPONENT;

	COMPONENT buzzer IS 
		PORT(
			alien               : IN alien_array(11 downto 0);
			clockWithPause 		: IN STD_LOGIC;
			RNG					: IN STD_LOGIC_VECTOR(9 downto 0);
			btn_0               : IN STD_LOGIC;
			buzzer1 			: BUFFER STD_LOGIC
		);
	END COMPONENT;

	COMPONENT RNG10 is
		PORT (
			set, clkToggle, clk10Mhz : IN STD_LOGIC;
			PRNG10 : BUFFER STD_LOGIC_VECTOR(9 downto 0)
		);			
	end COMPONENT;

BEGIN
------PORT MAPS------------------------------------------------------------------------------
	U0 : ADXL345_controller port map('1', max10_clk, OPEN, data_x, data_y, data_z, GSENSOR_SDI, GSENSOR_SDO, GSENSOR_CS_N, GSENSOR_SCLK);
	MC : controller generic map(x_start => x_min, y_start => (240 + ship_height/2)) port map(data_x, data_y, ship.x, ship.y, ship.exhaust, ship.right, pauseClock);
	PC : pause port map(startOfGame, max10_clk, shoot, pause_toggle, pauseClock, paused);
	U1 : RNG10 port map(reset_RNG, '0', max10_clk, RNG);
	B0 : buzzer port map(aliens, pauseClock, RNG, shoot, buzzer1);

	SC : FOR i in 0 to (max_digits - 1) GENERATE
		SC : scoreboard port map (score, i, digit(i));
	END GENERATE;

------VARIABLE DECLARATIONS------------------------------------------------------------------
	PROCESS(disp_ena, row, column)
		variable calcA : INTEGER range -64 to 640;
		variable calcB : INTEGER range -64 to 640;
		variable calcC : INTEGER range -64 to 640;
		variable calcD : INTEGER range -64 to 640;
		
		variable up_downNot					: boolean 	:= true;
		variable mountain_height 			: integer 	:= 0;
		variable mountain_counter  			: integer 	:= 0;
		variable mountain_clk_counter		: integer 	:= 0;
		
	BEGIN

    IF(disp_ena = '1') THEN        --display time
	 
------DRAWS THE HORIZONTAL BARS THAT DEFINE PLAY REGION--------------------------------------
		IF( (((row < y_max) AND (row > (y_max - bar_thickness))) OR ((row > y_min) AND (row < (y_min + bar_thickness)))) AND (column >= 0 AND column <= 640)  ) THEN
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
		if( rising_edge(pauseClock)) then
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

------DRAWS THE PLAYERS SHIP EXHAUST ON THE SCREEN-------------------------------------------
		calcA := column - ship.x;		--Relative X position
		calcB := ship.y - row;			--Relative Y position
		calcC := -(ship_height * calcA)/ship_length + ship_height;	--Check if in area

		IF (ship.right = '1' AND ship.exhaust > 0) THEN
			IF ((calcB rem 2) = 1 AND calcA < 0 AND calcB < ship_height AND calcA > -(2 * ship.exhaust)) THEN
				colorconcat <= "100000001000";
			END IF;
		ELSIF (ship.right = '0' AND ship.exhaust > 0) THEN
			IF ((calcB rem 2) = 1 AND calcA > ship_length AND calcB < ship_height AND calcA < ship_length + (2 * ship.exhaust)) THEN
				colorconcat <= "100000001000";
			END IF;
		END IF;

------DRAWS THE PLAYER PROJECTILES ON THE SCREEN---------------------------------------------
		FOR i in 0 to (max_pproj - 1) LOOP
			IF (p_proj(i).e = '1') THEN
				IF (row = p_proj(i).y AND column >= p_proj(i).x AND column <= (p_proj(i).x + 20)) THEN
					colorconcat <= "111100000000";
				END IF;
			END IF;
		END LOOP;	

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

------DRAWS THE ENEMIES ON THE SCREEN--------------------------------------------------------
		FOR i in 0 to 11 LOOP
			IF (aliens(i).alive = '1') THEN
				calcA := aliens(i).x - column;	--Relative X position
				calcB := aliens(i).y - row;		--Relative Y position
				calcC := (aliens(i).size) * 6;			--Calc adjusted size
				
				IF ((calcB <= calcC AND calcB >= 0) AND (calcA <= calcC AND calcA >= 0)) THEN
					IF ((calcB = calcC OR calcB = 0) OR (calcA = calcC OR calcA = 0)) THEN
						colorconcat <= "111111111111";
					ELSE
						colorconcat <= aliens(i).color;
					END IF;
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
------PLAYER LASER DATA----------------------------------------------------------------------
	projectileMoveClock : process (max10_clk, paused)
	variable proj_clock_counter : integer := 0;
	begin
		if(rising_edge(max10_clk) AND paused = '0') then
			proj_clock_counter := proj_clock_counter + 1;		
		end if;
		
		if (proj_clock_counter > 90000) then
			projectile_clock <= NOT projectile_clock;
			proj_clock_counter := 0;
		end if;

	end process;

	hndl_Projectile : PROCESS (shoot, max10_clk)
		VARIABLE ei : INTEGER range 0 to 31; --Entity Index
	BEGIN
		IF (paused = '0' AND falling_edge(shoot)) THEN
			score <= aliens(ei rem 11).min_p;
			p_proj(ei).hs1 <= '1';
			selProj <= ei;
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
				IF (p_proj(i).collision = '1') THEN
					p_proj(i).e <= '0';
				END IF;
				IF (p_proj(i).hs1 = '1') THEN
					p_proj(selProj).e <= '1';
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

------ALIEN PROCESSING-----------------------------------------------------------------------
	Move_CLK : process (max10_clk, paused)
	variable movement_counter : integer range 0 to 200000 := 0;
	begin
		if(rising_edge(max10_clk) AND paused = '0') then
			movement_counter := movement_counter + 1;
			if (movement_counter >= 200000) then
				movement_clock <= NOT movement_clock;
				movement_counter := 0;
			end if;
		end if;
	end process;

	hndl_Alien : process (pauseClock)
	begin
		FOR i in 0 to 11 LOOP
			IF(rising_edge(pauseClock)) THEN
				IF (aliens(i).collision = '1') THEN
					aliens(i).alive <= '0';
				END IF;

				IF (aliens(i).alive = '0') THEN
					aliens(i).tsls <= aliens(i).tsls + 1;
				END IF;

				IF (aliens(i).hs2 = '1') THEN
					aliens(i).hs1 <= '0';
				END IF;

				IF (i < 4 AND aliens(i).alive = '0' AND aliens(i).tsls >= (aliens(i).min_p * 50000000)) THEN
					aliens(i).alive <= '1';
					aliens(i).size <= to_integer(unsigned(RNG(2 downto 0))) + 3;
					aliens(i).color <= "110000001100";
					aliens(i).hs1 <= '1';
					aliens(i).tsls <= 0;

				ELSIF ( (i < 8 AND aliens(i).alive = '0' AND aliens(i).tsls >= (aliens(i).min_p * 50000000)) ) THEN
					aliens(i).alive <= '1';
					aliens(i).size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(2 downto 0)) + 3);
					aliens(i).color <= "000011000100";
					aliens(i).hs1 <= '1';
					aliens(i).tsls <= 0;

					IF (score > 1000 AND RNG(1) = '1') THEN
						aliens(i).min_p <= aliens(i).min_p - 2;
					END IF;
					
				ELSIF ( (i < 12 AND aliens(i).alive = '0' AND aliens(i).tsls >= (aliens(i).min_p * 50000000))) THEN
					aliens(i).alive <= '1';
					aliens(i).size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(9 downto 7)) + 3);
					aliens(i).color <= "000000001100";
					aliens(i).hs1 <= '1';
					aliens(i).tsls <= 0;

					IF (score > 2000) THEN
						aliens(i).min_p <= aliens(i).min_p - 2;
					END IF;

				ELSIF ((aliens(i).x > 60000) OR (aliens(i).x <= 0)) THEN
					aliens(i).alive <= '0';
				END IF;
			END IF;
		END LOOP;
	END PROCESS;

	move_Alien : process (movement_clock)
	VARIABLE randomValue : INTEGER range 0 to 2048;
	begin
		FOR i in 0 to 11 LOOP
			IF (rising_edge(movement_clock) AND aliens(i).alive = '1') THEN
				IF (aliens(i).hs1 = '1') THEN
					randomValue := to_integer(unsigned(RNG( 8 downto (i rem 3) ))) * 8;
					aliens(i).x <= 750 + randomValue/4;
					aliens(i).y <= ((randomValue + y_max + 6*aliens(i).size) rem (y_min - (y_max + 6*aliens(i).size)) + (y_max + 6*aliens(i).size) + 8);
					aliens(i).hs2 <= '1';
				ELSE
					aliens(i).hs2 <= '0';
					aliens(i).x <= aliens(i).x - 1;
				END IF;
			END IF;
			
			IF (aliens(i).alive = '0') THEN
				aliens(i).x <= 750;
				aliens(i).y <= 240;
			END IF;
		END LOOP;
	END PROCESS;

------COLLISION DETECTION--------------------------------------------------------------------
    SA : PROCESS (pauseClock)
	VARIABLE rst_Screen : STD_LOGIC := '0';
    BEGIN
        IF (rst_Screen = '1') THEN
            FOR i in 0 to 11 LOOP
                aliens(i).collision <= '1';
            END LOOP;
			FOR i in 0 to (max_pproj - 1) LOOP
				p_proj(i).collision <= '1';
			END LOOP;
        ELSE
            FOR i in 0 to 11 LOOP
                aliens(i).collision <= '0';
            END LOOP;
			FOR i in 0 to (max_pproj - 1) LOOP
				p_proj(i).collision <= '0';
			END LOOP;
        END IF;

        rst_Screen := '0';

        FOR i in 0 to 11 LOOP
			--Alien and Player Ship Collision--
            IF (Paused = '0' AND 
            aliens(i).x >= ship.x AND 
            (aliens(i).x - (6 * aliens(i).size)) <= (ship.x + ship_length) AND 
            (aliens(i).y - (6 * aliens(i).size)) <= ship.y AND 
            aliens(i).y >= (ship.y - ship_height + ((aliens(i).x - (6 * aliens(i).size) - ship.x)*ship_height)/ship_length) AND
            aliens(i).y >= (ship.y - ship_height)) THEN
                spare_ships <= spare_ships - 1;
                rst_Screen := '1';
            ELSIF (Paused = '1' AND startOfGame = '1') THEN
                spare_ships <= 3;
            END IF;

			--Alien and Projectile Collision--
			FOR j in 0 to (max_pproj - 1) LOOP
				IF ((p_proj(j).x + 20) >= (aliens(i).x - (6 * aliens(i).size)) AND
				p_proj(j).x <= aliens(i).x AND
				p_proj(j).y >= (aliens(i).y - (6 * aliens(i).size)) AND
				p_proj(j).y <= aliens(i).y AND p_proj(j).e = '1') THEN
					aliens(i).collision <= '1';
					p_proj(j).collision <= '1';
				END IF;
			END LOOP;
        END LOOP;

        IF ( spare_ships < 0 ) THEN
            ship.dead <= '1';
        END IF;
    END PROCESS;


END ARCHITECTURE;