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
		ss_y : INTEGER := 57; --(y_max - bar_thickness - 5)

		expLookup : int_array(0 to 10) := (0, 1, 3, 5, 5, 5, 4, 3, 2, 2, 1);
		awardScore : INT_ARRAY(0 to 7) := (10000, 500, 300, 250, 250, 200, 200, 150)
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
	signal score : INTEGER range 0 to 999999 := 0;
	signal digit : seg_array((max_digits - 1) downto 0);

	--Lives--
	signal spare_ships : INTEGER range -1 to 6 := 3;
	signal rstScreenS  : STD_LOGIC := '0';
	
	--Aliens--
	signal aliens : alien_array(11 downto 0) := (
		0 => (color => "000000000000", collision => '0', alive => '0', min_p => 11, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		1 => (color => "000000000000", collision => '0', alive => '0', min_p => 20, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		2 => (color => "000000000000", collision => '0', alive => '0', min_p => 29, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		3 => (color => "000000000000", collision => '0', alive => '0', min_p => 35, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		4 => (color => "000000000000", collision => '0', alive => '0', min_p => 15, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		5 => (color => "000000000000", collision => '0', alive => '0', min_p => 21, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		6 => (color => "000000000000", collision => '0', alive => '0', min_p => 12, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		7 => (color => "000000000000", collision => '0', alive => '0', min_p => 17, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		8 => (color => "000000000000", collision => '0', alive => '0', min_p => 04, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
		9 => (color => "000000000000", collision => '0', alive => '0', min_p => 05, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
	   10 => (color => "000000000000", collision => '0', alive => '0', min_p => 03, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0),
	   11 => (color => "000000000000", collision => '0', alive => '0', min_p => 07, hs1 => '0', hs2 => '0', size => 1, tsls => 0, x => 640, y => 240, die => '0', scorePart => 0, expClk => 0, deathX => 0, deathY => 0)
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
			dead  : IN STD_LOGIC;
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
	PC : pause port map(ship.dead, startOfGame, max10_clk, shoot, pause_toggle, pauseClock, paused);
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
		variable calcR : INTEGER range -31 to 31;
		
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
------DRAWS THE END GAME STUFF---------------------------------------------------------------
	IF (ship.dead = '1') THEN
		IF (column = 86 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 91 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 118 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 130 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 241) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 150 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 151 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 133 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 136 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 139 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 122 AND row = 235) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 99 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 324 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 345 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 379 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 381 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 370 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 364 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 343 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 361 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 375 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 371 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 347 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 345 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 501 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 500 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 515 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 561 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 555 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 545 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 518 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 535 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 549 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 545 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 522 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 517 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 173 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 158 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 156 AND row = 261) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 172 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 186 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 205 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 219 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 219 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 198 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 195 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 207 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 192 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 168 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 255) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 170 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 230 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 233 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 235 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 243 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 280 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 288 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 292 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 282 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 275 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 274 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 249 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 402 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 401 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 412 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 419 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 235) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 422 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 437 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 242) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 436 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 461 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 490 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 462 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 473 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 462 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 465 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 481 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 91 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 248) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 261) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 111 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 123 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 128 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 135 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 151 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 150 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 136 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 121 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 110 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 96 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 323 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 352 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 381 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 380 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 369 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 362 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 339 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 363 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 376 AND row = 218) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 369 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 345 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 341 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 346 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 502 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 498 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 523 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 563 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 554 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 543 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 514 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 537 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 551 AND row = 220) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 543 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 520 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 518 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 171 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 157 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 158 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 173 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 189 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 207 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 219 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 218 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 196 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 196 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 207 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 189 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 167 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 172 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 230 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 233 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 236 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 281 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 289 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 280 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 275 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 251 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 244 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 402 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 218) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 401 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 415 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 422 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 417 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 438 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 435 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 468 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 490 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 491 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 463 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 473 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 223) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 228) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 461 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 466 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 481 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 484 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 92 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 229) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 110 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 127 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 127 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 126 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 137 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 148 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 137 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 137 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 119 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 111 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 93 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 323 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 357 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 383 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 228) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 379 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 369 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 360 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 335 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 364 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 376 AND row = 220) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 367 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 344 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 341 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 348 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 503 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 503 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 530 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 565 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 553 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 542 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 511 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 539 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 553 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 541 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 519 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 519 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 170 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 155 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 159 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 174 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 191 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 248) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 216 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 193 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 198 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 208 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 186 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 167 AND row = 261) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 229) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 174 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 231 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 234 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 236 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 251 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 282 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 286 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 289 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 276 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 268 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 246 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 246 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 252 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 241 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 403 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 400 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 422 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 439 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 439 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 435 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 433 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 491 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 491 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 484 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 454 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 465 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 229) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 460 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 468 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 482 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 481 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 93 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 108 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 130 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 127 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 127 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 139 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 146 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 137 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 135 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 117 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 91 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 327 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 322 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 362 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 385 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 389 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 368 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 358 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 332 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 366 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 377 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 365 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 343 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 341 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 349 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 503 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 495 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 536 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 566 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 552 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 541 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 507 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 541 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 554 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 538 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 518 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 521 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 168 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 155 AND row = 218) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 160 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 175 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 193 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 210 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 215 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 190 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 199 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 208 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 208 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 183 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 167 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 176 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 231 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 234 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 236 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 255 AND row = 282) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 282 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 286 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 290 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 277 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 276 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 265 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 246 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 246 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 253 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 238 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 403 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 399 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 421 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 421 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 419 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 439 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 439 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 434 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 434 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 491 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 491 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 484 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 454 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 467 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 459 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 229) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 454 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 469 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 482 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 94 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 108 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 126 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 128 AND row = 235) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 141 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 144 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 133 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 111 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 89 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 327 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 322 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 322 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 365 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 387 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 388 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 377 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 368 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 356 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 367 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 377 AND row = 223) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 377 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 362 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 342 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 341 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 351 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 497 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 503 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 495 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 541 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 566 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 551 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 539 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 542 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 554 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 536 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 517 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 523 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 166 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 154 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 162 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 177 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 195 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 212 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 241) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 213 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 188 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 200 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 208 AND row = 220) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 207 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 181 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 166 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 248) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 179 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 231 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 234 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 236 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 259 AND row = 283) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 283 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 218) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 286 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 290 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 276 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 272 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 276 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 262 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 246 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 218) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 246 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 253 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 236 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 404 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 405 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 399 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 398 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 420 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 424 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 415 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 438 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 433 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 435 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 482 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 491 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 490 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 483 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 472 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 454 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 456 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 468 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 241) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 478 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 458 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 470 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 483 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 96 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 110 AND row = 235) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 134 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 248) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 128 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 143 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 141 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 130 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 88 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 327 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 327 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 324 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 368 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 388 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 388 AND row = 218) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 376 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 367 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 353 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 369 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 377 AND row = 255) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 360 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 341 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 342 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 353 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 497 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 502 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 546 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 566 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 550 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 537 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 502 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 544 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 554 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 533 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 525 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 178 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 197 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 213 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 221 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 211 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 185 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 202 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 208 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 206 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 178 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 166 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 165 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 181 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 232 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 234 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 237 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 263 AND row = 282) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 283 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 287 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 290 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 275 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 272 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 277 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 259 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 253 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 233 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 404 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 405 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 398 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 399 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 424 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 420 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 424 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 411 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 433 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 438 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 433 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 437 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 490 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 482 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 470 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 457 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 469 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 241) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 473 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 457 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 456 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 471 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 483 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 473 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 87 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 97 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 111 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 114 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 135 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 129 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 145 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 139 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 133 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 128 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 111 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 87 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 322 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 220) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 327 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 327 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 371 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 389 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 387 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 375 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 366 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 351 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 325 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 370 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 376 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 357 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 341 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 342 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 355 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 498 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 502 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 498 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 550 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 565 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 549 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 535 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 500 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 545 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 553 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 531 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 527 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 163 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 154 AND row = 255) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 165 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 179 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 199 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 215 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 208 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 182 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 203 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 223) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 204 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 176 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 166 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 165 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 184 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 232 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 235 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 238 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 268 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 287 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 289 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 273 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 272 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 277 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 278 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 256 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 253 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 231 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 398 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 405 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 404 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 398 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 400 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 420 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 434 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 438 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 433 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 489 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 482 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 467 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 458 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 470 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 242) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 248) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 472 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 456 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 458 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 472 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 483 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 470 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 88 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 99 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 114 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 111 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 135 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 130 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 147 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 137 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 133 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 126 AND row = 228) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 114 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 322 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 331 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 373 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 385 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 374 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 366 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 349 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 323 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 371 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 228) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 375 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 354 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 343 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 357 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 498 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 501 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 500 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 553 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 563 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 548 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 532 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 499 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 546 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 552 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 528 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 529 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 161 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 229) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 154 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 167 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 180 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 200 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 216 AND row = 261) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 206 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 179 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 204 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 201 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 174 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 165 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 166 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 187 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 228 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 232 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 235 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 239 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 287 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 288 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 273 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 273 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 278 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 277 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 253 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 252 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 229 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 399 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 405 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 403 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 402 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 424 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 419 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 419 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 403 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 434 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 437 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 444 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 488 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 489 AND row = 261) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 481 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 464 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 459 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 471 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 242) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 470 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 459 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 474 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 484 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 467 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 88 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 100 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 242) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 255) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 134 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 131 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 148 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 152 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 135 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 134 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 124 AND row = 232) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 107 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 108 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 114 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 104 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 323 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 328 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 335 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 375 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 384 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 373 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 365 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 347 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 322 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 373 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 374 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 352 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 343 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 358 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 499 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 501 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 556 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 561 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 547 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 528 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 497 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 547 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 550 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 526 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 531 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 160 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 155 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 169 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 182 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 202 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 217 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 203 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 177 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 205 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 199 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 172 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 165 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 242) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 167 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 190 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 228 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 233 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 235 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 240 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 275 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 288 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 286 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 272 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 273 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 278 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 276 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 251 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 248 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 251 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 228 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 400 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 405 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 210) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 403 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 405 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 424 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 419 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 246) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 420 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 425 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 400 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 435 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 222) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 269) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 437 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 448 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 489 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 488 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 461 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 460 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 472 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 249) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 253) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 223) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 468 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 454 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 220) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 461 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 484 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 465 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 89 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 102 AND row = 220) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 114 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 115 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 260) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 131 AND row = 228) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 149 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 151 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 133 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 135 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 122 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 106 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 114 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 102 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 86 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 324 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 325 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 377 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 382 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 371 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 365 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 345 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 374 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 373 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 349 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 245) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 344 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 360 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 500 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 229) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 500 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 509 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 559 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 558 AND row = 208) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 546 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 524 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 497 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 548 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 548 AND row = 264) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 524 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 517 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 533 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 159 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 155 AND row = 259) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 170 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 184 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 203 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 218 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 220 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 201 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 175 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 206 AND row = 212) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 230) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 195 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 170 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 168 AND row = 209) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 192 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 229 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 233 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 235 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 241 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 278 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 288 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 274 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 228) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 275 AND row = 270) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 249 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 249 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 249 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 401 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 402 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 408 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 419 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 421 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 398 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 436 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 273) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 436 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 454 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 490 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 258) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 458 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 241) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 461 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 473 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 243) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 250) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 465 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 453 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 219) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 463 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 484 AND row = 214) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 204) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 462 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 91 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 224) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 247) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 116 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 112 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 104 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 118 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 130 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 256) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 125 AND row = 241) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 132 AND row = 227) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 150 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 151 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 133 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 136 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 138 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 122 AND row = 235) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 105 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 109 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 113 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 99 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 85 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 326 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 329 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 324 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 320 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 345 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 379 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 390 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 381 AND row = 205) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 370 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 364 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 343 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 321 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 375 AND row = 216) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 378 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 371 AND row = 266) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 347 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 340 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 345 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 361 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 501 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 505 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 504 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 500 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 515 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 561 AND row = 262) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 554 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 545 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 518 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 496 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 549 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 545 AND row = 267) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 521 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 516 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 517 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 535 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 158 AND row = 211) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 153 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 156 AND row = 261) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 172 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 186 AND row = 281) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 205 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 219 AND row = 254) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 219 AND row = 221) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 198 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 173 AND row = 196) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 207 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 209 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 192 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 168 AND row = 263) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 255) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 164 AND row = 239) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 170 AND row = 207) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 195 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 230 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 233 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 235 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 243 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 280 AND row = 271) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 284 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 285 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 288 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 291 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 282 AND row = 193) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 271 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 275 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 202) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 279 AND row = 231) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 273 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 268) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 236) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 245 AND row = 203) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 249 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 247 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 227 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 402 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 407 AND row = 213) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 406 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 401 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 411 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 423 AND row = 275) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 418 AND row = 272) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 417 AND row = 235) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 422 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 426 AND row = 197) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 427 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 397 AND row = 194) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 437 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 242) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 440 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 436 AND row = 277) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 432 AND row = 278) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 461 AND row = 280) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 490 AND row = 279) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 492 AND row = 257) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 486 AND row = 265) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 479 AND row = 274) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 455 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 276) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 237) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 462 AND row = 238) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 473 AND row = 240) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 475 AND row = 244) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 251) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 252) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 480 AND row = 225) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 476 AND row = 226) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 462 AND row = 233) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 234) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 217) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 452 AND row = 200) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 465 AND row = 199) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 477 AND row = 198) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 481 AND row = 206) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 485 AND row = 215) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 201) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 487 AND row = 195) THEN
			colorconcat <= "011000100000";
			ELSIF (column = 459 AND row = 195) THEN
			colorconcat <= "011000100000";
		END IF;
	END IF;

------DRAWS ENEMY EXPLOSIONS ON THE SCREEN---------------------------------------------------
		FOR i in 0 to 11 LOOP
			IF (aliens(i).expClk > 0 AND ship.dead = '0' AND ( ( ((column - aliens(i).deathX) ** 2) + ((row - aliens(i).deathY) ** 2) ) <= ((aliens(i).size * 6) ** 2) )) THEN
				colorconcat <= "111111111111";
			END IF;
		END LOOP;

------DRAWS THE PLAYER SHIP ON THE SCREEN----------------------------------------------------
		calcA := column - ship.x;		--Relative X position
		calcB := ship.y - row;			--Relative Y position
		calcC := -(ship_height * calcA)/ship_length + ship_height;	--Check if in area

		IF (ship.right = '1' AND (calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0) AND ship.dead = '0') THEN
			IF ((calcA = 1 OR calcA = ship_length) OR (calcB = 1 OR calcB = calcC)) THEN
				colorconcat <= "111111111111";
			ELSE
				colorconcat <= "111100000000";
			END IF;
		END IF;

		calcA := column - ship.x;		--Relative X position
		calcB := ship.y - row;			--Relative Y position
		calcC := (ship_height * calcA)/ship_length;	--Check if in area

		IF (ship.right = '0' AND (calcA > 0 AND calcA <= ship_length) AND (calcB <= calcC AND calcB > 0) AND ship.dead = '0') THEN
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
			IF (aliens(i).alive = '1' AND ship.dead = '0') THEN
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
		if(rising_edge(max10_clk) AND paused = '0' AND ship.dead = '0') then
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
	Move_CLK : process (pauseClock)
	variable movement_counter : integer range 0 to 200000 := 0;
	begin
		if(rising_edge(pauseClock)) then
			movement_counter := movement_counter + 1;
			if (movement_counter >= 200000) then
				movement_clock <= NOT movement_clock;
				movement_counter := 0;
			end if;
		end if;
	end process;

	hndl_Alien : process (pauseClock)
	VARIABLE updateScore : INTEGER range 0 to 999999 := 0;
	begin
		IF (rising_edge(pauseClock)) THEN
			updateScore := 0;
			FOR i in 0 to 11 LOOP
				IF (aliens(i).expClk > 0) THEN
					aliens(i).expClk <= aliens(i).expClk - 1;
				END IF;

				IF (aliens(i).collision = '1' AND aliens(i).alive = '1') THEN
					aliens(i).alive <= '0';
					updateScore := updateScore + awardScore(aliens(i).size);
					aliens(i).deathX <= aliens(i).x;
					aliens(i).deathY <= aliens(i).y;
					aliens(i).expClk <= 25000000;
				ELSE
					updateScore := updateScore;
				END IF;

				IF(aliens(i).die = '1') THEN
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

					IF (score > 10000 AND RNG(1) = '1' AND aliens(i).min_p > 10) THEN
						aliens(i).min_p <= aliens(i).min_p - 2;
					END IF;
					
				ELSIF ( (i < 12 AND aliens(i).alive = '0' AND aliens(i).tsls >= (aliens(i).min_p * 50000000))) THEN
					aliens(i).alive <= '1';
					aliens(i).size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(9 downto 7)) + 3);
					aliens(i).color <= "000000001100";
					aliens(i).hs1 <= '1';
					aliens(i).tsls <= 0;

					IF (score > 20000 AND aliens(i).min_p > 2) THEN
						aliens(i).min_p <= aliens(i).min_p - 2;
					END IF;

				ELSIF ((aliens(i).x > 60000) OR (aliens(i).x <= 0)) THEN
					aliens(i).alive <= '0';
				END IF;
			END LOOP;
			score <= score + updateScore;
		END IF;
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
		IF (rising_edge(pauseClock)) THEN
			rstScreenS <= rst_Screen;
			IF (rst_Screen = '1') THEN
				FOR i in 0 to 11 LOOP
					aliens(i).die <= '1';
				END LOOP;
			ELSE
				FOR i in 0 to 11 LOOP
					aliens(i).die <= '0';
					aliens(i).collision <= '0';
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
					rst_Screen := '1';
				END IF;

				--Alien and Projectile Collision--
				FOR j in 0 to (max_pproj - 1) LOOP
					IF ((p_proj(j).x + 20) >= (aliens(i).x - (6 * aliens(i).size)) AND
					p_proj(j).x <= aliens(i).x AND
					p_proj(j).y >= (aliens(i).y - (6 * aliens(i).size)) AND
					p_proj(j).y <= aliens(i).y AND p_proj(j).e = '1' AND aliens(i).alive = '1' AND aliens(i).x < (640 + aliens(i).size * 6)) THEN
						aliens(i).collision <= '1';
					END IF;
				END LOOP;
			END LOOP;
		END IF;
    END PROCESS;

------SPARE LIVES AND END O' GAME------------------------------------------------------------
	LifeMngr : PROCESS (rstScreenS, pauseClock)
	BEGIN
		IF (rising_edge(rstScreenS) AND spare_ships < 4) THEN
			spare_ships <= spare_ships - 1;
		END IF;

		IF (Paused = '1' AND startOfGame = '1') THEN
			spare_ships <= 3;
        END IF;

		IF (spare_ships = -1) THEN
			ship.dead <= '1';
		ELSE
			ship.dead <= '0';
		END IF;
	END PROCESS;

END ARCHITECTURE;