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
		FOR i in 0 to 1 LOOP
			IF (column = (86*(1-i)*(1-i)*(1-i) + 258*i*(1-i)*(1-i) + 259*i + 91*i*i*i) AND row = (194*(1-i)*(1-i)*(1-i) + 588*i*(1-i)*(1-i) + 589*i + 200*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (91*(1-i)*(1-i)*(1-i) + 279*i*(1-i)*(1-i) + 292*i + 105*i*i*i) AND row = (200*(1-i)*(1-i)*(1-i) + 604*i*(1-i)*(1-i) + 627*i + 224*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (105*(1-i) + 116*i) AND row = (224*(1-i) + 247*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (116*(1-i) + 116*i) AND row = (247*(1-i) + 257*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (116*(1-i)*(1-i)*(1-i) + 348*i*(1-i)*(1-i) + 344*i + 112*i*i*i) AND row = (257*(1-i)*(1-i)*(1-i) + 811*i*(1-i)*(1-i) + 828*i + 276*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (112*(1-i)*(1-i)*(1-i) + 327*i*(1-i)*(1-i) + 315*i + 105*i*i*i) AND row = (276*(1-i)*(1-i)*(1-i) + 828*i*(1-i)*(1-i) + 834*i + 279*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (105*(1-i)*(1-i)*(1-i) + 315*i*(1-i)*(1-i) + 323*i + 118*i*i*i) AND row = (279*(1-i)*(1-i)*(1-i) + 842*i*(1-i)*(1-i) + 843*i + 281*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (118*(1-i)*(1-i)*(1-i) + 406*i*(1-i)*(1-i) + 419*i + 130*i*i*i) AND row = (281*(1-i)*(1-i)*(1-i) + 843*i*(1-i)*(1-i) + 837*i + 276*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (130*(1-i)*(1-i)*(1-i) + 376*i*(1-i)*(1-i) + 375*i + 125*i*i*i) AND row = (276*(1-i)*(1-i)*(1-i) + 825*i*(1-i)*(1-i) + 822*i + 256*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (125*(1-i) + 125*i) AND row = (256*(1-i) + 241*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (125*(1-i) + 132*i) AND row = (241*(1-i) + 227*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (132*(1-i)*(1-i)*(1-i) + 423*i*(1-i)*(1-i) + 443*i + 150*i*i*i) AND row = (227*(1-i)*(1-i)*(1-i) + 631*i*(1-i)*(1-i) + 600*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (150*(1-i)*(1-i)*(1-i) + 460*i*(1-i)*(1-i) + 460*i + 151*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 590*i*(1-i)*(1-i) + 583*i + 193*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (151*(1-i)*(1-i)*(1-i) + 447*i*(1-i)*(1-i) + 404*i + 133*i*i*i) AND row = (193*(1-i)*(1-i)*(1-i) + 578*i*(1-i)*(1-i) + 578*i + 193*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (133*(1-i)*(1-i)*(1-i) + 393*i*(1-i)*(1-i) + 399*i + 136*i*i*i) AND row = (193*(1-i)*(1-i)*(1-i) + 585*i*(1-i)*(1-i) + 592*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (136*(1-i)*(1-i)*(1-i) + 414*i*(1-i)*(1-i) + 417*i + 139*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 596*i*(1-i)*(1-i) + 599*i + 201*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (139*(1-i)*(1-i)*(1-i) + 417*i*(1-i)*(1-i) + 368*i + 122*i*i*i) AND row = (201*(1-i)*(1-i)*(1-i) + 612*i*(1-i)*(1-i) + 708*i + 235*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (122*(1-i)*(1-i)*(1-i) + 362*i*(1-i)*(1-i) + 318*i + 106*i*i*i) AND row = (235*(1-i)*(1-i)*(1-i) + 702*i*(1-i)*(1-i) + 606*i + 200*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (106*(1-i)*(1-i)*(1-i) + 318*i*(1-i)*(1-i) + 322*i + 109*i*i*i) AND row = (200*(1-i)*(1-i)*(1-i) + 599*i*(1-i)*(1-i) + 596*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (109*(1-i)*(1-i)*(1-i) + 337*i*(1-i)*(1-i) + 345*i + 113*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 594*i*(1-i)*(1-i) + 587*i + 194*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (113*(1-i)*(1-i)*(1-i) + 341*i*(1-i)*(1-i) + 321*i + 99*i*i*i) AND row = (194*(1-i)*(1-i)*(1-i) + 583*i*(1-i)*(1-i) + 581*i + 193*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (99*(1-i)*(1-i)*(1-i) + 262*i*(1-i)*(1-i) + 258*i + 86*i*i*i) AND row = (193*(1-i)*(1-i)*(1-i) + 578*i*(1-i)*(1-i) + 578*i + 194*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (321*(1-i)*(1-i)*(1-i) + 961*i*(1-i)*(1-i) + 965*i + 326*i*i*i) AND row = (194*(1-i)*(1-i)*(1-i) + 586*i*(1-i)*(1-i) + 589*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (326*(1-i) + 329*i) AND row = (198*(1-i) + 200*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (329*(1-i) + 329*i) AND row = (200*(1-i) + 234*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (329*(1-i)*(1-i)*(1-i) + 989*i*(1-i)*(1-i) + 989*i + 329*i*i*i) AND row = (234*(1-i)*(1-i)*(1-i) + 758*i*(1-i)*(1-i) + 808*i + 271*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (329*(1-i)*(1-i)*(1-i) + 985*i*(1-i)*(1-i) + 981*i + 324*i*i*i) AND row = (271*(1-i)*(1-i)*(1-i) + 820*i*(1-i)*(1-i) + 825*i + 276*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (324*(1-i)*(1-i)*(1-i) + 967*i*(1-i)*(1-i) + 961*i + 320*i*i*i) AND row = (276*(1-i)*(1-i)*(1-i) + 832*i*(1-i)*(1-i) + 837*i + 279*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (320*(1-i)*(1-i)*(1-i) + 958*i*(1-i)*(1-i) + 975*i + 345*i*i*i) AND row = (279*(1-i)*(1-i)*(1-i) + 845*i*(1-i)*(1-i) + 846*i + 281*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (345*(1-i)*(1-i)*(1-i) + 1108*i*(1-i)*(1-i) + 1115*i + 379*i*i*i) AND row = (281*(1-i)*(1-i)*(1-i) + 842*i*(1-i)*(1-i) + 840*i + 272*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (379*(1-i)*(1-i)*(1-i) + 1164*i*(1-i)*(1-i) + 1174*i + 390*i*i*i) AND row = (272*(1-i)*(1-i)*(1-i) + 787*i*(1-i)*(1-i) + 756*i + 236*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (390*(1-i)*(1-i)*(1-i) + 1170*i*(1-i)*(1-i) + 1162*i + 381*i*i*i) AND row = (236*(1-i)*(1-i)*(1-i) + 667*i*(1-i)*(1-i) + 642*i + 205*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (381*(1-i)*(1-i)*(1-i) + 1132*i*(1-i)*(1-i) + 1125*i + 370*i*i*i) AND row = (205*(1-i)*(1-i)*(1-i) + 603*i*(1-i)*(1-i) + 596*i + 196*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (370*(1-i) + 364*i) AND row = (196*(1-i) + 193*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (364*(1-i) + 343*i) AND row = (193*(1-i) + 193*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (343*(1-i)*(1-i)*(1-i) + 984*i*(1-i)*(1-i) + 966*i + 321*i*i*i) AND row = (193*(1-i)*(1-i)*(1-i) + 579*i*(1-i)*(1-i) + 579*i + 194*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (361*(1-i)*(1-i)*(1-i) + 1100*i*(1-i)*(1-i) + 1116*i + 375*i*i*i) AND row = (201*(1-i)*(1-i)*(1-i) + 612*i*(1-i)*(1-i) + 629*i + 216*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (375*(1-i)*(1-i)*(1-i) + 1135*i*(1-i)*(1-i) + 1135*i + 378*i*i*i) AND row = (216*(1-i)*(1-i)*(1-i) + 670*i*(1-i)*(1-i) + 671*i + 238*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (378*(1-i)*(1-i)*(1-i) + 1135*i*(1-i)*(1-i) + 1132*i + 371*i*i*i) AND row = (238*(1-i)*(1-i)*(1-i) + 764*i*(1-i)*(1-i) + 776*i + 266*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (371*(1-i)*(1-i)*(1-i) + 1096*i*(1-i)*(1-i) + 1064*i + 347*i*i*i) AND row = (266*(1-i)*(1-i)*(1-i) + 822*i*(1-i)*(1-i) + 835*i + 276*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (347*(1-i)*(1-i)*(1-i) + 1023*i*(1-i)*(1-i) + 1022*i + 340*i*i*i) AND row = (276*(1-i)*(1-i)*(1-i) + 821*i*(1-i)*(1-i) + 816*i + 236*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (340*(1-i)*(1-i)*(1-i) + 1020*i*(1-i)*(1-i) + 1020*i + 340*i*i*i) AND row = (236*(1-i)*(1-i)*(1-i) + 646*i*(1-i)*(1-i) + 610*i + 201*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (340*(1-i)*(1-i)*(1-i) + 1024*i*(1-i)*(1-i) + 1028*i + 345*i*i*i) AND row = (201*(1-i)*(1-i)*(1-i) + 599*i*(1-i)*(1-i) + 597*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (345*(1-i)*(1-i)*(1-i) + 1048*i*(1-i)*(1-i) + 1073*i + 361*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 595*i*(1-i)*(1-i) + 599*i + 201*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (496*(1-i)*(1-i)*(1-i) + 1488*i*(1-i)*(1-i) + 1494*i + 501*i*i*i) AND row = (193*(1-i)*(1-i)*(1-i) + 584*i*(1-i)*(1-i) + 591*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (501*(1-i)*(1-i)*(1-i) + 1518*i*(1-i)*(1-i) + 1518*i + 505*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 603*i*(1-i)*(1-i) + 610*i + 239*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (505*(1-i) + 504*i) AND row = (239*(1-i) + 274*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (504*(1-i) + 500*i) AND row = (274*(1-i) + 276*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (500*(1-i)*(1-i)*(1-i) + 1475*i*(1-i)*(1-i) + 1480*i + 515*i*i*i) AND row = (276*(1-i)*(1-i)*(1-i) + 840*i*(1-i)*(1-i) + 841*i + 281*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (515*(1-i)*(1-i)*(1-i) + 1632*i*(1-i)*(1-i) + 1661*i + 561*i*i*i) AND row = (281*(1-i)*(1-i)*(1-i) + 848*i*(1-i)*(1-i) + 836*i + 262*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (561*(1-i)*(1-i)*(1-i) + 1711*i*(1-i)*(1-i) + 1703*i + 555*i*i*i) AND row = (262*(1-i)*(1-i)*(1-i) + 730*i*(1-i)*(1-i) + 654*i + 203*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (555*(1-i)*(1-i)*(1-i) + 1658*i*(1-i)*(1-i) + 1645*i + 545*i*i*i) AND row = (203*(1-i)*(1-i)*(1-i) + 603*i*(1-i)*(1-i) + 593*i + 196*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (545*(1-i)*(1-i)*(1-i) + 1619*i*(1-i)*(1-i) + 1617*i + 518*i*i*i) AND row = (196*(1-i)*(1-i)*(1-i) + 581*i*(1-i)*(1-i) + 580*i + 193*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (518*(1-i)*(1-i)*(1-i) + 1515*i*(1-i)*(1-i) + 1491*i + 496*i*i*i) AND row = (193*(1-i)*(1-i)*(1-i) + 578*i*(1-i)*(1-i) + 579*i + 193*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (535*(1-i)*(1-i)*(1-i) + 1628*i*(1-i)*(1-i) + 1640*i + 549*i*i*i) AND row = (201*(1-i)*(1-i)*(1-i) + 614*i*(1-i)*(1-i) + 626*i + 215*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (549*(1-i)*(1-i)*(1-i) + 1673*i*(1-i)*(1-i) + 1667*i + 545*i*i*i) AND row = (215*(1-i)*(1-i)*(1-i) + 694*i*(1-i)*(1-i) + 770*i + 267*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (545*(1-i)*(1-i)*(1-i) + 1614*i*(1-i)*(1-i) + 1585*i + 522*i*i*i) AND row = (267*(1-i)*(1-i)*(1-i) + 827*i*(1-i)*(1-i) + 836*i + 275*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (522*(1-i)*(1-i)*(1-i) + 1549*i*(1-i)*(1-i) + 1548*i + 516*i*i*i) AND row = (275*(1-i)*(1-i)*(1-i) + 816*i*(1-i)*(1-i) + 810*i + 233*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (516*(1-i)*(1-i)*(1-i) + 1548*i*(1-i)*(1-i) + 1548*i + 517*i*i*i) AND row = (233*(1-i)*(1-i)*(1-i) + 618*i*(1-i)*(1-i) + 602*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (517*(1-i)*(1-i)*(1-i) + 1558*i*(1-i)*(1-i) + 1590*i + 535*i*i*i) AND row = (199*(1-i)*(1-i)*(1-i) + 592*i*(1-i)*(1-i) + 595*i + 201*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (173*(1-i)*(1-i)*(1-i) + 505*i*(1-i)*(1-i) + 483*i + 158*i*i*i) AND row = (196*(1-i)*(1-i)*(1-i) + 595*i*(1-i)*(1-i) + 618*i + 211*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (158*(1-i)*(1-i)*(1-i) + 461*i*(1-i)*(1-i) + 458*i + 153*i*i*i) AND row = (211*(1-i)*(1-i)*(1-i) + 660*i*(1-i)*(1-i) + 679*i + 240*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (153*(1-i)*(1-i)*(1-i) + 460*i*(1-i)*(1-i) + 460*i + 156*i*i*i) AND row = (240*(1-i)*(1-i)*(1-i) + 761*i*(1-i)*(1-i) + 764*i + 261*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (156*(1-i)*(1-i)*(1-i) + 482*i*(1-i)*(1-i) + 500*i + 172*i*i*i) AND row = (261*(1-i)*(1-i)*(1-i) + 808*i*(1-i)*(1-i) + 828*i + 279*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (172*(1-i)*(1-i)*(1-i) + 529*i*(1-i)*(1-i) + 537*i + 186*i*i*i) AND row = (279*(1-i)*(1-i)*(1-i) + 843*i*(1-i)*(1-i) + 844*i + 281*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (186*(1-i)*(1-i)*(1-i) + 590*i*(1-i)*(1-i) + 598*i + 205*i*i*i) AND row = (281*(1-i)*(1-i)*(1-i) + 843*i*(1-i)*(1-i) + 841*i + 275*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (205*(1-i)*(1-i)*(1-i) + 635*i*(1-i)*(1-i) + 651*i + 219*i*i*i) AND row = (275*(1-i)*(1-i)*(1-i) + 810*i*(1-i)*(1-i) + 785*i + 254*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (219*(1-i)*(1-i)*(1-i) + 664*i*(1-i)*(1-i) + 664*i + 219*i*i*i) AND row = (254*(1-i)*(1-i)*(1-i) + 737*i*(1-i)*(1-i) + 689*i + 221*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (219*(1-i)*(1-i)*(1-i) + 648*i*(1-i)*(1-i) + 623*i + 198*i*i*i) AND row = (221*(1-i)*(1-i)*(1-i) + 627*i*(1-i)*(1-i) + 596*i + 195*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (198*(1-i)*(1-i)*(1-i) + 574*i*(1-i)*(1-i) + 535*i + 173*i*i*i) AND row = (195*(1-i)*(1-i)*(1-i) + 579*i*(1-i)*(1-i) + 580*i + 196*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (195*(1-i)*(1-i)*(1-i) + 600*i*(1-i)*(1-i) + 612*i + 207*i*i*i) AND row = (200*(1-i)*(1-i)*(1-i) + 606*i*(1-i)*(1-i) + 621*i + 215*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (207*(1-i)*(1-i)*(1-i) + 626*i*(1-i)*(1-i) + 628*i + 209*i*i*i) AND row = (215*(1-i)*(1-i)*(1-i) + 657*i*(1-i)*(1-i) + 670*i + 233*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (209*(1-i)*(1-i)*(1-i) + 632*i*(1-i)*(1-i) + 615*i + 192*i*i*i) AND row = (233*(1-i)*(1-i)*(1-i) + 777*i*(1-i)*(1-i) + 818*i + 276*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (192*(1-i)*(1-i)*(1-i) + 545*i*(1-i)*(1-i) + 522*i + 168*i*i*i) AND row = (276*(1-i)*(1-i)*(1-i) + 839*i*(1-i)*(1-i) + 826*i + 263*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (168*(1-i) + 164*i) AND row = (263*(1-i) + 255*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (164*(1-i) + 164*i) AND row = (255*(1-i) + 239*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (164*(1-i)*(1-i)*(1-i) + 491*i*(1-i)*(1-i) + 494*i + 170*i*i*i) AND row = (239*(1-i)*(1-i)*(1-i) + 662*i*(1-i)*(1-i) + 646*i + 207*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (170*(1-i)*(1-i)*(1-i) + 528*i*(1-i)*(1-i) + 559*i + 195*i*i*i) AND row = (207*(1-i)*(1-i)*(1-i) + 595*i*(1-i)*(1-i) + 586*i + 200*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (227*(1-i)*(1-i)*(1-i) + 679*i*(1-i)*(1-i) + 683*i + 230*i*i*i) AND row = (195*(1-i)*(1-i)*(1-i) + 592*i*(1-i)*(1-i) + 597*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (230*(1-i)*(1-i)*(1-i) + 694*i*(1-i)*(1-i) + 699*i + 233*i*i*i) AND row = (199*(1-i)*(1-i)*(1-i) + 597*i*(1-i)*(1-i) + 598*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (233*(1-i)*(1-i)*(1-i) + 703*i*(1-i)*(1-i) + 705*i + 235*i*i*i) AND row = (199*(1-i)*(1-i)*(1-i) + 600*i*(1-i)*(1-i) + 630*i + 234*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (235*(1-i)*(1-i)*(1-i) + 710*i*(1-i)*(1-i) + 709*i + 243*i*i*i) AND row = (234*(1-i)*(1-i)*(1-i) + 813*i*(1-i)*(1-i) + 812*i + 277*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (243*(1-i)*(1-i)*(1-i) + 764*i*(1-i)*(1-i) + 822*i + 280*i*i*i) AND row = (277*(1-i)*(1-i)*(1-i) + 861*i*(1-i)*(1-i) + 852*i + 271*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (280*(1-i)*(1-i)*(1-i) + 851*i*(1-i)*(1-i) + 852*i + 284*i*i*i) AND row = (271*(1-i)*(1-i)*(1-i) + 796*i*(1-i)*(1-i) + 785*i + 231*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (284*(1-i) + 285*i) AND row = (231*(1-i) + 199*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (285*(1-i) + 288*i) AND row = (199*(1-i) + 198*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (288*(1-i)*(1-i)*(1-i) + 873*i*(1-i)*(1-i) + 876*i + 292*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 594*i*(1-i)*(1-i) + 592*i + 196*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (292*(1-i)*(1-i)*(1-i) + 876*i*(1-i)*(1-i) + 873*i + 282*i*i*i) AND row = (196*(1-i)*(1-i)*(1-i) + 582*i*(1-i)*(1-i) + 582*i + 194*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (282*(1-i)*(1-i)*(1-i) + 830*i*(1-i)*(1-i) + 816*i + 271*i*i*i) AND row = (194*(1-i)*(1-i)*(1-i) + 582*i*(1-i)*(1-i) + 582*i + 194*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (271*(1-i)*(1-i)*(1-i) + 813*i*(1-i)*(1-i) + 818*i + 275*i*i*i) AND row = (194*(1-i)*(1-i)*(1-i) + 585*i*(1-i)*(1-i) + 591*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (275*(1-i) + 279*i) AND row = (199*(1-i) + 202*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (279*(1-i) + 279*i) AND row = (202*(1-i) + 231*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (279*(1-i)*(1-i)*(1-i) + 840*i*(1-i)*(1-i) + 838*i + 274*i*i*i) AND row = (231*(1-i)*(1-i)*(1-i) + 789*i*(1-i)*(1-i) + 803*i + 272*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (274*(1-i)*(1-i)*(1-i) + 796*i*(1-i)*(1-i) + 757*i + 247*i*i*i) AND row = (272*(1-i)*(1-i)*(1-i) + 838*i*(1-i)*(1-i) + 833*i + 268*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (247*(1-i)*(1-i)*(1-i) + 737*i*(1-i)*(1-i) + 736*i + 245*i*i*i) AND row = (268*(1-i)*(1-i)*(1-i) + 794*i*(1-i)*(1-i) + 789*i + 236*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (245*(1-i)*(1-i)*(1-i) + 735*i*(1-i)*(1-i) + 735*i + 245*i*i*i) AND row = (236*(1-i)*(1-i)*(1-i) + 660*i*(1-i)*(1-i) + 617*i + 203*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (245*(1-i)*(1-i)*(1-i) + 739*i*(1-i)*(1-i) + 741*i + 249*i*i*i) AND row = (203*(1-i)*(1-i)*(1-i) + 601*i*(1-i)*(1-i) + 598*i + 198*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (249*(1-i)*(1-i)*(1-i) + 766*i*(1-i)*(1-i) + 764*i + 247*i*i*i) AND row = (198*(1-i)*(1-i)*(1-i) + 590*i*(1-i)*(1-i) + 586*i + 194*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (247*(1-i)*(1-i)*(1-i) + 710*i*(1-i)*(1-i) + 684*i + 227*i*i*i) AND row = (194*(1-i)*(1-i)*(1-i) + 580*i*(1-i)*(1-i) + 581*i + 195*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (397*(1-i)*(1-i)*(1-i) + 1189*i*(1-i)*(1-i) + 1194*i + 402*i*i*i) AND row = (195*(1-i)*(1-i)*(1-i) + 590*i*(1-i)*(1-i) + 594*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (402*(1-i) + 406*i) AND row = (199*(1-i) + 200*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (406*(1-i) + 407*i) AND row = (200*(1-i) + 213*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (407*(1-i)*(1-i)*(1-i) + 1224*i*(1-i)*(1-i) + 1223*i + 406*i*i*i) AND row = (213*(1-i)*(1-i)*(1-i) + 684*i*(1-i)*(1-i) + 820*i + 274*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (406*(1-i)*(1-i)*(1-i) + 1219*i*(1-i)*(1-i) + 1212*i + 401*i*i*i) AND row = (274*(1-i)*(1-i)*(1-i) + 824*i*(1-i)*(1-i) + 826*i + 275*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (401*(1-i)*(1-i)*(1-i) + 1196*i*(1-i)*(1-i) + 1192*i + 397*i*i*i) AND row = (275*(1-i)*(1-i)*(1-i) + 828*i*(1-i)*(1-i) + 831*i + 278*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (397*(1-i)*(1-i)*(1-i) + 1190*i*(1-i)*(1-i) + 1194*i + 412*i*i*i) AND row = (278*(1-i)*(1-i)*(1-i) + 839*i*(1-i)*(1-i) + 840*i + 280*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (412*(1-i)*(1-i)*(1-i) + 1277*i*(1-i)*(1-i) + 1281*i + 426*i*i*i) AND row = (280*(1-i)*(1-i)*(1-i) + 840*i*(1-i)*(1-i) + 839*i + 278*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (426*(1-i)*(1-i)*(1-i) + 1279*i*(1-i)*(1-i) + 1275*i + 423*i*i*i) AND row = (278*(1-i)*(1-i)*(1-i) + 832*i*(1-i)*(1-i) + 828*i + 275*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (423*(1-i)*(1-i)*(1-i) + 1263*i*(1-i)*(1-i) + 1258*i + 419*i*i*i) AND row = (275*(1-i)*(1-i)*(1-i) + 825*i*(1-i)*(1-i) + 822*i + 272*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (419*(1-i)*(1-i)*(1-i) + 1255*i*(1-i)*(1-i) + 1254*i + 418*i*i*i) AND row = (272*(1-i)*(1-i)*(1-i) + 813*i*(1-i)*(1-i) + 764*i + 235*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (418*(1-i)*(1-i)*(1-i) + 1254*i*(1-i)*(1-i) + 1253*i + 422*i*i*i) AND row = (235*(1-i)*(1-i)*(1-i) + 593*i*(1-i)*(1-i) + 597*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (422*(1-i)*(1-i)*(1-i) + 1271*i*(1-i)*(1-i) + 1276*i + 426*i*i*i) AND row = (199*(1-i)*(1-i)*(1-i) + 597*i*(1-i)*(1-i) + 594*i + 197*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (426*(1-i)*(1-i)*(1-i) + 1284*i*(1-i)*(1-i) + 1284*i + 427*i*i*i) AND row = (197*(1-i)*(1-i)*(1-i) + 588*i*(1-i)*(1-i) + 587*i + 195*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (427*(1-i)*(1-i)*(1-i) + 1275*i*(1-i)*(1-i) + 1194*i + 397*i*i*i) AND row = (195*(1-i)*(1-i)*(1-i) + 581*i*(1-i)*(1-i) + 581*i + 195*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (432*(1-i)*(1-i)*(1-i) + 1296*i*(1-i)*(1-i) + 1302*i + 437*i*i*i) AND row = (195*(1-i)*(1-i)*(1-i) + 591*i*(1-i)*(1-i) + 595*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (437*(1-i)*(1-i)*(1-i) + 1323*i*(1-i)*(1-i) + 1323*i + 440*i*i*i) AND row = (199*(1-i)*(1-i)*(1-i) + 600*i*(1-i)*(1-i) + 607*i + 242*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (440*(1-i) + 440*i) AND row = (242*(1-i) + 276*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (440*(1-i) + 436*i) AND row = (276*(1-i) + 277*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (436*(1-i)*(1-i)*(1-i) + 1301*i*(1-i)*(1-i) + 1297*i + 432*i*i*i) AND row = (277*(1-i)*(1-i)*(1-i) + 831*i*(1-i)*(1-i) + 834*i + 279*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (432*(1-i)*(1-i)*(1-i) + 1297*i*(1-i)*(1-i) + 1309*i + 461*i*i*i) AND row = (279*(1-i)*(1-i)*(1-i) + 840*i*(1-i)*(1-i) + 841*i + 280*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (461*(1-i)*(1-i)*(1-i) + 1461*i*(1-i)*(1-i) + 1469*i + 490*i*i*i) AND row = (280*(1-i)*(1-i)*(1-i) + 843*i*(1-i)*(1-i) + 842*i + 279*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (490*(1-i)*(1-i)*(1-i) + 1475*i*(1-i)*(1-i) + 1479*i + 492*i*i*i) AND row = (279*(1-i)*(1-i)*(1-i) + 828*i*(1-i)*(1-i) + 775*i + 257*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (492*(1-i)*(1-i)*(1-i) + 1474*i*(1-i)*(1-i) + 1468*i + 486*i*i*i) AND row = (257*(1-i)*(1-i)*(1-i) + 771*i*(1-i)*(1-i) + 779*i + 265*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (486*(1-i)*(1-i)*(1-i) + 1449*i*(1-i)*(1-i) + 1446*i + 479*i*i*i) AND row = (265*(1-i)*(1-i)*(1-i) + 819*i*(1-i)*(1-i) + 822*i + 275*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (479*(1-i)*(1-i)*(1-i) + 1427*i*(1-i)*(1-i) + 1398*i + 455*i*i*i) AND row = (275*(1-i)*(1-i)*(1-i) + 827*i*(1-i)*(1-i) + 828*i + 276*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (455*(1-i) + 452*i) AND row = (276*(1-i) + 276*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(1-i) + 452*i) AND row = (276*(1-i) + 237*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(1-i) + 462*i) AND row = (237*(1-i) + 238*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (462*(1-i)*(1-i)*(1-i) + 1403*i*(1-i)*(1-i) + 1419*i + 473*i*i*i) AND row = (238*(1-i)*(1-i)*(1-i) + 716*i*(1-i)*(1-i) + 719*i + 240*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (473*(1-i)*(1-i)*(1-i) + 1422*i*(1-i)*(1-i) + 1425*i + 475*i*i*i) AND row = (240*(1-i)*(1-i)*(1-i) + 722*i*(1-i)*(1-i) + 727*i + 244*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (475*(1-i)*(1-i)*(1-i) + 1427*i*(1-i)*(1-i) + 1430*i + 477*i*i*i) AND row = (244*(1-i)*(1-i)*(1-i) + 738*i*(1-i)*(1-i) + 747*i + 251*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (477*(1-i)*(1-i)*(1-i) + 1437*i*(1-i)*(1-i) + 1437*i + 480*i*i*i) AND row = (251*(1-i)*(1-i)*(1-i) + 762*i*(1-i)*(1-i) + 762*i + 252*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (480*(1-i)*(1-i)*(1-i) + 1443*i*(1-i)*(1-i) + 1443*i + 480*i*i*i) AND row = (252*(1-i)*(1-i)*(1-i) + 750*i*(1-i)*(1-i) + 696*i + 225*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (480*(1-i)*(1-i)*(1-i) + 1438*i*(1-i)*(1-i) + 1428*i + 476*i*i*i) AND row = (225*(1-i)*(1-i)*(1-i) + 656*i*(1-i)*(1-i) + 659*i + 226*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (476*(1-i)*(1-i)*(1-i) + 1428*i*(1-i)*(1-i) + 1422*i + 462*i*i*i) AND row = (226*(1-i)*(1-i)*(1-i) + 697*i*(1-i)*(1-i) + 700*i + 233*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (462*(1-i) + 452*i) AND row = (233*(1-i) + 234*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(1-i) + 452*i) AND row = (234*(1-i) + 217*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(1-i)*(1-i)*(1-i) + 1356*i*(1-i)*(1-i) + 1356*i + 452*i*i*i) AND row = (217*(1-i)*(1-i)*(1-i) + 625*i*(1-i)*(1-i) + 602*i + 200*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(1-i)*(1-i)*(1-i) + 1359*i*(1-i)*(1-i) + 1376*i + 465*i*i*i) AND row = (200*(1-i)*(1-i)*(1-i) + 601*i*(1-i)*(1-i) + 599*i + 199*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (465*(1-i) + 477*i) AND row = (199*(1-i) + 198*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (477*(1-i) + 481*i) AND row = (198*(1-i) + 206*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (481*(1-i)*(1-i)*(1-i) + 1448*i*(1-i)*(1-i) + 1453*i + 485*i*i*i) AND row = (206*(1-i)*(1-i)*(1-i) + 634*i*(1-i)*(1-i) + 645*i + 215*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (485*(1-i)*(1-i)*(1-i) + 1458*i*(1-i)*(1-i) + 1461*i + 487*i*i*i) AND row = (215*(1-i)*(1-i)*(1-i) + 645*i*(1-i)*(1-i) + 628*i + 201*i*i*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (487*(1-i) + 487*i) AND row = (201*(1-i) + 195*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (487*(1-i) + 459*i) AND row = (195*(1-i) + 195*i)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (459*(1-i)*(1-i)*(1-i) + 1333*i*(1-i)*(1-i) + 1296*i + 432*i*i*i) AND row = (195*(1-i)*(1-i)*(1-i) + 585*i*(1-i)*(1-i) + 586*i + 195*i*i*i)) THEN
				colorconcat <= "011000100000";
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

------DRAWS ENEMY EXPLOSIONS ON THE SCREEN---------------------------------------------------
		FOR i in 0 to 11 LOOP
			IF (aliens(i).expClk > 0 AND ( ( ((column - aliens(i).deathX) ** 2) + ((row - aliens(i).deathY) ** 2) ) <= ((aliens(i).size * 6) ** 2) )) THEN
				colorconcat <= "111111111111";
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