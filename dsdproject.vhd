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
		FOR i in 0 to 10 LOOP
		calcR := i;

			IF (column = (86*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (258*calcR)/10*((10-calcR)*(10-calcR))/100 + 259*expLookup(i) + 91*(calcR*calcR*calcR)/1000) AND row = (194*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (588*calcR)/10*((10-calcR)*(10-calcR))/100 + 589*expLookup(i) + 200*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (91*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (279*calcR)/10*((10-calcR)*(10-calcR))/100 + 292*expLookup(i) + 105*(calcR*calcR*calcR)/1000) AND row = (200*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (604*calcR)/10*((10-calcR)*(10-calcR))/100 + 627*expLookup(i) + 224*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (105*(10-calcR)/10 + 116*calcR/10) AND row = (224*(10-calcR)/10 + 247*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (116*(10-calcR)/10 + 116*calcR/10) AND row = (247*(10-calcR)/10 + 257*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (116*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (348*calcR)/10*((10-calcR)*(10-calcR))/100 + 344*expLookup(i) + 112*(calcR*calcR*calcR)/1000) AND row = (257*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (811*calcR)/10*((10-calcR)*(10-calcR))/100 + 828*expLookup(i) + 276*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (112*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (327*calcR)/10*((10-calcR)*(10-calcR))/100 + 315*expLookup(i) + 105*(calcR*calcR*calcR)/1000) AND row = (276*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (828*calcR)/10*((10-calcR)*(10-calcR))/100 + 834*expLookup(i) + 279*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (105*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (315*calcR)/10*((10-calcR)*(10-calcR))/100 + 323*expLookup(i) + 118*(calcR*calcR*calcR)/1000) AND row = (279*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (842*calcR)/10*((10-calcR)*(10-calcR))/100 + 843*expLookup(i) + 281*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (118*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (406*calcR)/10*((10-calcR)*(10-calcR))/100 + 419*expLookup(i) + 130*(calcR*calcR*calcR)/1000) AND row = (281*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (843*calcR)/10*((10-calcR)*(10-calcR))/100 + 837*expLookup(i) + 276*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (130*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (376*calcR)/10*((10-calcR)*(10-calcR))/100 + 375*expLookup(i) + 125*(calcR*calcR*calcR)/1000) AND row = (276*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (825*calcR)/10*((10-calcR)*(10-calcR))/100 + 822*expLookup(i) + 256*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (125*(10-calcR)/10 + 125*calcR/10) AND row = (256*(10-calcR)/10 + 241*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (125*(10-calcR)/10 + 132*calcR/10) AND row = (241*(10-calcR)/10 + 227*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (132*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (423*calcR)/10*((10-calcR)*(10-calcR))/100 + 443*expLookup(i) + 150*(calcR*calcR*calcR)/1000) AND row = (227*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (631*calcR)/10*((10-calcR)*(10-calcR))/100 + 600*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (150*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (460*calcR)/10*((10-calcR)*(10-calcR))/100 + 460*expLookup(i) + 151*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (590*calcR)/10*((10-calcR)*(10-calcR))/100 + 583*expLookup(i) + 193*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (151*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (447*calcR)/10*((10-calcR)*(10-calcR))/100 + 404*expLookup(i) + 133*(calcR*calcR*calcR)/1000) AND row = (193*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (578*calcR)/10*((10-calcR)*(10-calcR))/100 + 578*expLookup(i) + 193*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (133*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (393*calcR)/10*((10-calcR)*(10-calcR))/100 + 399*expLookup(i) + 136*(calcR*calcR*calcR)/1000) AND row = (193*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (585*calcR)/10*((10-calcR)*(10-calcR))/100 + 592*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (136*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (414*calcR)/10*((10-calcR)*(10-calcR))/100 + 417*expLookup(i) + 139*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (596*calcR)/10*((10-calcR)*(10-calcR))/100 + 599*expLookup(i) + 201*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (139*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (417*calcR)/10*((10-calcR)*(10-calcR))/100 + 368*expLookup(i) + 122*(calcR*calcR*calcR)/1000) AND row = (201*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (612*calcR)/10*((10-calcR)*(10-calcR))/100 + 708*expLookup(i) + 235*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (122*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (362*calcR)/10*((10-calcR)*(10-calcR))/100 + 318*expLookup(i) + 106*(calcR*calcR*calcR)/1000) AND row = (235*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (702*calcR)/10*((10-calcR)*(10-calcR))/100 + 606*expLookup(i) + 200*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (106*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (318*calcR)/10*((10-calcR)*(10-calcR))/100 + 322*expLookup(i) + 109*(calcR*calcR*calcR)/1000) AND row = (200*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (599*calcR)/10*((10-calcR)*(10-calcR))/100 + 596*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (109*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (337*calcR)/10*((10-calcR)*(10-calcR))/100 + 345*expLookup(i) + 113*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (594*calcR)/10*((10-calcR)*(10-calcR))/100 + 587*expLookup(i) + 194*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (113*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (341*calcR)/10*((10-calcR)*(10-calcR))/100 + 321*expLookup(i) + 99*(calcR*calcR*calcR)/1000) AND row = (194*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (583*calcR)/10*((10-calcR)*(10-calcR))/100 + 581*expLookup(i) + 193*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (99*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (262*calcR)/10*((10-calcR)*(10-calcR))/100 + 258*expLookup(i) + 86*(calcR*calcR*calcR)/1000) AND row = (193*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (578*calcR)/10*((10-calcR)*(10-calcR))/100 + 578*expLookup(i) + 194*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (321*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (961*calcR)/10*((10-calcR)*(10-calcR))/100 + 965*expLookup(i) + 326*(calcR*calcR*calcR)/1000) AND row = (194*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (586*calcR)/10*((10-calcR)*(10-calcR))/100 + 589*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (326*(10-calcR)/10 + 329*calcR/10) AND row = (198*(10-calcR)/10 + 200*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (329*(10-calcR)/10 + 329*calcR/10) AND row = (200*(10-calcR)/10 + 234*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (329*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (989*calcR)/10*((10-calcR)*(10-calcR))/100 + 989*expLookup(i) + 329*(calcR*calcR*calcR)/1000) AND row = (234*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (758*calcR)/10*((10-calcR)*(10-calcR))/100 + 808*expLookup(i) + 271*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (329*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (985*calcR)/10*((10-calcR)*(10-calcR))/100 + 981*expLookup(i) + 324*(calcR*calcR*calcR)/1000) AND row = (271*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (820*calcR)/10*((10-calcR)*(10-calcR))/100 + 825*expLookup(i) + 276*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (324*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (967*calcR)/10*((10-calcR)*(10-calcR))/100 + 961*expLookup(i) + 320*(calcR*calcR*calcR)/1000) AND row = (276*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (832*calcR)/10*((10-calcR)*(10-calcR))/100 + 837*expLookup(i) + 279*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (320*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (958*calcR)/10*((10-calcR)*(10-calcR))/100 + 975*expLookup(i) + 345*(calcR*calcR*calcR)/1000) AND row = (279*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (845*calcR)/10*((10-calcR)*(10-calcR))/100 + 846*expLookup(i) + 281*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (345*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1108*calcR)/10*((10-calcR)*(10-calcR))/100 + 1115*expLookup(i) + 379*(calcR*calcR*calcR)/1000) AND row = (281*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (842*calcR)/10*((10-calcR)*(10-calcR))/100 + 840*expLookup(i) + 272*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (379*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1164*calcR)/10*((10-calcR)*(10-calcR))/100 + 1174*expLookup(i) + 390*(calcR*calcR*calcR)/1000) AND row = (272*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (787*calcR)/10*((10-calcR)*(10-calcR))/100 + 756*expLookup(i) + 236*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (390*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1170*calcR)/10*((10-calcR)*(10-calcR))/100 + 1162*expLookup(i) + 381*(calcR*calcR*calcR)/1000) AND row = (236*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (667*calcR)/10*((10-calcR)*(10-calcR))/100 + 642*expLookup(i) + 205*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (381*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1132*calcR)/10*((10-calcR)*(10-calcR))/100 + 1125*expLookup(i) + 370*(calcR*calcR*calcR)/1000) AND row = (205*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (603*calcR)/10*((10-calcR)*(10-calcR))/100 + 596*expLookup(i) + 196*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (370*(10-calcR)/10 + 364*calcR/10) AND row = (196*(10-calcR)/10 + 193*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (364*(10-calcR)/10 + 343*calcR/10) AND row = (193*(10-calcR)/10 + 193*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (343*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (984*calcR)/10*((10-calcR)*(10-calcR))/100 + 966*expLookup(i) + 321*(calcR*calcR*calcR)/1000) AND row = (193*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (579*calcR)/10*((10-calcR)*(10-calcR))/100 + 579*expLookup(i) + 194*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (361*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1100*calcR)/10*((10-calcR)*(10-calcR))/100 + 1116*expLookup(i) + 375*(calcR*calcR*calcR)/1000) AND row = (201*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (612*calcR)/10*((10-calcR)*(10-calcR))/100 + 629*expLookup(i) + 216*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (375*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1135*calcR)/10*((10-calcR)*(10-calcR))/100 + 1135*expLookup(i) + 378*(calcR*calcR*calcR)/1000) AND row = (216*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (670*calcR)/10*((10-calcR)*(10-calcR))/100 + 671*expLookup(i) + 238*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (378*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1135*calcR)/10*((10-calcR)*(10-calcR))/100 + 1132*expLookup(i) + 371*(calcR*calcR*calcR)/1000) AND row = (238*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (764*calcR)/10*((10-calcR)*(10-calcR))/100 + 776*expLookup(i) + 266*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (371*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1096*calcR)/10*((10-calcR)*(10-calcR))/100 + 1064*expLookup(i) + 347*(calcR*calcR*calcR)/1000) AND row = (266*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (822*calcR)/10*((10-calcR)*(10-calcR))/100 + 835*expLookup(i) + 276*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (347*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1023*calcR)/10*((10-calcR)*(10-calcR))/100 + 1022*expLookup(i) + 340*(calcR*calcR*calcR)/1000) AND row = (276*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (821*calcR)/10*((10-calcR)*(10-calcR))/100 + 816*expLookup(i) + 236*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (340*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1020*calcR)/10*((10-calcR)*(10-calcR))/100 + 1020*expLookup(i) + 340*(calcR*calcR*calcR)/1000) AND row = (236*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (646*calcR)/10*((10-calcR)*(10-calcR))/100 + 610*expLookup(i) + 201*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (340*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1024*calcR)/10*((10-calcR)*(10-calcR))/100 + 1028*expLookup(i) + 345*(calcR*calcR*calcR)/1000) AND row = (201*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (599*calcR)/10*((10-calcR)*(10-calcR))/100 + 597*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (345*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1048*calcR)/10*((10-calcR)*(10-calcR))/100 + 1073*expLookup(i) + 361*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (595*calcR)/10*((10-calcR)*(10-calcR))/100 + 599*expLookup(i) + 201*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (496*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1488*calcR)/10*((10-calcR)*(10-calcR))/100 + 1494*expLookup(i) + 501*(calcR*calcR*calcR)/1000) AND row = (193*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (584*calcR)/10*((10-calcR)*(10-calcR))/100 + 591*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (501*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1518*calcR)/10*((10-calcR)*(10-calcR))/100 + 1518*expLookup(i) + 505*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (603*calcR)/10*((10-calcR)*(10-calcR))/100 + 610*expLookup(i) + 239*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (505*(10-calcR)/10 + 504*calcR/10) AND row = (239*(10-calcR)/10 + 274*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (504*(10-calcR)/10 + 500*calcR/10) AND row = (274*(10-calcR)/10 + 276*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (500*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1475*calcR)/10*((10-calcR)*(10-calcR))/100 + 1480*expLookup(i) + 515*(calcR*calcR*calcR)/1000) AND row = (276*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (840*calcR)/10*((10-calcR)*(10-calcR))/100 + 841*expLookup(i) + 281*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (515*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1632*calcR)/10*((10-calcR)*(10-calcR))/100 + 1661*expLookup(i) + 561*(calcR*calcR*calcR)/1000) AND row = (281*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (848*calcR)/10*((10-calcR)*(10-calcR))/100 + 836*expLookup(i) + 262*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (561*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1711*calcR)/10*((10-calcR)*(10-calcR))/100 + 1703*expLookup(i) + 555*(calcR*calcR*calcR)/1000) AND row = (262*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (730*calcR)/10*((10-calcR)*(10-calcR))/100 + 654*expLookup(i) + 203*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (555*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1658*calcR)/10*((10-calcR)*(10-calcR))/100 + 1645*expLookup(i) + 545*(calcR*calcR*calcR)/1000) AND row = (203*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (603*calcR)/10*((10-calcR)*(10-calcR))/100 + 593*expLookup(i) + 196*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (545*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1619*calcR)/10*((10-calcR)*(10-calcR))/100 + 1617*expLookup(i) + 518*(calcR*calcR*calcR)/1000) AND row = (196*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (581*calcR)/10*((10-calcR)*(10-calcR))/100 + 580*expLookup(i) + 193*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (518*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1515*calcR)/10*((10-calcR)*(10-calcR))/100 + 1491*expLookup(i) + 496*(calcR*calcR*calcR)/1000) AND row = (193*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (578*calcR)/10*((10-calcR)*(10-calcR))/100 + 579*expLookup(i) + 193*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (535*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1628*calcR)/10*((10-calcR)*(10-calcR))/100 + 1640*expLookup(i) + 549*(calcR*calcR*calcR)/1000) AND row = (201*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (614*calcR)/10*((10-calcR)*(10-calcR))/100 + 626*expLookup(i) + 215*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (549*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1673*calcR)/10*((10-calcR)*(10-calcR))/100 + 1667*expLookup(i) + 545*(calcR*calcR*calcR)/1000) AND row = (215*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (694*calcR)/10*((10-calcR)*(10-calcR))/100 + 770*expLookup(i) + 267*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (545*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1614*calcR)/10*((10-calcR)*(10-calcR))/100 + 1585*expLookup(i) + 522*(calcR*calcR*calcR)/1000) AND row = (267*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (827*calcR)/10*((10-calcR)*(10-calcR))/100 + 836*expLookup(i) + 275*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (522*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1549*calcR)/10*((10-calcR)*(10-calcR))/100 + 1548*expLookup(i) + 516*(calcR*calcR*calcR)/1000) AND row = (275*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (816*calcR)/10*((10-calcR)*(10-calcR))/100 + 810*expLookup(i) + 233*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (516*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1548*calcR)/10*((10-calcR)*(10-calcR))/100 + 1548*expLookup(i) + 517*(calcR*calcR*calcR)/1000) AND row = (233*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (618*calcR)/10*((10-calcR)*(10-calcR))/100 + 602*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (517*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1558*calcR)/10*((10-calcR)*(10-calcR))/100 + 1590*expLookup(i) + 535*(calcR*calcR*calcR)/1000) AND row = (199*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (592*calcR)/10*((10-calcR)*(10-calcR))/100 + 595*expLookup(i) + 201*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (173*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (505*calcR)/10*((10-calcR)*(10-calcR))/100 + 483*expLookup(i) + 158*(calcR*calcR*calcR)/1000) AND row = (196*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (595*calcR)/10*((10-calcR)*(10-calcR))/100 + 618*expLookup(i) + 211*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (158*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (461*calcR)/10*((10-calcR)*(10-calcR))/100 + 458*expLookup(i) + 153*(calcR*calcR*calcR)/1000) AND row = (211*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (660*calcR)/10*((10-calcR)*(10-calcR))/100 + 679*expLookup(i) + 240*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (153*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (460*calcR)/10*((10-calcR)*(10-calcR))/100 + 460*expLookup(i) + 156*(calcR*calcR*calcR)/1000) AND row = (240*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (761*calcR)/10*((10-calcR)*(10-calcR))/100 + 764*expLookup(i) + 261*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (156*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (482*calcR)/10*((10-calcR)*(10-calcR))/100 + 500*expLookup(i) + 172*(calcR*calcR*calcR)/1000) AND row = (261*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (808*calcR)/10*((10-calcR)*(10-calcR))/100 + 828*expLookup(i) + 279*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (172*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (529*calcR)/10*((10-calcR)*(10-calcR))/100 + 537*expLookup(i) + 186*(calcR*calcR*calcR)/1000) AND row = (279*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (843*calcR)/10*((10-calcR)*(10-calcR))/100 + 844*expLookup(i) + 281*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (186*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (590*calcR)/10*((10-calcR)*(10-calcR))/100 + 598*expLookup(i) + 205*(calcR*calcR*calcR)/1000) AND row = (281*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (843*calcR)/10*((10-calcR)*(10-calcR))/100 + 841*expLookup(i) + 275*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (205*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (635*calcR)/10*((10-calcR)*(10-calcR))/100 + 651*expLookup(i) + 219*(calcR*calcR*calcR)/1000) AND row = (275*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (810*calcR)/10*((10-calcR)*(10-calcR))/100 + 785*expLookup(i) + 254*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (219*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (664*calcR)/10*((10-calcR)*(10-calcR))/100 + 664*expLookup(i) + 219*(calcR*calcR*calcR)/1000) AND row = (254*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (737*calcR)/10*((10-calcR)*(10-calcR))/100 + 689*expLookup(i) + 221*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (219*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (648*calcR)/10*((10-calcR)*(10-calcR))/100 + 623*expLookup(i) + 198*(calcR*calcR*calcR)/1000) AND row = (221*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (627*calcR)/10*((10-calcR)*(10-calcR))/100 + 596*expLookup(i) + 195*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (574*calcR)/10*((10-calcR)*(10-calcR))/100 + 535*expLookup(i) + 173*(calcR*calcR*calcR)/1000) AND row = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (579*calcR)/10*((10-calcR)*(10-calcR))/100 + 580*expLookup(i) + 196*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (600*calcR)/10*((10-calcR)*(10-calcR))/100 + 612*expLookup(i) + 207*(calcR*calcR*calcR)/1000) AND row = (200*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (606*calcR)/10*((10-calcR)*(10-calcR))/100 + 621*expLookup(i) + 215*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (207*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (626*calcR)/10*((10-calcR)*(10-calcR))/100 + 628*expLookup(i) + 209*(calcR*calcR*calcR)/1000) AND row = (215*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (657*calcR)/10*((10-calcR)*(10-calcR))/100 + 670*expLookup(i) + 233*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (209*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (632*calcR)/10*((10-calcR)*(10-calcR))/100 + 615*expLookup(i) + 192*(calcR*calcR*calcR)/1000) AND row = (233*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (777*calcR)/10*((10-calcR)*(10-calcR))/100 + 818*expLookup(i) + 276*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (192*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (545*calcR)/10*((10-calcR)*(10-calcR))/100 + 522*expLookup(i) + 168*(calcR*calcR*calcR)/1000) AND row = (276*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (839*calcR)/10*((10-calcR)*(10-calcR))/100 + 826*expLookup(i) + 263*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (168*(10-calcR)/10 + 164*calcR/10) AND row = (263*(10-calcR)/10 + 255*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (164*(10-calcR)/10 + 164*calcR/10) AND row = (255*(10-calcR)/10 + 239*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (164*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (491*calcR)/10*((10-calcR)*(10-calcR))/100 + 494*expLookup(i) + 170*(calcR*calcR*calcR)/1000) AND row = (239*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (662*calcR)/10*((10-calcR)*(10-calcR))/100 + 646*expLookup(i) + 207*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (170*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (528*calcR)/10*((10-calcR)*(10-calcR))/100 + 559*expLookup(i) + 195*(calcR*calcR*calcR)/1000) AND row = (207*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (595*calcR)/10*((10-calcR)*(10-calcR))/100 + 586*expLookup(i) + 200*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (227*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (679*calcR)/10*((10-calcR)*(10-calcR))/100 + 683*expLookup(i) + 230*(calcR*calcR*calcR)/1000) AND row = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (592*calcR)/10*((10-calcR)*(10-calcR))/100 + 597*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (230*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (694*calcR)/10*((10-calcR)*(10-calcR))/100 + 699*expLookup(i) + 233*(calcR*calcR*calcR)/1000) AND row = (199*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (597*calcR)/10*((10-calcR)*(10-calcR))/100 + 598*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (233*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (703*calcR)/10*((10-calcR)*(10-calcR))/100 + 705*expLookup(i) + 235*(calcR*calcR*calcR)/1000) AND row = (199*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (600*calcR)/10*((10-calcR)*(10-calcR))/100 + 630*expLookup(i) + 234*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (235*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (710*calcR)/10*((10-calcR)*(10-calcR))/100 + 709*expLookup(i) + 243*(calcR*calcR*calcR)/1000) AND row = (234*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (813*calcR)/10*((10-calcR)*(10-calcR))/100 + 812*expLookup(i) + 277*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (243*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (764*calcR)/10*((10-calcR)*(10-calcR))/100 + 822*expLookup(i) + 280*(calcR*calcR*calcR)/1000) AND row = (277*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (861*calcR)/10*((10-calcR)*(10-calcR))/100 + 852*expLookup(i) + 271*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (280*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (851*calcR)/10*((10-calcR)*(10-calcR))/100 + 852*expLookup(i) + 284*(calcR*calcR*calcR)/1000) AND row = (271*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (796*calcR)/10*((10-calcR)*(10-calcR))/100 + 785*expLookup(i) + 231*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (284*(10-calcR)/10 + 285*calcR/10) AND row = (231*(10-calcR)/10 + 199*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (285*(10-calcR)/10 + 288*calcR/10) AND row = (199*(10-calcR)/10 + 198*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (288*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (873*calcR)/10*((10-calcR)*(10-calcR))/100 + 876*expLookup(i) + 292*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (594*calcR)/10*((10-calcR)*(10-calcR))/100 + 592*expLookup(i) + 196*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (292*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (876*calcR)/10*((10-calcR)*(10-calcR))/100 + 873*expLookup(i) + 282*(calcR*calcR*calcR)/1000) AND row = (196*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (582*calcR)/10*((10-calcR)*(10-calcR))/100 + 582*expLookup(i) + 194*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (282*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (830*calcR)/10*((10-calcR)*(10-calcR))/100 + 816*expLookup(i) + 271*(calcR*calcR*calcR)/1000) AND row = (194*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (582*calcR)/10*((10-calcR)*(10-calcR))/100 + 582*expLookup(i) + 194*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (271*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (813*calcR)/10*((10-calcR)*(10-calcR))/100 + 818*expLookup(i) + 275*(calcR*calcR*calcR)/1000) AND row = (194*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (585*calcR)/10*((10-calcR)*(10-calcR))/100 + 591*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (275*(10-calcR)/10 + 279*calcR/10) AND row = (199*(10-calcR)/10 + 202*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (279*(10-calcR)/10 + 279*calcR/10) AND row = (202*(10-calcR)/10 + 231*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (279*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (840*calcR)/10*((10-calcR)*(10-calcR))/100 + 838*expLookup(i) + 274*(calcR*calcR*calcR)/1000) AND row = (231*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (789*calcR)/10*((10-calcR)*(10-calcR))/100 + 803*expLookup(i) + 272*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (274*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (796*calcR)/10*((10-calcR)*(10-calcR))/100 + 757*expLookup(i) + 247*(calcR*calcR*calcR)/1000) AND row = (272*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (838*calcR)/10*((10-calcR)*(10-calcR))/100 + 833*expLookup(i) + 268*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (247*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (737*calcR)/10*((10-calcR)*(10-calcR))/100 + 736*expLookup(i) + 245*(calcR*calcR*calcR)/1000) AND row = (268*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (794*calcR)/10*((10-calcR)*(10-calcR))/100 + 789*expLookup(i) + 236*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (245*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (735*calcR)/10*((10-calcR)*(10-calcR))/100 + 735*expLookup(i) + 245*(calcR*calcR*calcR)/1000) AND row = (236*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (660*calcR)/10*((10-calcR)*(10-calcR))/100 + 617*expLookup(i) + 203*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (245*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (739*calcR)/10*((10-calcR)*(10-calcR))/100 + 741*expLookup(i) + 249*(calcR*calcR*calcR)/1000) AND row = (203*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (601*calcR)/10*((10-calcR)*(10-calcR))/100 + 598*expLookup(i) + 198*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (249*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (766*calcR)/10*((10-calcR)*(10-calcR))/100 + 764*expLookup(i) + 247*(calcR*calcR*calcR)/1000) AND row = (198*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (590*calcR)/10*((10-calcR)*(10-calcR))/100 + 586*expLookup(i) + 194*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (247*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (710*calcR)/10*((10-calcR)*(10-calcR))/100 + 684*expLookup(i) + 227*(calcR*calcR*calcR)/1000) AND row = (194*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (580*calcR)/10*((10-calcR)*(10-calcR))/100 + 581*expLookup(i) + 195*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (397*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1189*calcR)/10*((10-calcR)*(10-calcR))/100 + 1194*expLookup(i) + 402*(calcR*calcR*calcR)/1000) AND row = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (590*calcR)/10*((10-calcR)*(10-calcR))/100 + 594*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (402*(10-calcR)/10 + 406*calcR/10) AND row = (199*(10-calcR)/10 + 200*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (406*(10-calcR)/10 + 407*calcR/10) AND row = (200*(10-calcR)/10 + 213*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (407*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1224*calcR)/10*((10-calcR)*(10-calcR))/100 + 1223*expLookup(i) + 406*(calcR*calcR*calcR)/1000) AND row = (213*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (684*calcR)/10*((10-calcR)*(10-calcR))/100 + 820*expLookup(i) + 274*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (406*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1219*calcR)/10*((10-calcR)*(10-calcR))/100 + 1212*expLookup(i) + 401*(calcR*calcR*calcR)/1000) AND row = (274*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (824*calcR)/10*((10-calcR)*(10-calcR))/100 + 826*expLookup(i) + 275*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (401*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1196*calcR)/10*((10-calcR)*(10-calcR))/100 + 1192*expLookup(i) + 397*(calcR*calcR*calcR)/1000) AND row = (275*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (828*calcR)/10*((10-calcR)*(10-calcR))/100 + 831*expLookup(i) + 278*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (397*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1190*calcR)/10*((10-calcR)*(10-calcR))/100 + 1194*expLookup(i) + 412*(calcR*calcR*calcR)/1000) AND row = (278*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (839*calcR)/10*((10-calcR)*(10-calcR))/100 + 840*expLookup(i) + 280*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (412*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1277*calcR)/10*((10-calcR)*(10-calcR))/100 + 1281*expLookup(i) + 426*(calcR*calcR*calcR)/1000) AND row = (280*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (840*calcR)/10*((10-calcR)*(10-calcR))/100 + 839*expLookup(i) + 278*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (426*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1279*calcR)/10*((10-calcR)*(10-calcR))/100 + 1275*expLookup(i) + 423*(calcR*calcR*calcR)/1000) AND row = (278*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (832*calcR)/10*((10-calcR)*(10-calcR))/100 + 828*expLookup(i) + 275*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (423*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1263*calcR)/10*((10-calcR)*(10-calcR))/100 + 1258*expLookup(i) + 419*(calcR*calcR*calcR)/1000) AND row = (275*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (825*calcR)/10*((10-calcR)*(10-calcR))/100 + 822*expLookup(i) + 272*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (419*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1255*calcR)/10*((10-calcR)*(10-calcR))/100 + 1254*expLookup(i) + 418*(calcR*calcR*calcR)/1000) AND row = (272*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (813*calcR)/10*((10-calcR)*(10-calcR))/100 + 764*expLookup(i) + 235*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (418*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1254*calcR)/10*((10-calcR)*(10-calcR))/100 + 1253*expLookup(i) + 422*(calcR*calcR*calcR)/1000) AND row = (235*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (593*calcR)/10*((10-calcR)*(10-calcR))/100 + 597*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (422*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1271*calcR)/10*((10-calcR)*(10-calcR))/100 + 1276*expLookup(i) + 426*(calcR*calcR*calcR)/1000) AND row = (199*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (597*calcR)/10*((10-calcR)*(10-calcR))/100 + 594*expLookup(i) + 197*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (426*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1284*calcR)/10*((10-calcR)*(10-calcR))/100 + 1284*expLookup(i) + 427*(calcR*calcR*calcR)/1000) AND row = (197*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (588*calcR)/10*((10-calcR)*(10-calcR))/100 + 587*expLookup(i) + 195*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (427*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1275*calcR)/10*((10-calcR)*(10-calcR))/100 + 1194*expLookup(i) + 397*(calcR*calcR*calcR)/1000) AND row = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (581*calcR)/10*((10-calcR)*(10-calcR))/100 + 581*expLookup(i) + 195*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (432*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1296*calcR)/10*((10-calcR)*(10-calcR))/100 + 1302*expLookup(i) + 437*(calcR*calcR*calcR)/1000) AND row = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (591*calcR)/10*((10-calcR)*(10-calcR))/100 + 595*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (437*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1323*calcR)/10*((10-calcR)*(10-calcR))/100 + 1323*expLookup(i) + 440*(calcR*calcR*calcR)/1000) AND row = (199*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (600*calcR)/10*((10-calcR)*(10-calcR))/100 + 607*expLookup(i) + 242*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (440*(10-calcR)/10 + 440*calcR/10) AND row = (242*(10-calcR)/10 + 276*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (440*(10-calcR)/10 + 436*calcR/10) AND row = (276*(10-calcR)/10 + 277*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (436*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1301*calcR)/10*((10-calcR)*(10-calcR))/100 + 1297*expLookup(i) + 432*(calcR*calcR*calcR)/1000) AND row = (277*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (831*calcR)/10*((10-calcR)*(10-calcR))/100 + 834*expLookup(i) + 279*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (432*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1297*calcR)/10*((10-calcR)*(10-calcR))/100 + 1309*expLookup(i) + 461*(calcR*calcR*calcR)/1000) AND row = (279*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (840*calcR)/10*((10-calcR)*(10-calcR))/100 + 841*expLookup(i) + 280*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (461*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1461*calcR)/10*((10-calcR)*(10-calcR))/100 + 1469*expLookup(i) + 490*(calcR*calcR*calcR)/1000) AND row = (280*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (843*calcR)/10*((10-calcR)*(10-calcR))/100 + 842*expLookup(i) + 279*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (490*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1475*calcR)/10*((10-calcR)*(10-calcR))/100 + 1479*expLookup(i) + 492*(calcR*calcR*calcR)/1000) AND row = (279*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (828*calcR)/10*((10-calcR)*(10-calcR))/100 + 775*expLookup(i) + 257*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (492*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1474*calcR)/10*((10-calcR)*(10-calcR))/100 + 1468*expLookup(i) + 486*(calcR*calcR*calcR)/1000) AND row = (257*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (771*calcR)/10*((10-calcR)*(10-calcR))/100 + 779*expLookup(i) + 265*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (486*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1449*calcR)/10*((10-calcR)*(10-calcR))/100 + 1446*expLookup(i) + 479*(calcR*calcR*calcR)/1000) AND row = (265*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (819*calcR)/10*((10-calcR)*(10-calcR))/100 + 822*expLookup(i) + 275*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (479*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1427*calcR)/10*((10-calcR)*(10-calcR))/100 + 1398*expLookup(i) + 455*(calcR*calcR*calcR)/1000) AND row = (275*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (827*calcR)/10*((10-calcR)*(10-calcR))/100 + 828*expLookup(i) + 276*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (455*(10-calcR)/10 + 452*calcR/10) AND row = (276*(10-calcR)/10 + 276*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(10-calcR)/10 + 452*calcR/10) AND row = (276*(10-calcR)/10 + 237*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(10-calcR)/10 + 462*calcR/10) AND row = (237*(10-calcR)/10 + 238*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (462*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1403*calcR)/10*((10-calcR)*(10-calcR))/100 + 1419*expLookup(i) + 473*(calcR*calcR*calcR)/1000) AND row = (238*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (716*calcR)/10*((10-calcR)*(10-calcR))/100 + 719*expLookup(i) + 240*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (473*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1422*calcR)/10*((10-calcR)*(10-calcR))/100 + 1425*expLookup(i) + 475*(calcR*calcR*calcR)/1000) AND row = (240*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (722*calcR)/10*((10-calcR)*(10-calcR))/100 + 727*expLookup(i) + 244*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (475*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1427*calcR)/10*((10-calcR)*(10-calcR))/100 + 1430*expLookup(i) + 477*(calcR*calcR*calcR)/1000) AND row = (244*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (738*calcR)/10*((10-calcR)*(10-calcR))/100 + 747*expLookup(i) + 251*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (477*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1437*calcR)/10*((10-calcR)*(10-calcR))/100 + 1437*expLookup(i) + 480*(calcR*calcR*calcR)/1000) AND row = (251*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (762*calcR)/10*((10-calcR)*(10-calcR))/100 + 762*expLookup(i) + 252*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (480*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1443*calcR)/10*((10-calcR)*(10-calcR))/100 + 1443*expLookup(i) + 480*(calcR*calcR*calcR)/1000) AND row = (252*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (750*calcR)/10*((10-calcR)*(10-calcR))/100 + 696*expLookup(i) + 225*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (480*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1438*calcR)/10*((10-calcR)*(10-calcR))/100 + 1428*expLookup(i) + 476*(calcR*calcR*calcR)/1000) AND row = (225*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (656*calcR)/10*((10-calcR)*(10-calcR))/100 + 659*expLookup(i) + 226*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (476*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1428*calcR)/10*((10-calcR)*(10-calcR))/100 + 1422*expLookup(i) + 462*(calcR*calcR*calcR)/1000) AND row = (226*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (697*calcR)/10*((10-calcR)*(10-calcR))/100 + 700*expLookup(i) + 233*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (462*(10-calcR)/10 + 452*calcR/10) AND row = (233*(10-calcR)/10 + 234*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*(10-calcR)/10 + 452*calcR/10) AND row = (234*(10-calcR)/10 + 217*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1356*calcR)/10*((10-calcR)*(10-calcR))/100 + 1356*expLookup(i) + 452*(calcR*calcR*calcR)/1000) AND row = (217*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (625*calcR)/10*((10-calcR)*(10-calcR))/100 + 602*expLookup(i) + 200*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (452*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1359*calcR)/10*((10-calcR)*(10-calcR))/100 + 1376*expLookup(i) + 465*(calcR*calcR*calcR)/1000) AND row = (200*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (601*calcR)/10*((10-calcR)*(10-calcR))/100 + 599*expLookup(i) + 199*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (465*(10-calcR)/10 + 477*calcR/10) AND row = (199*(10-calcR)/10 + 198*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (477*(10-calcR)/10 + 481*calcR/10) AND row = (198*(10-calcR)/10 + 206*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (481*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1448*calcR)/10*((10-calcR)*(10-calcR))/100 + 1453*expLookup(i) + 485*(calcR*calcR*calcR)/1000) AND row = (206*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (634*calcR)/10*((10-calcR)*(10-calcR))/100 + 645*expLookup(i) + 215*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (485*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1458*calcR)/10*((10-calcR)*(10-calcR))/100 + 1461*expLookup(i) + 487*(calcR*calcR*calcR)/1000) AND row = (215*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (645*calcR)/10*((10-calcR)*(10-calcR))/100 + 628*expLookup(i) + 201*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (487*(10-calcR)/10 + 487*calcR/10) AND row = (201*(10-calcR)/10 + 195*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (487*(10-calcR)/10 + 459*calcR/10) AND row = (195*(10-calcR)/10 + 195*calcR/10)) THEN
				colorconcat <= "011000100000";
			ELSIF (column = (459*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (1333*calcR)/10*((10-calcR)*(10-calcR))/100 + 1296*expLookup(i) + 432*(calcR*calcR*calcR)/1000) AND row = (195*((10-calcR)*(10-calcR)*(10-calcR))/1000 + (585*calcR)/10*((10-calcR)*(10-calcR))/100 + 586*expLookup(i) + 195*(calcR*calcR*calcR)/1000)) THEN
				colorconcat <= "011000100000";
			END IF;
		END LOOP;

------DRAWS ENEMY EXPLOSIONS ON THE SCREEN---------------------------------------------------
		FOR i in 0 to 11 LOOP
			IF (aliens(i).expClk > 0 AND ( ( ((column - aliens(i).deathX) ** 2) + ((row - aliens(i).deathY) ** 2) ) <= ((aliens(i).size * 6) ** 2) )) THEN
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