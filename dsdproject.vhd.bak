--------------------------------------------------------------------------------
--
--   FileName:         hw_image_generator.vhd
--   Dependencies:     none
--   Design Software:  Quartus II 64-bit Version 12.1 Build 177 SJ Full Version
--
--   HDL CODE IS PROVIDED "AS IS."  DIGI-KEY EXPRESSLY DISCLAIMS ANY
--   WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
--   PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL DIGI-KEY
--   BE LIABLE FOR ANY INCIDENTAL, SPECIAL, INDIRECT OR CONSEQUENTIAL
--   DAMAGES, LOST PROFITS OR LOST DATA, HARM TO YOUR EQUIPMENT, COST OF
--   PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR SERVICES, ANY CLAIMS
--   BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY DEFENSE THEREOF),
--   ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER SIMILAR COSTS.
--
--   Version History
--   Version 1.0 05/10/2013 Scott Larson
--     Initial Public Release
--    
--------------------------------------------------------------------------------
--
-- Altered 10/13/19 - Tyler McCormick 
-- Test pattern is now 8 equally spaced 
-- different color vertical bars, from black (left) to white (right)

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

package custom_types is
	type alien_t is record
		alive : STD_LOGIC;
		size : INTEGER;
		color : STD_LOGIC_VECTOR(11 downto 0);
		x : INTEGER;
		y : INTEGER;
		collision : STD_LOGIC := '0';
	end record alien_t;
	
	type ship_t is record
		alive : STD_LOGIC := '1';
		x : INTEGER;
		y : INTEGER;
		collision : STD_LOGIC := '0';
	end record ship_t;
	
	type alien_array is array (integer range <>) of alien_t;
	
	type int_array is array (integer range <>) of integer;
	
end package;

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;
USE work.custom_types.all;

ENTITY dsdproject IS
  GENERIC(
		
		y_max : INTEGER := 67;
		y_min : INTEGER := 413;
		x_max : INTEGER := 365;
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
	 clkfucka, clkfuckb : OUT STD_LOGIC;
	 
	 reset_RNG : IN STD_LOGIC
	 
	 );
END entity;

ARCHITECTURE behavior OF dsdproject IS
	signal colorconcat : STD_LOGIC_VECTOR(11 downto 0);
	signal ship : ship_t := (x => x_min, y => (240 + ship_height/2));
	-- signal ship_x : INTEGER := x_min;
	-- signal ship_y : INTEGER := (240 + ship_height/2);
	
	signal spare_ships : INTEGER := 3;
	signal score : INTEGER := 0;
	signal alien : alien_array(11 downto 0);
	
	signal clock_x, clock_y : STD_LOGIC := '0';
	signal data_x_magnitude, data_y_magnitude : std_logic_vector(7 downto 0);
	signal countX : integer := 1;
	signal countY : integer := 1;
	signal data_x, data_y, data_z : STD_LOGIC_VECTOR(15 downto 0);	
	
	signal RNG : std_logic_vector(9 downto 0);

	
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
	clkfucka <= clock_x;
	clkfuckb <= clock_y;

	U0 : ADXL345_controller port map('1', max10_clk, open, data_x, data_y, data_z, GSENSOR_SDI, GSENSOR_SDO, GSENSOR_CS_N, GSENSOR_SCLK);
	U1 : RNG10 port map(reset_RNG, '0', max10_clk, RNG);

	PROCESS(disp_ena, row, column)
		variable calcA : INTEGER;
		variable calcB : INTEGER;
		variable calcC : INTEGER;
		
	BEGIN

    IF(disp_ena = '1') THEN        --display time
	 
------DRAWS THE HORIZONTAL BARS THAT DEFINE PLAY REGION---------------------------------------------------------------------------------
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
		
------DRAWS THE REMAINING LIVES ON THE SCREEN-------------------------------------------------
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

------DRAWS THE ENEMIES ON THE SCREEN----------------------------------------------------------
		FOR i in 0 to 11 LOOP
		IF (alien(i).alive = true) THEN
			calcA := column - alien(i).x_pos;	--Relative X position
			calcB := alien(i).y_pos - row;		--Relative Y position
			calcC := alien(i).size * 8;			--Calc adjusted size
			
			IF ((calcB <= calcC AND calcB >= 0) AND (calcA <= calcC AND calcA >= 0)) THEN
				IF ((calcB = calcC OR calcB = 0) OR (calcA = calcC OR calcA = 0)) THEN
					colorconcat <= "000000000000";
				ELSE
					colorconcat <= alien(i).color;
				END IF;
			END IF;
		END IF;
		END LOOP;

------DRAWS THE SCOREBOARD ON THE SCREEN----------------------------------------------------------------------------------------------------

		
		red <= "0000" & colorconcat(11 downto 8);
		green <= "0000" & colorconcat(7 downto 4);
		blue <= "0000" & colorconcat(3 downto 0);
		
    ELSE                           --blanking time
      red <= (OTHERS => '0');
      green <= (OTHERS => '0');
      blue <= (OTHERS => '0');
    END IF;
  
  END PROCESS;
 
 ------Clock for X Axis Movement-----------------------------------------------------------------------------------------------------------

  xAxisClock : process ( max10_clk )	
	variable clockDivX : natural := 255;
	begin
		if(rising_edge(max10_clk)) then
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

------Clock for Y Axis Movement-----------------------------------------------------------------------------------------------------------
	
	yAxisClock : process ( max10_clk )	
	variable clockDivY : natural := 255;
	begin
		if(rising_edge(max10_clk)) then
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
				if (countY > ( 10000 * clockDivY ) ) then
					clock_y <= NOT clock_y;
					countY <= 1;
				end if;
			end if;	
		end if;	
	end process;

------X Axis Movement----------------------------------------------------------------------------------------------------------------
	
	xLocationAdjust : process (clock_x)
	begin
		if(reset_accel = '0') then
			--redAdjust <= "0000";
			ship_x <= ship_x;
		else
			if(rising_edge(clock_x)) then
				if(data_x(11) = '1') then		--RIGHT
					if (ship_x = x_max) then
						ship_x <= x_max;
					else
						ship_x <= ship_x+1;
					end if;
				else 									--LEFT
					if (ship_x = x_min) then
						ship_x <= x_min;
					else	
						ship_x <= ship_x-1;
					end if;
				end if;
			end if;
		end if;
	end process;

------Y Axis Movement------------------------------------------------------------------------------------------------------------------
	
	yLocationAdjust : process (clock_y)
	begin
		if(reset_accel = '0') then
			--greenadjust <= "0000";
			ship_y <= ship_y;
		else
			if(rising_edge(clock_y)) then
				if(data_y(11) = '1') then --forward/up
					if (ship_y = y_max + ship_height) then
						ship_y <= y_max + ship_height;
					else
						ship_y <= ship_y-1;
					end if;
				else 							--backward/down
					if (ship_y = y_min) then
						ship_y <= y_min;
					else
						ship_y <= ship_y+1;
					end if;
				end if;
			end if;
		end if;
	end process;
  
  
END architecture;
