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

package integer_array is
	type int_array is array (integer range <>) of integer;
end package;

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;
USE work.integer_array.all;

ENTITY hw9p1 IS
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
    blue     :  OUT  STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0')); --blue magnitude output to DAC
END entity;

ARCHITECTURE behavior OF hw9p1 IS
	signal colorconcat : STD_LOGIC_VECTOR(11 downto 0);
	signal ship_x : INTEGER := x_min;
	signal ship_y : INTEGER := (240 + ship_height/2);
	signal calc_y : INTEGER;
	signal calc_x : INTEGER;
	signal calc_gen : INTEGER;
	
	signal spare_ships : INTEGER := 3;
	signal score : INTEGER := 0;
	signal score_bits : STD_LOGIC_VECTOR(55 downto 0) := (OTHERS => '0');

BEGIN

	PROCESS(disp_ena, row, column)
	BEGIN

    IF(disp_ena = '1') THEN        --display time
	 
------DRAWS THE HORIZONTAL BARS THAT DEFINE PLAY REGION---------------------------------------------------------------------------------
		IF( ((row < y_max) AND (row > (y_max - bar_thickness))) OR ((row > y_min) AND (row < (y_min + bar_thickness)))  ) THEN
			colorconcat <= "000000000000";
		ELSE
			colorconcat <= "111111111111";
		END IF;
		
------DRAWS THE PLAYER SHIP ON THE SCREEN--------------------------------------------------------------------------------------------------
		IF ( ((ship_y - row) <= (ship_height - (((column-ship_x)*ship_height)/ship_length))) AND ((column-ship_x) <= ship_length) AND ((ship_y - row) > 0) AND ((column - ship_x) > 0) ) THEN
			IF ( ((ship_y - row) = (ship_height - (((column-ship_x)*ship_height)/ship_length))) OR ((column - ship_x) = 1) OR ((ship_y - row) = 1) OR ((column - ship_x) = ship_length) ) THEN
				colorconcat <= "000000000000";
			ELSE
				colorconcat <= "111100000000";
			END IF;
		END IF;
		
------DRAWS THE REMAINING LIVES ON THE SCREEN---------------------------------------------------------------------------------------------
		FOR i in 0 to 2 LOOP
			IF (spare_ships > i) THEN
				IF ( ((ss_y - row) <= (ship_height - (((column-ss_x(i))*ship_height)/ship_length))) AND ((column-ss_x(i)) <= ship_length) AND ((ss_y - row) > 0) AND ((column - ss_x(i)) > 0) ) THEN
					IF ( ((ss_y - row) = (ship_height - (((column-ss_x(i))*ship_height)/ship_length))) OR ((column - ss_x(i)) = 1) OR ((ss_y - row) = 1) OR ((column - ss_x(i)) = ship_length) ) THEN
						colorconcat <= "000000000000";
					ELSE
						colorconcat <= "111100000000";
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
END architecture;
