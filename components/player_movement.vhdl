LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY controller IS
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
        --RST   : IN STD_LOGIC
    );
END ENTITY;

ARCHITECTURE ctrl_arch OF controller is
    SIGNAL data_x_magnitude : STD_LOGIC_VECTOR(7 downto 0);
    SIGNAL data_y_magnitude : STD_LOGIC_VECTOR(7 downto 0);
    SIGNAL clock_x : STD_LOGIC;
    SIGNAL clock_y : STD_LOGIC;
    SIGNAL x : INTEGER range 0 to 640 := x_start;
    SIGNAL y : INTEGER range 0 to 480 := y_start;
    SIGNAL right : STD_LOGIC := '1';

BEGIN

    Xpos <= x;
    Ypos <= y;
    R <= right;

------Clock for X Axis Movement--------------------------------------------------------------
    xAxisClock : process ( CLK )	
    variable clockDivX : natural := 255;
    VARIABLE countX : INTEGER range 0 to 256000 := 0;
    begin
        if(rising_edge(CLK)) then
            for i in 0 to 7 loop
                data_x_magnitude(i) <= dataX(i);
            end loop;
            if(data_x_magnitude(7 downto 4) = "0000" or data_x_magnitude(7 downto 4) = "1111") then
                clock_x <= clock_x;
            else
                if(dataX(11) = '0') then -- tilt left starts with 000
                    clockDivX := 255 - to_integer(unsigned(data_x_magnitude));
                else -- tilt right starts at FF
                    clockDivX := to_integer(unsigned(data_x_magnitude));
                end if;
                
                if (clockDivX = 0) then
                    clockDivX := 255;
                else
                    clockDivX := clockDivX;
                end if;
                
                countX := countX + 1;
                if (countX > ( 1000 * clockDivX ) ) then
                    clock_x <= NOT clock_x;
                    countX := 1;
                end if;
            end if;
        end if;	
    end process;

------Clock for Y Axis Movement--------------------------------------------------------------
    yAxisClock : process ( CLK )	
    variable clockDivY : natural := 255;
    VARIABLE countY : INTEGER range 0 to 512000 := 0;
    begin
        if(rising_edge( CLK )) then
            for i in 0 to 7 loop
                data_y_magnitude(i) <= dataY(i);
            end loop;
            if (data_y_magnitude(7 downto 4) = "0000" or data_y_magnitude(7 downto 4) = "1111") then
                clock_y <= clock_y;
            else
                if(dataY(11) = '0') then 
                    clockDivY := 255 - to_integer(unsigned(data_y_magnitude));
                else 
                    clockDivY := to_integer(unsigned(data_y_magnitude));
                end if;
                
                if (clockDivY = 0) then
                    clockDivY := 255;
                else
                    clockDivY := clockDivY;
                end if;
                
                countY := countY + 1;
                if (countY > ( 2000 * clockDivY ) ) then
                    clock_y <= NOT clock_y;
                    countY := 1;
                end if;
            end if;	
        end if;	
    end process;

------X Axis Movement------------------------------------------------------------------------
    xLocationAdjust : process (clock_x)
    begin
        if(rising_edge(clock_x)) then
            if(dataX(11) = '1') then		--RIGHT
                if (x = x_max - ship_length) then
                    x <= x_max - ship_length;
                    Exha <= 0;
                else
                    x <= x + 1;
                    Exha <= to_integer(255 - unsigned(dataX(7 downto 0))) / 48;
                end if;
            else 									--LEFT
                if (x = x_min) then
                    x <= x_min;
                    Exha <= 0;
                else	
                    x <= x-1;
                    Exha <= to_integer(unsigned(dataX(7 downto 0))) / 48;
                end if;
            end if;
            right <= dataX(11);
        end if;
    end process;

------Y Axis Movement------------------------------------------------------------------------
    yLocationAdjust : process (clock_y)
    begin
        if(rising_edge(clock_y)) then
            if(dataY(11) = '1') then --forward/up
                if (y = y_max + ship_height) then
                    y <= y_max + ship_height;
                else
                    y <= y - 1;
                end if;
            else 							--backward/down
                if (y = y_min + 1) then
                    y <= y_min + 1;
                else
                    y <= y + 1;
                end if;
            end if;
        end if;
    end process;
END ARCHITECTURE;