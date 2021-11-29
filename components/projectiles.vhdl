LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY projectile IS
    GENERIC(
        ship_length : INTEGER := 36
    );
    PORT(
        yOrigin : IN INTEGER range 0 to 480;
        xOrigin : IN INTEGER range 0 to 640;
        Xpos    : BUFFER INTEGER;
        Ypos    : OUT INTEGER;
        Right   : IN STD_LOGIC;
        Exist   : BUFFER STD_LOGIC;
        Spawn   : IN STD_LOGIC;
        clock   : IN STD_LOGIC
    );
END ENTITY;

ARCHITECTURE projectile_arch OF projectile IS
    SIGNAL projectile_clock : STD_LOGIC;
BEGIN

    projectileMoveClock : PROCESS (clock)
    VARIABLE proj_clock_counter : integer := 0;
    BEGIN
        IF (rising_edge(clock)) THEN
            proj_clock_counter := proj_clock_counter + 1;		
        END IF;
        IF (proj_clock_counter > 90000) THEN
            projectile_clock <= NOT projectile_clock;
            proj_clock_counter := 0;
        END IF;
    END PROCESS;

    move_Projectile : PROCESS (projectile_clock)
    BEGIN
        IF (rising_edge(projectile_clock)) THEN	
            IF (Spawn = '1') THEN
                Ypos <= yOrigin - 2;
                IF (Right = '1') THEN
                    Xpos <= xOrigin + ship_length;
                ELSE
                    Xpos <= xOrigin;
                END IF;
                Exist <= '1';
            ELSE
                IF (Right = '1') THEN
                    Xpos <= Xpos + 1;
                ELSE
                    Xpos <= Xpos - 1;
                END IF;
            END IF;

            IF (Xpos > 640 OR Xpos < -20) THEN
                Exist <= '0';
            END IF;
        END IF;
    END PROCESS;
END ARCHITECTURE;