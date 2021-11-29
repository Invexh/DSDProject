LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.custom_types.ALL;

ENTITY alien IS
    GENERIC(
        index : INTEGER := 0;
        --Play area bounds
		y_max : INTEGER := 67;
		y_min : INTEGER := 413;
		x_max : INTEGER := 320;
		x_min : INTEGER := 25;

        dMinP : INT_ARRAY(0 to 11) := (11, 20, 29, 35, 15, 21, 12, 17, 04, 05, 03, 07)
    );
    PORT(
        MOVE_CLK : IN STD_LOGIC;    --MOVEMENT CLOCK
        CLK      : IN STD_LOGIC;    --CLOCK WITH PAUSE
        alien    : INOUT alien_t := ("000000000000", '0', '0', 11, '0', '0', 1, 0, 0, 0);   --ALIEN OBJECT
        RNG		 : IN STD_LOGIC_VECTOR(9 downto 0);
        score    : IN INTEGER range 0 to 999999
    );
END ENTITY;

ARCHITECTURE alien_arch OF alien IS
BEGIN

    hndl_Alien : process (CLK)
    begin
        IF(rising_edge(CLK)) THEN
            IF (alien.alive = '0') THEN
                alien.tsls <= alien.tsls + 1;
            END IF;

            IF (alien.hs2 = '1') THEN
                alien.hs1 <= '0';
            END IF;

            IF (index < 4 AND alien.alive = '0' AND alien.tsls >= (alien.min_p * 50000000)) THEN
                alien.alive <= '1';
                alien.size <= to_integer(unsigned(RNG(2 downto 0))) + 3;
                alien.color <= "110000001100";
                alien.hs1 <= '1';
                alien.tsls <= 0;

            ELSIF ( (index < 8 AND alien.alive = '0' AND alien.tsls >= (alien.min_p * 50000000)) ) THEN
                alien.alive <= '1';
                alien.size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(2 downto 0)) + 3);
                alien.color <= "000011000100";
                alien.hs1 <= '1';
                alien.tsls <= 0;

                IF (score > 1000 AND RNG(1) = '1') THEN
                    alien.min_p <= alien.min_p - 2;
                END IF;
                
            ELSIF ( (index < 12 AND alien.alive = '0' AND alien.tsls >= (alien.min_p * 50000000))) THEN
                alien.alive <= '1';
                alien.size <= to_integer(unsigned(RNG(5 downto 3) XOR RNG(9 downto 7)) + 3);
                alien.color <= "000000001100";
                alien.hs1 <= '1';
                alien.tsls <= 0;

                IF (score > 2000) THEN
                    alien.min_p <= alien.min_p - 2;
                END IF;

            ELSIF ((alien.x > 60000) OR (alien.x <= 0)) THEN
                alien.alive <= '0';
            END IF;
        END IF;
    END PROCESS;

    move_Alien : process (MOVE_CLK)
    VARIABLE randomValue : INTEGER;
    begin
        IF (rising_edge(MOVE_CLK) AND alien.alive = '1') THEN
            IF (alien.hs1 = '1') THEN
                randomValue := to_integer(unsigned(RNG( 8 downto (index rem 3) ))) * 8;
                alien.x <= 750 + randomValue/4;
                alien.y <= ((randomValue + y_max + 6*alien.size) rem (y_min - (y_max + 6*alien.size)) + (y_max + 6*alien.size) + 8);
                alien.hs2 <= '1';
            ELSE
                alien.hs2 <= '0';
                alien.x <= alien.x - 1;
            END IF;
        END IF;
        
        IF (alien.alive = '0') THEN
            alien.x <= 750;
            alien.y <= 240;
        END IF;
    END PROCESS;

END ARCHITECTURE;

