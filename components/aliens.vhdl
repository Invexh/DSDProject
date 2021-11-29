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
