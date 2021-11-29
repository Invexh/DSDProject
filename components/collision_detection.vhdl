------COLLISION DETECTION--------------------------------------------------------------------
hndl_Collision : process (max10_clk)
begin
    FOR i in 0 to 11 LOOP
        --ALIEN AND PLAYER SHIP COLLISION--
        IF (pause = '0' AND spare_ships >= 1 AND 
        alien(i).x >= ship.x AND 
        (alien(i).x - (6 * alien(i).size)) <= (ship.x + ship_length) AND 
        (alien(i).y - (6 * alien(i).size)) <= ship.y AND 
        alien(i).y >= (ship.y - ship_height + ((alien(i).x - (6 * alien(i).size) - ship.x)*ship_height)/ship_length) AND
        alien(i).y >= (ship.y - ship_height)) THEN
            spare_ships <= spare_ships - 1;
        ELSIF (pause = '1' AND startOfGameFlag = '1') THEN
            spare_ships <= 3;
        END IF;

        --ALIEN AND PLAYER LASER COLLISION--
        FOR j in 0 to (max_pproj - 1) LOOP
            IF ((p_proj(j).x + 20) >= (alien(i).x - (6 * alien(i).size)) AND
            p_proj(j).x <= alien(i).x AND
            p_proj(j).y >= (alien(i).y - (6 * alien(i).size)) AND
            p_proj(j).y <= alien(i).y) THEN
                alien(i).collision <= '1';
                p_proj(i).collision <= '1';
            END IF;
            IF (p_proj(j).e = '0') THEN
                p_proj(j).collision <= '0';
            END IF;
        END LOOP;

    END LOOP;
END PROCESS;
