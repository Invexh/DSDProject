LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.custom_types.ALL;

ENTITY buzzer IS 
	PORT(
        alien               : IN alien_array(11 downto 0);
		clockWithPause 		: IN STD_LOGIC;
		RNG					: IN STD_LOGIC_VECTOR(9 downto 0);
        btn_0               : IN STD_LOGIC;
		buzzer1 			: BUFFER STD_LOGIC
	);
END ENTITY;

ARCHITECTURE ExplosionsAndPews OF buzzer IS
    SIGNAL exp_sound : STD_LOGIC := '0';
    SIGNAL bz1_clk : STD_LOGIC := '0';
    SIGNAL pew_sound : STD_LOGIC := '0';

BEGIN
    ------BUZZER1 -------------------------------------------------
    buzzer1_clock : process(clockWithPause)
        VARIABLE C3_counter : integer range 0 to 38230 := 0;
        VARIABLE exp_clk_counter : integer range 0 to 9610 := 0;
        VARIABLE RNG_instance : integer range 0 to 9610 := 7500;		--generates random-ish clock for white noise

    BEGIN
        IF(rising_edge(clockWithPause)) THEN	
            IF(exp_sound = '1') THEN
                IF (exp_clk_counter > RNG_instance ) THEN
                    bz1_clk <= NOT bz1_clk;
                    RNG_instance := 300*(to_integer(unsigned(RNG(7 downto 3)))+1);
                    exp_clk_counter := 0;
                ELSE
                    bz1_clk <= bz1_clk;
                    exp_clk_counter := exp_clk_counter + 1;
                END IF;	
            ELSIF(pew_sound = '1') THEN		
                IF (C3_counter > 38225 ) 	THEN	-- cycles to get frequency of 130.813 (C3) 
                    bz1_clk <= NOT bz1_clk;
                    C3_counter := 0;
                ELSE
                    bz1_clk <= bz1_clk;
                    C3_counter := C3_counter + 1;					
                END IF;
            ELSE
                bz1_clk <= bz1_clk;
            END IF;
        END IF;	
    END PROCESS;

    buzzer1_process : process(bz1_clk, clockWithPause, btn_0 )
        VARIABLE pew_counter : integer range 0 to 51 := 0;
        VARIABLE exp_counter : integer range 0 to 1001 := 0;
        VARIABLE sound_done  : std_logic := '0';
        VARIABLE hs5 : boolean := false;
        VARIABLE hs6 : boolean := false;

    BEGIN
        IF(rising_edge(bz1_clk)) THEN	
            IF(exp_counter > 0) THEN		-- explosion
                IF(exp_counter < 1000) THEN
                    buzzer1 <= NOT buzzer1;
                    exp_counter := exp_counter + 1;
                ELSE
                    exp_counter := 0;
                    sound_done := '1';
                END IF;			
            ELSIF (pew_counter > 0) THEN					-- pew
                IF (pew_counter < 50) THEN
                    buzzer1 <= NOT buzzer1;
                    pew_counter := pew_counter + 1;
                ELSE
                    sound_done := '1';
                    pew_counter := 0;
                END IF;
            ELSIF(exp_sound = '1') THEN
                exp_counter := 1;
            ELSIF(pew_sound = '1') THEN
                pew_counter := 1;
            ELSE
                buzzer1 <= buzzer1;
                sound_done := '0';
            END IF;
        END IF;
        
        -- flags for sound creation
        IF(rising_edge(clockWithPause)) THEN
            FOR i in 0 to 11 LOOP
                IF(alien(i).collision = '1') THEN
                    exp_sound <= '1';
                ELSIF (sound_done = '1') THEN
                    exp_sound <= '0';	
                ELSE
                    exp_sound <= exp_sound;
                END IF;
            END LOOP;
            
            IF(btn_0 = '0') THEN
                pew_sound <='1';
                hs6 := true;
            ELSIF( sound_done = '1') THEN
                pew_sound <= '0';
                hs6 := false;
            ELSE
                pew_sound <= pew_sound;
            END IF;
        END IF;			    
    END PROCESS;
END ARCHITECTURE;

-- ------BUZZER1 -------------------------------------------------
-- buzzer1_clock : process(clockWithPause)
-- variable C3_counter : integer := 0;
-- variable exp_clk_counter : integer := 0;
-- variable RNG_instance : integer := 7500;		--generates random-ish clock for white noise	
-- begin
--     IF(rising_edge(clockWithPause)) THEN	
--         IF(exp_sound = '1') THEN
--             IF (exp_clk_counter > RNG_instance ) THEN
--                 bz1_clk <= NOT bz1_clk;
--                 RNG_instance := 300*(to_integer(unsigned(RNG(7 downto 3)))+1);
--                 exp_clk_counter := 0;
--             ELSE
--                 bz1_clk <= bz1_clk;
--                 exp_clk_counter := exp_clk_counter + 1;
--             END IF;	
--         ELSIF(pew_sound = '1') THEN		
--             IF (C3_counter > 38225 ) 	THEN	-- cycles to get frequency of 130.813 (C3) 
--                 bz1_clk <= NOT bz1_clk;
--                 C3_counter := 0;
--             ELSE
--                 bz1_clk <= bz1_clk;
--                 C3_counter := C3_counter + 1;					
--             END IF;
--         ELSE
--             bz1_clk <= bz1_clk;
--         END IF;
--     END IF;	
-- end process;

-- buzzer1_process : process(bz1_clk, clockWithPause, btn_0 )
-- variable pew_counter : integer := 0;
-- variable exp_counter : integer := 0;
-- variable sound_done  : std_logic := '0';
-- variable hs5 : boolean := false;
-- variable hs6 : boolean := false;


-- begin
--     IF(rising_edge(bz1_clk)) THEN	
--         IF(exp_counter > 0) THEN		-- explosion
--             IF(exp_counter < 1000) THEN
--                 buzzer1 <= NOT buzzer1;
--                 exp_counter := exp_counter + 1;
--             ELSE
--                 exp_counter := 0;
--                 sound_done := '1';
--             END IF;			
--         ELSIF (pew_counter > 0) THEN					-- pew
--             IF (pew_counter < 50) THEN
--                 buzzer1 <= NOT buzzer1;
--                 pew_counter := pew_counter + 1;
--             ELSE
--                 sound_done := '1';
--                 pew_counter := 0;
--             END IF;
--         ELSIF(exp_sound = '1') THEN
--             exp_counter := 1;
--         ELSIF(pew_sound = '1') THEN
--             pew_counter := 1;
--         ELSE
--             buzzer1 <= buzzer1;
--             sound_done := '0';
--         END IF;
--     END IF;
    
--     -- flags for sound creation
--     IF(rising_edge(clockWithPause)) THEN
--         for i in 0 to 11 loop
--             IF(alien(i).collision = '1') THEN
--                 exp_sound <= '1';
--             ELSIF (sound_done = '1') THEN
--                 exp_sound <= '0';	
--             ELSE
--                 exp_sound <= exp_sound;
--             END IF;
--         end loop;
        
--         IF(btn_0 = '0') THEN
--             pew_sound <='1';
--             hs6 := true;
--         ELSIF( sound_done = '1') THEN
--             pew_sound <= '0';
--             hs6 := false;
--         ELSE
--             pew_sound <= pew_sound;
--         END IF;
--     END IF;			
        
-- end process;