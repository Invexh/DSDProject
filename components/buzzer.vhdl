------BUZZER1 -------------------------------------------------
buzzer1_clock : process(clockWithPause)
variable C3_counter : integer := 0;
variable exp_clk_counter : integer := 0;
variable RNG_instance : integer := 7500;		--generates random-ish clock for white noise	
begin
    if(rising_edge(clockWithPause)) then	
        if(exp_sound = '1') then
            if (exp_clk_counter > RNG_instance ) then
                bz1_clk <= not bz1_clk;
                RNG_instance := 300*(to_integer(unsigned(RNG(7 downto 3)))+1);
                exp_clk_counter := 0;
            else
                bz1_clk <= bz1_clk;
                exp_clk_counter := exp_clk_counter + 1;
            end if;	
        elsif(pew_sound = '1') then		
            if (C3_counter > 38225 ) 	then	-- cycles to get frequency of 130.813 (C3) 
                bz1_clk <= not bz1_clk;
                C3_counter := 0;
            else
                bz1_clk <= bz1_clk;
                C3_counter := C3_counter + 1;					
            end if;
        else
            bz1_clk <= bz1_clk;
        end if;
    end if;	
end process;

buzzer1_process : process(bz1_clk, clockWithPause, shoot )
variable pew_counter : integer := 0;
variable exp_counter : integer := 0;
variable sound_done  : std_logic := '0';
variable hs5 : boolean := false;
variable hs6 : boolean := false;


begin
    if(rising_edge(bz1_clk)) then	
        if(exp_counter > 0) then		-- explosion
            if(exp_counter < 1000) then
                buzzer1 <= not buzzer1;
                exp_counter := exp_counter + 1;
            else
                exp_counter := 0;
                sound_done := '1';
            end if;			
        elsif (pew_counter > 0) then					-- pew
            if (pew_counter < 50) then
                buzzer1 <= not buzzer1;
                pew_counter := pew_counter + 1;
            else
                sound_done := '1';
                pew_counter := 0;
            end if;
        elsif(exp_sound = '1') then
            exp_counter := 1;
        elsif(pew_sound = '1') then
            pew_counter := 1;
        else
            buzzer1 <= buzzer1;
            sound_done := '0';
        end if;
    end if;
    
    -- flags for sound creation
    if(rising_edge(clockWithPause)) then
        for i in 0 to 11 loop
            if(alien(i).collision = '1') then
                exp_sound <= '1';
            elsif (sound_done = '1') then
                exp_sound <= '0';	
            else
                exp_sound <= exp_sound;
            end if;
        end loop;
        
        if(shoot = '0') then
            pew_sound <='1';
            hs6 := true;
        elsif( sound_done = '1') then
            pew_sound <= '0';
            hs6 := false;
        else
            pew_sound <= pew_sound;
        end if;
    end if;			
        
end process;