LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY pause IS
    PORT(
        dead  : IN STD_LOGIC;
        start : OUT STD_LOGIC;
        clock : IN STD_LOGIC;
        btn_0 : IN STD_LOGIC;
        btn_1 : IN STD_LOGIC;
        pauseClk : OUT STD_LOGIC;
        paused   : OUT STD_LOGIC
    );
END ENTITY;

ARCHITECTURE pause_arch OF pause IS
    SIGNAL pause : STD_LOGIC := '1';
    SIGNAL SoG : STD_LOGIC := '1';

BEGIN
    start <= SoG;
    paused <= pause;

    pauseProcess : process ( clock, btn_1, SoG )
    begin	

        if(SoG = '1') then
            if(btn_0 = '0') then
                pause <= '0';
            else
                pause <= '1';
            end if;
        elsif(falling_edge(btn_1)) then
            pause <= not pause;
        else
            pause <= pause;
        end if;

        pauseClk <= clock AND (NOT pause) AND (NOT dead);

    end process;

    start_proc : process(btn_0)
    begin
        if(falling_edge(btn_0) AND pause = '1') then
            SoG <= '0';
        end if;
    end process;
END ARCHITECTURE;