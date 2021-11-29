LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.custom_types.ALL;

ENTITY scoreboard IS
    GENERIC(
        e0 : INTEGER := 1;
        e1 : INTEGER := 10;
        e2 : INTEGER := 100;
        e3 : INTEGER := 1000;
        e4 : INTEGER := 10000;
        e5 : INTEGER := 100000
    );
    PORT(
        score : IN INTEGER range 0 to 999999;
        index : IN INTEGER range 0 to 5;
        digit : OUT seg_digit
    );
END ENTITY;

ARCHITECTURE scoreboard_arch OF scoreboard IS
    SIGNAL val : INTEGER range 0 to 15;
BEGIN
    hndl_Digits : process(score)
	begin
        case index is
            when 0 => val <= (score/e5) mod 10;
            when 1 => val <= (score/e4) mod 10;
            when 2 => val <= (score/e3) mod 10;
            when 3 => val <= (score/e2) mod 10;
            when 4 => val <= (score/e1) mod 10;
            when 5 => val <= (score/e0) mod 10;
        end case;
	END PROCESS;

    digit.s <= "1110011" when (val = 9) else
               "1111111" when (val = 8) else
               "1110000" when (val = 7) else
               "1011111" when (val = 6) else
               "1011011" when (val = 5) else
               "0110011" when (val = 4) else
               "1111001" when (val = 3) else
               "1101101" when (val = 2) else
               "0110000" when (val = 1) else
               "1111110" when (val = 0) else
               "1000111";
END ARCHITECTURE;