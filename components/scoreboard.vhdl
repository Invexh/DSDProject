------UPDATE DIGTIS WITH SCORE VALUE---------------------------------------------------------
hndl_Digits : process(score)
VARIABLE valu : INTEGER;
VARIABLE modC : INTEGER;
begin
    FOR i in 0 to (max_digits - 1) LOOP
        valu := (score/(10 ** i)) mod 10;
        
        case valu is
            when 9 => digit(max_digits-i-1).s <= "1110011";
            when 8 => digit(max_digits-i-1).s <= "1111111";
            when 7 => digit(max_digits-i-1).s <= "1110000";
            when 6 => digit(max_digits-i-1).s <= "1011111";
            when 5 => digit(max_digits-i-1).s <= "1011011";
            when 4 => digit(max_digits-i-1).s <= "0110011";
            when 3 => digit(max_digits-i-1).s <= "1111001";
            when 2 => digit(max_digits-i-1).s <= "1101101";
            when 1 => digit(max_digits-i-1).s <= "0110000";
            when 0 => digit(max_digits-i-1).s <= "1111110";
            when others => digit(max_digits-i-1).s <= "1000111";
        end case;
    END LOOP;
END PROCESS;