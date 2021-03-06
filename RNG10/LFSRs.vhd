--team effort
--LFSRs Att2

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR3 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (2 DOWNTO 0));
END LFSR3;

ARCHITECTURE LFSR3_arch OF LFSR3 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (2 DOWNTO 0) := (2 => '0', OTHERS =>'1');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (2 => '0', OTHERS =>'1');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(2) XOR Currstate(1);
  Nextstate <= feedback & Currstate(2 DOWNTO 1);
  output <= Currstate;

END LFSR3_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR5 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (4 DOWNTO 0));
END LFSR5;

ARCHITECTURE LFSR5_arch OF LFSR5 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (4 DOWNTO 0) := (4 => '1', OTHERS =>'0');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (4 => '1', OTHERS =>'0');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(4) XOR Currstate(2);
  Nextstate <= feedback & Currstate(4 DOWNTO 1);
  output <= Currstate;

END LFSR5_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR7 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (6 DOWNTO 0));
END LFSR7;

ARCHITECTURE LFSR7_arch OF LFSR7 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (6 DOWNTO 0) := (6 => '1', OTHERS =>'0');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (6 => '1', OTHERS =>'0');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(6) XOR Currstate(5);
  Nextstate <= feedback & Currstate(6 DOWNTO 1);
  output <= Currstate;

END LFSR7_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR13 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (12 DOWNTO 0));
END LFSR13;

ARCHITECTURE LFSR13_arch OF LFSR13 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (12 DOWNTO 0) := (12 => '0', OTHERS =>'1');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (12 => '0', OTHERS =>'1');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(12) XOR Currstate(3) XOR Currstate(2) XOR Currstate(0);
  Nextstate <= feedback & Currstate(12 DOWNTO 1);
  output <= Currstate;

END LFSR13_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR17 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (16 DOWNTO 0));
END LFSR17;

ARCHITECTURE LFSR17_arch OF LFSR17 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (16 DOWNTO 0) := "10101010101010101";
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= "10101010101010101";
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(16) XOR Currstate(13);
  Nextstate <= feedback & Currstate(16 DOWNTO 1);
  output <= Currstate;

END LFSR17_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR19 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (18 DOWNTO 0));
END LFSR19;

ARCHITECTURE LFSR19_arch OF LFSR19 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (18 DOWNTO 0) := (18 => '1', OTHERS =>'0');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (18 => '1', OTHERS =>'0');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(18) XOR Currstate(5) XOR Currstate(1) XOR Currstate(0);
  Nextstate <= feedback & Currstate(18 DOWNTO 1);
  output <= Currstate;

END LFSR19_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR31 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (30 DOWNTO 0));
END LFSR31;

ARCHITECTURE LFSR31_arch OF LFSR31 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (30 DOWNTO 0) := (30 => '0', OTHERS =>'1');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (30 => '0', OTHERS =>'1');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(30) XOR Currstate(27);
  Nextstate <= feedback & Currstate(30 DOWNTO 1);
  output <= Currstate;

END LFSR31_arch;


LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY LFSR61 IS
	PORT (
		Clk: in std_logic;
		Rst: IN std_logic;
      output: OUT std_logic_vector (60 DOWNTO 0));
END LFSR61;

ARCHITECTURE LFSR61_arch OF LFSR61 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (60 DOWNTO 0) := (60 => '1', OTHERS =>'0');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (60 => '1', OTHERS =>'0');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(60) XOR Currstate(59) XOR Currstate(45) XOR Currstate(44);
  Nextstate <= feedback & Currstate(60 DOWNTO 1);
  output <= Currstate;

END LFSR61_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY LFSR89 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (88 DOWNTO 0));
END LFSR89;

ARCHITECTURE LFSR89_arch OF LFSR89 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (88 DOWNTO 0) := (88 => '1', OTHERS =>'0');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (88 => '1', OTHERS =>'0');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(88) XOR Currstate(50);
  Nextstate <= feedback & Currstate(88 DOWNTO 1);
  output <= Currstate;

END LFSR89_arch;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY LFSR107 IS
  PORT (Clk, Rst: IN std_logic;
        output: OUT std_logic_vector (106 DOWNTO 0));
END LFSR107;

ARCHITECTURE LFSR107_arch OF LFSR107 IS
  SIGNAL Nextstate, Currstate: std_logic_vector (106 DOWNTO 0) := (106 => '1', OTHERS =>'0');
  SIGNAL feedback: std_logic;
BEGIN

  StateReg: PROCESS (Clk,Rst)
  BEGIN
    IF (Rst = '0') THEN
      Currstate <= (106 => '1', OTHERS =>'0');
    ELSIF (rising_edge(Clk)) THEN
      Currstate <= Nextstate;
    END IF;
  END PROCESS;
  
  feedback <= Currstate(106) XOR Currstate(104) XOR Currstate(43) XOR Currstate(41);
  Nextstate <= feedback & Currstate(106 DOWNTO 1);
  output <= Currstate;

END LFSR107_arch;