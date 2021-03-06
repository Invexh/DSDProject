library ieee;
use ieee.std_logic_1164.all;

entity RNG10 is
	port (
		set, clkToggle, clk10Mhz : in std_logic;
		PRNG10 : buffer std_logic_vector(9 downto 0));			
end entity;

architecture RNG10_arch of RNG10 is	
	component Clock_Divider is
		port ( clk,reset: in std_logic;
			clock_out: out std_logic);
	end component;
	
	component LFSR3 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(2 downto 0));
	end component;
	
	component LFSR5 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(4 downto 0));
	end component;
	
	component LFSR7 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(6 downto 0));
	end component;
	
	component LFSR13 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(12 downto 0));
	end component;
	
	component LFSR17 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(16 downto 0));
	end component;
	
	component LFSR19 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(18 downto 0));
	end component;
	
	component LFSR31 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(30 downto 0));
	end component;
	
	component LFSR61 is 
		port( clk, rst: in std_logic;
			output : out std_logic_vector(60 downto 0));
	end component;
	
	component LFSR89 IS
	  PORT (Clk, Rst: IN std_logic;
			output: OUT std_logic_vector (88 DOWNTO 0));
	end component;
	
	component LFSR107 IS
		PORT (Clk, Rst: IN std_logic;
			output: OUT std_logic_vector (106 DOWNTO 0));
	end component;

	signal SR3 : std_logic_vector(2 downto 0);
	signal SR5 : std_logic_vector(4 downto 0);
	signal SR7 : std_logic_vector(6 downto 0);
	signal SR13 : std_logic_vector(12 downto 0);
	signal SR17 : std_logic_vector(16 downto 0);
	signal SR19 : std_logic_vector(18 downto 0);
	signal SR31 : std_logic_vector(30 downto 0);
	signal SR61 : std_logic_vector(60 downto 0);
	signal SR89 : std_logic_vector(88 downto 0);
	signal SR107 : std_logic_vector(106 downto 0);
	signal dispHex, blank_L, dispPoint : std_logic;
	signal inData1, inData0 : std_logic_vector(3 downto 0);
	signal clk1Mhz, clk, clkset : std_logic;
	 

begin
	U1 : LFSR3 port map ( clk=>clk, rst=>set, output=>SR3);
	U2 : LFSR5 port map ( clk=>clk, rst=>set, output=>SR5);
	U3 : LFSR7 port map ( clk=>clk, rst=>set, output=>SR7);
	U4 : LFSR13 port map ( clk=>clk, rst=>set, output=>SR13);
	U5 : LFSR17 port map ( clk=>clk, rst=>set, output=>SR17);
	U6 : LFSR19 port map ( clk=>clk, rst=>set, output=>SR19);
	U7 : LFSR31 port map ( clk=>clk, rst=>set, output=>SR31);
	U8 : LFSR61 port map ( clk=>clk, rst=>set, output=>SR61);
	U9 : LFSR89 port map ( clk=>clk, rst=>set, output=>SR89);
	U10 : LFSR107 port map ( clk=>clk, rst=>set, output=>SR107);
	U11 : Clock_Divider port map (clk=>Clk10Mhz, reset=>clkset, clock_out=>clk1Mhz);
	
	clock : process (clkToggle, clk1Mhz, set)
	begin
		if clkToggle = '0' then
			clk<= clk1Mhz;
		else
			clk<='0';
		end if;
	end process clock;

	PRNG : process(clk,set)
	begin
		if(rising_edge(clk)) then
			PRNG10(9) <= SR107(106);
			PRNG10(8) <= SR89(88);
			PRNG10(7) <= SR61(60);
			PRNG10(6) <= SR31(30);
			PRNG10(5) <= SR19(18);
			PRNG10(4) <= SR17(16);
			PRNG10(3) <= SR13(12);
			PRNG10(2) <= SR7(6);
			PRNG10(1) <= SR5(4);
			PRNG10(0) <= SR3(2);	
		end if;
		
		if(set = '0') then
			PRNG10(9) <= SR107(106);
			PRNG10(8) <= SR89(88);
			PRNG10(7) <= SR61(60);
			PRNG10(6) <= SR31(30);
			PRNG10(5) <= SR19(18);
			PRNG10(4) <= SR17(16);
			PRNG10(3) <= SR13(12);
			PRNG10(2) <= SR7(6);
			PRNG10(1) <= SR5(4);
			PRNG10(0) <= SR3(2);	
		end if;
	end process PRNG;
end architecture;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;
  
entity Clock_Divider is
	port ( clk,reset: in std_logic;
	clock_out: out std_logic);
end Clock_Divider;
  
architecture ClockDivider_arch of Clock_Divider is
  
	signal count: integer:= 1;
	signal tmp : std_logic := '0';
  
	begin
  
		process(clk,reset)
		begin
			if(reset='1') then
				count<=1;
				tmp<='0';
			elsif(rising_edge(clk)) then
				count <=count+1;
				if (count = 10) then
					tmp <= NOT tmp;
					count <= 1;
				end if;
			end if;
			clock_out <= tmp;
		end process;
end architecture;
