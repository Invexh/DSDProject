LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

package custom_types is
	type int_array is array (integer range <>) of integer;

	type alien_t is record
		color : STD_LOGIC_VECTOR(11 downto 0);
		collision : STD_LOGIC;
		alive : STD_LOGIC;
		min_p : INTEGER range 0 to 31;
		hs1 : STD_LOGIC;
		hs2 : STD_LOGIC;
		size : INTEGER range 0 to 7;
		tsls : INTEGER;
		x : INTEGER range 0 to 2000;
		y : INTEGER range 0 to 480;
		scorePart : INTEGER range 0 to 10000;
		die : STD_LOGIC;
		expClk : INTEGER;
		deathX : INTEGER range 0 to 640;
		deathY : INTEGER range 0 to 480;
	end record alien_t;
	
	type ship_t is record
		alive : STD_LOGIC;
		x : INTEGER range 0 to 640;
		y : INTEGER range 0 to 480;
		collision : STD_LOGIC;
		right : STD_LOGIC;
		exhaust : INTEGER range 0 to 7;
		dead : STD_LOGIC;
	end record ship_t;

	type seg_digit is record
		s : STD_LOGIC_VECTOR(0 to 6);
	end record seg_digit;

	type player_proj is record
		x : INTEGER; --Y POS
		y : INTEGER; --X POS
		collision : STD_LOGIC;
		hs1 : STD_LOGIC;	--Entity Handshake 1
		hs2 : STD_LOGIC;	--Entity Handshake 2
		e : STD_LOGIC; --Entity bit
		right : std_logic;
	end record player_proj;
	
	type alien_proj is record
		x : INTEGER; --Y POS
		y : INTEGER; --X POS
		hs1 : STD_LOGIC;	--Entity Handshake 1
		hs2 : STD_LOGIC;	--Entity Handshake 2
		e : STD_LOGIC; --Entity bit
		parent : integer; --alien the projectile spawned from
	end record alien_proj;
	
	type player_proj_array is array (integer range <>) of player_proj;
	type alien_proj_array is array (integer range <>) of alien_proj;
	type alien_array is array (integer range <>) of alien_t;
	type seg_array is array (integer range <>) of seg_digit;
	
end package;