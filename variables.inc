var
	// Other variables	
	mychar : string = "A";
	myp, mym : pointer;
	strpos, p, q, r, u, v, x, y : byte;
	tempInt : Integer;
	message : string = "Press 'S' or Fire to start! Welcome to Paganitzu! Use Joysticks or Keys: QA = Up/Down M, = Left/Right. Check out BlueBilby.com ... ";
	strlen : byte;
	titledata : incbin("trse.bin") compressed;
	
	// Records
	gameStats_record = record
		lives : byte;
		bonus : Integer;
		keys : byte;
		room: byte;
		score : Integer;
		hiScore : Integer;
		gameRunning : boolean;
		frameDelayMax : byte;
	end;
	gameStats : gameStats_record;
	
	// Constants
	const BlankTile : byte = 0;
	const DirtTile : byte = 1;
	const WallTile : byte = 2;						// These tiles cannot be walked on
	const BrickTile : byte = 3;						//
	const KeyholeTile : byte = 4;					//
	const SpikesShaftTile : byte = 5;			//
	const PipeTile : byte = 6;						//
	const DiamondTile : byte = 13;				// From here on are special tiles
	const KeyTile : byte = 17;
	const SpikesHeadTile : byte = 18;
	const RockTile : byte = 19;
	const WaterTile : byte = 20;					// To here
	const SpiderTile : byte = 21;
	const SnakeTile : byte = 23;
	const SnakeLeftTile : byte = 27;
	const PlayerTile : byte = 31;
	const PlayerLeftTile : byte = 33;
	
	const gridWidth : byte = 13;
	const gridHeight : byte = 8;
	
	const NumberOfRooms : byte = 1;
	
	playerRoomX: array[] of byte = ( 1);
	playerRoomY: array[] of byte = ( 1);
	
	// Store current level live data
	gameGrid : array[gridWidth*gridHeight] of byte;
	lookupGridY : array[gridHeight] of Integer = BuildTable("i*GRIDWIDTH");
	lookupScreenY : array[gridHeight] of Integer = BuildTable("i*8");
	lookupScreenX : array[gridWidth] of Integer = BuildTable("i*2");
	
	// Tile records
	diamondTile_record = record
			animationFrame : byte;
			frameCount : byte;
			frameDelay : byte;
			frameDelayMax : byte;
			interFrameDelay : byte; // Delay before animation starts.
			interFrameDelayMax : byte;
			animatedThisFrame : boolean;
			points : byte;
	end;
	diamondRecord : diamondTile_record;
	
	spiderTile_record = record
			animationFrame : byte;
			frameCount : byte;
			frameDelay : byte;
			animatedThisFrame : boolean;
	end;
	spiderRecord : spiderTile_record;
	
	snakeTile_record = record
			animationFrame : byte;
			frameCount : byte;
			facingDirection : boolean; // ( false = left, true = right )
			animatedThisFrame : boolean;
	end;
	snakeRecord : snakeTile_record;
	
	playerTile_record = record
			animationFrame : byte;
			frameCount : byte;
			frameDelay : byte;
			x, y : byte;
			lastDirection : byte; // ( 0 = left, 1 = right, 2 = up, 3 = down )
			facingDirection : boolean; // ( false = left, true = right )
	end;
	playerRecord : playerTile_record;

	// HUD and Tile Data
	HUDData: array[] of byte = (255, 255, 255, 255, 255, 255, 255, 095, 223, 223, 127, 127, 255, 119, 119, 119, 093, 127, 255, 095, 119, 119, 119, 127, 255, 119, 119, 119, 127, 127, 255, 119, 223, 223, 127, 127, 255, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 255, 255, 255, 255, 255, 255, 255, 095, 127, 125, 125, 095, 253, 253, 221, 221, 221, 255, 255, 125, 253, 221, 125, 127, 255, 221, 221, 221, 221, 255, 253, 127, 127, 125, 221, 095, 255, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 255, 255, 255, 255, 255, 255, 253, 253, 093, 221, 095, 095, 253, 255, 125, 221, 253, 255, 253, 255, 125, 221, 127, 127, 253, 255, 125, 221, 255, 223, 253, 093, 095, 125, 093, 127, 255, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 255, 255, 255, 255, 255, 255, 255, 221, 213, 221, 245, 255, 255, 221, 223, 221, 223, 255, 255, 215, 215, 213, 247, 255, 255, 221, 223, 247, 253, 255, 255, 221, 213, 247, 215, 255, 255, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 255, 255, 255, 255, 255, 255, 253, 127, 127, 125, 223, 095, 253, 221, 221, 221, 221, 255, 253, 125, 221, 221, 223, 127, 253, 221, 221, 221, 223, 223, 253, 127, 125, 223, 125, 127, 255, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000);
	TileData: array[] of byte = (000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 068, 068, 021, 085, 081, 068, 021, 085, 021, 084, 085, 021, 021, 084, 068, 017, 042, 168, 213, 086, 213, 086, 213, 086, 213, 086, 213, 086, 213, 086, 063, 252, 170, 170, 149, 085, 149, 085, 149, 085, 170, 170, 085, 149, 085, 149, 085, 149, 042, 168, 215, 214, 223, 246, 223, 246, 215, 214, 215, 214, 213, 086, 063, 252, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 000, 000, 000, 000, 255, 255, 085, 085, 085, 085, 255, 255, 000, 000, 000, 000, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 253, 112, 085, 112, 085, 112, 255, 192, 000, 000, 000, 000, 013, 112, 013, 112, 013, 127, 013, 085, 013, 085, 003, 255, 000, 000, 000, 000, 000, 000, 000, 000, 003, 255, 013, 085, 013, 085, 013, 127, 013, 112, 013, 112, 000, 000, 000, 000, 255, 192, 085, 112, 085, 112, 253, 112, 013, 112, 013, 112, 013, 112, 013, 112, 253, 127, 085, 085, 085, 085, 253, 127, 013, 112, 013, 112, 002, 192, 010, 240, 042, 252, 170, 255, 255, 170, 063, 168, 015, 160, 003, 128, 002, 192, 010, 240, 042, 252, 170, 255, 085, 170, 021, 168, 005, 160, 001, 128, 001, 192, 005, 240, 021, 252, 085, 255, 255, 085, 063, 084, 015, 080, 003, 064, 002, 064, 010, 080, 042, 084, 170, 085, 255, 170, 063, 168, 015, 160, 003, 128, 252, 000, 204, 000, 255, 000, 003, 192, 000, 240, 003, 060, 000, 012, 000, 048, 034, 032, 034, 032, 170, 168, 017, 016, 017, 016, 017, 016, 017, 016, 085, 084, 015, 240, 063, 124, 255, 223, 255, 255, 255, 255, 251, 255, 062, 252, 015, 240, 042, 168, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 042, 168, 128, 002, 143, 242, 047, 248, 015, 240, 131, 194, 047, 248, 011, 224, 032, 008, 000, 000, 143, 242, 143, 242, 047, 248, 003, 192, 047, 248, 139, 226, 008, 032, 002, 064, 010, 160, 042, 160, 040, 000, 010, 128, 064, 160, 032, 040, 010, 160, 000, 144, 002, 168, 010, 128, 010, 040, 002, 160, 016, 168, 032, 040, 010, 160, 000, 036, 000, 170, 002, 160, 002, 138, 002, 160, 004, 168, 032, 040, 010, 160, 000, 144, 002, 168, 010, 168, 010, 000, 002, 160, 016, 168, 032, 040, 010, 160, 001, 128, 010, 160, 010, 168, 000, 040, 002, 160, 010, 001, 040, 008, 010, 160, 006, 000, 042, 128, 002, 160, 040, 160, 010, 128, 042, 004, 040, 008, 010, 160, 024, 000, 170, 000, 010, 128, 162, 128, 010, 128, 042, 016, 040, 008, 010, 160, 006, 000, 042, 128, 042, 160, 000, 160, 010, 128, 042, 004, 040, 008, 010, 160, 015, 192, 015, 144, 013, 072, 006, 168, 005, 064, 003, 000, 003, 000, 002, 128, 015, 192, 015, 144, 013, 072, 006, 168, 005, 064, 015, 192, 044, 192, 032, 160, 003, 240, 006, 240, 033, 112, 042, 144, 001, 080, 000, 192, 000, 192, 002, 128, 003, 240, 006, 240, 033, 112, 042, 144, 001, 080, 003, 240, 003, 056, 010, 008);
	
	// Levels data
	Room0: array[] of byte = (WallTile, WallTile, WallTile, WallTile, WallTile, WallTile, WallTile, WallTile, BrickTile, BrickTile, BrickTile, PipeTile+1, BrickTile,
														WallTile, BlankTile, WallTile, WallTile, WallTile, WallTile, WaterTile, WaterTile, PipeTile, PipeTile+5, BrickTile, PipeTile+1, BrickTile,
														WallTile, BlankTile, BlankTile, BlankTile, BlankTile, BlankTile, WaterTile, WaterTile, BrickTile, PipeTile+3, PipeTile, PipeTile+2, BrickTile,
														WallTile, BlankTile, RockTile, WallTile, WallTile, BlankTile, DirtTile, DirtTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile,
														WallTile, BlankTile, SpiderTile, BlankTile, SnakeTile, WallTile, DirtTile, DirtTile, KeyTile, BlankTile, BlankTile, PipeTile+4, PipeTile,
														WallTile, BlankTile, WallTile, WallTile, WallTile, WallTile,WallTile, WallTile, WallTile, DiamondTile, DiamondTile, PipeTile+3, PipeTile+5,
														BrickTile, KeyTile, BlankTile, BlankTile, BlankTile, BlankTile, DiamondTile, DiamondTile, DiamondTile, BrickTile, BrickTile, BrickTile, PipeTile+1,
														BrickTile, BrickTile, BrickTile, BrickTile, KeyholeTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile, PipeTile+1 );
														