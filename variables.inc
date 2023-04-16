var
	// Other variables	
	mychar : string = "A";
	myp, mym : pointer;
	strpos, p, q, u, v : byte;
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
		titlePlayed : boolean;
	end;
	gameStats : gameStats_record;
	
	// Game HUD
	HUDData: array[] of byte = (063, 255, 255, 255, 255, 255, 063, 095, 223, 223, 127, 127, 063, 119, 119, 119, 093, 127, 063, 095, 119, 119, 119, 127, 063, 119, 119, 119, 127, 127, 063, 119, 223, 223, 127, 127, 063, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 063, 255, 255, 255, 255, 255, 063, 095, 127, 125, 125, 095, 061, 253, 221, 221, 221, 255, 063, 125, 253, 221, 125, 127, 063, 221, 221, 221, 221, 255, 061, 127, 127, 125, 221, 095, 063, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 063, 255, 255, 255, 255, 255, 061, 253, 093, 221, 095, 095, 061, 255, 125, 221, 253, 255, 061, 255, 125, 221, 127, 127, 061, 255, 125, 221, 255, 223, 061, 093, 095, 125, 093, 127, 063, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 063, 255, 255, 255, 255, 255, 063, 221, 213, 221, 245, 255, 063, 221, 223, 221, 223, 255, 063, 215, 215, 213, 247, 255, 063, 221, 223, 247, 253, 255, 063, 221, 213, 247, 215, 255, 063, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 063, 255, 255, 255, 255, 255, 061, 127, 127, 125, 223, 095, 061, 221, 221, 221, 221, 255, 061, 125, 221, 221, 223, 127, 061, 221, 221, 221, 223, 223, 061, 127, 125, 223, 125, 127, 063, 255, 255, 255, 255, 255, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000);
	spriteData: array[] of byte = (000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 068, 068, 021, 085, 081, 068, 021, 085, 021, 084, 085, 021, 021, 084, 068, 017, 042, 168, 213, 086, 213, 086, 213, 086, 213, 086, 213, 086, 213, 086, 063, 252, 170, 170, 149, 085, 149, 085, 149, 085, 170, 170, 085, 149, 085, 149, 085, 149, 042, 168, 215, 214, 223, 246, 223, 246, 215, 214, 215, 214, 213, 086, 063, 252, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 017, 016, 000, 000, 000, 000, 255, 255, 085, 085, 085, 085, 255, 255, 000, 000, 000, 000, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 013, 112, 253, 112, 085, 112, 085, 112, 255, 192, 000, 000, 000, 000, 013, 112, 013, 112, 013, 127, 013, 085, 013, 085, 003, 255, 000, 000, 000, 000, 000, 000, 000, 000, 003, 255, 013, 085, 013, 085, 013, 127, 013, 112, 013, 112, 000, 000, 000, 000, 255, 192, 085, 112, 085, 112, 253, 112, 013, 112, 013, 112, 013, 112, 013, 112, 253, 127, 085, 085, 085, 085, 253, 127, 013, 112, 013, 112, 002, 192, 010, 240, 042, 252, 170, 255, 255, 170, 063, 168, 015, 160, 003, 128, 002, 192, 010, 240, 042, 252, 170, 255, 085, 170, 021, 168, 005, 160, 001, 128, 001, 192, 005, 240, 021, 252, 085, 255, 255, 085, 063, 084, 015, 080, 003, 064, 002, 064, 010, 080, 042, 084, 170, 085, 255, 170, 063, 168, 015, 160, 003, 128, 252, 000, 204, 000, 255, 000, 003, 192, 000, 240, 003, 060, 000, 012, 000, 048, 034, 032, 034, 032, 170, 168, 017, 016, 017, 016, 017, 016, 017, 016, 085, 084, 015, 240, 063, 124, 255, 223, 255, 255, 255, 255, 251, 255, 062, 252, 015, 240, 042, 168, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 042, 168, 131, 194, 047, 248, 015, 240, 143, 242, 035, 200, 143, 242, 039, 216, 015, 240, 003, 192, 047, 248, 143, 242, 015, 240, 035, 200, 143, 242, 039, 216, 143, 242, 002, 064, 010, 160, 042, 160, 040, 000, 010, 128, 064, 160, 032, 040, 010, 160, 000, 144, 002, 168, 010, 128, 010, 040, 002, 160, 016, 168, 032, 040, 010, 160, 000, 036, 000, 170, 002, 160, 002, 138, 002, 160, 004, 168, 032, 040, 010, 160, 000, 144, 002, 168, 010, 168, 010, 000, 002, 160, 016, 168, 032, 040, 010, 160, 001, 128, 010, 160, 010, 168, 000, 040, 002, 160, 010, 001, 040, 008, 010, 160, 006, 000, 042, 128, 002, 160, 040, 160, 010, 128, 042, 004, 040, 008, 010, 160, 024, 000, 170, 000, 010, 128, 162, 128, 010, 128, 042, 016, 040, 008, 010, 160, 006, 000, 042, 128, 042, 160, 000, 160, 010, 128, 042, 004, 040, 008, 010, 160, 015, 192, 015, 144, 013, 072, 006, 168, 005, 064, 003, 000, 003, 000, 002, 128, 015, 192, 015, 144, 013, 072, 006, 168, 005, 064, 015, 192, 044, 192, 032, 160, 003, 240, 006, 240, 033, 112, 042, 144, 001, 080, 000, 192, 000, 192, 002, 128, 003, 240, 006, 240, 033, 112, 042, 144, 001, 080, 003, 240, 003, 056, 010, 008);
	// Constants
	const BlankTile : byte = 0;
	const DirtTile : byte = 1;
	const WallTile : byte = 2;						// These tiles cannot be walked on
	const BrickTile : byte = 3;						//
	const KeyholeTile : byte = 4;					//
	const SpikesShaftTile : byte = 5;			//
	const PipeTile : byte = 6;						//
	const DiamondTile : byte = 13;
	const KeyTile : byte = 17;
	const SpikesHeadTile : byte = 18;
	const RockTile : byte = 19;
	const WaterTile : byte = 20;
	const SpiderTile : byte = 21;
	const SnakeRightTile : byte = 23;
	const SnakeLeftTile : byte = 27;
	const PlayerLeftTile : byte = 31;
	const PlayerRightTile : byte = 33;
	
	const gridWidth : byte = 13;
	const gridHeight : byte = 8;
	
	// Tile records
	diamondTile_record = record
			animationFrame : byte = 0;
			frameCount : byte = 4;
			frameDelay : byte = 20;
			interFrameDelay : byte = 100; // Delay before animation starts.
			points : byte = 1;
	end;
	diamondRecord : diamondTile_record;
	
	spiderTile_record = record
			animationFrame : byte = 0;
			frameCount : byte = 2;
			frameDelay : byte = 20;
	end;
	spiderRecord : spiderTile_record;
	
	snakeTile_record = record
			animationFrame : byte = 0;
			frameCount : byte = 2;
			frameDelay : byte = 20;
	end;
	snakeRecord : snakeTile_record;
	
	playerTile_record = record
			animationFrame : byte = 0;
			frameCount : byte = 2;
			frameDelay : byte = 20;
			x, y : byte;
			lastDirection : byte; // ( 0 = left, 1 = right, 2 = up, 3 = down )
			facingDirection : boolean; // ( false = left, true = right )
	end;
	playerRecord : playerTile_record;
	
	// Levels data
	// Store current level live data
	gameGrid : array[gridWidth*gridHeight] of byte;
	
	Room0: array[] of byte = (WallTile, WallTile, WallTile, WallTile, WallTile, WallTile, WallTile, WallTile, BrickTile, BrickTile, BrickTile, PipeTile+1, BrickTile,
														WallTile, BlankTile, WallTile, WallTile, WallTile, WallTile, WaterTile, WaterTile, PipeTile, PipeTile+5, WallTile, PipeTile+1, BrickTile,
														WallTile, BlankTile, BlankTile, BlankTile, BlankTile, BlankTile, WaterTile, WaterTile, BrickTile, PipeTile+3, PipeTile, PipeTile+2, BrickTile,
														WallTile, BlankTile, RockTile, WallTile, WallTile, BlankTile, DirtTile, DirtTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile,
														WallTile, BlankTile, BlankTile, BlankTile, SnakeRightTile, WallTile, DirtTile, DirtTile, KeyTile, BlankTile, BlankTile, PipeTile+4, PipeTile,
														WallTile, BlankTile, WallTile, WallTile, WallTile, WallTile,WallTile, WallTile, WallTile, DiamondTile, DiamondTile, PipeTile+3, PipeTile+5,
														BrickTile, KeyTile, BlankTile, BlankTile, BlankTile, BlankTile, DiamondTile, DiamondTile, DiamondTile, BrickTile, BrickTile, BrickTile, PipeTile+5,
														BrickTile, BrickTile, BrickTile, BrickTile, KeyholeTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile, BrickTile, PipeTile+5 );
														