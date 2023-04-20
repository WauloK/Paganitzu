// All keys collected. Player hit Keyhole tile
//procedure EndLevel();
//begin
	// Code goes here. TODO
//end;

// Perform some sort of action based on what tile the player is on
procedure PlayerActionTile();
begin
	u := playerRecord.x;
	v := playerRecord.y;
	r := gameGrid[u+lookupGridY[v]];
	// Change tile to player
	gameGrid[u+lookupGridY[v]] := playerTile;
	case r of
		BlankTile:
			return;
		DirtTile:
			return;
		KeyholeTile:
			begin
				//EndLevel();
			end;
		DiamondTile:
			begin
				gameStats.score := gameStats.score + 1;
				FxPlay(sfxDiamond);
			end;
		else
		begin
			return;
		end;
	//end;
end;

// Check if player tile can move to new pos
function CheckTileAt(u,v: global byte) : Boolean;
begin
	r := gameGrid[u+lookupGridY[v]];
	b := true;
	case r of
		WallTile:
			b := false;
		BrickTile:
			b := false;
		SpikesShaftTile:
			b := false;
		PipeTile:
			b := false;
		SpikesHeadTile:
			b := false;
		WaterTile:
			b := false;
		SpiderTile:
			b := false;
		SnakeTile:
			b := false;
		KeyholeTile:
			begin
				if (gameStats.keysLeft > 0) then b := false;
			end;
	end;
	// Player movement sound effect
	if (b = true) then FxPlay(sfxMove);
	CheckTileAt := b;
end;

// Set tile where player used to be, to blankTile
procedure SetTileToBlank(u,v : global byte);
begin
	gameGrid[u+lookupGridY[v]] := blankTile;
end;

// Things to do with player tile
procedure UpdatePlayerTile();
begin
	// Get player input
	p := Input::GetJoystick(1);
	q := Input::GetJoystick(2);
	s := Input::GetPressedKey();	// Scan for key values separately
	
	// Check for movement
	if (p = Input::Joy_None and (q = Input::Joy_None and (s = Input::Key_None))) then return;
	
	// Up
	if (p = Input::Joy_Up or (q = Input::Joy_Up or (s = Input::Key_Q))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.y > 0) then
		begin
			if (CheckTileAt(playerRecord.x, playerRecord.y-1) = true) then
			begin
				SetTileToBlank(playerRecord.x, playerRecord.y);
				playerRecord.y := playerRecord.y -1;
				playerRecord.lastDirection := 2;
				PlayerActionTile();
			end;
		end;
	end;
	// Down
	if (p = Input::Joy_Down or (q = Input::Joy_Down or (s = Input::Key_A))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.y < gridHeight) then
		begin
			if (CheckTileAt(playerRecord.x, playerRecord.y+1) = true) then
			begin
				SetTileToBlank(playerRecord.x, playerRecord.y);
				playerRecord.y := playerRecord.y +1;
				playerRecord.lastDirection := 3;
				PlayerActionTile();
			end;
		end;
	end;
	// Up
	if (p = Input::Joy_Left or (q = Input::Joy_Left or (s = Input::Key_M))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.x > 0) then
		begin
			if (CheckTileAt(playerRecord.x-1, playerRecord.y) = true) then
			begin
				SetTileToBlank(playerRecord.x, playerRecord.y);
				playerRecord.x := playerRecord.x -1;
				playerRecord.lastDirection := 0;
				PlayerActionTile();
			end;
		end;
	end;
	// Down
	if (p = Input::Joy_Right or (q = Input::Joy_Right or (s = Input::Key_COMMA))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.x < gridWidth) then
		begin
			if (CheckTileAt(playerRecord.x+1, playerRecord.y) = true) then
			begin
				SetTileToBlank(playerRecord.x, playerRecord.y);
				playerRecord.x := playerRecord.x +1;
				playerRecord.lastDirection := 1;
				PlayerActionTile();
			end;
		end;
	end;
end;