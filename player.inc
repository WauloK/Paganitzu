// All keys collected. Player hit Keyhole tile
procedure EndLevel();
begin
	FxPlay(sfxKeyhole);
end;

// Move rock if possible
procedure ActionRockTile(x,y : global byte);
begin
	// Has rock moved flag?
	q := 0; // 1 if rock moves
	// Check tile left
	if (playerRecord.lastDirection = 0) then
	begin
		x := x -1;
		p := gameGrid[x+lookupGridY[y]];
	end;
	// Check tile right
	if (playerRecord.lastDirection = 1) then
	begin
		x := x +1;
		p := gameGrid[x+lookupGridY[y]];
	end;
	// Check tile above
	if (playerRecord.lastDirection = 2) then
	begin
		y := y -1;
		p := gameGrid[x+lookupGridY[y]];
	end;
	// Check tile below
	if (playerRecord.lastDirection = 3) then
	begin
		y := y +1;
		p := gameGrid[x+lookupGridY[y]];
	end;
	// Change tile if blank
	if (p = BlankTile) then
	begin
		gameGrid[x+lookupGridY[y]] := RockTile;
		q := 1;
	end;
	// Change tile if Water
	if (p = WaterTile) then
	begin
		gameGrid[x+lookupGridY[y]] := BlankTile;
		q := 1;
	end;
	if (q = 1) then
	begin
		FxPlay(sfxRock);
	end;
end;

procedure ChangeTileToPlayer(x,y : global byte);
begin
	// Change tile to player
	gameGrid[x+lookupGridY[y]] := playerTile;
	q := 1; // Player moved
end;

// Perform some sort of action based on what tile the player is on
function PlayerActionTile(x,y : global byte) : byte;
begin
	r := gameGrid[x+lookupGridY[y]];
	q := 1; // Player has moved
	case r of
		BlankTile:
			begin
				ChangeTileToPlayer(x,y);
				return;
			end;
		DirtTile:
			begin
				ChangeTileToPlayer(x,y);
				return;
			end;
		KeyholeTile:
			begin
				EndLevel();
			end;
		KeyTile:
			begin
				ChangeTileToPlayer(x,y);
				gameStats.keysLeft := gameStats.keysLeft -1;
				gameStats.score := gameStats.score + 1;
				UpdateHUDNumbers();
				if (gameStats.keysLeft = 0) then
				begin
					FxPlay(sfxNoKeys);
				end
				else
				begin
					FxPlay(sfxKey);
				end;
			end;
		DiamondTile:
			begin
				ChangeTileToPlayer(x,y);
				gameStats.score := gameStats.score + 5;
				UpdateHUDNumbers();
				FxPlay(sfxDiamond);
			end;
		RockTile:
			begin
				ActionRockTile(x,y);
			end;
		end;
	PlayerActionTile := q;
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
		SpikesHeadTileActive:
			b := false;
		KeyholeTile:
			begin
				if (gameStats.keysLeft > 0) then b := false;
			end;
	end;
	// Can't walk on pipes
	if (r > SpikesShaftTile and (r < DiamondTile)) then
	begin
		b := false;
	end;
	// Player movement sound effect
	if (b = true) then
	begin
		FxPlay(sfxMove);
		if (playerRecord.animationFrame = 0) then
		begin
			playerRecord.animationFrame := 1;
		end
		else
		begin
			playerRecord.animationFrame := 0;
		end;
	end;
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
	// Is player dead already?
	if (playerRecord.isDead = true) then
	begin
		KillPlayer();
		if (gameStats.lives > 0) then
		begin
			CopyRoomToGameGrid(gameStats.room);
			DrawRoomTiles();
		end;
	end;
	
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
				playerRecord.lastDirection := 2;
				q := PlayerActionTile(playerRecord.x, playerRecord.y-1);
				if (q = 1) then
				begin
					SetTileToBlank(playerRecord.x, playerRecord.y);
					DrawSprite(playerRecord.x, playerRecord.y, BlankTile);
					playerRecord.y := playerRecord.y -1;
					ChangeTileToPlayer(playerRecord.x,playerRecord.y);
				end;
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
				playerRecord.lastDirection := 3;
				q := PlayerActionTile(playerRecord.x, playerRecord.y+1);
				if (q = 1) then
				begin
					SetTileToBlank(playerRecord.x, playerRecord.y);
					DrawSprite(playerRecord.x, playerRecord.y, BlankTile);
					playerRecord.y := playerRecord.y +1;
					ChangeTileToPlayer(playerRecord.x,playerRecord.y);
				end;
			end;
		end;
	end;
	// Left
	if (p = Input::Joy_Left or (q = Input::Joy_Left or (s = Input::Key_M))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.x > 0) then
		begin
			if (CheckTileAt(playerRecord.x-1, playerRecord.y) = true) then
			begin
				playerRecord.lastDirection := 0;
				q := PlayerActionTile(playerRecord.x-1, playerRecord.y);
				if (q = 1) then
				begin
					SetTileToBlank(playerRecord.x, playerRecord.y);
					DrawSprite(playerRecord.x, playerRecord.y, BlankTile);
					playerRecord.x := playerRecord.x -1;
					ChangeTileToPlayer(playerRecord.x,playerRecord.y);
					playerRecord.facingDirection := false;
				end;
			end;
		end;
	end;
	// Right
	if (p = Input::Joy_Right or (q = Input::Joy_Right or (s = Input::Key_COMMA))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.x < gridWidth) then
		begin
			if (CheckTileAt(playerRecord.x+1, playerRecord.y) = true) then
			begin
				playerRecord.lastDirection := 1;
				q := PlayerActionTile(playerRecord.x+1, playerRecord.y);
				if (q = 1) then
				begin
					SetTileToBlank(playerRecord.x, playerRecord.y);
					DrawSprite(playerRecord.x, playerRecord.y, BlankTile);
					playerRecord.x := playerRecord.x +1;
					ChangeTileToPlayer(playerRecord.x,playerRecord.y);
					playerRecord.facingDirection := true;
				end;
			end;
		end;
	end;
	DrawPlayerTile();
end;