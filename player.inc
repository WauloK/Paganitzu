// Check if player can move to new tile pos
procedure PlayerActionTile();
begin
	
end;

// Check if player tile can move to new pos
procedure CheckTileAt(u,v);
begin
	r := gameGrid[u+lookupGridY[v]];
	case r of
		WallTile:
			return false;
		BrickTile:
			return false;
		SpikesShaftTile:
			return false;
		PipeTile:
			return false;
		SpikesHeadTile:
			return false;
		WaterTile:
			return false;
		SpiderTile:
			return false;
		SnakeTile:
			return false;
		else
		begin
			return true;
		end;
	end;
end;

// Things to do with player tile
procedure UpdatePlayerTile();
begi
	// Get player input
	p := Input::GetJoystick(1);
	q := Input::GetJoystick(2);
	s := Input::GetPressedKey();	// Scan for key values separately
	
	// Check for movement
	if (p = Input::Joy_None and (q = Input::Joy_None and (s = Input::Key_None))) then return;
	
	// Up
	if (p = Input::Joy_Left or (q = Input::Joy_Left or (s = Input::Key_Q))) then
	begin
		// Can't move up if at top of screen
		if (playerRecord.y > 0) then
		begin
			if (CheckTileAt(playerRecord.x, playerRecord.y) = true) then
			begin
				playerRecord.y := playerRecord.y -1;
				PlayerActionTile();
			end;
		end;
	end;
end;