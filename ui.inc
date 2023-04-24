// Draw and manage Title Screen
procedure TitleScreen();
begin
	myp := #mychar;
	mym := #message;
	strlen := Functions::SizeOfString(#message);
	v := 0;
	Screen::Cls(1);
	// Decompress the image to the screen
	Compression::Decompress(#titledata, #screen);
	// Set up font
		// Set pen colour
	Screen::SetPen(2);
	Screen::SetPaper(0);
	Font::SetFont(#Font::font1);
	
	// Keep titlescreen going until S pressed
	while (gameStats.gameRunning = false) do
	begin
		p := Input::GetJoystick(1);
		q := Input::GetJoystick(2);
		if (p = Input::Joy_Fire or (q = Input::Joy_Fire or (Input::GetPressedKey() = Input::Key_S))) then
		begin
				gameStats.gameRunning := true;
		end;
		// Draw new char in the onscreen scroll every 4 pixels
		if (v = 0) then
		begin
			v := 4;
			myp[0] := mym[strpos];
			Font::DrawTextAt(#mychar,31,59,false);
			if (strpos < strlen) then strpos := strpos + 1 else strpos := 0;
		end;
		for u := 0 to 200 do wait(50);	
		// Smooth scroll the message 1 pixel at a time
		Screen::doSmoothScroll(30591,5);
		v := v - 1;
	end;
end;

// In-game HUD
procedure DrawHUD();
begin
	Screen::Cls(1);
	Sprite::SetSize(6,64);
	Sprite::SetData(#HUDData);
	Sprite::DrawAt(26,0,0,false);
	// Set Sprite data to main game data
	Sprite::SetSize(2,8);
	Sprite::SetData(#TileData);
end;

// Draw numbers on the screen padded and with trailing zeroes
procedure DrawPadded(zscore : Integer; x, y: global byte);
begin
	u := x;
	if (zscore < 10000) then x := x + 1;
	if (zscore < 1000) then x := x + 1;
	if (zscore < 100) then x := x + 1;
	if (zscore < 10) then x := x + 1;
	Functions::IntegerToString(zscore);
	if (zscore > 0) then
	begin
		Font::DrawTextAt(#Functions_s,x,y,false);	
	end;
	Font::DrawTextAt("0",u+5,y,false);
end;

// Draw numbers on the screen padded
procedure DrawBonusPadded(bscore : Integer; x, y: global byte);
begin
	u := x;
	if (bscore < 1) then 
	begin
		Font::DrawTextAt("0",u+6,y,false);
		return;
	end;
	if (bscore < 100) then
	begin
		Font::DrawTextAt(" ",x,y,false);
		x := x + 1;
	end;
	if (bscore < 10) then
	begin
		Font::DrawTextAt(" ",x,y,false);
		x := x + 1;
	end;
	Functions::IntegerToString(bscore);
	Font::DrawTextAt(#Functions_s,x,y,false);	
end;

// Update HUD numbers
procedure UpdateHUDNumbers();
begin
	Screen::SetPen(2);
	Screen::SetPaper(0);
	// Draw score
	DrawPadded(gameStats.score,26,20);
	// Draw room number
	Functions::ByteToString(gameStats.room);
	if (gameStats.room < 10) then
	begin
		Font::DrawTextAt(#Functions_s,29,7,false);
		Font::DrawTextAt("0",28,7,false);
	end
	else
	begin
		Font::DrawTextAt(#Functions_s,28,7,false);
	end;
	// Draw lives left zero-padded
	Functions::ByteToString(gameStats.lives);
	Font::DrawTextAt(#Functions_s,29,33,false);
	Font::DrawTextAt("0",28,33,false);
	// Draw keys left zero-padded
	Functions::ByteToString(gameStats.keysLeft);
	Font::DrawTextAt(#Functions_s,29,46,false);
	Font::DrawTextAt("0",28,46,false);
	// Draw bonus points aka timer
	DrawBonusPadded(gameStats.bonus,27,59);
end;

// Copy RoomX to gameGrid
procedure CopyRoomToGameGrid(r : global byte);
begin
	for u := 0 to gridWidth*gridHeight do
	begin
		case r of
			1:
				gameGrid[u] := Room1[u];
			2:
				gameGrid[u] := Room2[u];
			3:
				gameGrid[u] := Room3[u];
			4:
				gameGrid[u] := Room4[u];
			5:
				gameGrid[u] := Room5[u];
			6:
				gameGrid[u] := Room6[u];
			7:
				gameGrid[u] := Room7[u];
			8:
				gameGrid[u] := Room8[u];
//			9:
//				gameGrid[u] := Room9[u];
//			10:
//				gameGrid[u] := Room10[u];
//			11:
//				gameGrid[u] := Room11[u];
//			12:
//				gameGrid[u] := Room12[u];
//			13:
//				gameGrid[u] := Room1[3u];
//			14:
//				gameGrid[u] := Room14[u];
//			15:
//				gameGrid[u] := Room15[u];
//			16:
//				gameGrid[u] := Room16[u];
//			17:
//				gameGrid[u] := Room17[u];
//			18:
//				gameGrid[u] := Room18[u];
//			19:
//				gameGrid[u] := Room19[u];
//			20:
//				gameGrid[u] := Room20[u];
		end;
	end;
end;

// Room Tiles Sprite draw common routine
procedure DrawSprite(x, y, q: global byte);
begin
	Sprite::DrawAt(lookupScreenX[x],lookupScreenY[y],q,false);
end;

// Player died
procedure KillPlayer();
begin
	gameStats.lives := gameStats.lives -1;
	if (gameStats.lives = 0) then
	begin
		FxPlay(sfxGameOver);
	end
	else
	begin
		FxPlay(sfxDied);
		playerRecord.isDead := false;
		gameStats.keysLeft := levelKeys[gameStats.room];
	end;
end;

// Check if snake spits poison at player
procedure SnakeAttackPlayer(x, y: global byte);
begin
	if (y <> playerRecord.y) then return;
	p := 0; // Non-zero if non-blank tile found between snake and player
	if (x > playerRecord.x) then
	begin
		for q := playerRecord.x to x do
		begin
			if (gameGrid[q+lookupGridY[y]] > BlankTile) then
			begin
				p := 1;
			end;
			if (gameGrid[q+lookupGridY[y]] = SnakeTile or (gameGrid[q+lookupGridY[y]] = PlayerTile)) then
			begin
				p := 0;
			end;
		end;
	end
	else
	begin
		for q := x to playerRecord.x do
		begin
			if (gameGrid[q+lookupGridY[y]] > BlankTile) then
			begin
				p := 1;
			end;
			if (gameGrid[q+lookupGridY[y]] = SnakeTile or (gameGrid[q+lookupGridY[y]] = PlayerTile)) then
			begin
				p := 0;
			end;
		end;			
	end;
	if (p = 0) then
	begin
		// Spit poison at player
		if (x > playerRecord.x) then
		begin
			s := lookupScreenX[playerRecord.x+1]-2;
			tempInt := 32*lookupScreenY[y]+64;
			for q := lookupScreenX[x]-1 to s step -1 do
			begin
				Screen::Poke(28672+q+tempInt,255);
				for p := 0 to 50 do begin wait(200); end;
				Screen::Poke(28672+q+tempInt,0);				
			end;
			playerRecord.isDead := true;
		end
		else
		begin
			s := lookupScreenX[playerRecord.x]+1;
			tempInt := 32*lookupScreenY[y]+64;
			for q := lookupScreenX[x]+2 to s do
			begin
				Screen::Poke(28672+q+tempInt,255);
				for p := 0 to 50 do begin wait(200); end;
				Screen::Poke(28672+q+tempInt,0);				
			end;
			playerRecord.isDead := true;
		end;
	end;
end;

// Draw player tile
procedure DrawPlayerTile();
begin
	v := playerRecord.animationFrame;
	if (playerRecord.facingDirection = true) then
	begin
		DrawSprite(playerRecord.x, playerRecord.y, PlayerTile+v);
	end
	else
	begin
		DrawSprite(playerRecord.x, playerRecord.y, PlayerTile+v+2);
	end;
end;

// Draw Snake tile
procedure DrawSnakeTile();
begin
	// Set direction snake faces, based on player location
	gameGrid[x+lookupGridY[y]] := SnakeTile;
	if (x > playerRecord.x) then
	begin
		gameGrid[x+lookupGridY[y]] := SnakeLeftTile;
	end;
	// Player is at same y-value as snake
	SnakeAttackPlayer(x,y);
	v := snakeRecord.animationFrame;
	if (gameGrid[x+lookupGridY[y]] = SnakeTile) then
	begin
		DrawSprite(x, y, u+v);
	end
	else
	begin
		DrawSprite(x, y, u+v);
	end;	
end;

// Check if player adjacent to spider
procedure CheckPlayerOrBlocked();
begin
	if (u = PlayerTile) then
	begin
		playerRecord.isDead := true;
	end;
	if (u > BlankTile) then
	begin
		v := v +1;
	end;
end;

// Make tile diamond
procedure MakeTileDiamond(p,q : global byte);
begin
	gameGrid[p+lookupGridY[q]] := DiamondTile;
end;

// Turn tiles into diamonds
procedure TurnTilesToDiamonds(p,q,u : global byte);
begin
	case u of
		RockTile:
			MakeTileDiamond(p,q);
		DirtTile:
			MakeTileDiamond(p,q);
	end;
	// Pipes
	if (u > SpikesShaftTile and (u < DiamondTile)) then begin
		MakeTileDiamond(p,q);
	end;
end;

// Check if tiles can be changed
procedure CheckSpiderDiamonds(x,y : global byte);
begin
	u := gameGrid[x-1+lookupGridY[y]];
	TurnTilesToDiamonds(x-1,y,u);
	u := gameGrid[x+1+lookupGridY[y]];
	TurnTilesToDiamonds(x+1,y,u);
	u := gameGrid[x+lookupGridY[y-1]];
	TurnTilesToDiamonds(x,y-1,u);
	u := gameGrid[x+lookupGridY[y+1]];
	TurnTilesToDiamonds(x,y+1,u);
end;

// Draw Spider tile
procedure DrawSpiderTile(x,y : global byte);
begin
	// Check for player adjacent or blocked
	v := 0; // if v = 4 then all directions blocked
	u := gameGrid[x-1+lookupGridY[y]];
	CheckPlayerOrBlocked();
	u := gameGrid[x+1+lookupGridY[y]];
	CheckPlayerOrBlocked();
	u := gameGrid[x+lookupGridY[y-1]];
	CheckPlayerOrBlocked();
	u := gameGrid[x+lookupGridY[y+1]];
	CheckPlayerOrBlocked();
	// Touching player
	if (playerRecord.isDead = true) then
	begin
		return;
	end;
	// Blocked
	if (v = 4) then
	begin
		// Turn spider into diamond
		gameGrid[x+lookupGridY[y]] := DiamondTile;
		// Check around spider to turn to diamonds
		CheckSpiderDiamonds(x,y);
		return;
	end;
	// Move spider
	u := gameGrid[x+lookupGridY[y]];
	// Spider is 'facing' left
	if (u = SpiderTileLeft) then
	begin
		u := gameGrid[x+lookupGridY[y+1]];
		if (u = BlankTile) then
		begin
			gameGrid[x+lookupGridY[y]] := BlankTile;
			gameGrid[x+lookupGridY[y+1]] := SpiderTileDownScanned;
		end
		else
		begin
			u := gameGrid[x-1+lookupGridY[y]];
			if (u = BlankTile) then begin
				gameGrid[x+lookupGridY[y]] := BlankTile;
				gameGrid[x-1+lookupGridY[y]] := SpiderTileLeft;
			end
			else
			begin
				gameGrid[x+lookupGridY[y]] := SpiderTileRight;
			end;
		end;
	end;
	// Spider is 'facing' down.
	if (u = SpiderTileDown) then
	begin
		u := gameGrid[x+1+lookupGridY[y]];
		if (u = BlankTile) then
		begin
			gameGrid[x+lookupGridY[y]] := BlankTile;
			gameGrid[x+1+lookupGridY[y]] := SpiderTileRightScanned;
		end
		else
		begin
			u := gameGrid[x+lookupGridY[y+1]];
			if (u = BlankTile) then begin
				gameGrid[x+lookupGridY[y]] := BlankTile;
				gameGrid[x+lookupGridY[y+1]] := SpiderTileDownScanned;
			end
			else
			begin
				gameGrid[x+lookupGridY[y]] := SpiderTileUp;
			end;
		end;
	end;
	// Spider is 'facing' right
	if (u = SpiderTileRight) then
	begin
		u := gameGrid[x+lookupGridY[y-1]];
		if (u = BlankTile) then
		begin
			gameGrid[x+lookupGridY[y]] := BlankTile;
			gameGrid[x+lookupGridY[y-1]] := SpiderTileUp;
		end
		else
		begin
			u := gameGrid[x+1+lookupGridY[y]];
			if (u = BlankTile) then begin
				gameGrid[x+lookupGridY[y]] := BlankTile;
				gameGrid[x+1+lookupGridY[y]] := SpiderTileRightScanned;
			end
			else
			begin
				gameGrid[x+lookupGridY[y]] := SpiderTileLeft;
			end;
		end;
	end;
	// Spider is 'facing' down.
	if (u = SpiderTileUp) then
	begin
		u := gameGrid[x-1+lookupGridY[y]];
		if (u = BlankTile) then
		begin
			gameGrid[x+lookupGridY[y]] := BlankTile;
			gameGrid[x-1+lookupGridY[y]] := SpiderTileLeft;
		end
		else
		begin
			u := gameGrid[x+lookupGridY[y-1]];
			if (u = BlankTile) then begin
				gameGrid[x+lookupGridY[y]] := BlankTile;
				gameGrid[x+lookupGridY[y-1]] := SpiderTileUp;
			end
			else
			begin
				gameGrid[x+lookupGridY[y]] := SpiderTileRightScanned;
			end;
		end;
	end;
	
	v := spiderRecord.animationFrame;
	DrawSprite(x, y, SpiderTile+v);
end;

// Draw room tiles to screen
procedure DrawRoomTiles();
begin
	// Update animation frame for all tiles
	// Scan through grid to update states and change sprites
	for y := 0 to gridHeight do
	begin
		for x := 0 to gridWidth do
		begin
			u := gameGrid[x+lookupGridY[y]];
			case u of
				PlayerTile:
					begin
						playerRecord.x := x;
						playerRecord.y := y;
						DrawPlayerTile();
					end;
				DiamondTile:
					begin
						v := diamondRecord.animationFrame;
						DrawSprite(x, y, u+v);
					end;
				SpiderTileLeft:
					begin
						DrawSpiderTile(x,y);					
					end;
				SpiderTileUp:
					begin
						DrawSpiderTile(x,y);					
					end;
				SpiderTileRight:
					begin
						DrawSpiderTile(x,y);
					end;
				SpiderTileDown:
					begin
						DrawSpiderTile(x,y);
					end;
				SnakeTile:
					begin
						DrawSnakeTile();
					end;
				SnakeLeftTile:
					begin
						DrawSnakeTile();
					end;
				SpiderTileDownScanned:
					begin
						gameGrid[x+lookupGridY[y]] := SpiderTileDown;
					end;
			else
			begin
				if (u <> SpiderTileRightScanned) then
				begin
					DrawSprite(x, y, u);					
				end;
			end;
			// Reset SpiderTileRightScanned
			if (y > 0) then begin
				u := gameGrid[x+lookupGridY[y-1]];
				if (u = SpiderTileRightScanned) then begin
					gameGrid[x+lookupGridY[y-1]] := SpiderTileRight;
				end;
			end;
		end;
	end;	
end;