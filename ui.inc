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
			0:
				gameGrid[u] := Room0[u];
			1:
				gameGrid[u] := Room1[u];
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
procedure CheckPlayerSpider();
begin
	if (u = PlayerTile) then
	begin
		playerRecord.isDead := true;
	end;
end;

// Draw Spider tile
procedure DrawSpiderTile();
begin
	// Check for player adjacent
	u := gameGrid[x-1+lookupGridY[y]];
	CheckPlayerSpider();
	u := gameGrid[x+1+lookupGridY[y]];
	CheckPlayerSpider();
	u := gameGrid[x+lookupGridY[y-1]];
	CheckPlayerSpider();
	u := gameGrid[x+lookupGridY[y+1]];
	CheckPlayerSpider();
	if (playerRecord.isDead = true) then
	begin
		return;
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
						DrawSpiderTile();					
					end;
				SpiderTileUp:
					begin
						DrawSpiderTile();					
					end;
				SpiderTileRight:
					begin
						DrawSpiderTile();
					end;
				SpiderTileDown:
					begin
						DrawSpiderTile();
					end;
				SnakeTile:
					begin
						DrawSnakeTile();
					end;
				SnakeLeftTile:
					begin
						DrawSnakeTile();
					end;
			else
			begin
				DrawSprite(x, y, u);
			end;
		end;
	end;	
end;