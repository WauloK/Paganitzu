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

// Room Tiles Sprite draw common routine
procedure DrawSprite(x, y, q: global byte);
begin
	Sprite::DrawAt(lookupScreenX[x],lookupScreenY[y],q,false);
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
		DrawSprite(playerRecord.x, playerRecord.y, PlayerTile+2+v);
	end;
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
			9:
				gameGrid[u] := Room9[u];
			10:
				gameGrid[u] := Room10[u];
			11:
				gameGrid[u] := Room11[u];
			12:
				gameGrid[u] := Room12[u];
			13:
				gameGrid[u] := Room13[u];
			14:
				gameGrid[u] := Room14[u];
			15:
				gameGrid[u] := Room15[u];
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
