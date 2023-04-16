// Draw and manage Title Screen
procedure TitleScreen();
begin
	myp := #mychar;
	mym := #message;
	strlen := Functions::SizeOfString(#message);
	v := 0;
	gameStats.titlePlayed := false;
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
end;
