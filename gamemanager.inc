// Reset Player and game stats
procedure ResetGameStats();
begin
	gameStats.room := 20;
	gameStats.score := 32123;
	gameStats.bonus := 400;
	gameStats.keys := 3;
	gameStats.hiScore := 0;
	gameStats.lives := 3;
	gameStats.frameDelayMax := 2;
	
	diamondRecord.animationFrame := 0;
	diamondRecord.frameCount := 3;
	diamondRecord.interFrameDelayMax := 100;
	diamondRecord.interFrameDelay := diamondRecord.interFrameDelayMax;
	diamondRecord.frameDelayMax := 3;
	diamondRecord.points := 1;
	diamondRecord.animatedThisFrame := false;
	
	spiderRecord.animationFrame := 0;
	spiderRecord.frameCount := 1;
	spiderRecord.animatedThisFrame := false;
	
	snakeRecord.animationFrame := 0;
	snakeRecord.frameCount := 3;
	snakeRecord.facingDirection := true;
	snakeRecord.animatedThisFrame := false;
	
	playerRecord.animationFrame := 0;
	playerRecord.frameCount := 2;
	playerRecord.frameDelay := gameStats.frameDelayMax;
	playerRecord.lastDirection := 1;
	playerRecord.facingDirection := true;
	
	
	//Loop();
end;

// Update diamond tile state
procedure UpdateDiamondTile();
begin
		// Update animation
		if (diamondRecord.interFrameDelay > 0) then
		begin
			diamondRecord.interFrameDelay := diamondRecord.interFrameDelay -1;
			return;
		end
		else
		begin
			if (diamondRecord.frameDelay > 0) then
			begin
				diamondRecord.frameDelay := diamondRecord.frameDelay -1;
				return;
			end
			else
			begin
			if (diamondRecord.animatedThisFrame = false) then
				begin
				diamondRecord.animatedThisFrame := true;
				diamondRecord.frameDelay := diamondRecord.frameDelayMax;
				if (diamondRecord.animationFrame = diamondRecord.frameCount) then
				begin
					diamondRecord.interFrameDelay := diamondRecord.interFrameDelayMax;
				end;
				if (diamondRecord.animationFrame < diamondRecord.frameCount) then
				begin
					diamondRecord.animationFrame := diamondRecord.animationFrame +1;
				end
				else
				begin
					diamondRecord.animationFrame := 0;
				end;
			end;
		end;	
	end;
end;

// Update snake tile state - TODO: Shoot player.
procedure UpdateSnakeTile();
begin
	begin
		if (snakeRecord.animatedThisFrame = false) then
		begin
			snakeRecord.animatedThisFrame := true;
			if (snakeRecord.animationFrame < snakeRecord.frameCount) then
			begin
				snakeRecord.animationFrame := snakeRecord.animationFrame +1;
			end
			else
			begin
				snakeRecord.animationFrame := 0;
			end;
		end;
	end;
end;

// Update snake tile state - TODO: Move spider.
procedure UpdateSpiderTile();
begin
	begin
		if (spiderRecord.animatedThisFrame = false) then
		begin
			spiderRecord.animatedThisFrame := true;
			if (spiderRecord.animationFrame < spiderRecord.frameCount) then
			begin
				spiderRecord.animationFrame := spiderRecord.animationFrame +1;
			end
			else
			begin
				spiderRecord.animationFrame := 0;
			end;
		end;
	end;
end;
