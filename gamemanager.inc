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
	diamondRecord.frameDelayMax := 30;
	diamondRecord.frameDelay := diamondRecord.frameDelayMax;
	diamondRecord.points := 1;
	
	spiderRecord.animationFrame := 0;
	spiderRecord.frameCount := 1;
	
	snakeRecord.animationFrame := 0;
	snakeRecord.frameCount := 3;
	snakeRecord.facingDirection := true;
	
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
	if (diamondRecord.frameDelay > 1) then
	begin
		diamondRecord.frameDelay := diamondRecord.frameDelay -1;
		return;
	end
	else
	begin
		if (diamondRecord.animationFrame < diamondRecord.frameCount) then
		begin
			diamondRecord.animationFrame := diamondRecord.animationFrame +1;
		end
		else
		begin
			diamondRecord.animationFrame := 0;
			diamondRecord.frameDelay := diamondRecord.frameDelayMax;
		end;
	end;
end;

// Update snake tile state - TODO: Shoot player.
procedure UpdateSnakeTile();
begin
	begin
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

// Update snake tile state - TODO: Move spider.
procedure UpdateSpiderTile();
begin
	begin
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