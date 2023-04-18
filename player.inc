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