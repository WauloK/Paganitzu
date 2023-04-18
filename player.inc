// Reset Player and game stats
procedure ResetGameStats();
begin
	gameStats.room := 20;
	gameStats.score := 32123;
	gameStats.bonus := 400;
	gameStats.keys := 3;
	gameStats.hiScore := 0;
	gameStats.lives := 3;
	
	diamondRecord.animationFrame := 0;
	diamondRecord.interFrameDelay := diamondRecord.interFrameMax;
	diamondRecord.frameDelay := diamondRecord.frameDelayMax;
	
	spiderRecord.animationFrame := 0;
	spiderRecord.frameDelay := spiderRecord.frameDelayMax;
	
	snakeRecord.animationFrame := 0;
	snakeRecord.frameDelay := snakeRecord.frameDelayMax;
	snakeRecord.facingDirection := true;
	
	playerRecord.animationFrame := 0;
	playerRecord.frameDelay := playerRecord.frameDelayMax;
	playerRecord.lastDirection := 1;
	playerRecord.facingDirection := true;
end;