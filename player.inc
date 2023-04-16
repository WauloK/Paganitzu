// Reset Player and game stats
procedure ResetPlayerGameStats();
begin
	gameStats.room := 20;
	gameStats.score := 32123;
	gameStats.bonus := 400;
	gameStats.keys := 3;
	gameStats.hiScore := 0;
	gameStats.lives := 3;
end;