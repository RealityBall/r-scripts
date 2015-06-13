conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select a.gameId, fanDuel, pa, overUnder, overUnderML from
(select a.gameId, sum(fanDuel) as fanDuel, sum(pa) as pa from hitterFantasyStats a, hitterDailyStats b
where
  a.gameId = b.gameId and
    a.id = b.id and
    a.pitcherIndex = b.pitcherIndex
group by gameId
) a, gameOdds b
where 
	a.gameId = b.id
;");

gameOdds = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

gameOdds$fsPa <- gameOdds$fanDuel / gameOdds$pa

gameOdds[gameOdds$overUnderML < 0,]$overUnderML = gameOdds[gameOdds$overUnderML < 0,]$overUnderML + 100
gameOdds[gameOdds$overUnderML > 0,]$overUnderML = gameOdds[gameOdds$overUnderML > 0,]$overUnderML - 100

summary(gameOdds[gameOdds$overUnder < 8,]$fanDuel)
summary(gameOdds[gameOdds$overUnder > 8,]$fanDuel)

summary(gameOdds[gameOdds$overUnder < 8,]$fsPa)
summary(gameOdds[gameOdds$overUnder > 8,]$fsPa)

summary(gameOdds[gameOdds$overUnder < 8 & gameOdds$overUnderML < -130,]$fsPa)
summary(gameOdds[gameOdds$overUnder > 8 & gameOdds$overUnderML < -130,]$fsPa)

hist(gameOdds$overUnder, 100);
hist(gameOdds$overUnderML, 100);

fit <- lm(fsPa ~ overUnderML + overUnder, data = gameOdds)
summary(fit)

fsPaSum <- summary(gameOdds$fsPa)
green <- (gameOdds$fsPa - fsPaSum[1])/(fsPaSum[6] - fsPaSum[1] + 0.0005)
summary(green)

plot(jitter(gameOdds$overUnder), jitter(gameOdds$overUnderML), col=rgb((colorRamp(c("blue", "red"))(green))/255), pch=19)