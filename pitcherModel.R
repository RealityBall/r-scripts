
# Compute and report on model predictions
hitReport <- function(x, y) {
  sortedByMl <- data.frame(mlGap = x$mlGap, pitcherPredict = y, win = x$win);
  sortedByMl <- sortedByMl[order(sortedByMl[,1]),];
  lines(sortedByMl$mlGap, sortedByMl$pitcherPredict);
  hits = ((sortedByMl$win == 1) & (sortedByMl$pitcherPredict > 0.5)) | ((sortedByMl$win == 0) & (sortedByMl$pitcherPredict < 0.5))
  sum(hits) / length(hits);
}

mydb = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(mydb, "select a.id, a.game, win, opposing, instr(a.game, b.team) as atHome, c.daynight, c.temp, 
  IF(instr(a.game, b.team) < 1, 
  	IF(e.visitorML < 0, e.visitorML + 100, e.visitorML - 100),
        IF(e.homeML < 0, e.homeML + 100, e.homeML - 100)) as ml, 
	IF(instr(a.game, b.team) = 1, 
		IF(e.visitorML < 0, e.visitorML + 100, e.visitorML - 100),
        IF(e.homeML < 0, e.homeML + 100, e.homeML - 100)) as oppMl from 
	pitcherDaily a, players b, gameConditions c, teams d, gameOdds e
where 
	a.id = 'verlj001' and
    a.id = b.id and
    a.game = c.id and a.game like concat('%', b.year, '%') and
    opposing = d.mnemonic and d.year = b.year and
    d.league = 'A' and
    e.id = a.game;");

rs = dbSendQuery(mydb, "select a.id, a.game, win, opposing, IF(instr(a.game, a.opposing) = 1, 0, 1) as atHome, c.daynight, c.temp, 
  IF(instr(a.game, a.opposing) = 1, 
		IF(e.visitorML < 0, e.visitorML + 100, e.visitorML - 100),
        IF(e.homeML < 0, e.homeML + 100, e.homeML - 100)) / 100.0 as ml, 
	IF(instr(a.game, a.opposing) < 1, 
		IF(e.visitorML < 0, e.visitorML + 100, e.visitorML - 100),
        IF(e.homeML < 0, e.homeML + 100, e.homeML - 100)) / 100.0 as oppMl 
from 
	pitcherDaily a, gameConditions c, gameOdds e
where 
    a.game = c.id and
    e.id = a.game and
    outs > 10
order by a.game;");

universe = fetch(rs, n=-1);

plot(jitter(universe$win, 0.05) ~ universe$ml);
plot(jitter(universe$win, 0.05) ~ universe$oppMl);
universe$mlGap = universe$oppMl - universe$ml;
plot(jitter(universe$win, 0.05) ~ universe$mlGap);

playerName <- 'fierm001';
player = universe[universe$id == playerName,];
player = data.frame(game=player$game, win=player$win, opp=as.factor(player$opposing), home=player$atHome, 
                    daynight=as.factor(player$daynight), temp=player$temp, ml=player$ml, oppMl=player$oppMl, mlGap=player$mlGap);

#
#  Moneyline gap
#
pitcherWinData <- player
pitcherWin <- glm(win ~ mlGap, data = pitcherWinData, family = binomial);
summary(pitcherWin);
pitcherPredict <- predict(pitcherWin, list(mlGap = pitcherWinData$mlGap), type = "response");
plot(player$mlGap, jitter(player$win, 0.1), pch = 16, xlab = "ML Gap", ylab = "Win (1) / Loss (0)");
title(paste("Win/Loss vs. Moneyline Gap for ", playerName));
hitReport(pitcherWinData, pitcherPredict);

#
#  Moneyline gap and opposing team
#
pitcherWinOpp <- glm(win ~ mlGap + opp, data = player, family = binomial);
summary(pitcherWinOpp);
pitcherPredict <- predict(pitcherWinOpp, list(mlGap = player$mlGap, opp = player$opp), type = "response");
plot(player$mlGap, jitter(player$win, 0.1), pch = 16, xlab = "ML Gap", ylab = "Win (1) / Loss (0)");
title(paste("Win/Loss vs. Moneyline Gap + Opposing Team for ", playerName));
hitReport(player, pitcherPredict);

#
#  Player Moneyline and Opposing Pitcher Moneyline
#
pitcherWinMl <- glm(win ~ ml + oppMl, data = player, family = binomial);
summary(pitcherWinMl);
pitcherPredict <- predict(pitcherWinMl, list(ml = player$ml, oppMl = player$oppMl), type = "response");
plot(player$mlGap, jitter(player$win, 0.1), pch = 16, xlab = "ML Gap", ylab = "Win (1) / Loss (0)");
title(paste("Win/Loss vs. Moneyline and Opposing Moneyline for ", playerName));
hitReport(player, pitcherPredict);

#
#  Player Moneyline and Opposing Pitcher Moneyline and Opposing Team
#
pitcherWinMlO <- glm(win ~ ml + oppMl + opp, data = player, family = binomial);
summary(pitcherWinMlO);
pitcherPredict <- predict(pitcherWinMlO, list(ml = player$ml, 
                                              oppMl = player$oppMl,
                                              opp = player$opp), type = "response");
plot(player$mlGap, jitter(player$win, 0.1), pch = 16, xlab = "ML Gap", ylab = "Win (1) / Loss (0)");
title(paste("Win/Loss vs. Moneyline, Opposing Moneyline and Opposing Team for ", playerName));
hitReport(player, pitcherPredict);

#
#  Player Moneyline and Opposing Pitcher Moneyline and Opposing Team and Home
#
pitcherWinMl <- glm(win ~ ml + oppMl + opp + home, data = player, family = binomial);
summary(pitcherWinMl);
pitcherPredict <- predict(pitcherWinMl, list(ml = player$ml, oppMl = player$oppMl,
                                             home = player$home, opp = player$opp), type = "response");
plot(player$mlGap, jitter(player$win, 0.1), pch = 16, xlab = "ML Gap", ylab = "Win (1) / Loss (0)");
title(paste("Win/Loss vs. Moneyline, Opposing Moneyline, Opposing Team and Home for ", playerName));
hitReport(player, pitcherPredict);

# Below is development code
nCharts <- 2
colors <- rainbow(nCharts)
plotchar <- seq(18,18+nCharts,1)
linetype <- c(1:nCharts) 

plot(jitter(player$player.temp), jitter(player$player.win, 0.1));

par(mfrow=c(1,1))
plot(c(-200, 200),c(0.0, 1.0), col=colors[0])
points(data$win ~ data$oppMl, col=colors[1])
target <- lm(data.win ~ data.oppMl, data = dataLogit)
abline(target, col=colors[1])
points(data$win ~ data$ml, col=colors[2])
opp <- lm(data.win ~ data.ml, data = dataLogit)
abline(opp, col=colors[2])

