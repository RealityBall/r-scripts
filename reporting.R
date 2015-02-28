
mydb = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost')
#mydb = dbConnect(MySQL(), user='mlbrsheetuser', password='mlbretrosh', dbname='mlbretrosheet', host='mysql.bustos.org')
dbListTables(mydb)

#rs = dbSendQuery(mydb, "select * from hitterDailyStats where id = 'cabrm001' order by date;")
rs = dbSendQuery(mydb, "select win, opposing, instr(a.game, b.team) as atHome, c.daynight, c.temp from 
pitcherDaily a, players b, gameConditions c
where 
a.id = 'verlj001' and
a.id = b.id and
a.game = c.id;")

data = fetch(rs, n=-1)
dataLogit = data.frame(data$win, as.factor(data$opposing), data$atHome, as.factor(data$daynight), data$temp)

nCharts <- 3
colors <- rainbow(nCharts)
plotchar <- seq(18,18+nCharts,1)
linetype <- c(1:nCharts) 

plot(c(as.Date('2013/04/01'), as.Date('2013/10/31')), c(0, 0.5), type="n", xlab="Date", ylab="Batting Average" ) 
lines(x=as.Date(data$date), y=data$battingAverage, col=colors[1])
lines(x=as.Date(data$date), y=data$RHbattingAverage, col=colors[2])
lines(x=as.Date(data$date), y=data$LHbattingAverage, col=colors[3])
title("Batting Average (2013) - Miguel Cabrera")
# add a legend 
legend("right", c("Overall", "Right Handed Pitchers", "Left Handed Pitchers"), cex=0.8, col=colors,
       pch=plotchar, lty=linetype)


plot(c(as.Date('2013/04/01'), as.Date('2013/10/31')), c(0, 1.0), type="n", xlab="Date", ylab="Slugging Percentage" ) 
lines(x=as.Date(data$date), y=data$sluggingPercentage, col=colors[1])
lines(x=as.Date(data$date), y=data$RHsluggingPercentage, col=colors[2])
lines(x=as.Date(data$date), y=data$LHsluggingPercentage, col=colors[3])
title("Slugging Percentage (2013) - Miguel Cabrera")
# add a legend 
legend("right", c("Overall", "Right Handed Pitchers", "Left Handed Pitchers"), cex=0.8, col=colors,
       pch=plotchar, lty=linetype)

rs = dbSendQuery(mydb, "select 
lineupPosition, substring(date, 1, 4) as year, sum(ab * battingAverage) / sum(ab) as ba, sum(pa * onBasePercentage) / sum(pa) as obp, sum(pa * sluggingPercentage) / sum(pa) as slg
from 
hitterDailyStats 
group by
lineupPosition, substring(date, 1, 4);")
data = fetch(rs, n=-1)

