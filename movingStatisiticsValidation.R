mydb = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(mydb, "select date, dailyBattingAverage from hitterDailyStats where id = 'cabrm001' order by date;");
series = fetch(rs, n=-1);
cleanSeries <- series[!is.na(series$dailyBattingAverage),];
rsMoving = dbSendQuery(mydb, "select date, battingAverageMov from hitterMovingStats where id = 'cabrm001' order by date")
movSeries = fetch(rsMoving, n=-1);
movSeries <- movSeries[!is.na(series$dailyBattingAverage),];
rsVolMoving = dbSendQuery(mydb, "select date, battingVolatility from hitterVolatilityStats where id = 'cabrm001' order by date")
volSeries = fetch(rsVolMoving, n=-1);
volSeries <- volSeries[!is.na(series$dailyBattingAverage),];

cleanSeries$filtered <- filter(timeSeries, rep(1.0 / 25.0, 25), sides = 1);
cleanSeries$sd <- ts(runSD(cleanSeries$dailyBattingAverage, n=100));

movSeries$filteredBattingAverageMov = filter(movSeries$battingAverageMov, rep(1.0 / 25.0, 25), sides = 1);
baMovSD = runsd (movSeries$battingAverageMov, 100, align="right")
plot(ts(movSeries$battingAverageMov));
lines(movSeries$filteredBattingAverageMov, col = colors[3]);
lines(baMovSD, col = colors[1]);

colors <- rainbow(3)

plot(cleanSeries$filtered);
plot(ts(movSeries$battingAverageMov));
lines(ts(movSeries$battingAverageMov), col = colors[1]);

plot(cleanSeries$sd);
lines(ts(volSeries$battingVolatility), col = colors[1]);

timeSeries <- ts(series$dailyBattingAverage);
plot(timeSeries);

plot(c(as.Date('2010/04/01'), as.Date('2014/10/31')), c(0, 0.5), type="n", xlab="Date", ylab="Batting Average" ) ;
lines(x=as.Date(series$date), y=series$filtered, col=colors[1]);
plot(filter(timeSeries, rep(1.0 / 25, 25), sides = 1));
plot(series$filtered);
