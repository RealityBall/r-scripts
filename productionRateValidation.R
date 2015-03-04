
conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select 
a.id, date, b.firstName, b.lastName, productionRate,
sum(fanDuel) as fanDuel, sum(draftKings) as draftKings, sum(draftster) as draftster,
sum(RHfanDuel) as RHfanDuel, sum(RHdraftKings) as RHdraftKings, sum(RHdraftster) as RHdraftster,
sum(LHfanDuel) as LHfanDuel, sum(LHdraftKings) as LHdraftKings, sum(LHdraftster) as LHdraftster
from 
hitterFantasyStats a, players b
where
a.id = 'avila001' and a.id = b.id and date < '2014/09/24' and b.year = '2014'
group by 
a.id, date
order by 
date;");

series = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)


nCharts <- 3
colors <- rainbow(nCharts)
plotchar <- seq(18,18+nCharts,1)
linetype <- c(1:nCharts) 

par(mfrow=c(1,1));
plot(c(as.Date('2014/04/01'), as.Date('2014/09/30')), c(0, 10), type="n", xlab="Date", ylab="Running" ) 
lines(x=as.Date(series$date), y=series$productionRate, col=colors[1]);
lines(x=as.Date(series$date), y=series$fanDuel, col=colors[2]);
