mydb = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(mydb, "select
a.id,
sum(RHstrikeOut + LHstrikeOut) as souts,
sum(RHflyBall + LHflyBall) as fly,
sum(RHgroundBall + LHgroundBall) as ground,
sum(RHbaseOnBalls + LHbaseOnBalls + RHhitByPitch + LHhitByPitch) as baseOnBalls,
sum(RHstrikeOut + LHstrikeOut + RHflyBall + LHflyBall+ RHgroundBall + LHgroundBall+ RHbaseOnBalls + LHbaseOnBalls + RHhitByPitch + LHhitByPitch) as total
from
hitterRawLHstats a, hitterRawRHstats b
where
a.id = b.id and a.gameId = b.gameId and instr(a.date, '2012') > 0 
group by
a.id
order by total desc");

series = fetch(rs, n=-1);
series <- series[series$total > 99,];
ratios <- data.frame(souts = series$souts / series$total, fly = series$fly / series$total, 
                     ground = series$ground / series$total, bb = series$baseOnBalls / series$total)
summary(ratios);
sd(ratios$souts);
sd(ratios$fly);
sd(ratios$ground);
sd(ratios$bb);
