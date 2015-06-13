# Correlation of $salary vs prediction
# Correlation of $salary vs actual

# Rank order, top 4 in each position, randomize

# Randomized line ups
#   

conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select a.date, a.fanDuelSalary, b.mlbComId, fanDuel from 
rotoGuruDailySalary a, rotoGuruPlayers b,
hitterFantasyStats c, idMapping d
where
a.rotoGuruId = b.rotoGuruId and
b.mlbComId = d.mlbId and
c.id = d.retroId and
c.date = a.date and
d.mlbPos <> 'P' and
a.fanDuelSalary > 0.0
                 ;");

salaries = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

plot(jitter(salaries$fanDuelSalary, 75), jitter(salaries$fanDuel))
boxplot(salaries$fanDuel ~ salaries$fanDuelSalary,
        xlab = "Salary ($)", ylab = "FanDuel Points",
        main = "FanDuel Points vs FanDuel Salary (2014)"
)

salaries$production = rep(1.0, nrow(salaries));
salaries$production[salaries$fanDuel <= 1.0] = 0.0;
salaries$value = salaries$production / salaries$fanDuelSalary;
plot(density(salaries$value))
seriesSalary <- series$fanDuelSalary
seriesSalary[seriesSalary == 0.0] = 1000000.0;
seriesValue <- series$production / seriesSalary
plot(density(seriesValue));
lines(plot(density(salaries$value)), col=2)

