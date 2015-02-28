mydb = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(mydb, "select
a.id,
sum(strikeOuts) as souts,
sum(flyOuts) as fly,
sum(groundOuts) as ground,
sum(strikeOuts + flyOuts + groundOuts) as total
from
pitcherDaily a
where
instr(a.date, '2014') > 0 
group by
a.id
order by total desc");

series = fetch(rs, n=-1);
series <- series[series$total > 99,];
ratios <- data.frame(souts = series$souts / series$total, fly = series$fly / series$total, 
                     ground = series$ground / series$total)
summary(ratios);
sd(ratios$souts);
sd(ratios$fly);
sd(ratios$ground);
