conn = dbConnect(MySQL(), user='root', password='', dbname='mlbretrosheet', host='localhost');

rs = dbSendQuery(conn, "
select * from hitterFantasyStats where id = 'abreb001' and pitcherIndex= 1 and productionInterval < 20 order by date;
");

series = fetch(rs, n=-1);
dbClearResult(rs)
dbDisconnect(conn)

hist(series$productionInterval, breaks=30);
sum(series$productionInterval > 0) / length(series$productionInterval);

productionRatio <- ddply(series, c("id"), summarize, productionInterval = sum(productionInterval > 0) / length(productionInterval));
