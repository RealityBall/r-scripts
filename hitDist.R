#
# Function to display fantasy score production densities by handedness and game system
#
hitDist <- function(bat.id="aokin001", date="2014/09/18", lookback=100) {

par(mfrow=c(3,1), mar=c(2.8,3,1,0.2), mgp=c(2,0.8,0), oma=c(1.5,0.2,2.0,0.2), cex=0.6)

sql <- paste("select 
a.id, a.firstName, a.lastName, date, 
sum(fanDuel) as fanDuel, sum(draftKings) as draftKings, sum(draftster) as draftster,
sum(RHfanDuel) as RHfanDuel, sum(RHdraftKings) as RHdraftKings, sum(RHdraftster) as RHdraftster,
sum(LHfanDuel) as LHfanDuel, sum(LHdraftKings) as LHdraftKings, sum(LHdraftster) as LHdraftster
from 
players a, hitterFantasyStats b
where
a.id = '", bat.id, "' and date < '", date, "' and a.id = b.id
group by 
a.id, date
order by 
date desc
limit ", lookback,sep="");

p.dat <- rQ(sql)
bat.name <- paste(unique(p.dat$firstName), unique(p.dat$lastName))

dr.dk <- density(p.dat[p.dat$id==bat.id,]$RHdraftKings)
dr.fd <- density(p.dat[p.dat$id==bat.id,]$RHfanDuel)
dr.ds <- density(p.dat[p.dat$id==bat.id,]$RHdraftster)
dl.dk <- density(p.dat[p.dat$id==bat.id,]$LHdraftKings)
dl.fd <- density(p.dat[p.dat$id==bat.id,]$LHfanDuel)
dl.ds <- density(p.dat[p.dat$id==bat.id,]$LHdraftster)
d.dk <- density(p.dat[p.dat$id==bat.id,]$draftKings)
d.fd <- density(p.dat[p.dat$id==bat.id,]$fanDuel)
d.ds <- density(p.dat[p.dat$id==bat.id,]$draftster)
xlm <- range(dr.dk$x, dr.fd$x, dr.ds$x, dl.dk$x, dl.fd$x, dl.ds$x, d.dk$x, d.fd$x, d.ds$x)
ylm <- range(dr.dk$y, dr.fd$y, dr.ds$y, dl.dk$y, dl.fd$y, dl.ds$y, d.dk$y, d.fd$y, d.ds$y)

plot(dr.dk, xlim=xlm, ylim=ylm, xlab="", ylab="Density", main="")
par(new=TRUE)
plot(dr.fd, xlim=xlm, ylim=ylm, xlab="", ylab="", col="orange", main="")
par(new=TRUE)
plot(dr.ds, xlim=xlm, ylim=ylm, xlab="", ylab="", col="green", main="")
mtext("against Right-handed pitching", cex=0.6)
mtext(paste(bat.name, "-", date, " - ", d.fd$n, "obs"), line=1.8, cex=0.6)

plot(dl.dk, xlim=xlm, ylim=ylm, xlab="", ylab="Density", main="")
par(new=TRUE)
plot(dl.fd, xlim=xlm, ylim=ylm, xlab="", ylab="", col="orange", main="")
par(new=TRUE)
plot(dl.ds, xlim=xlm, ylim=ylm, xlab="", ylab="", col="green", main="")
mtext("against Left-handed pitching", cex=0.6)

plot(d.dk, xlim=xlm, ylim=ylm, xlab="", ylab="Density", main="")
par(new=TRUE)
plot(d.fd, xlim=xlm, ylim=ylm, xlab="", ylab="", col="orange", main="")
par(new=TRUE)
plot(d.ds, xlim=xlm, ylim=ylm, xlab="", ylab="", col="green", main="")
mtext("against all pitching", cex=0.6)
mtext("Fantasy Score", side=1, line=2.8, cex=0.6)

}
