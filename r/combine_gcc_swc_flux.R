# 
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

# 
gcc.con.df <- readRDS('cache/gcc_control.rds')
gcc.iri.df <- readRDS('cache/gcc_irrigated.rds')
gcc.iri.df$treat <- 'iri'
gcc.con.df$treat <- 'con'
gcc.both.df <- rbind(gcc.con.df,gcc.iri.df)
gcc.both.df$treat <- as.factor(gcc.both.df$treat)

gcc.daily.df <- summaryBy(GCC+RCC~Date+treat,data = gcc.both.df,
                          FUN=median,na.rm=T,keep.names = T)
# 
swc.df <- read.csv('paddock soil moisture.csv')
swc.df$Date <- as.Date(strptime(swc.df$TIMESTAMP,'%Y-%m-%d %H:%M:%S'))
library(doBy)
swc.daily.df <- summaryBy(VWC_Avg~Date + treat,data = swc.df,
                          FUN=median,na.rm=T,keep=T)
swc.daily.df$treat <- as.factor(swc.daily.df$treat)

# 
flux.con.df <- readRDS('E:/repo/paddock_flux/cache/flux_con.rds')
flux.iri.df <- readRDS('E:/repo/paddock_flux/cache/flux_iri.rds')

flux.con.df$treat <- 'con'
flux.iri.df$treat <- 'iri'

flux.both.df <- rbind(flux.con.df,flux.iri.df)
flux.both.df$Date <- as.Date(flux.both.df$TIMESTAMP_TS)
flux.daily.df <- summaryBy(`Fc_raw_Avg_mg/m^2/s` + `Fe_raw_Avg_W/m^2` ~Date + treat,
                           data = flux.both.df[flux.both.df$flag.agc=='1'&flux.both.df$flag.ustar=='1',],
                           FUN=median,na.rm=T,keep.names = T)

names(flux.daily.df) <- c('Date','treat','FC','FE')

flux.daily.df$treat <- as.factor(flux.daily.df$treat)
flux.daily.df$FC[flux.daily.df$FC>10 | flux.daily.df$FC< -3] <- NA
# flux.daily.df$FE[flux.daily.df$FC>10] <- NA
# 
x.range <- range(range(flux.daily.df$Date,na.rm=T),range(swc.df$Date,na.rm=T),range(gcc.both.df$Date,na.rm=T))
x.range <- as.Date(x.range)
date.range = x.range
library(lubridate)
start.day <- paste0(year(x.range[1]),'-',month(x.range[1]),'-','1')
end.day <- paste0(year(x.range[2]),'-',month(x.range[2]),'-','1')
mons.vec =  seq(as.Date(start.day),as.Date(end.day),by='mon')

pdf('flux_gcc_vwc.pdf',width = 6,height = 6*4*0.4)
palette(col.df$auLandscape[c(4,2)])
par(mar=c(5,5,0,1),
    mfrow=c(4,1))

# 
plot(FC~Date,data = flux.daily.df,col=treat,
     pch=16,xlim = x.range,
     xaxt='n',xlab='',ylab=expression(Median~C~Flux~(g~C~m^-2~s^-1)))
abline(h=0,lwd=2,col='grey')
legend('topright',legend = levels(gcc.daily.df$treat),pch=16,col = palette(),bty='n')
# 
abline(v = mons.vec,lty='dotted',col='grey')

# 
plot(c(FE)~Date,data = flux.daily.df,col=treat,
     pch=16,xlim = x.range,
     xaxt='n',xlab='',ylab=expression(Median~LE~Flux~(W~m^-2)),ylim=c(0,500))
# 
abline(v = mons.vec,lty='dotted',col='grey')

# 
plot(GCC~Date,data = gcc.daily.df,col=treat,
     pch=16,ylim=c(0.3,0.5),xlim = x.range,
     xaxt='n',xlab='')
# 
abline(v = mons.vec,lty='dotted',col='grey')
# legend('topright',legend = levels(gcc.daily.df$treat),pch=16,col = palette(),bty='n')
# 
plot(VWC_Avg~Date,data = swc.daily.df,col=treat,
     pch=16,xlim = x.range,
     xaxt='n',xlab='')
# 
abline(v = mons.vec,lty='dotted',col='grey')
# 

mon.c <- format(mons.vec,'%m')
axis(1,at = mons.vec,labels = mon.c)
# mtext('2018',side = 1,adj=0,line = 3)
# mtext('2019',side = 1,adj=0.5,line = 3)
yr.vec <- unique(year(swc.daily.df$Date))
where.c <-which(mon.c =='01') / length(mon.c)
num.yr <- length(where.c)
mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)

dev.off()
