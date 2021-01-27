library(lubridate)
soil.fn.vec <- list.files(path='data/swc/Control/slow/',pattern = 'FluxCon_soil_vars',full.names = T)

tmp.ls <- list()
for(i in seq_along(soil.fn.vec)){
  tmp.df <- read.table(soil.fn.vec[i],header=T,skip=1,sep=',')
  
  tmp.ls[[i]] <- tmp.df[c(-1,-2),]
}
swc.con.df <- do.call(rbind,tmp.ls)
swc.con.df$treat <- 'control'

# 
soil.fn.vec.irrig <- list.files(path='data/swc/Irrig/slow/',pattern = 'FluxIrr_soil_vars',full.names = T)

tmp.ls.irrig <- list()
for(i in seq_along(soil.fn.vec.irrig)){
  tmp.df <- read.table(soil.fn.vec.irrig[i],header=T,skip=1,sep=',')
  
  tmp.ls.irrig[[i]] <- tmp.df[c(-1,-2),]
}
swc.irg.df <- do.call(rbind,tmp.ls.irrig)
swc.irg.df$treat <- 'irrigated'

swc.paddock.df <- rbind(swc.con.df,swc.irg.df)

# 
swc.paddock.df$TIMESTAMP <- strptime(swc.paddock.df$TIMESTAMP,format = '%Y-%m-%d %H:%M:%S')
swc.paddock.df$TIMESTAMP <-as.POSIXct(swc.paddock.df$TIMESTAMP)

swc.paddock.df$VWC_Avg <- as.numeric(swc.paddock.df$VWC_Avg)
swc.paddock.df$VWC_Avg[swc.paddock.df$VWC_Avg >=0.6] <-NA
swc.paddock.df$VWC_Avg[swc.paddock.df$VWC_Avg ==0] <-NA
swc.paddock.df$treat <- as.factor(swc.paddock.df$treat)
swc.paddock.df <- swc.paddock.df[!is.na(swc.paddock.df$TIMESTAMP),]
# 
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
palette(col.df$flower)
plot(VWC_Avg~TIMESTAMP,data = swc.paddock.df,col=treat,pch=16,xaxt='n',xlab='')
legend('topright',legend=levels(swc.paddock.df$treat),pch=16,col=palette())

# plot date
date.range = range(swc.paddock.df$TIMESTAMP,na.rm=T)
mons.vec =  seq(date.range[1],date.range[2],by='mon')

mon.c <- format(mons.vec,'%m')
axis(1,at = mons.vec,labels = mon.c)
# mtext('2018',side = 1,adj=0,line = 3)
# mtext('2019',side = 1,adj=0.5,line = 3)
yr.vec <- unique(year(swc.paddock.df$TIMESTAMP))
where.c <-which(mon.c =='01') / length(mon.c)
num.yr <- length(where.c)
if(num.yr>0){
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
}

