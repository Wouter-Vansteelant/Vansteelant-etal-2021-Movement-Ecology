##########################################################
######### DETERMINE NR DAILY OBSERVATIONS 
##########################################################
### produce table with all individual observations per indday
ul <- function(x) length(unique(x))
obsperday <- aggregate(as.numeric(data$dt), by=list(data$indday), FUN=ul)
colnames(obsperday)[1:2] <- c("indday","daily.obs")

##########################################################
######### DETERMINE DAILY TIME RANGE BETWEEN 1ST AND LAST OBS
##########################################################

# calculate time at morning roost as the first location obtained each day
starts.days <- aggregate(as.numeric(data$dt), by=list(data$indday), FUN=min)
colnames(starts.days)[1:2] <- c("indday","daystart")
starts.days$d.first <- as.POSIXct(starts.days$daystart,origin = "1970-01-01", tz = "UTC")

# calculate time at evening roost as the last location obtained each day
ends.days<-aggregate(as.numeric(data$dt), by=list(data$indday), FUN=max)
colnames(ends.days)[1:2] <- c("indday","dayend")
ends.days$d.last <- as.POSIXct(ends.days$dayend, origin = "1970-01-01", tz = "UTC")

# join dataframes containing start and endtimes per day
daytimes <- merge(starts.days,ends.days,by="indday")
daytimes <- merge(daytimes,obsperday,by="indday")

# calculate timerange in hours between first and last observations
daytimes$daily.range <- as.numeric(difftime(daytimes$d.last,daytimes$d.first,units='hours'))
daytimes <- daytimes[,c("indday","d.first","d.last","daily.range","daily.obs")]

# merge start and endtimes to original dataframe
data <- merge(data,daytimes,by="indday", all.x = T)

# calculate cumulative travel distane excluding last fix
na.sum <- function(x) sum(x,na.rm=TRUE)
daycumdist <- aggregate(as.numeric(data$dist.f),by=list(data$indday),FUN='na.sum')
colnames(daycumdist)[1:2] <- c("indday","daily.cum.dist")
data <- merge(data,daycumdist,by="indday",all.x=T)

### determine first and last obs for each travel day
################################################################
# extract locations at start and end of each day
start.locs <- data[which(data$dt== data$d.first),c("indday","long","lat","dt")]
end.locs <- data[which(data$dt == data$d.last),c("indday","long","lat","dt")]
colnames(start.locs)[1:4] <-c("indday","stlon","stlat","d.first")
colnames(end.locs)[1:4] <-c("indday","endlon","endlat","d.last")
daylocs <- merge(start.locs,end.locs,by="indday")

### CALCULATE DAILY DISTANCE AND DIRECTON 
##################################################
daylocs$daily.dist <- deg.dist(daylocs$stlon,daylocs$stlat,daylocs$endlon,daylocs$endlat)*1000
daylocs$daily.dir <- earth.bear(daylocs$stlon,daylocs$stlat,daylocs$endlon,daylocs$endlat)
daylocs$daily.dir  <- ifelse(daylocs$daily.dir > 180, daylocs$daily.dir-360, daylocs$daily.dir)

data <- merge(data,daylocs,all.x=T)
days <- merge(daylocs,daytimes,all.x=T)

data <- data[,!(colnames(data) %in% c("d.first","d.last","stlon","stlat","endlon","endlat"))]

rm(starts.days,ends.days,end.locs,start.locs,daytimes,obsperday,daylocs,daycumdist)
