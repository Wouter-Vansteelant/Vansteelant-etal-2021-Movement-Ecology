## PRODUCES FIG1d-g

### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("rptR","ggthemes","gridExtra","rsq","lmerTest")
ipak(packages)

#####################################
### Summarize performance per trip###  
#####################################
### Calculate summary statistics per trip, which we can use to look at individual consistency
# Variables of interest are
# departure, arrival, restdays, traveldays, total duration, 

trip.summary <- data %>%
  group_by(tripID) %>%
  summarize(trip.start = min(dt, na.rm = TRUE),
            trip.end = max(dt, na.rm = TRUE),
            trip.dur = round(as.numeric(difftime(trip.end,trip.start,unit="days")))) 
trip.summary <- as.data.frame(trip.summary)

lun <- function(x) length(unique(x))
trip.restdays <- data %>%
  filter(travel == 0,
         !(as.character(country) %in% c("Morocco","Western Sahara","Madagascar"))) %>%
  group_by(tripID) %>%
  summarize(trip.restdays = lun(indday)) 
trip.restdays <- as.data.frame(trip.restdays)

trip.traveldays <- data %>%
  filter(travel >= 1) %>%
  group_by(tripID) %>%
  summarize(trip.traveldays = lun(indday),
            trip.cumdist = sum(dist.f,na.rm=TRUE)/1000) 
trip.traveldays <- as.data.frame(trip.traveldays)

trip.summary <- merge(trip.summary,trip.traveldays,all.x=TRUE)
trip.summary <- merge(trip.summary,trip.restdays,all.x=TRUE)
trip.summary$trip.restdays <- ifelse(is.na(trip.summary$trip.restdays)==TRUE,0,trip.summary$trip.restdays)

trip.summary <- merge(trip.summary,unique(data[,c("tripID","dev","trip","yr")]),all.x=TRUE)

trip.summary$trip.start.yday <- yday(trip.summary$trip.start)
trip.summary$trip.end.yday <- yday(trip.summary$trip.end)

rm(trip.traveldays,trip.restdays)

trip.dayhrs <- data %>%
  filter(travel > 0 & spd.f*3.6 >= 5 & daynight == 'day') %>%
  group_by(tripID) %>%
  summarize(trip.dayhrs = sum((dur.f)/(3600),na.rm=TRUE))
trip.dayhrs <- as.data.frame(trip.dayhrs)

trip.nighthrs <- data %>%
  filter(travel > 0 & spd.f*3.6 >= 5 & daynight != 'day') %>%
  group_by(tripID) %>%
  summarize(trip.nighthrs = sum((dur.f)/(3600),na.rm=TRUE))
trip.nighthrs <- as.data.frame(trip.nighthrs)

trip.summary <- merge(trip.summary,trip.dayhrs,all.x=TRUE)
trip.summary <- merge(trip.summary,trip.nighthrs,all.x=TRUE)
trip.summary$trip.prop.night <- trip.summary$trip.nighthrs/(trip.summary$trip.nighthrs+trip.summary$trip.dayhrs)

rm(trip.dayhrs,trip.nighthrs)

## Standardize arrival and departure dates for visualisation purposes
mean.dep.aut <- mean(trip.summary[which(trip.summary$trip == "out"),]$trip.start.yday)
mean.dep.spr <- mean(trip.summary[which(trip.summary$trip == "return"),]$trip.start.yday)
trip.summary$trip.start.rel <- ifelse(trip.summary$trip == 'out',trip.summary$trip.start.yday-mean.dep.aut,trip.summary$trip.start.yday-mean.dep.spr)

mean.arr.aut <- mean(trip.summary[which(trip.summary$trip == "out"),]$trip.end.yday)
mean.arr.spr <- mean(trip.summary[which(trip.summary$trip == "return"),]$trip.end.yday)
trip.summary$trip.end.rel <- ifelse(trip.summary$trip == 'out',trip.summary$trip.end.yday-mean.arr.aut,trip.summary$trip.end.yday-mean.arr.spr)

mean.dur.aut <- mean(trip.summary[which(trip.summary$trip == "out"),]$trip.dur)
mean.dur.spr <- mean(trip.summary[which(trip.summary$trip == "return"),]$trip.dur)
trip.summary$trip.dur.rel <- ifelse(trip.summary$trip == 'out',trip.summary$trip.dur-mean.dur.aut,trip.summary$trip.dur-mean.dur.spr)

# Annotate cycle of migration
trip.summary$cycle <- ifelse((trip.summary$trip == "out" & trip.summary$yr == 2012) | (trip.summary$trip == "return" & trip.summary$yr == 2013),"2012-2013",
                             ifelse((trip.summary$trip == "out" & trip.summary$yr == 2013) | (trip.summary$trip == "return" & trip.summary$yr == 2014),"2013-2014",
                                    ifelse((trip.summary$trip == "out" & trip.summary$yr == 2014) | (trip.summary$trip == "return" & trip.summary$yr == 2015),"2014-2015",
                                           ifelse((trip.summary$trip == "out" & trip.summary$yr == 2015) | (trip.summary$trip == "return" & trip.summary$yr == 2016),"2015-2016",
                                                  ifelse((trip.summary$trip == "out" & trip.summary$yr == 2016) | (trip.summary$trip == "return" & trip.summary$yr == 2017),"2016-2017",
                                                         ifelse((trip.summary$trip == "out" & trip.summary$yr == 2017) | (trip.summary$trip == "return" & trip.summary$yr == 2018),"2017-2018","2018-2019"))))))


 
cul <- function(x)length(unique(x))

trip.summary <- merge(trip.summary,unique(data[,c("tripID","dev","trip")]),all.x=TRUE)

ind.summ <- trip.summary %>%
  group_by(dev,trip) %>%
  summarize(n.trips = cul(tripID),
            mean.trip.end.rel = mean(trip.end.rel),
            sd.trip.end.rel = sd(trip.end.rel),
            mean.trip.start.rel = mean(trip.start.rel),
            sd.trip.start.rel = sd(trip.start.rel),
            mean.trip.end = mean(trip.end.yday),
            sd.trip.end = sd(trip.end.yday),
            mean.trip.start = mean(trip.start.yday),
            sd.trip.start = sd(trip.start.yday),
            mean.trip.dur = mean(trip.dur),
            sd.trip.dur = sd(trip.dur),
            mean.trip.cumdist = mean(trip.cumdist),
            sd.trip.cumdist = sd(trip.cumdist),
            mean.trip.restday = mean(trip.restdays),
            sd.trip.restday = sd(trip.restdays),
            mean.trip.traveldays = mean(trip.traveldays),
            sd.trip.traveldays = sd(trip.traveldays),
            mean.trip.dayhrs = mean(trip.dayhrs),
            sd.trip.dayhrs = sd(trip.dayhrs),
            mean.trip.nighthrs = mean(trip.nighthrs),
            sd.trip.nighthrs = sd(trip.nighthrs)) 
ind.summ <- as.data.frame(ind.summ)

ind.summ <- merge(ind.summ,unique(meta[,c("dev","sex","age")]))
#write.csv(ind.summ,'./Figures 2021/TableS1_SummaryStats-perID&Season-v20210105.csv')

data$md <- as.character(format(data$dt,"%m-%d"))
ydays <- unique(data[which(data$yr == 2018),c("yday","md")])
#write.csv(ydays,'./Final Figures/yday-ref.csv')

starts <- merge(data,trip.summary[,c("tripID","trip.start","trip.end")],all.x=TRUE)
p1s <- starts[which(starts$dt == starts$trip.start),c("tripID","dev","lat","long")]
p2s <- starts[which(starts$dt == starts$trip.end),c("tripID","dev","lat","long")]
colnames(p1s)[3:4]<-c("start.lat",'start.long')
colnames(p2s)[3:4]<-c("end.lat",'end.long')
detours <- merge(p1s,p2s,by=c("tripID","dev"))
detours$shortest <- deg.dist(long1=detours$start.long,long2=detours$end.long,lat1=detours$start.lat,lat2=detours$end.lat)
detours <- merge(detours,unique(data[,c("tripID","trip")]),all.x=TRUE)

detours <- merge(detours,trip.summary[,c("tripID","trip.cumdist")])
detours$detour <- detours$trip.cumdist/detours$shortest

ind.detours<- detours %>%
  group_by(dev,trip) %>%
  summarize(mean.detour = mean(detour),
            sd.detour=sd(detour))

#write.csv(ind.detours,'./Figures 2021/detours-v20210105.csv')

trip.summary <- merge(trip.summary,detours,all.x=TRUE)
ind.summ <- merge(ind.summ,ind.detours,all.x=TRUE)
rm(ind.detours,detours,p1s,p2s,starts,ydays)

rm(mean.arr.aut,mean.dep.aut,mean.arr.spr,mean.dep.spr,mean.dur.spr,mean.dur.aut)
