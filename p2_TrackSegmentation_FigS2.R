
### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("RODBC","ggplot2","lubridate","maptools","circular","lattice","fossil","RColorBrewer","rgdal","sp","raster","tidyverse")
ipak(packages)

# load metadata
meta <- read.csv('./Data/Metadata_full-v20201207.csv')

#  if starting from this scrip then load latest data product (resampled, biome-annotated, incl Move Stats) 
#data <- read.csv('./Data/EF-resampled-MoveStats-Biomes-v20210415.csv')

# Format data
#data <- data[,!(colnames(data) %in% c("X","julian","segment","trip.end","trip.start","segment.f","flag","daily.range","daily.obs","daily.dist","daily.dur","daily.dir","burst"))]
#data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
#data$date <- as.Date(data$dt,tz='UTC')
#data$alt <- as.numeric(data$alt)

data <- data[order(data$dev,data$dt),]

#source('pooling_mellone.R')
#data <- rbind(data,mellone)
#data <- merge(data,unique(meta[,c("dev","colony")]),all.x=T)

############################################################
#####     Classify Day vs Night observations            ####     
############################################################
crds <- cbind(data$long,data$lat)
crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

dates <- as.POSIXct(strptime(data$dt,format="%Y-%m-%d"),tz="UTC")

# calculate sunrise times
srise <- sunriset(crds, dates, direction=c("sunrise"),POSIXct.out=TRUE)
colnames(srise)[1:2] <- c("srise.dayfrac","srise.time")

# calculate sunset times
sset <- sunriset(crds, dates, direction=c("sunset"),POSIXct.out=TRUE)
colnames(sset)[1:2] <- c("sset.dayfrac","sset.time")

# append sunrise and sunset in exact times back to dataframe
data$srise_R <- srise[,2]
data$sset_R <- sset[,2]

# calculate solar noon
snoon <- solarnoon(crds,dates,POSIXct.out=TRUE)
colnames(snoon)[1:2] <- c("snoon.dayfrac","snoon.time")

# append solar noon and calculate dt relative to noon
data$snoon_R <- snoon[,2]
data$dt_to_noon <- as.numeric(difftime(data$dt,data$snoon_R,units='hours'))

# calculate srise and sset relative to solar noon
data$srise_to_noon <- as.numeric(difftime(data$srise_R,data$snoon_R,units='hours'))
data$sset_to_noon <- as.numeric(difftime(data$sset_R,data$snoon_R,units='hours'))

# calculate start of astronomical dawn
crep1 <- crepuscule(crds, dates, solarDep=18, direction=c("dawn"),POSIXct.out=TRUE)
colnames(crep1)[1:2] <- c("srise.dayfrac","srise.time")

# calculate end of astronumical dusk
crep2 <- crepuscule(crds, dates, solarDep=18, direction=c("dusk"),POSIXct.out=TRUE)
colnames(crep2)[1:2] <- c("sset.dayfrac","sset.time")

# append sunrise and sunset in exact times back to dataframe
data$crep1_R <- crep1[,2]
data$crep2_R <- crep2[,2]

# calculate astronomical dawn and dusk relative to noon
data$crep1_to_noon <- as.numeric(difftime(data$crep1_R,data$snoon_R,units='hours'))
data$crep2_to_noon <- as.numeric(difftime(data$crep2_R,data$snoon_R,units='hours'))


# classify day and night locations
data$daynight <- ifelse(data$dt > data$crep1_R & data$dt < data$srise_R,'dawn',
                        ifelse(data$dt >= data$srise_R & data$dt < data$sset_R,'day',
                               ifelse(data$dt >= data$sset_R & data$dt < data$crep2_R,'dusk','night')))


# get limits for sunrise and sunset in both seasons
srise <- cbind(srise,snoon$snoon.time)
colnames(srise)[3] <- "snoon.time"
sset <- cbind(sset,snoon$snoon.time)
colnames(sset)[3] <- "snoon.time"
srise$srise.to.noon <- as.numeric(difftime(srise$srise.time,srise$snoon.time,units='hours'))
sset$sset.to.noon <- as.numeric(difftime(sset$sset.time,sset$snoon.time,units='hours'))

srise$trip <- ifelse(month(srise$srise.time) > 7,'out','return')
srise2 <- srise %>%
  group_by(trip) %>%
  summarize(srise.min = min(srise.dayfrac),
            srise.max = max(srise.dayfrac),
            srise.to.noon.min = min(srise.to.noon),
            srise.to.noon.max = max(srise.to.noon)) %>%
  ungroup()

sset$trip <- ifelse(month(sset$sset.time) > 7,'out','return')
sset2 <- sset %>%
  group_by(trip) %>%
  summarize(sset.min = min(sset.dayfrac),
            sset.max = max(sset.dayfrac),
            sset.to.noon.min = min(sset.to.noon),
            sset.to.noon.max = max(sset.to.noon)) %>%
  ungroup()

suntimes <- merge(srise2,sset2)

# Remove obsolete
rm(crds,dates,srise,sset,srise2,sset2,crep1,crep2,snoon)


### DAILY MOVEMENT STATISTICS
################################
source('sidescript_CalcDailyStats-inclCumDist.R')
data$daily.straight <- data$daily.dist/data$daily.cum.dist

### Distinguish travel from rest day sand segment track     ####
################################################################
## Use 100km as threshold for travel
data$travel <- ifelse(data$daily.dist < 100*1000,0,1)

## Segment track in travel vs stop
data <- data[order(data$dev,data$dt),]
data$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$tripID,data$travel))))))))
data$segment <- sprintf("%04d",data$segment)
data$segment <- paste(data$dev,data$segment,sep="_")
data$travel2 <- ifelse(data$daily.dist > 750000,2,data$travel)

### Calculate duration for each stopover event
rm(segs)
source('sidescript_CalcStopovers.R')
segs <- merge(segs,unique(meta[,c("dev","sex","colony")]),all.x=TRUE)

ref <- unique(meta[which(meta$dev %in% unique(data$dev)),c("colony","colony.lat","colony.long")])
segs$dist.to.colony <- deg.dist(long1=ref[[3]],lat1=ref[[2]],long2=segs$st.long,lat2=segs$st.lat)*1000
segs <- subset(segs,segs$dist.to.colony < 8000000 & segs$dist.to.colony >= 1700000)
segs$trip2 <- ifelse(segs$trip == 'out','Autumn','Spring')

write.csv(segs,'EF_resampled_stop_segments-v20210415.csv')

#########################################
#####     MAP NOCTURNAL FLIGHTS       ####     
#########################################

## Set limits for geographical domain
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5)
latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5)

# ref colony
ale.long <- ref$colony.long[1]
ale.lat <- ref$colony.lat[1]

data <- data[order(data$dev,data$dt),]

# produce map
suntimes$trip <- as.factor(suntimes$trip)
suntimes$trip2 <- ifelse(suntimes$trip == 'out','Autumn','Spring')

### CREATE BASE MAP

### CREATE BASE MAP
basemap <- ggplot()+
  geom_raster(data=hdf3[which(is.na(hdf3$biome2)==FALSE),],mapping=aes(long,lat,alpha=alt,fill=biome2))+
  scale_alpha_continuous(name="Elevation",range=c(0.35,1))+
  scale_fill_manual(name="Landscape",values=c("desert"="wheat3","humid forest"="olivedrab4","other"="grey30"))+
  #geom_polygon(data=biomes.df, aes(x = long, y = lat, group = paste(id,piece),fill=factor(BIOME)),alpha=.5) +
  #scale_fill_grey(start=0.2,end=0.8,na.value="white",aesthetics="fill")+
  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='white',fill='transparent',size=.2)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add distance buffers
  geom_polygon(data=buf_1000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_2000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_3000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_4000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_5000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_6000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_7000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_8000km,aes(x=long,y=lat,group=group),size=.4,col='grey50',fill='transparent',linetype='dotted')+
  ## Layout map
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip2)

p1 <- basemap +
    ## Add tracking data
  geom_path(data=data[which(data$travel != 0),],aes(x=long,y=lat,col=daynight,group=segment),size=.5,alpha=.7)+
  scale_colour_manual(values=c("day"='orangered',"dawn"="yellow","dusk"="cornflowerblue","night"="navyblue"))+
  ## Add stopovers
  # geom_point(data=segs,aes(x=st.long,y=st.lat),size=.9,col='black')+
  ## Add star for Alegranza
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
  ## Add stop-overs
  # Add stop-overs
  geom_point(data=segs,aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey30',fill='white',alpha=.4,shape=21)+
  scale_size_binned(name="Stop-over duration [days]",range=c(1,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))+
  geom_point(data=segs,aes(x=st.long,y=st.lat),col='grey30',size=.3)+
  ## Layout map
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        strip.text		    = element_text(size=11,face='bold'),
        strip.background.x = element_blank(),
        legend.position   = c(0.09,0.13),
        legend.direction  = 'vertical',
        legend.spacing.y = unit (0, "cm"),
        legend.title 	    = element_text(size=9,face='bold'),
        legend.text 	    = element_text(size=9),
        legend.background = element_rect(fill='white',colour='black'),
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank())+
  guides(fill = FALSE,
         alpha=FALSE,
         colour = guide_legend(order=1,title ="Solar time",ncol = 2, title.position='top'),
         size = FALSE)

ggsave(plot=p1,filename="./Figures 2021/FigS1_Map_NightTravel-v20210605.tiff",width=8.5,height=5,dpi=300)
dev.off()
rm(p1)


#########################################
#####     EXPLORE TIME BUDGETS      ####     
#########################################

p1 <- ggplot()+
  geom_boxplot(data=data[which(data$travel %in% c(2,1,0)),],
               aes(y=spd.f*3.6,x=factor(round(dt_to_noon)),
                   group=factor(round(dt_to_noon)):factor(travel2),colour=trip),alpha=.1,colour='white')+
  facet_grid(trip2~.)+
  xlab("Time vs. Solar Noon") + ylab("Speed (km/h)") +
  geom_rect(data=suntimes,aes(xmin = -Inf, xmax = factor(round(srise.to.noon.min)),ymin=-Inf,ymax=Inf),fill='grey40',alpha=.2)+
  geom_rect(data=suntimes,aes(xmin = factor(round(srise.to.noon.min)), xmax = factor(round(srise.to.noon.max)),ymin=-Inf,ymax=Inf),fill='orange',alpha=.2)+
  geom_rect(data=suntimes,aes(xmin = factor(round(sset.to.noon.min)), xmax = factor(round(sset.to.noon.max)),ymin=-Inf,ymax=Inf),fill='orange',alpha=.2)+
  geom_rect(data=suntimes,aes(xmin = factor(round(sset.to.noon.max)), xmax = Inf , ymin=-Inf,ymax=Inf),fill='grey40',alpha=.2)+
  geom_boxplot(data=data[which(data$travel %in% c(0,1,2)),],
               aes(y=spd.f*3.6,x=factor(round(dt_to_noon)),fill=factor(travel2),colour=trip,
                   group=factor(round(dt_to_noon)):factor(travel2)),outlier.size=0.2,alpha=.8)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual("Travel km",values=c("1"='grey70',"0"="grey50","2"="grey90"),labels=c("2"=">750","1"="101-750","0"="<100"))+
  theme_bw()+
  scale_x_discrete(limits = as.factor(seq(-12,14,1))) +
  theme(legend.position   = c(0.75,0.93),
        legend.direction  = 'horizontal',
        legend.key = element_rect(fill='transparent'),
        legend.background = element_rect(fill='white',colour='grey20'),
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=11,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=11,face='bold'),
        axis.line         = element_line(size=.4))+
  guides(fill=guide_legend(title.position='top'),
         col=FALSE)

ggsave(plot=p1,filename='./Figures 2021/FigS1_TimeBudgets-ResampledData-v20210421.tiff',width=8,height=5,dpi=300)
dev.off()
rm(p1)

### TIME BUDGETS VS BIOMES
p1 <- ggplot()+
  geom_boxplot(data=data[which(data$travel %in% c(2,1,0) & is.na(data$biome3)==FALSE),],
               aes(y=spd.f*3.6,x=factor(round(dt_to_noon)),
                   group=factor(round(dt_to_noon)):factor(biome3),fill=biome3),colour='grey40',alpha=.1,colour='white')+
  facet_grid(trip2~.)+
  xlab("Time vs. Solar Noon") + ylab("Speed (km/h)") +
  geom_rect(data=suntimes,aes(xmin = -Inf, xmax = factor(round(srise.to.noon.min)),ymin=-Inf,ymax=Inf),fill='grey40',alpha=.2)+
  geom_rect(data=suntimes,aes(xmin = factor(round(srise.to.noon.min)), xmax = factor(round(srise.to.noon.max)),ymin=-Inf,ymax=Inf),fill='orange',alpha=.2)+
  geom_rect(data=suntimes,aes(xmin = factor(round(sset.to.noon.min)), xmax = factor(round(sset.to.noon.max)),ymin=-Inf,ymax=Inf),fill='orange',alpha=.2)+
  geom_rect(data=suntimes,aes(xmin = factor(round(sset.to.noon.max)), xmax = Inf , ymin=-Inf,ymax=Inf),fill='grey40',alpha=.2)+
  geom_boxplot(data=data[which(data$travel %in% c(0,1,2) & is.na(data$biome3)==FALSE),],
               aes(y=spd.f*3.6,x=factor(round(dt_to_noon)),fill=factor(biome3),
                   group=factor(round(dt_to_noon)):factor(biome3)),colour='grey40',outlier.size=0.2,alpha=.8)+
  scale_fill_viridis_d(name='Landscape')+
  theme_bw()+
  scale_x_discrete(limits = as.factor(seq(-12,14,1))) +
  theme(legend.position   = c(0.75,0.93),
        legend.direction  = 'horizontal',
        legend.key = element_rect(fill='transparent'),
        legend.background = element_rect(fill='white',colour='grey20'),
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=11,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=11,face='bold'),
        axis.line         = element_line(size=.4))+
  guides(fill=guide_legend(title.position='top'),
         col=FALSE)

ggsave(plot=p1,filename='./Figures 2021/FigS1_TimeBudgets-vs-Biome-v20210421.tiff',width=8,height=5,dpi=300)
dev.off()
rm(p1)

# hereafter we no longer need suntimes df
#rm(suntimes)

#########################################
#####     PRODUCE KMLS PER BIRD      ####     
#########################################
#data$kml.group <- paste(data$track,data$mth)

#source("NONAME_track2kml.R")

# Produce kml of tracking data concerning migratory movements
# split per year and month for each bird

#data2 <- subset(data,is.na(data$spd)==FALSE)
#data2 <- data2[order(data2$dev,data2$dt),]

#data2$col <- ifelse(data2$travel == 2,'red',ifelse(data2$travel ==1, 'orange','blue'))

#for(i in 1:length(unique(data2$tripID))){
#  t.ss <- subset(data2, tripID == unique(data2$tripID)[i])
#  ss <- t.ss[order(t.ss$dt),]	## data must be ordered on time before generating a kml! ##
#  
#  filename <- as.character(paste("./KMLs/Track",unique(ss$tripID),"_v20201207",sep=''))
#  
#  NONAME.track2kml(
#   latitude=ss$lat, 
#    longitude=ss$long, 
#    altitude=ss$alt, 
#    datetime=ss$dt,
#    col.variable=ss$travel,
#    col.scheme = ss$col,
#    data.variables=data.frame(ss$dev,ss$dt,ss$spd,ss$alt),	# generates an attribute table for every GPS fix including the variables specified here
#    output.filename= filename)	
#    }

#rm(data2,t.ss,ss)

