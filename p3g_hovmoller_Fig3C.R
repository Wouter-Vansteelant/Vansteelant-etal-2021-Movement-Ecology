
### READ NON-MIGRATION DATA (MAPPING ONLY)
###########################################
nonmig <- read.csv('./Data/EF_SummerWinter-v20210105.csv')
nonmig <- nonmig[,!(colnames(nonmig) %in% c("X","julian","segment","trip.end","trip.start","segment.f","flag","daily.range","daily.obs","daily.dist","daily.dur","daily.dir"))]
nonmig$dt <- as.POSIXct(strptime(nonmig$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
nonmig$date <- as.Date(nonmig$dt)
nonmig$alt <- as.numeric(nonmig$alt)
## order dataframe chronologically per device
nonmig<-nonmig[order(nonmig$tripID,nonmig$dt),]

# remove faulty fixes over Indian Ocean in winter
nonmig <- subset(nonmig,!(nonmig$long > 50.1 & is.na(nonmig$country == TRUE)))

# create grouping variable for drawing movement paths
nonmig$group <- paste(nonmig$dev,nonmig$cycle,nonmig$phase,nonmig$mth)
nonmig <- nonmig[order(nonmig$group,nonmig$dt),]


### prep stop-over data
###########################################
# Annotate cycle of migration
segs$cycle <- ifelse(as.Date(segs$st.start) < as.Date("2013-07-01"),"2012-13",
                     ifelse(as.Date(segs$st.start)< as.Date("2014-07-01"),"2013-14",
                            ifelse(as.Date(segs$st.start) < as.Date("2015-07-01"),"2014-15",
                                   ifelse(as.Date(segs$st.start)< as.Date("2016-07-01"),"2015-16",
                                          ifelse(as.Date(segs$st.start) < as.Date("2017-07-01"),"2016-17",
                                                 ifelse(as.Date(segs$st.start) < as.Date("2018-07-01"),"2017-18",
                                                        ifelse(as.Date(segs$st.start) < as.Date("2019-07-01"),"2018-19",
                                                               ifelse(as.Date(segs$st.start) < as.Date("2020-07-01"),"2019-20","2020-2021"))))))))

# remove cycle for which only summer data were recorded
cycle.starts <- segs %>%
  group_by(cycle) %>%
  summarize(cycle.start = as.Date(paste(min(yr),"-07-01 00:00:01",sep=''))) 
cycle.starts <- as.data.frame(cycle.starts)

segs <- merge(segs,cycle.starts,all.x=TRUE)

segs$cycle.time <- as.numeric(difftime(segs$st.start,segs$cycle.start,units="days"))


### EXTRACT PRECIPITATION DATA
########################################################
humid.fields <- NCEP.gather(variable = 'prate.sfc', level = 'gaussian', months.minmax = c(1,12), years.minmax = c(2013,2020), lat.southnorth = c(-20,30), lon.westeast = c(-20,55), reanalysis2 = TRUE, return.units = FALSE, status.bar=TRUE)
humids <- NCEP.array2df(humid.fields, var.names=NULL)
colnames(humids)[4] <- "humid"

humids$humid <- humids$humid * 3600 * 6
colnames(humids)[1:3] <- c("dt","lat","long")

humids$crds <- paste(humids$long,humids$lat,sep='_')

# clip grid points to africa
countries <- shapefile("./Maps/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
africa <- countries[countries$continent %in% c("Africa"),]

hum2 <- unique(humids[,c("long","lat")])
crds <- cbind(hum2$long,hum2$lat)
crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

crds <- spTransform(crds, CRS(proj4string(africa))) # transform CRS
crds_subset <- crds[africa, ]
crds <- as.data.frame(crds_subset) 
colnames(crds) <- c("long","lat")
crds$crds <- paste(crds$long,crds$lat,sep="_")
in.africa <- unique(crds$crds)

rain.subset <- subset(humids,humids$crds %in% in.africa)
rm(hum2,crds,crds_subset,humids)
humids <- rain.subset
rm(rain.subset)

# extract time variables
humids$yr <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\1", humids$dt)
humids$mth <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\2", humids$dt)
humids$day <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\3", humids$dt)
humids$hr <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\4", humids$dt)

humids$date <- as.Date(paste(humids$yr,humids$mth,humids$day,sep='-'),tz='UTC')
humids$yday <- yday(humids$date)

# daily rain per date and gird point
rain.aggregate.daily<- humids %>%
  group_by(lat,long,date,yday,yr) %>%
  summarize(sum_hum = sum(humid, na.rm = TRUE)) 
rain.aggregate.daily<- as.data.frame(rain.aggregate.daily)

rain.aggregate.daily$cycle <- ifelse(rain.aggregate.daily$date < as.Date("2013-07-01"),"2012-13",
                     ifelse(rain.aggregate.daily$date< as.Date("2014-07-01"),"2013-14",
                            ifelse(rain.aggregate.daily$date < as.Date("2015-07-01"),"2014-15",
                                   ifelse(rain.aggregate.daily$date< as.Date("2016-07-01"),"2015-16",
                                          ifelse(rain.aggregate.daily$date < as.Date("2017-07-01"),"2016-17",
                                                 ifelse(rain.aggregate.daily$date< as.Date("2018-07-01"),"2017-18",
                                                        ifelse(rain.aggregate.daily$date< as.Date("2019-07-01"),"2018-19",
                                                               ifelse(rain.aggregate.daily$date < as.Date("2020-07-01"),"2019-20","2020-21"))))))))

# remove cycle for which only summer data were recorded
cycle.starts <- rain.aggregate.daily %>%
  group_by(cycle) %>%
  summarize(cycle.start = as.Date(paste(min(yr),"-07-01 00:00:01",sep=''))) 
cycle.starts <- as.data.frame(cycle.starts)
cycle.starts[which(cycle.starts$cycle =="2012-13"),]$cycle.start <- as.Date("2012-07-01")

rain.aggregate.daily <- merge(rain.aggregate.daily,cycle.starts,all.x=TRUE)

rain.aggregate.daily$cycle.time <- as.numeric(difftime(rain.aggregate.daily$date,rain.aggregate.daily$cycle.start,units="days"))

# mean daily rain per latitude and yday
rain.aggregate.year <- rain.aggregate.daily %>%
  group_by(lat,cycle.time) %>%
  summarize(mean_hum = mean(sum_hum, na.rm = TRUE)) 
rain.aggregate.year<- as.data.frame(rain.aggregate.year)

# order tracking data
data <- data[order(data$dev,data$dt),]


library(scales)
#cols <- c("orangered","navyblue")
#min<- min(data$daily.dist,na.rm=TRUE)/1000
#max<- max(data$daily.dist,na.rm=TRUE)/1000

cols <- brewer.pal(n = 5, name = "RdYlBu")

ggplot() +
  geom_tile(data=rain.aggregate.year,aes(x=cycle.time,y=lat,fill=mean_hum))+
  scale_fill_gradientn(colors=brewer.pal(n = 5, name = "Greys"),
                       na.value="grey20",aesthetics="fill",
                       values = rescale(c(0,2,5,7,14)),
                       limits=c(0, 16))+
  geom_path(data=nonmig[which(nonmig$lat < 0),],aes(x=cycle.time,y=lat,group=paste(tripID,phase)),col='darkgoldenrod4',size=.1,alpha=.4) +
  geom_path(data=data[which(data$daily.travel < 25),],aes(x=cycle.time,y=lat,group=segment,col=(tailwind.r)),size=.4) +
## colour scale for wind gain index
# scale_colour_gradientn(colours = cols, 
#                         values = rescale(c(-2, -1, -0.25, 0, 0.25, 1, 2)),
#                         guide = "colorbar", limits=c(-2, 2)) +
  ## colour scale for realized tailwind
  scale_colour_gradientn(colours = cols, 
                         values = rescale(c(-20/3.6, -5/3.6, 0, 5/3.6, 20/3.6)),
                         limits=c(-50/3.6, 50/3.6)) +
  ## add stop-overs
  geom_point(data=segs,aes(x=cycle.time,y=st.lat),col='black',size=.4)+
  ## layout graph and axes
  theme_bw()+ xlab("Date") + 
  scale_x_continuous(breaks=c(62,92,123,153,184,215,243,274,304,335),labels=c("Sept","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"),limits=c(60,362),expand=c(0,0))+
  scale_y_continuous(breaks = nsbrks, labels = nslbls, limits = c(-22,32.5), expand = c(0, 0)) +
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.09,0.21),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=8,face='bold'),
        legend.text 	    = element_text(size=6,face='bold'),
        legend.background    = element_rect(fill='white'),
        legend.spacing.y  = unit(0,"cm"),
        axis.text		      = element_text(size=10),
        axis.title.x		    = element_text(size=11,face='bold'),
        axis.title.y    = element_blank())+
  ## layout legend
  guides(fill=guide_colorbar(title =expression(paste(bold("Rainfall (mm day"^-1,")"),bold(")"))),
                             title.position='top'),
        # colour=guide_colorbar(title ="Local Wind\nGain Index (w)",
        #                       title.position='top'),
         colour=guide_colorbar(title =expression(paste(bold("Tailwind (ms"^-1,")"),bold(")"))),
                               title.position='top'),
         linetype=FALSE)

ggsave('./Figures 2021/Fig2B_Tracks-vs-Hovmollerb-v20210421.tiff',width=8.5,height=3.5,dpi=300)

rm(rain.aggregate.year,rain.aggregate.daily,cycle.starts)
