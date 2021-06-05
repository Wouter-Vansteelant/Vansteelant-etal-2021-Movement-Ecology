### Set Working directory
#setwd("C:/Users/wvanste1/Documents/2019 - Eleonoras Falcon/analyses/")

### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("geosphere","RODBC","ggplot2","lubridate","maptools","circular","lattice","fossil","RColorBrewer","rgdal","sp","raster","tidyverse")
ipak(packages)

#rm(list= ls()[!(ls() %in% c('ndvi_aut','ndvi_spr',"nonmig"))])

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

### READ MIGRATION DATA (TO BE RESAMPLED)
##########################################
data <- read.csv('./Data/EF_migration-v20210105.csv')
data <- data[,!(colnames(data) %in% c("X","julian","segment","trip.end","trip.start","segment.f","flag","daily.range","daily.obs","daily.dist","daily.dur","daily.dir"))]
data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
data$date <- as.Date(data$dt)
data$alt <- as.numeric(data$alt)
## order dataframe chronologically per device
data<-data[order(data$tripID,data$dt),]

### RESAMPLE MIGRATION DATA
##########################################
## read resample function
source('Rfunction_resampling_movement.R')
data_resample2<-moveTimeSample(data,data$dt,data$tripID,60,10,subset=TRUE)

# Check dresampling by counting nr of observations per day
ul <- function(x) length(unique(x))
obsperday <- aggregate(as.numeric(data_resample2$dt), by=list(data_resample2$indday), FUN=ul)
colnames(obsperday)[1:2] <- c("indday","daily.obs")

data <- data_resample2
rm(data_resample2)

# remove faulty points 
data <- subset(data,!(data$lat <1 & data$long < 0))

# create same grouping variable for drawing paths as made for nonmig dataset
data$group <- paste(data$dev,data$cycle,data$phase,data$mth)
data <- data[order(data$group,data$dt),]


#########################################
#####     MOVEMENT STATISTICS        ####     
#########################################
# order dataframe
data<-data[order(data$tripID,data$dt),]

source('sidescript_pt2pt_fxns.R')
## define functions to calculate forward distances and duration
calcdist <- function(x) pt2pt.distance(longitude=x$long,latitude=x$lat)
calcdur <- function(x) pt2pt.duration(datetime=x$dt)
## define functions to calculate backward distances and duration
calcdist.b <- function(x) pt2pt.back.distance(longitude=x$long,latitude=x$lat)
calcdur.b <- function(x) pt2pt.back.duration(datetime=x$dt)

## We must order the dataframe in order to ensure the correct application of our coming functions
data <- data[order(data$tripID,data$dt),]
v1 <- lapply(split(data,data$tripID),"calcdist")
v2 <- lapply(split(data,data$tripID),"calcdur")
v1b <- lapply(split(data,data$tripID),"calcdist.b")
v2b <- lapply(split(data,data$tripID),"calcdur.b")

data$dist.f <- as.numeric(unlist(v1))
data$dur.f <- as.numeric(unlist(v2))
data$spd.f <- data$dist.f/data$dur.f
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

data$spd <- (data$spd.f + data$spd.b) / 2

data <- data[which(data$spd < 30),]

## We must order the dataframe in order to ensure the correct application of our coming functions
data <- data[order(data$tripID,data$dt),]
v1 <- lapply(split(data,data$tripID),"calcdist")
v2 <- lapply(split(data,data$tripID),"calcdur")
v1b <- lapply(split(data,data$tripID),"calcdist.b")
v2b <- lapply(split(data,data$tripID),"calcdur.b")

data$dist.f <- as.numeric(unlist(v1))
data$dur.f <- as.numeric(unlist(v2))
data$spd.f <- data$dist.f/data$dur.f
data$dist.b <- as.numeric(unlist(v1b))
data$dur.b <- as.numeric(unlist(v2b))
data$spd.b <- data$dist.b/data$dur.b

data$spd <- (data$spd.f + data$spd.b) / 2

calcdir <- function(x) pt2pt.direction(longitude=x$long,latitude=x$lat)
v3 <- lapply(split(data,data$tripID),"calcdir")
data$dir <- as.numeric(unlist(v3))

rm(v1,v2,v3,v1b,v2b)

write.csv(data,paste('./Data/EF-resampled-MoveStats-v20210415.csv',sep=''))

### READ STOP-OVER DATA 
##########################################
segs <- read.csv('./Data/EF_migration_stop-segments-v20210105.csv')
segs$st.start <- as.POSIXct(strptime(segs$st.start, format="%Y-%m-%d %H:%M:%S"), tz='UTC')
segs$st.end <- as.POSIXct(strptime(segs$st.end, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

### PREPARE ENVIRONMENTAL DATA LAYERS
##########################################
## Set limits for geographical domain
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5)
latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5)

# country boundaries
countries <- readOGR("~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- countries[countries$continent %in% c("Africa","Eiurope") | 
                         countries$admin %in% c("Spain","Italy","Greece","Portugal","Malta","Iran","Georgia",
                                                "Northern Cyprus","Turkey","Syria","Lebanon","Jordan","Iraq",
                                                "Israel","Palestine","Yemen","Saudi Arabia","Kuwait","United Arab Emirates"),]

political <- fortify(countries, region = "admin")
political$country <- political$id
political$group <- ifelse(political$country %in% unique(data$country) & political$piece == 1,1,0)

## Read biomes
#library(dplyr)
#biomes <- readOGR("./Maps/BIOMES/wwf_terr_ecos.shp")
#biomes@data$id = rownames(biomes@data)
#biomes.points = fortify(biomes, region="id")
#biomes.df = merge(biomes.points, biomes@data, by="id")
#rm(biomes.points,biomes)
biomes <- readOGR("~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/BIOMES/wwf_terr_ecos.shp")

## Read lakes
lakes <- shapefile("./Maps/ne_50m_lakes/ne_50m_lakes")

### Read NDVI maps
ndvi_autumn <- raster("~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/ndvi/Greenest_pixel_composite_autumn-fullb.tif")
ndvi_aut = projectRaster(ndvi_autumn, crs = proj4string(lakes), method = "bilinear")

ndvi_spring <- raster("~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/ndvi/Greenest_pixel_composite_spring-fullb.tif")
ndvi_spr = projectRaster(ndvi_spring, crs = proj4string(lakes), method = "bilinear")

rm(ndvi_autumn,ndvi_spring)

### Read DEM
dem <- raster('~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/alt_30s_bil/alt.bil')
dem2 <- crop(dem, extent(longlimits[1],longlimits[2],latlimits[1],latlimits[2]))
dem3 <- aggregate(dem2,5,FUN='mean')
hdf2 <- rasterToPoints(dem3)
hdf2 <- data.frame(hdf2)
colnames(hdf2) <- c("long","lat","alt")
rm(dem,dem2,dem3)

# extract biome value for each raster cell and all data points
pts <- SpatialPoints(cbind(hdf2$long, hdf2$lat), 
                     proj4string = CRS(proj4string(biomes)))
hdf2$biome <- sp::over(pts, biomes[,c("BIOME")])
hdf2$biome <- ifelse(is.na(hdf2$biome)==TRUE,NA,
                     ifelse(hdf2$biome == 13,'desert',
                            ifelse(hdf2$biome == 1,'humid forest',
                                   # ifelse(hdf2$biome == 2,'forest',
                                   ifelse(hdf2$biome == 14,'humid forest','other'))))#)

hdf2$ndvi.aut <- raster::extract(ndvi_aut,pts)
hdf2$ndvi.spr <- raster::extract(ndvi_spr,pts)

hdf2$biome2 <- ifelse(hdf2$biome %in% c("other") & hdf2$ndvi.aut < 0.25 & hdf2$ndvi.spr < 0.25 & hdf2$lat > 8,"desert",
                      ifelse(hdf2$biome %in% c("other") & (hdf2$ndvi.aut < 0.25 | hdf2$ndvi.spr < 0.25) & hdf2$lat > 8,"desert - seasonal",hdf2$biome))
                                    

rm(pts)

### CALCULATE GREAT CIRCLE ROUTES FOR REFERENCE
#determine position alegranza
meta <- read.csv('./Data/Metadata_full-v20201207.csv')
ref <- unique(meta[which(meta$dev %in% unique(data$dev)),c("colony","colony.lat","colony.long")])

# determine nearest point on madagascar as destination in autumn
madagascar <- countries[countries$admin == "Madagascar",]
dest.long <- as.numeric(dist2Line(cbind(ref$colony.long,ref$colony.lat), madagascar, distfun=distGeo)[,2])
dest.lat <- as.numeric(dist2Line(cbind(ref$colony.long,ref$colony.lat), madagascar, distfun=distGeo)[,3])
dest <- as.data.frame(cbind(dest.long,dest.lat))

# determine mean starting point on madagascar as start in spring
spring.starts <- data %>%
  filter(phase == 'return') %>%
 # order(tripID,dt) %>%
  group_by(tripID) %>%
  summarize(start.long = head(long,1),
            start.lat = head(lat,1)) 
starts <- spring.starts %>%
  summarize(start.long = mean(start.long),
            start.lat = mean(start.lat)) 
starts <- as.data.frame(starts)

# create spatial points
coordinates(ref) <- ~ colony.long + colony.lat
proj4string(ref) <- crs(lakes)
coordinates(dest) <- ~ dest.long + dest.lat
proj4string(dest) <- crs(lakes)
coordinates(starts) <- ~ start.long + start.lat
proj4string(starts) <- crs(lakes)

# deteremine GC routes for each season
gcRoute.autumn <- gcIntermediate(ref,dest,n=50)
gcRoute.autumn <- as.data.frame(gcRoute.autumn)
gcRoute.spring <- gcIntermediate(starts,ref,n=50)
gcRoute.spring <- as.data.frame(gcRoute.spring)
rm(madagascar,dest,starts,spring.starts,meta)

### 1000KM BUFFERS
# create buffers at 1000km intervals from colony for mapping
pts <- data.frame(lon = ref$colony.long,lat=ref$colony.lat)

make_GeodesicBuffer <- function(pts, width) {
  
  # A) Construct buffers as points at given distance and bearing ---------------
  
  dg <- seq(from = 0, to = 360, by = 5)
  
  # Construct equidistant points defining circle shapes (the "buffer points")
  buff.XY <- geosphere::destPoint(p = pts, 
                                  b = rep(dg, each = length(pts)), 
                                  d = width)
  
  # B) Make SpatialPolygons -------------------------------------------------
  
  # Group (split) "buffer points" by id
  buff.XY <- as.data.frame(buff.XY)
  id  <- rep(1:dim(pts)[1], times = length(dg))
  lst <- split(buff.XY, id)
  
  # Make SpatialPolygons out of the list of coordinates
  poly   <- lapply(lst, sp::Polygon, hole = FALSE)
  polys  <- lapply(list(poly), sp::Polygons, ID = NA)
  spolys <- sp::SpatialPolygons(Srl = polys, 
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  # Disaggregate (split in unique polygons)
  spolys <- sp::disaggregate(spolys)
  return(spolys)
}

buf_1000km <- make_GeodesicBuffer(as.matrix(pts), width = 1000*10^3)
buf_2000km <- make_GeodesicBuffer(as.matrix(pts), width = 2000*10^3)
buf_3000km <- make_GeodesicBuffer(as.matrix(pts), width = 3000*10^3)
buf_4000km <- make_GeodesicBuffer(as.matrix(pts), width = 4000*10^3)
buf_5000km <- make_GeodesicBuffer(as.matrix(pts), width = 5000*10^3)
buf_6000km <- make_GeodesicBuffer(as.matrix(pts), width = 6000*10^3)
buf_7000km <- make_GeodesicBuffer(as.matrix(pts), width = 7000*10^3)
buf_8000km <- make_GeodesicBuffer(as.matrix(pts), width = 8000*10^3)

### FALCON PHOTOGRAPH
# get falcon picture to include in paper
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

img <- get_png("./Figures 2021/Falcon-b.png")


### Annotate data with biome and ndvi data
############################################################################
## Annotate data with biome column
pts <- SpatialPoints(cbind(data$long, data$lat), 
                     proj4string = CRS(proj4string(biomes)))
biomevalues <- sp::over(pts, biomes[,c("BIOME")])
data$biome <- biomevalues$BIOME
data$biome <- ifelse(is.na(data$biome)==TRUE,'sea',
                     ifelse(data$biome == 13,'desert',
                            ifelse(data$biome == 1,'humid forest',
                   #                ifelse(data$biome == 2,'humid forest',
                                          ifelse(data$biome == 14,'humid forest','other'))))#)


rm(pts,biomevalues)

## Annotate data with NDVI column
aut <- subset(data,data$trip == "out")[,c("dev","dt","lat","long")]
spr <- subset(data,data$trip == "return")[,c("dev","dt","lat","long")]

aut.pts <- SpatialPoints(cbind(aut$long, aut$lat), 
                         proj4string = CRS(proj4string(lakes)))
spr.pts <- SpatialPoints(cbind(spr$long, spr$lat), 
                         proj4string = CRS(proj4string(lakes)))

ndvi.aut.values <- raster::extract(ndvi_aut,aut.pts)
ndvi.spr.values <- raster::extract(ndvi_spr,spr.pts)

aut$ndvi <- ndvi.aut.values
spr$ndvi <- ndvi.spr.values

ndvi.pts <- rbind(aut,spr)

data <- merge(data,ndvi.pts,all.x=TRUE)
rm(aut,spr,aut.pts,spr.pts,ndvi.aut.values,ndvi.spr.values,ndvi.pts)
rm(ndvi_aut,ndvi_spr)

data$biome2 <- ifelse(data$biome %in% c("other") & data$ndvi < 0.25 & data$lat > 8,"desert",data$biome)
data$biome2 <- ifelse(is.na(data$biome2)==TRUE,'other',data$biome2)

# we only want to account for Sahara and congo basin crossing and therefore correct 
# landscape annotations for small 'forest' and 'desert' patches in East Africa
data$biome2 <- ifelse(data$biome2 == 'humid forest' & data$long > 35,'other',data$biome2)
data$biome2 <- ifelse(data$biome2 == 'humid forest' & data$trip == "return",'other',data$biome2)

# segment trips based on biome crossings, extract longest desert crossing
data <- data[order(data$dev,data$dt),]
data$biome.seg <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$tripID,data$biome2))))))))
data$biome.seg <- sprintf("%04d",data$biome.seg)
data$biome.seg <- paste(data$dev,data$biome.seg,sep="_")

seglengths <- data %>%
  filter(biome2 == "desert")  %>%
  group_by(biome.seg,tripID,biome2) %>%
  summarize(biome.seg.length = sum(dist.f, na.rm=TRUE)/1000) 
seglengths <- as.data.frame(seglengths)

longest.desert.crossing.per.trip <- seglengths %>%
  group_by(tripID) %>%
  summarize(longest.crossing.time = max(biome.seg.length),
            longest.crossing = biome.seg[which(biome.seg.length == longest.crossing.time)])

longest.crossings <- as.data.frame(longest.desert.crossing.per.trip)[,c("tripID","longest.crossing")]
data <- merge(data,longest.crossings,all.x=TRUE)

data$biome3 <- ifelse(data$biome.seg == data$longest.crossing,'desert',
                      ifelse(data$biome2 == 'desert','other',data$biome2))

rm(longest.crossings,longest.desert.crossing.per.trip,seglengths)

data <- data[,!(colnames(data) %in% c("biome","biome2","ndvi","longest.crossing","biome.seg"))]
write.csv(data,paste('./Data/EF-resampled-MoveStats-biomes-v20210415.csv',sep=''))

### CALCULATE DISTANCE THRESHOLDS TO DELINEATE BIOME 'AREAS' ON ALL PLOTS
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# calculate distance thresholds for desert per season
desert.trip <- data %>%
  filter(biome3 == "desert") %>%
  group_by(trip,dev,tripID) %>%
  summarize(start = min(dt),
            start.dist = dist.to.colony[dt == start],
            end = max(dt),
            end.dist = dist.to.colony[dt == end]) 

desert.trip.id <- desert.trip  %>%
  group_by(trip,dev) %>%
  summarize(mean.start.dist = mean(start.dist),
            mean.end.dist = mean(end.dist)) 

desert.trip.mean <- desert.trip.id  %>%
  group_by(trip) %>%
  summarize(desert.start.dist = mean(mean.start.dist),
            desert.end.dist = mean(mean.end.dist)) 
desert.trip.mean <- as.data.frame(desert.trip.mean)
rm(desert.trip,desert.trip.id)

# calculate distance thresholds for forest per season
forest.trip <- data %>%
  filter(biome3 == "humid forest") %>%
  group_by(trip,dev,tripID) %>%
  summarize(start = min(dt),
            start.dist = dist.to.colony[dt == start],
            end = max(dt),
            end.dist = dist.to.colony[dt == end]) 

forest.trip.id <- forest.trip  %>%
  group_by(trip,dev) %>%
  summarize(mean.start.dist = mean(start.dist),
            mean.end.dist = mean(end.dist)) 

forest.trip.mean <- forest.trip.id  %>%
  group_by(trip) %>%
  summarize(forest.start.dist = mean(mean.start.dist),
            forest.end.dist = mean(mean.end.dist)) 
forest.trip.mean <- as.data.frame(forest.trip.mean)
rm(forest.trip,forest.trip.id)

# calculate distance thresholds for sea per season
sea.trip <- data %>%
  filter(biome3 == "sea" & data$long > 35) %>%
  group_by(trip,dev,tripID) %>%
  summarize(start = min(dt),
            start.dist = dist.to.colony[dt == start],
            end = max(dt),
            end.dist = dist.to.colony[dt == end]) 

sea.trip.id <- sea.trip  %>%
  group_by(trip,dev) %>%
  summarize(mean.start.dist = mean(start.dist),
            mean.end.dist = mean(end.dist)) 

sea.trip.mean <- sea.trip.id  %>%
  group_by(trip) %>%
  summarize(sea.start.dist = mean(mean.start.dist),
            sea.end.dist = mean(mean.end.dist)) 
sea.trip.mean <- as.data.frame(sea.trip.mean)
rm(sea.trip,sea.trip.id)

# merge all distance thresholds in one df
trip.dists <- merge(desert.trip.mean,forest.trip.mean,all.x=TRUE)
trip.dists <- merge(trip.dists,sea.trip.mean,all.x=TRUE)
rm(forest.trip.mean,desert.trip.mean,sea.trip.mean)

###################################################
#####     PRODUCE FIG1: ANNUAL CYCLE        ####     
###################################################
data <- data[order(data$dev,data$dt),]

# create the breaks- and label vectors
ewbrks <- seq(-10,50,10)
nsbrks <- seq(-20,40,10)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x, "°W", sep=""), ifelse(x > 0, paste(x, "°E", sep=""),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(-x, "°S", sep=""), ifelse(x > 0, paste(x, "°N", sep=""),x))))


### CREATE BASE MAP
basemap <- ggplot()+
  geom_raster(data=hdf2[which(is.na(hdf2$biome2)==FALSE),],mapping=aes(long,lat,fill=biome2,alpha=alt))+
  scale_alpha_continuous(name="Elevation",range=c(0.35,1))+
  scale_fill_manual(name="Biome",values=c("desert"="wheat3","desert - seasonal"="gold3","humid forest"="olivedrab4","other"="grey30"))+
#  scale_fill_manual(name="Landscape",values=c("desert"="wheat4","desert - seasonal"="grey70","humid forest"="olivedrab4","other"="grey30"))+
  #geom_polygon(data=biomes.df, aes(x = long, y = lat, group = paste(id,piece),fill=factor(BIOME)),alpha=.5) +
  #scale_fill_grey(start=0.2,end=0.8,na.value="white",aesthetics="fill")+
  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='white',fill='transparent',size=.2)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add distance buffers
  geom_polygon(data=buf_1000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_2000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_3000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_4000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_5000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_6000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_7000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  geom_polygon(data=buf_8000km,aes(x=long,y=lat,group=group),size=.4,col='grey40',fill='transparent',linetype='dotted')+
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) 
#  xlab("\nLongitude [º]") + ylab ("Latitude [º]\n")

p1 <- basemap + theme_bw() + 
  # Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.185,0.24),
        legend.direction  = 'vertical',
        legend.spacing.y = unit (0, "cm"),
        legend.box.margin = margin(t=0.02,r=0,b=0,l=0,unit="cm"),
        legend.title 	    = element_text(size=9,face='bold'),
        legend.text 	    = element_text(size=9),
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=11),
   #     axis.title		    = element_text(size=11,face='bold').
        axis.title        = element_blank())+
  # Add white box bottom right and lay-out legend
  annotate(geom="rect",xmin=longlimits[1],xmax=7.2,ymin=latlimits[1],ymax=7.1,col='black',fill='white')+
  guides(colour= guide_legend(order =1,nrow =2,title.position='top'),
         size = guide_legend(order =2,nrow =2,title.position='top'),
         fill=guide_legend(order=3,nrow =2,title.position='top',override.aes= list(alpha = .4)),
         alpha=guide_legend(order = 4,title.position='top',nrow=2,override.aes=list(fill='grey30')))

### ADD TRACKING DATA TO BASEMAP
p2 <- p1 +
  ## Add tracking data
  geom_path(data=nonmig,aes(x=long,y=lat,col=phase,group=group),size=.4,alpha=.7)+
  geom_path(data=data[which(data$trip %in% c("out","return") & data$travel == 1),],aes(x=long,y=lat,col=phase,group=group),size=.5,alpha=.8)+ 
  # Add stop-overs
  geom_point(data=segs,aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=phase),fill='white',alpha=.4,shape=21)+
  scale_size_binned(name="Stop-over duration [days]",range=c(1,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))+
  geom_point(data=segs,aes(x=st.long,y=st.lat,col=phase),size=.3)+
  # Add colour scale
  scale_colour_manual(name = "Annual stages",values=c("out"="orangered","return"="cornflowerblue",'summer'="goldenrod","winter"="darkgoldenrod4"),
                      labels =c("out"="Autumn","return"="Spring",'summer'="Summer","winter"="Winter"))+
  # Add seasonal GC routes between Alegranza and Madagascar
  geom_path(data=gcRoute.autumn,aes(x=lon,y=lat),col='darkred',linetype='solid',size=0.8,
            arrow = arrow(length = unit(0.6,"cm")))+
  geom_path(data=gcRoute.spring,aes(x=lon,y=lat),col='blue',linetype='solid',size=0.8,
            arrow = arrow(length = unit(0.6,"cm")))


# Add falcon image to plot
gg <- p2 + annotation_custom(img, xmin = 25, xmax = 42, ymin = 15, ymax = latlimits[2])
#gg

ggsave(plot=gg,path='./Figures 2021/',filename='Fig1A_AllTracks_AnnualCycle-v20210421.tiff',width=8,height=7,dpi=300)
rm(basemap,p1,p2,gg)

### CREATE PANEL B OF FIGURE 1 : DISTANCE TO COLONY VS TIME IN CYCLE
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
p1 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=0,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=desert.start.dist/1000,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.5)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=desert.end.dist/1000,ymax=forest.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=forest.start.dist/1000,ymax=forest.end.dist/1000,xmin=-Inf,xmax=Inf),fill='olivedrab4',alpha=.5)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=forest.end.dist/1000,ymax=sea.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
 # geom_hline(data=trip.dists[which(trip.dists$trip == "out"),],aes(yintercept=forest.start.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
#  geom_hline(data=trip.dists[which(trip.dists$trip == "out"),],aes(yintercept=forest.end.dist/1000),col='chartreuse4',linetype='solid',size=.4)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "out"),],aes(ymin=sea.end.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  ## Add tracking data
  geom_path(data=nonmig,aes(x=cycle.time,y=dist.to.colony/1000,col=phase,group=group),size=.6,alpha=.7)+
  geom_path(data=data[which(data$phase %in% c("out","return")),],aes(x=cycle.time,y=dist.to.colony/1000,col=phase,group=group),size=.6,alpha=.8)+
  #scale_colour_viridis_d()+
  scale_colour_manual(values=c("out"="orangered","return"="cornflowerblue",'summer'="goldenrod","winter"="darkgoldenrod4"))+
  ##Add stop-overs
  geom_point(data=segs,aes(y=dist.to.colony/1000,x=cycle.time),col='black',size=.3)+
  ## Layout map
  coord_cartesian(expand=FALSE) + scale_y_reverse() +
  scale_x_continuous(breaks=c(92,123,153,184),labels=c("Oct","Nov","Dec","Jan"),limits=c(80,180),position='top')+
  theme_bw() + xlab("Date") + ylab ("Distance to colony (km)") + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text		      = element_text(size=11),
        axis.title		    = element_text(size=12,face='bold'))

ggsave(plot=p1,filename='./Figures 2021/Fig1Ba_AllTracks_AnnualCycle-autumn-v20210421.tiff',width=4,height=4.2,dpi=300)
rm(p1)

p1 <- ggplot()+
  geom_rect(data=trip.dists[which(trip.dists$trip == "return"),],aes(ymin=0,ymax=desert.end.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "return"),],aes(ymin=desert.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='wheat3',alpha=.5)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "return"),],aes(ymin=sea.end.dist/1000,ymax=desert.start.dist/1000,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "return"),],aes(ymin=sea.start.dist/1000,ymax=sea.end.dist/1000,xmin=-Inf,xmax=Inf),fill='white',alpha=.3)+
  geom_rect(data=trip.dists[which(trip.dists$trip == "return"),],aes(ymin=sea.start.dist/1000,ymax=Inf,xmin=-Inf,xmax=Inf),fill='grey30',alpha=.3)+
  ## Add tracking data
  geom_path(data=nonmig,aes(x=cycle.time,y=dist.to.colony/1000,col=phase,group=group),size=.6,alpha=.7)+
  geom_path(data=data[which(data$phase %in% c("out","return")),],aes(x=cycle.time,y=dist.to.colony/1000,col=phase,group=group),size=.6,alpha=.8)+
  #scale_colour_viridis_d()+
  scale_colour_manual(values=c("out"="orangered","return"="cornflowerblue",'summer'="goldenrod","winter"="darkgoldenrod4"))+
  ##Add stop-overs
  geom_point(data=segs,aes(y=dist.to.colony/1000,x=cycle.time),col='black',size=.3)+
  ## Layout map
  coord_cartesian(expand=FALSE) + scale_y_reverse() +
  scale_x_continuous(breaks=c(274,304,335),labels=c("Apr","May","Jun"),limits=c(262,362),position='top')+
  theme_bw() + xlab("Date") + ylab ("Distance to colony [km]") + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        panel.grid.minor.x   = element_blank(),
        panel.grid.major.x   = element_blank(),
        panel.grid.minor.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        panel.grid.major.y   = element_line(size=.2,linetype='dashed',colour='grey40'),
        axis.line         = element_line(size=.4),
        axis.text		      = element_text(size=11),
        axis.title		    = element_text(size=12,face='bold'))

ggsave(plot=p1,filename='./Figures 2021/Fig1Bb_AllTracks_AnnualCycle-spring-v20210421.tiff',width=4,height=4.2,dpi=300)
rm(p1)

### PLOT BIOME CLASSIFICATION TRACKING DATA
### ### ### ### ### ### ### ### ### ### ### 
# create DEM split per season
hdf.aut <- hdf2[,c("long","lat","alt","biome",'ndvi.aut')]
hdf.spr <- hdf2[,c("long","lat","alt","biome",'ndvi.spr')]
colnames(hdf.aut)[5] <- "ndvi"
colnames(hdf.spr)[5] <- "ndvi"
hdf.aut$trip <- rep("out")
hdf.spr$trip <- rep("return")

hdf3 <- rbind(hdf.aut,hdf.spr)
hdf3$biome2 <- ifelse(hdf3$biome %in% c("other") & hdf3$ndvi < 0.25 & hdf3$lat > 8,"desert",hdf3$biome)

rm(hdf.aut,hdf.spr)

# create new trip name
data$trip2 <- ifelse(data$trip == 'out','Autumn','Spring')
segs$trip2 <- ifelse(segs$trip == 'out','Autumn','Spring')
hdf3$trip2 <- ifelse(hdf3$trip == 'out','Autumn','Spring')

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

### Add tracking data
p1 <- basemap +
  ## Add tracking data
  geom_path(data=data[which(data$trip %in% c("out","return") & data$travel == 1 & is.na(data$biome3) == FALSE),],aes(x=long,y=lat,col=biome3,group=group),size=.5,alpha=.8)+ 
  # Add stop-overs
  geom_point(data=segs,aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur))),col='grey30',fill='white',alpha=.4,shape=21)+
  scale_size_binned(name="Stop-over dur. [days]",range=c(1,8),breaks=c(1,3,7,14),labels=c("1","2-3","4-7","8-14"))+
  geom_point(data=segs,aes(x=st.long,y=st.lat),col='grey30',size=.3)+
  # Add colour scale
  scale_colour_viridis_d(name = "Biome")

p2 <- p1  + theme_bw() + 
#  annotate(geom="rect",xmin=longlimits[1],xmax=7.2,ymin=latlimits[1],ymax=7.1,col='black',fill='white')+
  # Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        strip.text		    = element_text(size=11,face='bold'),
        strip.background.x = element_blank(),
        legend.position   = c(0.075,0.18),
        legend.direction  = 'vertical',
        legend.spacing.y = unit (0, "cm"),
        legend.title 	    = element_text(size=9,face='bold'),
        legend.text 	    = element_text(size=9),
        legend.background = element_rect(fill='white',colour='black'),
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank())+
  # Add white box bottom right and lay-out legend
#  annotate('rect',xmin=longlimits[1]+0.15,xmax=5.2,ymin=-20,ymax=-10,col='white',fill='white')+
  guides(colour= guide_legend(order=1,nrow =4,title.position='top'),
         size = FALSE,
         fill= FALSE,
         alpha=FALSE)

ggsave(plot=p2,path='./Figures 2021/',filename='FigS4_AllTracks_AnnotationBiomes-v20210421.tiff',width=8.5,height=5,dpi=300)
rm(basemap,p1,p2,gg)


###################################################
#####     FIG SX: COLONY COMPARISON            ####     
###################################################
coltracks <- readOGR('./Data/EF_AllPops_Tracks.kml')
coltracks.names <- as.data.frame(coltracks,row.names=TRUE)
coltracks.names$id <- seq(0,11,1) 

coltracks <- fortify(coltracks)
coltracks <- merge(coltracks,coltracks.names[,c("id","Name")])

coltracks$colony <- do.call(rbind,strsplit(coltracks$Name,"_"))[,2]
coltracks$trip <- do.call(rbind,strsplit(coltracks$Name,"_"))[,3]

coltracks$trip <- ifelse(coltracks$trip == "Spring2","Spring",coltracks$trip)
coltracks$colony <- ifelse(coltracks$colony == "Culembretes","Columbretes",coltracks$colony)

coltracks$colony <- factor(coltracks$colony,levels=c("Canaries","Columbretes","Sardinia","Svetac","Greece","Cyprus"))

longlimits <- c(min(coltracks$long)-0.5,max(coltracks$long)+0.5)
latlimits <- c(min(coltracks$lat)-0.5,max(coltracks$lat)+0.5)

# country boundaries
countries <- readOGR("~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- countries[countries$continent %in% c("Africa","Eiurope") | 
                         countries$admin %in% c("Spain","Italy","Greece","Portugal","Malta","Iran","Georgia",
                                                "Northern Cyprus","Turkey","Syria","Lebanon","Jordan","Iraq",
                                                "Israel","Palestine","Yemen","Saudi Arabia","Kuwait","United Arab Emirates"),]

political <- fortify(countries, region = "admin")
political$country <- political$id
political$group <- ifelse(political$country %in% unique(data$country) & political$piece == 1,1,0)

# dem and ndvi

### Read DEM
dem <- raster('~/Documents/2019 - Eleonoras Falcon/EF01 - analyses/Maps/alt_30s_bil/alt.bil')
dem2 <- crop(dem, extent(longlimits[1],longlimits[2],latlimits[1],latlimits[2]))
dem3 <- aggregate(dem2,5,FUN='mean')
hdf2 <- rasterToPoints(dem3)
hdf2 <- data.frame(hdf2)
colnames(hdf2) <- c("long","lat","alt")
rm(dem,dem2,dem3)

# extract biome value for each raster cell and all data points
pts <- SpatialPoints(cbind(hdf2$long, hdf2$lat), 
                     proj4string = CRS(proj4string(biomes)))
hdf2$biome <- sp::over(pts, biomes[,c("BIOME")])
hdf2$biome <- ifelse(is.na(hdf2$biome)==TRUE,NA,
                     ifelse(hdf2$biome == 13,'desert',
                            ifelse(hdf2$biome == 1,'humid forest',
                                   # ifelse(hdf2$biome == 2,'forest',
                                   ifelse(hdf2$biome == 14,'humid forest','other'))))#)
hdf2$biome <- as.character(hdf2$biome)

hdf2$ndvi.aut <- raster::extract(ndvi_aut,pts)
hdf2$ndvi.spr <- raster::extract(ndvi_spr,pts)

rm(pts)

hdf.spr <- hdf2[,c("lat","long","biome","ndvi.spr","alt")]
hdf.aut <- hdf2[,c("lat","long","biome","ndvi.aut","alt")]
colnames(hdf.spr)[4] <- "ndvi"
colnames(hdf.aut)[4] <- "ndvi"
hdf.spr$trip <- rep("Spring")
hdf.aut$trip <- rep("Autumn")

hdf3 <- rbind(hdf.spr,hdf.aut)

hdf3$biome2 <- ifelse(hdf3$biome %in% c("other") & hdf3$ndvi < 0.25 & hdf3$lat > 8,"desert")
hdf3$biome2 <- ifelse(is.na(hdf3$biome2) == TRUE & is.na(hdf3$alt) == FALSE,"other",hdf3$biome2)


### CREATE BASE MAP
basemap <- ggplot()+
  geom_raster(data=hdf3,mapping=aes(long,lat,fill=biome2,alpha=alt))+
  scale_alpha_continuous(name="Elevation",range=c(0.35,1))+
  scale_fill_manual(name="Biome",values=c("desert"="wheat3","desert - seasonal"="gold3","humid forest"="olivedrab4","other"="grey30"))+
  #  scale_fill_manual(name="Landscape",values=c("desert"="wheat4","desert - seasonal"="grey70","humid forest"="olivedrab4","other"="grey30"))+
  #geom_polygon(data=biomes.df, aes(x = long, y = lat, group = paste(id,piece),fill=factor(BIOME)),alpha=.5) +
  #scale_fill_grey(start=0.2,end=0.8,na.value="white",aesthetics="fill")+
  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='white',fill='transparent',size=.2)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
 ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip)
#  xlab("\nLongitude [º]") + ylab ("Latitude [º]\n")

p1 <- basemap + theme_bw() + 
  # Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        strip.text = element_text(size=12,face='bold'),
        legend.position   = "bottom",
        legend.direction  = 'horizontal',
        legend.spacing.y = unit (0, "cm"),
        legend.box.margin = margin(t=0.02,r=0,b=0,l=0,unit="cm"),
        legend.title 	    = element_text(size=9,face='bold'),
        legend.text 	    = element_text(size=9),
        panel.grid        = element_blank(),
        axis.text		      = element_text(size=11),
        #     axis.title		    = element_text(size=11,face='bold').
        axis.title        = element_blank())+
  # Add white box bottom right and lay-out legend
  guides(colour= guide_legend(order =1,nrow =2,title.position='top'),
         size = FALSE,
         fill= FALSE,
         alpha=FALSE)

### ADD TRACKING DATA TO BASEMAP
p2 <- p1 +
  ## Add tracking data
  geom_path(data=coltracks,aes(x=long,y=lat,col=colony,group=id),size=.5,alpha=.8)+ 
  # Add colour scale
  scale_colour_viridis_d(option="C")

ggsave(plot=p2,filename='./Figures 2021/FigSx_ColonyComparison.tiff',dpi=300,width=8,height=5.5)
