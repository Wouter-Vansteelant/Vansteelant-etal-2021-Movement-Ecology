### Read all csv files with ITCZ data
####################################################################
require(tidyverse)
read_plus <- function(flnm) {
  read_delim(flnm,delim=',') %>% 
    mutate(filename = flnm)
}

itcz <-
  list.files(path = "./Maps/ITCZ/",
             pattern = "*.txt", 
             full.names = T) %>% 
  map_df(~read_plus(.))

itcz <- as.data.frame(itcz)
colnames(itcz)[2] <- "long"

### Extract relevant information from filenames
####################################################################
# We can extract year, month and dekade from file names
itcz$yr <- as.integer(gsub(".*[//]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\2", itcz$filename))
itcz$mmd <- gsub(".*[//]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\3", itcz$filename)

# Make sure we can group each ITCZ line per dekade
itcz$group <- paste(itcz$yr,itcz$mmd,sep="_")

# First two digits of mmd represent month
itcz$mth <- as.integer(substr(x=itcz$mmd,start=1,stop=2))

# Last digit of mmd represents dekade
RIGHT = function(x,n){
  substring(x,nchar(x)-n+1)
}
itcz$dec <- as.numeric(RIGHT(itcz$mmd, 1))

# order data from W to E for each dekade
itcz <- itcz[order(itcz$group,itcz$lon),]
itcz$trip <- ifelse(itcz$mth == 4,"return",ifelse(itcz$mth == 10,"out",NA))
itcz <- subset(itcz,itcz$trip %in% c("return","out"))

itcz.seasonal <- itcz %>%
  group_by(long,trip) %>%
  summarize(mean_lat = mean(lat, na.rm = TRUE),
            max_lat = quantile(lat, prob = 0.90, na.rm = TRUE),
            min_lat = quantile(lat, prob = 0.10, na.rm = TRUE)) 
itcz.seasonal <- as.data.frame(itcz.seasonal)

### EXTRACT SEASONAL WIND FIELDS
########################################################
library(RNCEP)
# Extract spring wind fields
uwind.fields.spring <- NCEP.gather(variable = 'uwnd', level = 850, months.minmax = c(4,4), years.minmax = c(2013,2020), lat.southnorth = c(-20,40), lon.westeast = c(-20,50), reanalysis2 = TRUE, return.units = FALSE, status.bar=FALSE)
vwind.fields.spring <- NCEP.gather(variable = 'vwnd', level = 850, months.minmax = c(4,4), years.minmax = c(2013,2020), lat.southnorth = c(-20,40), lon.westeast = c(-20,50), reanalysis2 = TRUE, return.units = FALSE, status.bar=TRUE)
uwinds.spring <- NCEP.array2df(uwind.fields.spring, var.names=NULL)
colnames(uwinds.spring)[4] <- "u"
vwinds.spring <- NCEP.array2df(vwind.fields.spring, var.names=NULL)
colnames(vwinds.spring)[4] <- "v"

# Extract autumn wind fields
uwind.fields.autumn <- NCEP.gather(variable = 'uwnd', level = 850, months.minmax = c(10,10), years.minmax = c(2012,2019), lat.southnorth = c(-20,40), lon.westeast = c(-20,50), reanalysis2 = TRUE, return.units = FALSE, status.bar=TRUE)
vwind.fields.autumn <- NCEP.gather(variable = 'vwnd', level = 850, months.minmax = c(10,10), years.minmax = c(2012,2019), lat.southnorth = c(-20,40), lon.westeast = c(-20,50), reanalysis2 = TRUE, return.units = FALSE, status.bar=TRUE)
uwinds.autumn <- NCEP.array2df(uwind.fields.autumn, var.names=NULL)
colnames(uwinds.autumn)[4] <- "u"
vwinds.autumn <- NCEP.array2df(vwind.fields.autumn, var.names=NULL)
colnames(vwinds.autumn)[4] <- "v"

# Bind spring and autumn data
uwinds <- rbind(uwinds.autumn,uwinds.spring)
vwinds <- rbind(vwinds.autumn,vwinds.spring)

wind.fields <- merge(uwinds,vwinds,by=c("datetime","longitude","latitude"))
colnames(wind.fields)[1:3] <- c("dt","long","lat")

wind.fields$yr <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\1", wind.fields$dt)
wind.fields$mth <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\2", wind.fields$dt)
wind.fields$day <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\3", wind.fields$dt)
wind.fields$hr <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\4", wind.fields$dt)

wind.fields$crds <- paste(wind.fields$long,wind.fields$lat,sep='_')
wind.fields$date <- as.Date(paste(wind.fields$yr,wind.fields$mth,wind.fields$day,sep='-'),tz='UTC')
wind.fields$trip <- ifelse(wind.fields$mth == "04",'return','out')

rm(uwinds,vwinds,uwinds.autumn,uwinds.spring,vwinds.autumn,vwinds.spring,vwind.fields.autumn,uwind.fields.autumn,vwind.fields.spring,uwind.fields.spring)

#### calculate wind support at each node
countries <- shapefile("./Maps/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- countries[countries$continent %in% c("Europe","Africa","Asia"),]
madagascar <- countries[countries$admin == "Madagascar",]

ale.long <- ref$colony.long[1]
ale.lat <- ref$colony.lat[1]

coords <- unique(wind.fields[,c("lat","long","trip")])
coords$dest.long <- ifelse(coords$trip == "return",ale.long,as.numeric(dist2Line(cbind(coords$long,coords$lat), madagascar, distfun=distGeo)[,2]))
coords$dest.lat <- ifelse(coords$trip == "return",ale.lat,as.numeric(dist2Line(cbind(coords$long,coords$lat), madagascar, distfun=distGeo)[,3]))
rm(madagascar)

#calculate intended travel direction as direction to 
# Madagacar (for outbound) and Alegranza (for inbound)
coords$itd <- earth.bear(long1=coords$long, lat1=coords$lat, long2=coords$dest.long, lat2=coords$dest.lat)
coords$itd <- ifelse(coords$itd > 180, coords$itd-360, coords$itd)

wind.fields <- merge(wind.fields,coords,all.x=TRUE)
rm(coords)

# calcualte wind support at each node
wind.fields$w <- (wind.fields$u^2 + wind.fields$v^2)^(1/2)			# determine strength of wind in m/s #
wind.fields$alfa <- atan2(wind.fields$u/wind.fields$w,wind.fields$v/wind.fields$w)*(180/base::pi)

### determine incident angle of wind, heading and flight direction with itd
wind.fields$beta <- wind.fields$alfa - wind.fields$itd
wind.fields$beta <- ifelse(wind.fields$beta > 180,wind.fields$beta - 360,ifelse(wind.fields$beta < -180,wind.fields$beta + 360,wind.fields$beta))

wind.fields$sidewind <- wind.fields$w * sin(rad(wind.fields$beta))
wind.fields$tailwind <- wind.fields$w * cos(rad(wind.fields$beta))

##### average wind fields
wind.fields.noon <- subset(wind.fields,wind.fields$hr == "12")

wind.seasonal <- wind.fields %>%
  group_by(long,lat,trip) %>%
  summarize(mean_u = mean(u, na.rm = TRUE),
            mean_v = mean(v, na.rm = TRUE),
            mean_tailwind = mean(tailwind, na.rm=TRUE)) 
wind.seasonal <- as.data.frame(wind.seasonal)


wind.seasonal$mean_w <- (wind.seasonal$mean_u^2 + wind.seasonal$mean_v^2)^(1/2)			# determine strength of wind in m/s #
wind.seasonal$alfa <- atan2(wind.seasonal$mean_u/wind.seasonal$mean_w,wind.seasonal$mean_v/wind.seasonal$mean_w)*(180/base::pi)

xx <- mk.new.lat.lon(lat=wind.seasonal$lat,long=wind.seasonal$long,bearing=wind.seasonal$alfa,distance=wind.seasonal$mean_w)
wind.seasonal$end_lat <- xx[,1]
wind.seasonal$end_long <- xx[,2]
wind.seasonal$delta_lat <- wind.seasonal$end_lat - wind.seasonal$lat
wind.seasonal$delta_long <- wind.seasonal$end_long - wind.seasonal$long

rm(wind.fields,wind.fields.noon,xx)

### EXTRACT PRECIPITATION DATA
########################################################
humid.fields.spring <- NCEP.gather(variable = 'prate.sfc', level = 'gaussian', months.minmax = c(4,4), years.minmax = c(2013,2020), lat.southnorth = c(-20,40), lon.westeast = c(-20,50), reanalysis2 = TRUE, return.units = FALSE, status.bar=TRUE)
humids.spring <- NCEP.array2df(humid.fields.spring, var.names=NULL)
colnames(humids.spring)[4] <- "humid"

humid.fields.autumn <- NCEP.gather(variable = 'prate.sfc', level = 'gaussian', months.minmax = c(10,10), years.minmax = c(2012,2019), lat.southnorth = c(-20,40), lon.westeast = c(-20,50), reanalysis2 = TRUE, return.units = FALSE, status.bar=TRUE)
humids.autumn <- NCEP.array2df(humid.fields.autumn, var.names=NULL)
colnames(humids.autumn)[4] <- "humid"

humids <- rbind(humids.autumn,humids.spring)
humids$humid <- humids$humid * 3600 * 6
colnames(humids)[1:3] <- c("dt","lat","long")

humids$yr <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\1", humids$dt)
humids$mth <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\2", humids$dt)
humids$day <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\3", humids$dt)
humids$hr <- gsub("([^.]+)[_]([^.]+)[_]([^.]+)[_]([^.]+).*", "\\4", humids$dt)

humids$crds <- paste(humids$long,humids$lat,sep='_')
humids$date <- as.Date(paste(humids$yr,humids$mth,humids$day,sep='-'),tz='UTC')
humids$trip <- ifelse(humids$mth == "04",'return','out')


rain.aggregate.year <- humids %>%
  group_by(long,lat,yr,trip) %>%
  summarize(sum_hum = sum(humid, na.rm = TRUE)) 
rain.aggregate.year <- as.data.frame(rain.aggregate.year)

rain.seasonal <- rain.aggregate.year %>%
  group_by(long,lat,trip) %>%
  summarize(mean_hum = mean(sum_hum, na.rm = TRUE)) 
rain.seasonal <- as.data.frame(rain.seasonal)

rm(humid.fields.autumn,humid.fields.spring,humids,humids.autumn,humids.spring,rain.aggregate.year)

#### Add alternative facetting variable
data$trip2 <- ifelse(is.na(data$trip)==TRUE,NA,ifelse(data$trip == "out","Autumn","Spring"))
segs$trip2 <- ifelse(is.na(segs$trip)==TRUE,NA,ifelse(segs$trip == "out","Autumn","Spring"))
itcz.seasonal$trip2 <- ifelse(is.na(itcz.seasonal$trip)==TRUE,NA,ifelse(itcz.seasonal$trip == "out","Autumn","Spring"))

wind.seasonal$trip2 <- ifelse(is.na(wind.seasonal$trip)==TRUE,NA,ifelse(wind.seasonal$trip == "out","Autumn","Spring"))
rain.seasonal$trip2 <- ifelse(is.na(rain.seasonal$trip)==TRUE,NA,ifelse(rain.seasonal$trip == "out","Autumn","Spring"))

#write.csv(wind.seasonal,'./Figures 2021/windfields-v20210105.csv')
#write.csv(rain.seasonal,'./Figures 2021/rainfields-v20210105.csv')
