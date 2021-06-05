### Read packages 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("RNCEP","rsq")
ipak(packages)

# read all wind data previously extracted by RNCEP
winds <- read.csv('./Figures 2021/EF_migration_traveldays_winddata-v20210105.csv')
winds <- winds[,!(colnames(winds) %in% c("X","travel"))]
winds$dt <- as.POSIXct(strptime(winds$dt, format="%Y-%m-%d %H:%M:%S"), tz='UTC')

# merge movement and wind data
data <- subset(data,data$travel >= 1)
data$yrtrip <- paste(data$trip,data$yr,sep='_')


data <- merge(data,winds,all.x=T)

################################################################################
####        DETERMINE INTENDED TRAVEL DIRECTION AT EACH LOC             ########
################################################################################
# determine closest point in Madagascar
# create df for country polygons
#countries <- shapefile("./Maps/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
#countries <- countries[countries$continent %in% c("Europe","Africa","Asia"),]
madagascar <- countries[countries$admin == "Madagascar",]

# for return trips use center point of Alegranza as destination
data$dest.long <- ifelse(data$trip == "return",ale.long,as.numeric(dist2Line(cbind(data$long,data$lat), madagascar, distfun=distGeo)[,2]))
data$dest.lat <- ifelse(data$trip == "return",ale.lat,as.numeric(dist2Line(cbind(data$long,data$lat), madagascar, distfun=distGeo)[,3]))
rm(madagascar)

#calculate intended travel direction as direction to 
# Madagacar (for outbound) and Alegranza (for inbound)
data$itd <- earth.bear(long1=data$long, lat1=data$lat, long2=data$dest.long, lat2=data$dest.lat)
data$itd <- ifelse(data$itd > 180, data$itd-360, data$itd)

# calculate loxodrome distance to goal
#data$dtg <- deg.dist(lat1=data$lat,long1=data$long,lat2=data$dest.lat,long2=data$dest.long)

#######              RECALCULATE WIND                 #############
###################################################################
### DETERMINE ANGLE WIND RELATIVE TO EARTH NORTH FROM U AND V COMPONENTS ### 
data$w <- (data$u^2 + data$v^2)^(1/2)			# determine strength of wind in m/s #
data$alfa <- atan2(data$u/data$w,data$v/data$w)*(180/pi)
data$c_alfa <- circular(data$alfa, units = 'degrees', template ='geographics')

###   CALCULATE SIDEWARD AND FORWARD SPEED OF BIRD AND WIND RELATIVE TO ITD
################################################################################

### determine incident angle of wind, heading and flight direction with itd
data$beta <- data$alfa - data$itd
data$beta <- ifelse(data$beta > 180,data$beta - 360,ifelse(data$beta < -180,data$beta + 360,data$beta))

data$teta <- data$dir - data$itd
data$teta <- ifelse(data$teta > 180,data$teta - 360,ifelse(data$teta < -180,data$teta + 360,data$teta))

## determine forward and sideword movement, active movement and wind movement
data$spd <- data$spd 
data$spd.sw <- data$spd * sin(rad(data$teta))
data$spd.fw <- data$spd * cos(rad(data$teta))

data$sidewind <- data$w * sin(rad(data$beta))
data$tailwind <- data$w * cos(rad(data$beta))

# calcualte realized tailwind
data$gamma <- data$alfa - data$dir
data$gamma <- ifelse(data$gamma > 180,data$gamma - 360,ifelse(data$gamma < -180,data$gamma + 360,data$gamma))
data$sidewind.r <- data$w * sin(rad(data$gamma))
data$tailwind.r <- data$w * cos(rad(data$gamma))

# calculate wind support gain
data$gain <- data$tailwind.r - data$tailwind

################################################################################
####        CLASSIFY COMPENSATION BASED ON U COMP MOVE AND WIND         ########
################################################################################

data$stw <- data$spd.sw/data$sidewind
data$comp <- rep('0')
data[which(abs(data$sidewind)*3.6 < 5),]$comp <- 'NSW'
data[which(abs(data$sidewind)*3.6 < 5 & sign(data$sidewind) == sign(data$spd.sw) & abs(data$spd.sw)*3.6 >= 5),]$comp <- 'DO'
data[which(abs(data$sidewind)*3.6 < 5 & sign(data$sidewind) != sign(data$spd.sw) & abs(data$spd.sw)*3.6 >= 5),]$comp <- 'CO'
data[which(abs(data$sidewind)*3.6 >= 5 & abs(data$stw) <= 0.2),]$comp <- 'CF'
data[which(abs(data$sidewind)*3.6 >= 5 & sign(data$sidewind) == sign(data$spd.sw) & abs(data$stw) > 0.2 & abs(data$stw) < 0.8),]$comp <- 'CP'
data[which(abs(data$sidewind)*3.6 >= 5 & sign(data$sidewind) != sign(data$spd.sw) & abs(data$stw) > 0.2 & abs(data$stw) < 0.8),]$comp <- 'CO'
data[which(abs(data$sidewind)*3.6 >= 5 & sign(data$sidewind) == sign(data$spd.sw) & abs(data$stw) >= 0.8 & abs(data$stw) < 1.2),]$comp <- 'DF'
data[which(abs(data$sidewind)*3.6 >= 5 & sign(data$sidewind) != sign(data$spd.sw) & abs(data$stw) >= 0.8 & abs(data$stw) < 1.2),]$comp <- 'CO'
data[which(abs(data$sidewind)*3.6 >= 5 & sign(data$sidewind) == sign(data$spd.sw) & abs(data$stw) >= 1.2),]$comp <- 'DO'
data[which(abs(data$sidewind)*3.6 >= 5 & sign(data$sidewind) != sign(data$spd.sw) & abs(data$spd.sw) >= 1.2),]$comp <- 'CO' 

