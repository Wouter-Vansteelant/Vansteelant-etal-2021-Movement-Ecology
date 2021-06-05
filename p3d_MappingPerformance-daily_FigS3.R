
# load relevant packages
packages <- c("ggquiver","RColorBrewer","scales")
ipak(packages)


#########################################################################
### APPEND DAILY STATS TO DATA AND PREPARE FOR MAPPING DAILY PERFORMANCE
#############################################################################
data <- merge(data,wind.days,all.x=TRUE)
data <- subset(data,data$daily.travel < 25 )
data <- data[order(data$dev,data$dt),]

segs$trip2 <- ifelse(segs$trip == "out","Autumn","Spring")
### CREATE BASE MAP

### CREATE BASE MAP
basemap <- ggplot()+
  geom_raster(data=hdf3[which(is.na(hdf3$biome2)==FALSE),],mapping=aes(long,lat,alpha=alt,fill=biome2))+
  scale_alpha_continuous(name="Elevation",range=c(0.35,1))+
  scale_fill_manual(name="Landscape",values=c("desert"="wheat3","humid forest"="olivedrab4","other"="grey30"))+
  #geom_polygon(data=biomes.df, aes(x = long, y = lat, group = paste(id,piece),fill=factor(BIOME)),alpha=.5) +
  #scale_fill_grey(start=0.2,end=0.8,na.value="white",aesthetics="fill")+
  ## Add borders, coastlines and likes
 # geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='white',fill='transparent',size=.2)+	
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
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip2)

##########################
### DAILY TRAVEL HRS MAP
##########################
cols <- c("orangered","navyblue")

min <- min(data$daily.travel)
max<- max(data$daily.travel)

p1 <- basemap +
 ## Add tracking data
  geom_path(data=data[which(data$daily.travel < 25 ),],aes(x=long,y=lat,col=daily.travel,group=segment),size=.5)+
  scale_colour_gradient(low='orangered',high='navyblue',limits=c(min,25))+
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
 ## Layout map
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.09,0.15),
        legend.background = element_rect(fill='white',colour='black'),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank(),
        plot.margin       = unit(c(0,0,12,0),"pt"))+
  guides(fill = FALSE,
         alpha=FALSE,
         colour = guide_colorbar(title ="Daily Travel\nTime (hrs)",title.position='top'))


#ggsave(plot=p1,filename="./Figures 2021/FigS3_Map_TravelHrs.tiff",width=8.5,height=6,dpi=300)
#dev.off()

##########################
### DAILY TRAVEL SPD
##########################
data$daily.spd <- data$daily.cum.dist/1000 / data$daily.travel

min <- min(data$speed.mean,na.rm=TRUE)*3.6
max <- max(data$speed.mean,na.rm=TRUE)*3.6

p2 <- basemap +
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=speed.mean*3.6,group=segment),size=.5)+
  #scale_colour_gradient(low='orangered',high='navyblue',limits=c(min.spd,150))+
  scale_colour_gradientn(colors=cols, 
                         values = rescale(c(15,25,40,55,70)),
                         limits = c(9,70),
                         guide="colorbar") +
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
  ## Layout map
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.09,0.15),
        legend.background = element_rect(fill='white',colour='black'),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        legend.margin    = margin(t=12,r=12,b=10,l=12,unit = "pt"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text		    = element_blank(),
        axis.text		      = element_text(size=10,),
        axis.title		    = element_blank(),
        plot.margin       = unit(c(12,0,12,0),"pt"))+
  guides(fill = FALSE,
         alpha=FALSE,
         colour = guide_colorbar(expression(paste("Daily Mean\nSpeed (km h"^-1,")")),title.position='top'))


#ggsave(plot=p2,filename="./Figures 2021/FigS3_Map_DailySpd.tiff",width=8.5,height=6,dpi=300)
#dev.off()

##########################
### DAILY DISTANCE MAP
##########################
min<- min(data$daily.dist,na.rm=TRUE)/1000
max<- max(data$daily.dist,na.rm=TRUE)/1000

p3 <- basemap +
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=daily.dist/1000,group=segment),size=.5)+
  #scale_colour_gradient(low='orangered',high='navyblue',limits=c(min.spd,150))+
  scale_colour_gradientn(colors=cols, 
                         values = rescale(c(100,350,800,1400)),
                         guide="colorbar") +
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
  ## Layout map
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.09,0.15),
        legend.background = element_rect(fill='white',colour='black'),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text		    = element_blank(),
        axis.text		      = element_text(size=10,),
        axis.title		    = element_blank(),
        plot.margin       = unit(c(12,0,12,0),"pt"))+
  guides(fill = FALSE,
         alpha=FALSE,
         colour = guide_colorbar(title ="Daily\nDistance (km)",title.position='top'))

#ggsave(plot=p3,filename="./Figures 2021/FigS3_Map_DailyDistance.tiff",width=8.5,height=6,dpi=300)
#dev.off()

px <- cowplot::plot_grid(p1,p3,p2,ncol=1,nrow=3,labels=c("a","b","c"),lab_size=18)
ggsave(plot=px,filename='./Figures 2021/FigS3_PerformanceMaps-b.tiff',dpi=300,width=8.5,height=12)

##########################
### DAILY CUMULATIVE DISTNACE MAP
##################################
min <- min(data$daily.cum.dist,na.rm=TRUE)/1000
max <- max(data$daily.cum.dist,na.rm=TRUE)/1000

p4 <- basemap +
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=daily.cum.dist/1000,group=segment),size=.5)+
  scale_colour_gradientn(colors=cols, 
                         values = rescale(c(100,500,1000,1800)),
                         guide="colorbar") +
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.9,col='black')+
  ## Layout map
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.09,0.15),
        legend.background = element_rect(fill='white',colour='black'),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text		    = element_blank(),
        axis.text		      = element_text(size=10,),
        axis.title		    = element_blank(),
        plot.margin       = unit(c(0,0,0,0),"cm"))+
  guides(fill = FALSE,
         alpha=FALSE,
         colour = guide_colorbar(title ="Daily Cumulative\nDistance (km)",title.position='top'))

ggsave(plot=p4,filename="./Figures 2021/FigS3_Map_DailyCumulativeDistance.tiff",width=8.5,height=6,dpi=300)
dev.off()



##########################
### DAILY STRAIGHTNESS MAP
##########################
min <- min(data$daily.dist/data$daily.cum.dist)
max <- max(data$daily.dist/data$daily.cum.dist)

p5 <- basemap +
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=daily.dist/daily.cum.dist,group=segment),size=.5)+
  scale_colour_gradientn(colors=cols, 
                         values = rescale(c(0.5,0.75,0.85,0.95,max)),
                         guide="colorbar") +
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.9,col='black')+
  ## Layout map
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.09,0.15),
        legend.background = element_rect(fill='white',colour='black'),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text		    = element_blank(),
        axis.text		      = element_text(size=10,),
        axis.title		    = element_blank(),
        plot.margin       = unit(c(0,0,0,0),"cm"))+
  guides(fill = FALSE,
         alpha=FALSE,
         colour = guide_colorbar(title ="Daily\nStraightness",title.position='top'))

ggsave(plot=p5,filename="./Figures 2021/FigS3_Map_DailyStraightness.tiff",width=8.5,height=6,dpi=300)
dev.off()
rm(p1,p2,p3,p4,p5,px,min,max,cols,cols2)


