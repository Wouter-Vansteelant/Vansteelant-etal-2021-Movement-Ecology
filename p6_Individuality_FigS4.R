## Select subset of birds with many repeated trips to visualize individual differences
nr.trips <- data %>%
  group_by(dev,trip) %>%
  summarize(nr.trips = length(unique(yr))) 

nr.trips <- as.data.frame(nr.trips)
manytrips <- unique(nr.trips[which(nr.trips$trip == 'return' & nr.trips$nr.trips > 1),]$dev)
manytrips <- manytrips[!(manytrips %in% c("B1013","B2368","B2392"))]

data <- data[order(data$dev,data$dt),]
data$trip2 <- ifelse(data$trip == "out","Autumn","Spring")


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


basemap + 
  geom_path(data=data[which(data$dev %in% manytrips),],aes(x=long,y=lat,col=dev,group=segment),size=.5)+
  scale_colour_viridis_d(option="magma")+
# Add stop-overs
  geom_point(data=segs[which(segs$dev %in% manytrips),],aes(x=st.long,y=st.lat,size=round(as.numeric(st.dur)),col=dev),fill='white',alpha=.4,shape=21)+
#  scale_size_binned(name="Stop duration [days]",range=c(1,8),
#                    breaks=c(1,3,7,12),labels=c("1","2-3","4-7","8-12"))+
  geom_point(data=segs[which(segs$dev %in% manytrips),],aes(x=st.long,y=st.lat,col=dev),size=.3)+
  theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.11,0.22),
        legend.direction  = 'vertical',
        legend.background = element_rect(fill='white',colour='black'),
        panel.grid       = element_blank(),
        legend.title 	    = element_text(size=11),
        legend.text 	    = element_text(size=10),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10,),
        axis.title		    = element_blank(),
        plot.margin       = unit(c(0,0,0,0),"cm"))+
  guides(fill = FALSE,
         alpha = FALSE,
         colour = guide_legend(title ="Falcon ID",title.position='top',ncol=2),
         size = FALSE)


ggsave("./Figures 2021/FigS7_Map_Individuals_v20210421.tiff",width=8.5,height=6,dpi=300)
dev.off()
