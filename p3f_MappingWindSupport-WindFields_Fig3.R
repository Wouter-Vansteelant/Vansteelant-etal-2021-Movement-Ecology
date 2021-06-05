## PRODUCES FIG2

### MAPPPING OF TRACKS VS ITCZ AND SEASONAL WIND FIELDS
########################################################
## Set limits for geographical domain
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5)
latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5)

# set colour scale
cols <- brewer.pal(n = 5, name = "RdYlBu")

ggplot()+
  ## Add humidity
  geom_tile(data=rain.seasonal, aes(x = long, y = lat, z=mean_hum, fill=mean_hum),alpha=.6) +
  #scale_fill_continuous(low='white',high='grey10',na.value="white",aesthetics="fill",breaks=c(50,100,200,300,500))+
  scale_fill_gradientn(colors=brewer.pal(n = 5, name = "Greys"),
                       na.value="grey20",aesthetics="fill",
                        values = rescale(c(0,25,50,125,250)),
                       limits=c(0, 510))+
  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='black',fill='transparent',size=.15)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add wind vectors
  stat_contour(data=rain.seasonal, aes(x = long, y = lat, z=mean_hum, fill=mean_hum), color="white", size=0.3,breaks=c(0,25,50,125,250))+
  ggquiver::geom_quiver(data=wind.seasonal,aes(x=long,y=lat,u=delta_long,v=delta_lat),col='grey40',size=.5)+
  ## Add ITCZ lines
  geom_line(data=itcz.seasonal,aes(x=long,y=mean_lat),col='red',size=.6)+
  geom_line(data=itcz.seasonal,aes(x=long,y=min_lat),col='red',size=.6,linetype='dashed')+
  geom_line(data=itcz.seasonal,aes(x=long,y=max_lat),col='red',size=.6,linetype='dashed')+
  geom_ribbon(data=itcz.seasonal,aes(x=long,ymin=min_lat,ymax=max_lat),fill='red',size=.8,alpha=.1)+
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=tailwind.r,group=segment),size=.5)+
  ## Define colour scale for wind support
 # scale_color_gradient2_tableau(palette = "Red-Blue Diverging", guide = "colourbar")+
  scale_colour_gradientn(colours = cols, 
                         values = rescale(c(-16, -5, 0, 5, 16)),
                         limits=c(-16, 16)) +
 #scale_colour_manual(values=c("2012"="lightgreen","2013"="blue","2014"="yellow","2015"="olivedrab2","2016"="orange","2017"="cornflowerblue","2018"="goldenrod2","2019"="violet"))+
 ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
  ## Add star for Alegranza
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip2) + theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.085,0.18),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=8,face='bold'),
        legend.text 	    = element_text(size=6,face='bold'),
        legend.background    = element_rect(fill='white'),
        legend.spacing.y  = unit(0,"cm"),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank())+
  guides(fill=guide_colorbar(order=1,title =expression(paste(bold("Rainfall (mm season"^-1,")"),bold(")"))),
                           title.position='top'),
       colour=guide_colorbar(order=2,title =expression(paste(bold("Tailwind (ms"^-1,")"),bold(")"))),
                             title.position='top'),
       linetype=FALSE)

ggsave('./Figures 2021/Fig3A_Tracks-vs-WindSupport_ITCZ_Windfields_wtStops-v20210421.tiff',width=8.5,height=6,dpi=300)
dev.off()


ggplot()+
  ## Add humidity
  geom_tile(data=rain.seasonal, aes(x = long, y = lat, z=mean_hum, fill=mean_hum),alpha=.8) +
  scale_fill_gradientn(colors=brewer.pal(n = 5, name = "Greys"),
                       na.value="grey20",aesthetics="fill",
                       values = rescale(c(0,50,100,250,510)),
                       limits=c(0, 510))+
  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='black',fill='transparent',size=.15)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add wind vectors
  stat_contour(data=rain.seasonal, aes(x = long, y = lat, z=mean_hum, fill=mean_hum), color="white", size=0.3,breaks=c(50,100,200,300,500))+
  ggquiver::geom_quiver(data=wind.seasonal,aes(x=long,y=lat,u=delta_long,v=delta_lat),col='grey40',size=.5)+
  ## Add ITCZ lines
  ## Add ITCZ lines
  geom_line(data=itcz.seasonal,aes(x=long,y=mean_lat),col='red',size=.6)+
  geom_line(data=itcz.seasonal,aes(x=long,y=min_lat),col='red',size=.6,linetype='dashed')+
  geom_line(data=itcz.seasonal,aes(x=long,y=max_lat),col='red',size=.6,linetype='dashed')+
  geom_ribbon(data=itcz.seasonal,aes(x=long,ymin=min_lat,ymax=max_lat),fill='red',size=.8,alpha=.2)+
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=tailwind,group=segment),size=.5)+
  ## Define colour scale for wind support
  scale_colour_gradientn(colours = cols, 
                         values = rescale(c(-16, -5, 0, 5, 16)),
                         guide = "colorbar", limits=c(-16, 16)) +
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
  ## Add star for Alegranza
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip2) + theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.085,0.18),
        legend.spacing.y  = unit(0,"cm"),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=8,face='bold'),
        legend.text 	    = element_text(size=6,face='bold'),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank())+
  guides(fill=guide_colorbar(order=1,title =expression(paste(bold("Rainfall (mm season"^-1,")"),bold(")"))),
                             title.position='top'),
         colour=guide_colorbar(order=2,title =expression(paste(bold("Tailwind\nto Goal (ms"^-1,")"),bold(")"))),
                               title.position='top'),
         linetype=FALSE)

ggsave('./Figures 2021/Fig3C_Tracks-vs-WindSupport-to-Goal_ITCZ_Windfields_wtStops-v20210421.tiff',width=8.5,height=6,dpi=300)
dev.off()

cols2 <- brewer.pal(n = 5, name = "BrBG")
cols <- brewer.pal(n = 7, name = "RdYlBu")


ggplot()+
  ## Add seaosnal wind support fields
  geom_tile(data=wind.seasonal, aes(x = long, y = lat, z=mean_tailwind, fill=mean_tailwind),alpha=.6) +
  scale_fill_gradientn(colours = cols2, 
                       values = rescale(c(-8, -5, 0, 5, 8)),
                       guide = "colorbar", 
                       limits=c(-8, 8)) +  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='black',fill='transparent',size=.15)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add wind vectors
  ggquiver::geom_quiver(data=wind.seasonal,aes(x=long,y=lat,u=delta_long,v=delta_lat),col='grey40',size=.5)+
  ## Add ITCZ lines
  ## Add ITCZ lines
  geom_line(data=itcz.seasonal,aes(x=long,y=mean_lat),col='black',size=.6)+
  geom_line(data=itcz.seasonal,aes(x=long,y=min_lat),col='black',size=.6,linetype='dashed')+
  geom_line(data=itcz.seasonal,aes(x=long,y=max_lat),col='black',size=.6,linetype='dashed')+
  geom_ribbon(data=itcz.seasonal,aes(x=long,ymin=min_lat,ymax=max_lat),fill='grey20',size=.8,alpha=.2)+
  ## Add tracking data
  geom_path(data=data,aes(x=long,y=lat,col=gain/w,group=segment),size=.5)+
  ## Define colour scale for wind support
  #scale_color_gradient2_tableau(palette = "Red-Blue Diverging", guide = "colourbar")+
  scale_colour_gradientn(colours = cols, 
                         values = rescale(c(-2, -1, -0.25, 0, 0.25, 1, 2)),
                         guide = "colorbar", limits=c(-2, 2)) +
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
  ## Add star for Alegranza
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip2) + theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.085,0.18),
        legend.direction  = 'horizontal',
        legend.title 	    = element_text(size=8,face='bold'),
        legend.text 	    = element_text(size=6,face='bold'),
        legend.background    = element_rect(fill='white'),
        legend.spacing.y  = unit(0,"cm"),
        #      legend.margin     = unit(0.2,"cm"),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank())+
  guides(fill=guide_colorbar(title =expression(paste(bold("Tailwind\nto Goal (ms"^-1,")"),bold(")"))),
                             title.position='top'),
         # Legend for winds
         colour=guide_colorbar(title ="Local Wind\nGain Index (w)",
                               title.position='top'),
         linetype=FALSE)


ggsave('./Figures 2021/Fig3B_Tracks-vs-WindGain_ITCZ_Windfields_wtStops-v20210421.tiff',width=8.5,height=6,dpi=300)
dev.off()


### PLOT ORIENTATION STRATEGIES
data$comp <- factor(data$comp, levels = c("CO", "CF", "CP", "NSW", "DF", "DO","0"))

ggplot()+
  ## Add seaosnal wind support fields
  geom_tile(data=wind.seasonal, aes(x = long, y = lat, z=mean_tailwind, fill=mean_tailwind),alpha=.6) +
  scale_fill_gradientn(colours = cols2, 
                       values = rescale(c(-8, -5, 0, 5, 8)),
                       guide = "colorbar", 
                       limits=c(-8, 8)) +  
  ## Add borders, coastlines and likes
  geom_polygon(data=political,aes(long,lat,group=paste(country,piece)),col='black',fill='transparent',size=.15)+	
  geom_polygon(data=lakes, aes(x = long, y = lat, group = group), fill = 'white') +
  ## Add wind vectors
  stat_contour(data=rain.seasonal, aes(x = long, y = lat, z=mean_hum, fill=mean_hum), color="white", size=0.3,breaks=c(50,100,200,300,500))+
  ggquiver::geom_quiver(data=wind.seasonal,aes(x=long,y=lat,u=delta_long,v=delta_lat),col='grey40',size=.5)+
  ## Add ITCZ lines
  geom_line(data=itcz.seasonal,aes(x=long,y=mean_lat),col='black',size=.6)+
  geom_line(data=itcz.seasonal,aes(x=long,y=min_lat),col='black',size=.6,linetype='dashed')+
  geom_line(data=itcz.seasonal,aes(x=long,y=max_lat),col='black',size=.6,linetype='dashed')+
  geom_ribbon(data=itcz.seasonal,aes(x=long,ymin=min_lat,ymax=max_lat),fill='grey20',size=.8,alpha=.2)+
  ## Add tracking data
  geom_path(data=data[which(data$comp != "0" & data$spd.f*3.6 > 5),],aes(x=long,y=lat,col=comp,group=segment),size=.6)+
  ## Define colour scale for Orientation strategy
  scale_colour_manual(values = c("0"="black","CF"="orange","CO" = "red","CP"="yellow",
                                 "DF" = "cornflowerblue","DO" = "blue","NSW" = "lightgreen"))+ 
  ## Add stopovers
  geom_point(data=segs,aes(x=st.long,y=st.lat),size=.4,col='black')+
  ## Add star for Alegranza
  geom_point(data=ref,aes(x=ale.long,y=ale.lat),size=2.1,col='red',shape=3)+
  ## Layout map
  coord_quickmap(xlim=longlimits,ylim=latlimits,expand=FALSE) + 
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  facet_grid(.~trip2) + theme_bw() + 
  ## Layout text items
  theme(panel.border	    = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = c(0.07,0.15),
        legend.direction  = 'vertical',
        legend.title 	    = element_text(size=8,face='bold'),
        legend.text 	    = element_text(size=6,face='bold'),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_blank())+
  guides(fill=FALSE,
         colour = guide_legend(title = 'Orientation',title.position='top',ncol=2),
         linetype=FALSE)


ggsave('./Figures 2021/Fig3D_Tracks-per-Orientation-vs-ITCZ_Windfields_wtStops-v20210421.tiff',width=8.5,height=6,dpi=300)
dev.off()


### BARPLOTS ORIENTATION VS DISTANCE TO COLONY AND BIOME
p1 <- ggplot()+
  geom_bar(data=data[which(data$comp != "0" & data$spd.f*3.6 > 5),],aes(x=round(dist.to.colony/100000),fill=comp,y=..count..,group=paste(round(dist.to.colony/100000),comp)),size=.6,width=1,position="fill")+
  scale_fill_manual(values = c("CO" = "red","CF"="orange","CP"="yellow","NSW" = "lightgreen",
                               "DF" = "cornflowerblue","DO" = "blue","0"="black"))+ 
  ## Layout map
  facet_grid(trip2~.)+
  theme_bw()+xlab("Distance to colony [*100km]")+
  xlim(-0.5,82.5)+ylim(0,1)+
  geom_hline(yintercept=0.5,linetype='dashed')+
  coord_cartesian(expand=FALSE)+ylab("Proportion active flight segments\n")+
  theme(panel.grid        = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_blank(),
        axis.text		      = element_text(size=10,face='bold'),
        axis.title		    = element_text(size=11,face='bold'))

p2 <- ggplot()+
  geom_bar(data=data[which(data$comp != "0" & data$spd.f*3.6 > 5),],aes(x=biome3,fill=comp,y=..count..,group=paste(biome3,comp)),size=.6,position="fill")+
  scale_fill_manual(values = c("CO" = "red","CF"="orange","CP"="yellow","NSW" = "lightgreen",
                               "DF" = "cornflowerblue","DO" = "blue","0"="black"))+ 
  ## Layout map
  facet_grid(trip2~.)+ylab(" ")+
  theme_bw()+xlab("Biome")+ ylim(0,1)+
  geom_hline(yintercept=0.5,linetype='dashed')+
  theme(panel.grid        = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.position   = 'none',
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=12,face='bold'),
        axis.text		      = element_text(size=10,face='bold'),
        axis.title		    = element_text(size=11,face='bold'))

# extract a legend that is laid out horizontally
legend_b <- get_legend(
  p1 + 
    guides(fill = guide_legend(title = 'Orientation',title.position='top',nrow=1)) +
    theme(legend.position = "bottom",
          legend.background = element_rect(fill="white",colour="black"))
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).

px <- cowplot::plot_grid(p1,p2,align="h",labels = c("a", "b"))
px2 <- cowplot::plot_grid(px, legend_b, ncol = 1, rel_heights = c(1, .15))

ggsave(plot=px2,filename='./Figures 2021/FigS6_Orientation-barplot-v20210421.tiff',dpi=300,width=8.5,height=6)

rm(legend_b,px,px2,p1,p2)
