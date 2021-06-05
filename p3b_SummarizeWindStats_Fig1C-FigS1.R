#####################################
### CALCULATE DAILY SUMMARY STATISTICS          
#####################################
lun <- function(x) length(unique(x))

data$travel.i <- ifelse(data$spd.f*3.6 > 5,1,0)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### SUMMARY STATS PER DAY
wind.days <- data %>%
  filter(travel.i == 1) %>%
  group_by(dev,yr,trip,tripID,indday) %>%
  summarize(w.mean = mean(w, na.rm = TRUE),
            tailwind.r.mean = mean(tailwind.r, na.rm=TRUE),
            sidewind.r.mean = mean(sidewind.r, na.rm=TRUE),
            tailwind.mean = mean(tailwind, na.rm=TRUE),
            sidewind.mean = mean(sidewind, na.rm=TRUE),
            speed.mean = mean(spd,na.rm=TRUE),
            daily.cum.dist = unique(daily.cum.dist,na.rm=TRUE),
            daily.dist = unique(daily.dist),
            daily.dir = unique(daily.dir),
            daily.travel = sum(dur.f,na.rm=TRUE)/3600,
            daily.long = median(long,na.rm=TRUE),
            daily.biome = getmode(biome3)) 
wind.days <- as.data.frame(wind.days)

wind.days$daily.spd = (wind.days$daily.dist/1000)/wind.days$daily.travel
wind.days$daily.cum.spd = (wind.days$daily.cum.dist/1000)/wind.days$daily.travel
wind.days$daily.straight = wind.days$daily.dist/wind.days$daily.cum.dist


wind.days.dayhrs <- data %>%
  filter(travel.i == 1 & daynight == 'day') %>%
  group_by(dev,yr,trip,tripID,indday) %>%
  summarize(daily.dayhrs = sum(dur.f,na.rm=TRUE)/3600)
wind.days.dayhrs <- as.data.frame(wind.days.dayhrs)

wind.days.nighthrs <- data %>%
  filter(travel.i == 1 & daynight != 'day') %>%
  group_by(dev,yr,trip,tripID,indday) %>%
  summarize(daily.nighthrs = sum(dur.f,na.rm=TRUE)/3600)
wind.days.nighthrs <- as.data.frame(wind.days.nighthrs)

wind.days <- merge(wind.days, wind.days.dayhrs,all.x=TRUE)
wind.days <- merge(wind.days, wind.days.nighthrs,all.x=TRUE)

rm(wind.days.dayhrs,wind.days.nighthrs)

### SUMMARY DIALY STATS PER TRIP
trip.stats <- wind.days %>%
  group_by(tripID) %>%
  summarize(trip.tailwind.r = mean(tailwind.r.mean, na.rm = TRUE),
            trip.sidewind.r = mean(sidewind.r.mean, na.rm = TRUE),
            trip.tailwind = mean(tailwind.mean, na.rm = TRUE),
            trip.sidewind = mean(sidewind.mean, na.rm = TRUE),
            trip.daily.dist = mean(daily.dist, na.rm = TRUE),
            trip.daily.spd = mean(daily.spd, na.rm = TRUE),
            trip.daily.cum.dist = mean(daily.cum.dist, na.rm = TRUE),
            trip.daily.cum.spd = mean(daily.cum.spd, na.rm = TRUE),
            trip.daily.travel = mean(daily.travel,na.rm=TRUE),
            trip.daily.dayhrs = sum(daily.dayhrs,na.rm=TRUE),
            trip.daily.nighthrs = sum(daily.nighthrs,na.rm=TRUE)) 
trip.stats <- as.data.frame(trip.stats)

### BIND DAILY STATS PER TRIP TO TRIP PERFORMANCE
trip.summary <- merge(trip.summary,trip.stats,all.x=TRUE)

first.yrs <- trip.summary %>%
  group_by(dev,trip) %>%
  summarize(first.yr = min(year(trip.start)))

trip.summary <- merge(trip.summary,first.yrs,all.x=TRUE)
trip.summary$trip.nr <- year(trip.summary$trip.start) - trip.summary$first.yr + 1

### SUMMARY TRIP STATS PER SEASON
season.stats <- trip.summary %>%
  group_by(trip) %>%
  summarize(tailwind.r = mean(trip.tailwind.r, na.rm = TRUE),
            tailwind.r.sd = sd(trip.tailwind.r, na.rm = TRUE),
            sidewind.r =  mean(trip.sidewind.r, na.rm = TRUE),
            sidewind.r.sd = sd(trip.sidewind.r, na.rm = TRUE),
            tailwind = mean(trip.tailwind, na.rm = TRUE),
            tailwind.sd = sd(trip.tailwind, na.rm = TRUE),
            sidewind =  mean(trip.sidewind, na.rm = TRUE),
            sidewind.sd = sd(trip.sidewind, na.rm = TRUE),
            daydist =  mean(trip.daily.dist, na.rm = TRUE),
            daydist.sd = sd(trip.daily.dist, na.rm = TRUE), 
            daytime =  mean(trip.daily.travel, na.rm = TRUE),
            daytime.sd = sd(trip.daily.travel, na.rm = TRUE),   
            dayhrs =  mean(trip.daily.dayhrs, na.rm = TRUE),
            dayhrs.sd = sd(trip.daily.dayhrs, na.rm = TRUE),   
            nighthrs =  mean(trip.daily.nighthrs, na.rm = TRUE),
            nighthrs.sd = sd(trip.daily.nighthrs, na.rm = TRUE),    
            detour =  mean(detour, na.rm = TRUE),
            detour.sd = sd(detour, na.rm = TRUE),
            cumdist =  mean(trip.cumdist, na.rm = TRUE),
            cumdist.sd = sd(trip.cumdist, na.rm = TRUE),
            dur =  mean(trip.dur, na.rm = TRUE),
            dur.sd = sd(trip.dur, na.rm = TRUE),
            traveldays =  mean(trip.traveldays, na.rm = TRUE),
           traveldays.sd = sd(trip.traveldays, na.rm = TRUE),
           restdays =  mean(trip.restdays, na.rm = TRUE),
            restdays.sd = sd(trip.restdays, na.rm = TRUE)) 
season.stats <- as.data.frame(season.stats)

### AVERAGE DAILY/TRIP STATISTICS PER INDIVIDUAL AND SEASON 
ind.stats <- trip.summary %>%
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
            mean.trip.tailwind.r = mean(trip.tailwind.r),
            sd.trip.tailwind.r = sd(trip.tailwind.r),
            mean.trip.tailwind = mean(trip.tailwind),
            sd.trip.tailwind = sd(trip.tailwind),
            mean.trip.daily.dist = mean(trip.daily.dist),
            sd.trip.daily.dist = sd(trip.daily.dist),
            mean.trip.daily.spd = mean(trip.daily.spd),
            sd.trip.daily.spd = sd(trip.daily.spd),
            mean.trip.daily.travel = mean(trip.daily.travel),
            sd.trip.daily.travel = sd(trip.daily.travel),
            mean.trip.daily.dayhrs = mean(trip.daily.dayhrs),
            sd.trip.daily.dayhrs = sd(trip.daily.dayhrs),
            mean.trip.daily.nighthrs = mean(trip.daily.nighthrs),
            sd.trip.daily.nighthrs = sd(trip.daily.nighthrs)) 

ind.stats <- as.data.frame(ind.stats)
ind.stats <- merge(ind.stats,unique(ind.summ[,c("dev","trip","mean.detour","sd.detour")]),all.x=TRUE)

#write.csv(ind.stats,'./Final Figures/ExtraTable1_SummaryStats-perID&Season.csv')


#####################################
#### PRODUCE BOXPLOTS FOR FIG 1
#####################################
library(ggpubr)

# function for summary stats
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

pi <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=detour,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("\nDetour Index")+ theme_bw()+
  ylim(1.1,2.1)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())

pi2 <- pi + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',..p.format..)),size=5,method='t.test',paired=TRUE,label.y=2.05,label.x=1.5)



pj <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.dur,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylim(18,77)+
  ylab("Trip Duration\n[days]")+ theme_bw()+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank(),)
pj2 <- pj + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',..p.format..)),size=5,method='t.test',paired=TRUE,label.y=74,label.x=1.5) 

#px <- grid.arrange(pa2,pb2,ncol=2)
#ggsave(plot=px,filename="./Draft1/Fig1c_BoxPlots-v20201022.tiff",width=6,height=3.5,dpi=300)
#rm(pa,pa2,pb,pb2,px)

pa <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.restdays,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("\nStop-over Days")+ theme_bw()+
  ylim(0,35)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())
pa2 <- pa + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=32.5,label.x=1.5) 


pb <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.traveldays,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylim(18,43)+
  ylab("\nTravel Days")+ theme_bw()+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())

pb2 <- pb + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=41.5,label.x=1.5) 

pe <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.daily.dist/1000,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("Mean Daily\nDistance [km]")+ theme_bw()+
  ylim(250,545)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())
pe2 <- pe + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=520,label.x=1.5) 

# B2423 not considered for this test due to odd outlier for spring migration
pf <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337","B2423")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.daily.travel,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("Mean Daily\nTravel Time [h]")+ theme_bw()+
  ylim(11,15.75)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank(),
        panel.spacing       = unit(c(0.75,0,0,0),"cm"))

pf2 <- pf + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=15.35,label.x=1.5) 


pg <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.daily.dayhrs,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("Diurnal\nFlight Hours")+ theme_bw()+
  ylim(180,475)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())
pg2 <- pg + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=460,label.x=1.5) 


ph <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.daily.nighthrs,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("Nocturnal\nTravel Hours")+ theme_bw()+
  ylim(40,127)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank(),
        panel.spacing       = unit(c(0.75,0,0,0),"cm"))

ph2 <- ph + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=122,label.x=1.5) 


pc <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.tailwind.r,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  ylab("Mean Tailwind\nto Track [m/s]")+ theme_bw()+
  scale_y_continuous(limits=c(-2.7,4.4))+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())
pc2 <- pc + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=4,label.x=1.5) 



pd <- ggplot(data=trip.summary[which(!(trip.summary$dev %in% c("B2337")) & trip.summary$trip.nr == 1),],
             aes(x=trip,y=trip.tailwind,col=trip,fill=trip)) + 
  geom_violin(trim=TRUE,alpha=.3)+
  #geom_smooth(method="glm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  stat_summary(fun.data=data_summary)+
  geom_line(aes(group=dev),col='grey20',size=.2)+
  geom_point(size=.8) +
  scale_y_continuous(limits=c(-2.7,4.4))+
  ylab("Mean Tailwind\nto Goal[m/s]")+ theme_bw()+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line         = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text.y		     = element_text(size=12),
        axis.title.y		  = element_text(size=14),
        axis.title.x		  = element_blank(),
        axis.text.x        = element_blank())
pd2 <- pd + stat_compare_means(aes(label = ifelse(as.numeric(..p.format..) < 0.001,'<0.001',formatC(as.numeric(..p.format..), digits = 3, format = "f"))),size=5,method='t.test',paired=TRUE,label.y=4,label.x=1.5) 

px <- cowplot::plot_grid(pi2,pj2,pa2,pb2,pg2,ph2,pe2,pf2,pc2,pd2, ncol=2, align="v",labels=c("c","d","e","f","g","h","i","j","k","l"),label_size=16)

ggsave(plot=px,filename="./Figures 2021/Fig1C_BoxPlots-v20210421.tiff",width=7,height=10,dpi=300)
dev.off()
rm(pa,pb,pc,pd,pe,pf,pg,ph,pa2,pb2,pe2,pf2,pc2,pd2,pg2,ph2,px,pi,pi2,pj,pj2,my_comparisons)

###########################################
### PRODUCE FIGS2
###########################################
##  trip duration vs detour
m1 <- lm(data=ind.summ[-c(9,33),],log(mean.trip.dur) ~ trip + mean.detour)
rsq <- paste("rsq =",sprintf(summary(m1)$r.squared, fmt = '%#.2f'),sep=' ')
p1 <- paste("p[d] =",sprintf(summary(m1)$coefficients[3,4], fmt = '%#.2f'),sep=' ')
p2 <- paste("p[s] =",sprintf(summary(m1)$coefficients[2,4], fmt = '%#.2f'),sep=' ')
p3 <- paste("p[s:d] =",sprintf(summary(m1)$coefficients[4,4], fmt = '%#.2f'),sep=' ')
string <- paste(rsq,p1,p2,sep="\n")

pa<- ggplot(data=ind.summ,aes(x=mean.detour,y=mean.trip.dur,col=trip,fill=trip)) + 
  geom_blank()+
  geom_smooth(method="lm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  geom_point(aes(col=trip,fill=trip),size=.6)+
  geom_errorbar(data=ind.summ,aes(ymax = mean.trip.dur + sd.trip.dur, ymin = mean.trip.dur - sd.trip.dur, height = .2,group=dev,col=trip),alpha=.5)+
  geom_errorbarh(data=ind.summ,aes(xmax = mean.detour + sd.detour, xmin = mean.detour - sd.detour, height = .2,group=dev,col=trip),alpha=.5)+
  ylab("Trip duration\n[days]")+
  xlab("Detour index\n")+ theme_bw() +
  scale_y_continuous(trans="log2")+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=12))+
  annotate("text",x=1.7,y=57,label=string,size=4,hjust=0,parse=FALSE)

#  annotate("text",x=1.68,y=21,label=rsq,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.68,y=19,label=p1,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.68,y=17.5,label=p2,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.68,y=16,label=p3,size=3.5,hjust=0,parse=TRUE)

#ggsave(p2,filename='./Final Figures/Fig1C_iii_Duration-vs-Detour.tiff',dpi=300,width=6,height=3.5)

m1 <- lm(data=ind.summ,log(mean.trip.restday+1)~mean.detour+trip)
rsq <- paste("rsq =",sprintf(summary(m1)$r.squared, fmt = '%#.2f'),sep=' ')
p1 <- paste("p[d] =",sprintf(summary(m1)$coefficients[3,4], fmt = '%#.2f'),sep=' ')
p2 <- paste("p[s] =",sprintf(summary(m1)$coefficients[2,4], fmt = '%#.2f'),sep=' ')
p3 <- paste("p[s:d] =",sprintf(summary(m1)$coefficients[4,4], fmt = '%#.2f'),sep=' ')
string <- paste(rsq,p1,p2,sep="\n")

pb<- ggplot(data=ind.summ,aes(x=mean.detour,y=mean.trip.restday+1,col=trip,fill=trip)) + 
  geom_blank()+
  geom_smooth(method="lm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  geom_point(size=.6)+
  geom_errorbar(data=ind.summ,aes(ymax = mean.trip.restday+1 + sd.trip.restday, ymin = mean.trip.restday+1 - sd.trip.restday, height = .2,group=dev,col=trip),alpha=.5)+
  geom_errorbarh(data=ind.summ,aes(xmax = mean.detour + sd.detour, xmin = mean.detour - sd.detour, height = .2,group=dev,col=trip),alpha=.5)+
  ylab("\nStop Days")+
  xlab("Detour index\n")+ theme_bw() +
  scale_y_continuous(trans="log2")+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=12))+
  annotate("text",x=1.7,y=16,label=string,size=4,hjust=0,parse=FALSE)

#+
#  annotate("text",x=1.7,y=30,label=rsq,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.7,y=28.5,label=p1,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.7,y=27,label=p2,size=3.5,hjust=0,parse=TRUE)

m1 <- lm(data=ind.summ,log(mean.trip.traveldays)~trip + mean.detour)
rsq <- paste("rsq =",sprintf(summary(m1)$r.squared, fmt = '%#.2f'),sep=' ')
p1 <- paste("p[d] =",sprintf(summary(m1)$coefficients[3,4], fmt = '%#.2f'),sep=' ')
p2 <- paste("p[s] =",sprintf(summary(m1)$coefficients[2,4], fmt = '%#.2f'),sep=' ')
p3 <- paste("p[s:d] =",sprintf(summary(m1)$coefficients[4,4], fmt = '%#.2f'),sep=' ')
string <- paste(rsq,p1,p2,sep="\n")

pc<- ggplot(data=ind.summ,aes(x=mean.detour,y=mean.trip.traveldays,col=trip,fill=trip)) + 
  geom_smooth(formula= 'y ~ x',method="lm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  geom_point(size=.6)+
  geom_errorbar(data=ind.summ,aes(ymax = mean.trip.traveldays + sd.trip.traveldays, ymin = mean.trip.traveldays - sd.trip.traveldays, height = .2,group=dev,col=trip),alpha=.5)+
  geom_errorbarh(data=ind.summ,aes(xmax = mean.detour + sd.detour, xmin = mean.detour - sd.detour, height = .2,group=dev,col=trip),alpha=.5)+
  ylab("\nTravel Days")+
  xlab("Detour index\n")+ theme_bw() +
  scale_y_continuous(trans="log2",breaks=c(20,30,40))+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line = element_line(size=.4),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=12))+
  annotate("text",x=1.7,y=36,label=string,size=4,hjust=0,parse=FALSE)

#  +
#  annotate("text",x=1.8,y=44,label=rsq,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.8,y=41.5,label=p1,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.8,y=39,label=p2,size=3.5,hjust=0,parse=TRUE)


m1 <- lm(data=ind.stats,mean.trip.tailwind.r~trip * mean.detour)
rsq <- paste("rsq =",sprintf(summary(m1)$r.squared, fmt = '%#.2f'),sep=' ')
p1 <- paste("p[d] =",sprintf(summary(m1)$coefficients[3,4], fmt = '%#.2f'),sep=' ')
p2 <- paste("p[s] =",sprintf(summary(m1)$coefficients[2,4], fmt = '%#.2f'),sep=' ')
p3 <- paste("p[s:d] =",sprintf(summary(m1)$coefficients[4,4], fmt = '%#.2f'),sep=' ')
string <- paste(rsq,p1,p2,sep="\n")

pd <- ggplot(data=ind.stats,aes(x=mean.detour,y=mean.trip.tailwind.r,col=trip,fill=trip)) + 
  geom_blank()+
#  geom_smooth(formula= 'y ~ x',method="lm",alpha=.2)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  geom_point(size=.6)+
  geom_errorbar(data=ind.stats,aes(ymax = mean.trip.tailwind.r + sd.trip.tailwind.r, ymin = mean.trip.tailwind.r - sd.trip.tailwind.r, height = .2,group=dev,col=trip),alpha=.5)+
  geom_errorbarh(data=ind.stats,aes(xmax = mean.detour + sd.detour, xmin = mean.detour - sd.detour, height = .2,group=dev,col=trip),alpha=.5)+
  ylab("Mean daily\ntailwind [m/s]")+
  xlab("Detour index\n")+ theme_bw() +
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.text        = element_blank(),
        strip.background	= element_blank(),
        axis.line = element_line(size=.4),
        legend.title 	    = element_text(size=12,face='bold'),
        legend.text 	    = element_text(size=8,face='bold'),
        axis.text		      = element_text(size=10),
        axis.title		    = element_text(size=12))  +
   annotate("text",x=1.7,y=1.9,label=string,size=4,hjust=0,parse=FALSE)
#  annotate("text",x=1.68,y=-1,label=rsq,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.68,y=-1.5,label=p1,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.68,y=-2,label=p2,size=3.5,hjust=0,parse=TRUE)+
#  annotate("text",x=1.68,y=-2.5,label=p2,size=3.5,hjust=0,parse=TRUE)

plot <- cowplot::plot_grid(pa,pb,pc,pd,ncol=2,labels=c("a","b","c","d"),label_size=16,align="v")
plot
ggsave(plot=plot,filename="./Figures 2021/FigS3_Performance-vs-Detour-v20210421.tiff",width=8.5,height=6,dpi=300)

dev.off()
rm(pa,pb,pc,pd,p1,plot,p1,p2,p3,rsq,m1)



### PRODUCE SUPPL FIGURES BOXPLOTS PERFORMANCE AND WIND SUPPORT PER Biome/SEASON
#########################################

library(ggpubr)
library(agricolae)

# function for summary stats
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

wind.days$daily.biome <- as.factor(wind.days$daily.biome)
wind.days$trip2 <- ifelse(wind.days$trip == "out","Autumn","Spring")

## DAILY TRAVEL TIME PER BIOME
#compare_means(daily.travel ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Autumn"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","humid forest"),c("other","sea"))

ss.aut <- wind.days[which(wind.days$trip2 == "Autumn"),]
ss.spr <- wind.days[which(wind.days$trip2 == "Spring"),]

Tukey_test <- aov(daily.travel~daily.biome, data=ss.aut) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(28)

pa <- ggplot(data=ss.aut,aes(y=daily.travel,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(1,29))+
  xlab("") + ylab("Daily Travel\nTime [h]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))
pa2 <- pa + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
#pa2 <- pa + stat_compare_means(method = "anova",label.y=28,label.x=0.75)+      # Add global p-value
#  stat_compare_means(label = "p.signif", method = "t.test",
#                     ref.group = "other",label.y=25.5)   


### DAILY TRAVEL TIME PER BIOME - spring
#compare_means(daily.travel ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Spring"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","sea"))

Tukey_test <- aov(daily.travel~daily.biome, data=ss.spr) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(28)

pb <- ggplot(data=ss.spr,aes(y=daily.travel,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(1,29))+
  xlab("") + ylab("Daily Travel\nTime [h]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pb2 <- pb + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
 #stat_compare_means(method = "anova",label.y=28,label.x=0.75)+      # Add global p-value
#  stat_compare_means(label = "p.signif", method = "t.test",
#                     ref.group = "other",label.y=25.5)   

### DAILY DISTANCE PER BIOME - autumn
#compare_means(daily.dist ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Autumn"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","humid forest"),c("other","sea"))

Tukey_test <- aov(daily.dist/1000~daily.biome, data=ss.aut) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups")

Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(1650)

pc <- ggplot(data=ss.aut,aes(y=daily.dist/1000,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(0,1750))+
  xlab("") + ylab("\nDaily Distance [km]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pc2 <- pc + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=1650,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=1450)   


### DAILY DISTANCE PER BIOME - spring
#compare_means(daily.dist ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Spring"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","sea"))

Tukey_test <- aov(daily.dist/1000~daily.biome, data=ss.spr) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(1650)

pd <- ggplot(data=ss.spr,aes(y=daily.dist/1000,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(0,1750))+
  xlab("") + ylab("\nDaily Distance [km]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pd2 <- pd + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=1650,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=1450)     


### DAILY SPEED PER BIOME - autumn
#compare_means(daily.spd ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Autumn"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","humid forest"),c("other","sea"))

Tukey_test <- aov(daily.spd~daily.biome, data=ss.aut) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(77)

pe <- ggplot(data=ss.aut,aes(y=daily.spd,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(0,80))+
  xlab("") + ylab("Daily Mean\nSpeed [km/h]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pe2 <- pe + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=77,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=69)   


### DAILY SPEED PER BIOME - spring
#compare_means(daily.spd ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Spring"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","sea"))

Tukey_test <- aov(daily.spd~daily.biome, data=ss.spr) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(77)

pf <- ggplot(data=ss.spr,aes(y=daily.spd,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(0,80))+
  xlab("") + ylab("Daily Mean\nSpeed [km/h]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pf2 <- pf + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=77,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=69)    


### DAILY TAILWIND PER BIOME - autumn
#compare_means(tailwind.r.mean ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Autumn"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","humid forest"),c("other","sea"))

Tukey_test <- aov(tailwind.r.mean~daily.biome, data=ss.aut) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(15)

pg <- ggplot(data=ss.aut,aes(y=tailwind.r.mean,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(-12,16))+
  xlab("") + ylab("Daily Mean Tailwind\nto Track [m/s]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pg2 <- pg +  geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=15,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=12)   


### DAILY realized tailwind- spring
#compare_means(tailwind.r.mean ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Spring"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","sea"))

Tukey_test <- aov(tailwind.r.mean~daily.biome, data=ss.spr) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(15)

ph <- ggplot(data=ss.spr, aes(y=tailwind.r.mean,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(-12,16))+
  xlab("") + ylab("Daily Mean Tailwind\nto Track [m/s]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

ph2 <- ph + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=15,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=12)    

### DAILY available TAILWIND PER BIOME - autumn
#compare_means(tailwind.mean ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Autumn"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","humid forest"),c("other","sea"))

Tukey_test <- aov(tailwind.mean~daily.biome, data=ss.aut) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(15)

pi <- ggplot(data=ss.aut,aes(y=tailwind.mean,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(-12,16))+
  xlab("") + ylab("Daily Mean Tailwind\nto Goal [m/s]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pi2 <- pi + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=15,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=12)   


### DAILY SPEED PER BIOME - spring
#compare_means(tailwind.mean ~daily.biome,  data = wind.days[which(wind.days$trip2 == "Spring"),], method ='t.test', ref.group = "other")
#my_comparisons <- list(c("other","desert"),c("other","sea"))

Tukey_test <- aov(tailwind.mean~daily.biome, data=ss.spr) %>%
  HSD.test("daily.biome", group=TRUE) %>%
  .$groups %>%
  as_tibble(rownames="daily.biome") %>%
  rename("Letters_Tukey"="groups") 
Tukey_test <- as.data.frame(Tukey_test)
Tukey_test$ypos <- rep(15)

pj <- ggplot(data=wind.days[which(wind.days$trip2 == "Spring"),],
             aes(y=tailwind.mean,x=factor(daily.biome),fill=factor(daily.biome),group=factor(daily.biome)))+
  geom_violin(colour='grey40',alpha=.8,trim=TRUE)+
  stat_summary(fun.data=data_summary)+
  scale_fill_viridis_d(name='Biome',limits=c("desert","humid forest","other","sea"))+
  theme_bw()+
  scale_y_continuous(limits=c(-12,16))+
  xlab("") + ylab("Daily Mean Tailwind\nto Goal [m/s]") +
  theme(legend.position   = 'none',
        panel.border	    = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background	= element_blank(),
        strip.text		    = element_blank(),
        axis.text.y		      = element_text(size=10),
        axis.text.x		      = element_blank(),
        axis.title.y		    = element_text(size=11,face='bold'),
        axis.title.x		    = element_blank(),
        axis.line         = element_line(size=.4))

pj2 <- pj + geom_text(data=Tukey_test, aes(label=Letters_Tukey, y = ypos))
rm(Tukey_test)
  #stat_compare_means(method = "anova",label.y=15,label.x=0.75)+      # Add global p-value
  #stat_compare_means(label = "p.signif", method = "t.test",
  #                   ref.group = "other",label.y=12)    


px <- cowplot::plot_grid(pa2,pb2,pc2,pd2,pe2,pf2,pg2,ph2,pi2,pj2,ncol=2,nrow=5,align="v",axis = "l", labels=c("a","b","c","d","e","f","g","h","i","j"),lab_size=16)

# extract a legend that is laid out horizontally
legend_b <- get_legend(
  pi + 
    guides(fill = guide_legend(nrow = 1,title.position="top")) +
    theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 30%
# of the height of one plot (via rel_heights).
pfin <- cowplot::plot_grid(px, legend_b, ncol = 1, rel_heights = c(5, .5))


ggsave(plot=pfin,filename='./Figures 2021/FigS5_Performance-vs-Biome-ViolinPlots_v20210421.tiff',dpi=300,width=9,height=10)
rm(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pa2,pb2,pc2,pd2,pe2,pf2,pg2,ph2,pi2,pj2,px,pfin,ss.aut,ss.spr)






