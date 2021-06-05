library(gridExtra)
library(ggExtra)
library(lme4)
library(lmerTest)
library(ggpubr)

#####################################
### PERFORMANCE VS WIND           ###  
#####################################


wind.days2 <- subset(wind.days, wind.days$daily.travel < 25)

## Daily Dist vs Tailwind
p1 <- ggplot(data=wind.days2,aes(y=daily.dist/1000,x=tailwind.r.mean*3.6,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  ylab("\nDaily Distance [km]")+
  xlab("Daily Realized Tailwind [km/h]")+
  scale_y_continuous(trans = 'log2')+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p1 <- ggExtra::ggMarginal(p1, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)

p2 <- ggplot(data=wind.days2,aes(y=daily.dist/1000,x=tailwind.mean*3.6,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  ylab("\nDaily Distance [km]")+
  xlab("Daily Tailwind to Goal [km/h]")+
  scale_y_continuous(trans = 'log2')+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p2 <- ggExtra::ggMarginal(p2, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)


p3 <- ggplot(data=wind.days2,aes(y=daily.travel,x=tailwind.r.mean*3.6,col=trip,fill=trip)) +
#  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Realized Tailwind [km/h]")+
  ylab("Daily Travel\nTime [h]")+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p3 <- ggExtra::ggMarginal(p3, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)

p4 <- ggplot(data=wind.days2,aes(x=tailwind.mean*3.6,y=daily.travel,col=trip,fill=trip)) +
#  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Tailwind To Goal [km/h]")+
  ylab("Daily Travel\nTime [h]")+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p4 <- ggExtra::ggMarginal(p4, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)


p5 <- ggplot(data=wind.days2,aes(y=speed.mean*3.6,x=tailwind.r.mean*3.6,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Realized Tailwind [km/h]")+
  ylab("Daily Mean\nTravel Speed [km/h]")+
  scale_y_continuous(trans = 'log2')+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p5 <- ggExtra::ggMarginal(p5, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)


p6 <- ggplot(data=wind.days2,aes(y=speed.mean*3.6,x=tailwind.mean*3.6,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Tailwind to Goal [km/h]")+
  ylab("Daily Mean\nTravel Speed [km/h]")+
  scale_y_continuous(trans = 'log2')+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p6 <- ggExtra::ggMarginal(p6, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)

pfull <- cowplot::plot_grid(p3,p4,p1,p2,p5,p6,align='v',axis="l",labels=c("i","ii","iii","iv","v","vi"),nrow=3)
pfull

#ggsave(plot=pfull,"./Figures 2021/Fig2B_Performance-vs-Tailwind-v20210415.tiff",width=6.2,height=5.5,dpi=300)
#dev.off()

rm(p3,p4,p1,p2,p5,p6)


### PLOT PERFORMANCE VS TRAVEL TIME

p10 <- ggplot(data=wind.days2,aes(y=daily.dist/1000,x=daily.travel,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Travel Time [h]")+
  ylab("Daily Distance [km]")+
  theme_bw()+
  stat_regline_equation() +    
  stat_cor(label.x=10.5)+
  scale_y_continuous(trans = 'log2')+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p10 <- ggExtra::ggMarginal(p10, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)

p12 <- ggplot(data=wind.days2,aes(y=speed.mean*3.6,x=daily.travel,col=trip,fill=trip)) +
  geom_smooth(method='lm',alpha=.2)+
  geom_point(size=.1,alpha=.4)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Travel Time [h]")+
  ylab("Daily Mean Travel Speed [km/h]")+
  theme_bw()+
  scale_y_continuous(trans = 'log2')+
  stat_regline_equation() +    
  stat_cor(label.x=10.5)+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        panel.grid.minor  = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p12 <- ggExtra::ggMarginal(p12, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)

pfull2 <- cowplot::plot_grid(p10,p12,ncol=1,align="v",labels=c("a","b"))
#ggsave(plot=pfull2,"./Figures 2021/Fig2A_Dist-Spd_TravelTime-v20210415.tiff",width=5.5,height=5.5,dpi=300)

pfin <- cowplot::plot_grid(pfull2,pfull,ncol=2,nrow=1,rel.widths=c(1.5,1),align='h')

ggsave(plot=pfin,"./Figures 2021/Fig5-full-v20210421.tiff",width=10,height=6,dpi=300)

rm(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,pfull,pfull2,pfin)

### EXTRA PLOTS STRAIGHTNESS
library(ggpubr)
my.formula <- y ~ x
p8 <- ggplot(data=wind.days2,aes(x=tailwind.r.mean*3.6,y=daily.straight,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2,formula = my.formula)+
  geom_point(size=.2,alpha=.6)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Realized Tailwind [km/h]")+
  ylab("Daily Straightness")+ 
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p8 <- ggExtra::ggMarginal(p8, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)


p9 <- ggplot(data=wind.days,aes(x=tailwind.mean*3.6,y=daily.straight,col=trip,fill=trip)) +
  geom_smooth(method="lm",alpha=.2)+
  geom_point(size=.2,alpha=.6)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Tailwind To Goal [km/h]")+
  ylab("Daily Straightness")+ 
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p9 <- ggExtra::ggMarginal(p9, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)





p11 <- ggplot(data=wind.days,aes(y=daily.straight,x=daily.travel,col=trip,fill=trip)) +
  geom_smooth(method='lm',alpha=.2)+
  geom_point(size=.2,alpha=.6)+
  scale_colour_manual(values=c(out='orangered',return='cornflowerblue'))+
  scale_fill_manual(values=c(out='orangered',return='cornflowerblue'))+
  xlab("Daily Travel Time [hrs]")+
  ylab("Daily Straightness")+
  theme_bw()+
  theme(legend.position   = 'right',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='white'),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=8),
        axis.title		    = element_text(size=10),
        axis.line         = element_line(size=0.4))+
  guides(col = FALSE,
         fill=FALSE)
p11 <- ggExtra::ggMarginal(p11, type = "density",groupColour = TRUE, groupFill = TRUE,alpha=.2)