
# order data from W to E for each dekade
itcz <- itcz[order(itcz$group,itcz$long),]
itcz$trip <- ifelse(itcz$mth %in% c(4,5,6),"return",ifelse(itcz$mth %in% c(9,10),"out",NA))
itcz <- subset(itcz,itcz$trip %in% c("return","out"))

### Calcualte lat of birds per decade per 5deg longitude in spring
####################################################################
ss <- subset(data,data$long > -17.5 & data$long < 37.5 & data$lat > 5 & data$lat < 15)
ss$long <- round(ss$long/5)*5

ss$day <- day(ss$dt)
ss$mth <- month(ss$dt)
ss$dec <- ifelse(ss$day <=10,1,
                 ifelse(ss$day <=20,2,3))

lat.per.long.dec <- ss %>%
  filter((trip == "out" & (daily.dir < 125 & daily.dir > 65)) | (trip == "return" & (daily.dir > -125 & daily.dir < -65)) )%>%
  group_by(dev,yr,trip,mth,dec,long) %>%
  summarize(lat = mean(lat, na.rm = TRUE)) 
lpld <- as.data.frame(lat.per.long.dec)

colnames(itcz)[1] <- "lat.itcz"

lpld <- merge(lpld,itcz[,c("lat.itcz","long","yr","trip","mth","dec")],all.x=TRUE)
lpld <- merge(lpld,meta[,c("dev","sex")],all.x=TRUE)
lpld$trip2 <- ifelse(lpld$trip == "out","Autumn","Spring")

lpld <- subset(lpld,is.na(lpld$lat.itcz)==FALSE)

# mixed models for lat falcons vs lat itf
m0 <- lmer(data=lpld,lat ~ 1 + (1|dev),REML=FALSE)
m1 <- lmer(data=lpld,lat ~ lat.itcz + (1|dev),REML=FALSE)
m2 <- lmer(data=lpld,lat ~ long + (1|dev),REML=FALSE)
m3 <- lmer(data=lpld,lat ~ trip + (1|dev),REML=FALSE)
m4 <- lmer(data=lpld,lat ~ lat.itcz + long + (1|dev),REML=FALSE)
m5 <- lmer(data=lpld,lat ~ lat.itcz + trip + (1|dev),REML=FALSE)
m6 <- lmer(data=lpld,lat ~ long + trip + (1|dev),REML=FALSE)
m7 <- lmer(data=lpld,lat ~ lat.itcz * trip + (1|dev),REML=FALSE)
m8 <- lmer(data=lpld,lat ~ long * trip + (1|dev),REML=FALSE)
m9 <- lmer(data=lpld,lat ~ lat.itcz * long + (1|dev),REML=FALSE)
m10 <- lmer(data=lpld,lat ~ lat.itcz * long * trip + (1|dev),REML=FALSE)


mcomp.itf <- anova(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
mcomp.itf <- as.data.frame(broom::tidy(mcomp.itf))
mcomp.itf$rsq.mod <- c(rsq(m0)$model,rsq(m1)$model,rsq(m2)$model,rsq(m3)$model,
                    rsq(m4)$model,rsq(m5)$model,rsq(m6)$model,
                    rsq(m7)$model,rsq(m8)$model,rsq(m9)$model,rsq(m10)$model)
mcomp.itf$rsq.fixed <- c(rsq(m0)$fixed,rsq(m1)$fixed,rsq(m2)$fixed,rsq(m3)$fixed,
                      rsq(m4)$fixed,rsq(m5)$fixed,rsq(m6)$fixed,
                      rsq(m7)$fixed,rsq(m8)$fixed,rsq(m9)$fixed,rsq(m10)$fixed)
mcomp.itf$rsq.random <- c(rsq(m0)$random,rsq(m1)$random,rsq(m2)$random,rsq(m3)$random,
                           rsq(m4)$random,rsq(m5)$random,rsq(m6)$random,
                          rsq(m7)$random,rsq(m8)$random,rsq(m9)$random,rsq(m10)$random)
p<- mcomp.itf$df
n <- length(lpld$lat)
mcomp.itf$AICc <- mcomp.itf$AIC + (2 * p^2 + 2 * p)/(n - p - 1)
mcomp.itf$resp <- "lat_bird"
mcomp.itf$predictors <- c("","ITF","long","season","ITF+long","ITF+season","long+season","ITF*season","long*season","long*ITF","long*ITF*season")

mcomp.itf <- mcomp.itf[order(mcomp.itf$AIC),]
mcomp.itf$deltaAIC <- mcomp.itf$AIC - min(mcomp.itf$AIC)

write.csv(mcomp.itf,'./Figures 2021/Table3-LatBird-vs-LatITCZ.csv')

#ss <- MuMIn::model.avg(m8,m4,m9,beta=TRUE)
#xx <- summary(ss)$coefmat.full
coeffs <- coef(summary(m10))
write.csv(coeffs,'./Figures 2021/Table4-LatBird-BestModel_v20210110.csv')

## plot Fig4

p1 <- ggplot(data=lpld,aes(y=lat,x=lat.itcz,fill=trip2,col=trip2)) +
  geom_abline(intercept=0,slope=1,linetype='dashed')+
  geom_smooth(method="lm",size=.3,alpha=.05)+
  scale_colour_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  geom_point(size=.3)+
  ylab("\nLat Falcon[º]")+
  xlab("Lat ITF[º]\n")+
  theme_bw()+ ylim(4,20)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='black'),
        panel.grid        = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=10,face='bold'),
        axis.title		    = element_text(size=12,face='bold'))
#+
#  guides(col = guide_legend(title="Season",title.position='top',ncol=1),fill=FALSE)


p2 <- ggplot(data=lpld,aes(y=lat.itcz,x=long,fill=trip2,col=trip2)) +
  geom_smooth(method="lm",size=.3,alpha=.05)+
  scale_colour_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  geom_point(size=.3)+
  ylab("\nLat ITF[º]")+
  xlab("Long[º]\n")+
  theme_bw()+ ylim(5,18)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='black'),
        panel.grid        = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=10,face='bold'),
        axis.title		    = element_text(size=12,face='bold'))
#+
#  guides(col = guide_legend(title="Season",title.position='top',ncol=1),fill=FALSE)



p3 <- ggplot(data=lpld,aes(y=lat,x=long,fill=trip2,col=trip2)) +
  geom_smooth(method="lm",size=.3,alpha=.05)+
  scale_colour_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  geom_point(size=.3)+
  ylab("\nLat Falcon[º]")+
  xlab("Long[º]\n")+
  theme_bw()+ ylim(5,18)+
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='black'),
        panel.grid        = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=10,face='bold'),
        axis.title		    = element_text(size=12,face='bold'))
#+
#  guides(col = guide_legend(title="Season",title.position='top',ncol=1),fill=FALSE)

lpld$predict <- predict(m1)
p4 <- ggplot(data=lpld,aes(y=lat,x=lat.itcz,col=trip2,fill=trip2)) +
 # geom_line(aes(y=predict,group=dev),size=.3)+
  geom_smooth(method='lm',size=.3,alpha=.05,aes(group=paste(trip2,dev)),fill='transparent')+
  geom_abline(intercept=0,slope=1,linetype='dashed')+
  scale_colour_manual(values=c("Autumn"="orangered","Spring"="cornflowerblue"))+
  geom_point(size=.3)+ylim(4,20)+
  ylab("\nLat Falcon[º]")+
  xlab("Lat ITF[º]\n")+
  theme_bw()+ 
  theme(legend.position   = 'none',
        legend.direction  = 'vertical',
        panel.border	    = element_rect(colour='black'),
        panel.grid        = element_blank(),
        strip.background	= element_rect(fill='white',colour='NA'),
        legend.title 	    = element_text(size=11,face='bold'),
        legend.text 	    = element_text(size=10,face='bold'),
        strip.text		    = element_text(size=14,face='bold'),
        axis.text		      = element_text(size=10,face='bold'),
        axis.title		    = element_text(size=12,face='bold'))

pfull <- cowplot::plot_grid(p2,p3,p1,p4,ncol=2,align="v",labels=c("a","b","c","d"))

ggsave("./Figures 2021/Fig4_Falcon-vs-ITF_v20210421.tiff",width=6.5,height=6.5,dpi=300)


