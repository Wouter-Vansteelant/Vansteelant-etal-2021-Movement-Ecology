library(rsq);library(lmerTest)

wind.days2 <- wind.days[-which(wind.days$daily.travel > 25),]
wind.days2$daily.nighthrs <- ifelse(is.na(wind.days2$daily.nighthrs) == TRUE,0,wind.days2$daily.nighthrs)
wind.days2$daily.dayhrs <- ifelse(is.na(wind.days2$daily.dayhrs) == TRUE,0,wind.days2$daily.dayhrs)

######################################
#### MODEL SELECTION AND AVERAGING #####
######################################

#### DAILY DISTANCE
######################################
m3  <- lmer(log(daily.dist/1000) ~ (1|dev) +  (1|yr),data=wind.days2,REML=FALSE)

m3a <- lmer(log(daily.dist/1000) ~ trip + (1|dev) +  (1|yr),data=wind.days2,REML=FALSE)
m3b <- lmer(log(daily.dist/1000) ~ daily.biome + (1|dev) +  (1|yr),data=wind.days2,REML=FALSE)
m3c <- lmer(log(daily.dist/1000) ~ tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3d <- lmer(log(daily.dist/1000) ~ abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3e <- lmer(log(daily.dist/1000) ~ daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3f <- lmer(log(daily.dist/1000) ~ trip + daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3g <- lmer(log(daily.dist/1000) ~ trip + tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3h <- lmer(log(daily.dist/1000) ~ trip + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3i <- lmer(log(daily.dist/1000) ~ trip + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3j <- lmer(log(daily.dist/1000) ~ daily.biome + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3k <- lmer(log(daily.dist/1000) ~ daily.biome + tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3l <- lmer(log(daily.dist/1000) ~ daily.biome + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3m <- lmer(log(daily.dist/1000) ~ tailwind.r.mean + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3n <- lmer(log(daily.dist/1000) ~ tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3o <- lmer(log(daily.dist/1000) ~ abs(sidewind.r.mean) + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3p <- lmer(log(daily.dist/1000) ~ trip + tailwind.r.mean + daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3q <- lmer(log(daily.dist/1000) ~ trip + tailwind.r.mean + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3r <- lmer(log(daily.dist/1000) ~ trip + tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3s <- lmer(log(daily.dist/1000) ~ trip + daily.biome + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3t <- lmer(log(daily.dist/1000) ~ trip + daily.biome + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3u <- lmer(log(daily.dist/1000) ~ trip + abs(sidewind.r.mean) + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3v <- lmer(log(daily.dist/1000) ~ daily.biome + tailwind.r.mean + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3w <- lmer(log(daily.dist/1000) ~ daily.biome + tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3x <- lmer(log(daily.dist/1000) ~ daily.biome + daily.travel + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3x <- lmer(log(daily.dist/1000) ~ daily.travel + tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3y <- lmer(log(daily.dist/1000) ~ trip * daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3z <- lmer(log(daily.dist/1000) ~ trip * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m31 <- lmer(log(daily.dist/1000) ~ trip * tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m32 <- lmer(log(daily.dist/1000) ~ trip * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m33 <- lmer(log(daily.dist/1000) ~ daily.biome * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m34 <- lmer(log(daily.dist/1000) ~ daily.biome * tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m35 <- lmer(log(daily.dist/1000) ~ daily.biome * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m36 <- lmer(log(daily.dist/1000) ~ tailwind.r.mean * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m37 <- lmer(log(daily.dist/1000) ~ tailwind.r.mean * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m38 <- lmer(log(daily.dist/1000) ~ daily.travel * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m39 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a1 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a2 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3a3 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.biome + trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a4 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel + trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a5 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel + daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3a6 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.biome * trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a7 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel * trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a8 <- lmer(log(daily.dist/1000) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel * daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

a1<- anova(m3,m3a,m3b,m3c,m3d,m3e,m3f,m3g,m3h,m3i,m3j,m3k,m3l,m3m,m3n,m3o,m3p,m3q,m3r,m3s,m3t,m3u,m3v,m3w,m3x,m3y,m3z,
           m31,m32,m33,m34,m35,m36,m37,m38,m39,m3a1,m3a2,m3a3,m3a4,m3a5,m3a6,m3a7,m3a8)
a1 <- as.data.frame(a1[,c(1:7,as.numeric(8))])
colnames(a1) <- c("df","AIC","BIC","logLik","dev","chisq","df_chisq","p")
a1$response <- rep("daily_dist")

mod.list <- list(m3,m3a,m3b,m3c,m3d,m3e,m3f,m3g,m3h,m3i,m3j,m3k,m3l,m3m,m3n,m3o,m3p,m3q,m3r,m3s,m3t,m3u,m3v,m3w,m3x,m3y,m3z,
                 m31,m32,m33,m34,m35,m36,m37,m38,m39,m3a1,m3a2,m3a3,m3a4,m3a5,m3a6,m3a7,m3a8)
rsq.m <- function(x) rsq(x,adj=TRUE)$model
rsq.f <- function(x) rsq(x,adj=TRUE)$fixed
rsq.r <- function(x) rsq(x,adj=TRUE)$random

a1$rsq.mod <- unlist(lapply(mod.list,FUN='rsq.m'))
a1$rsq.fix <- unlist(lapply(mod.list,FUN='rsq.f'))
a1$rsq.ran <- unlist(lapply(mod.list,FUN='rsq.r'))
a1$mod <- as.character(c("m3","m3a","m3b","m3c","m3d","m3e","m3f","m3g",'m3h',"m3i","m3j",'m3k',"m3l",'m3m','m3n','m3o','m3p','m3q','m3r',"m3s",'m3t',"m3u","m3v",'m3w',"m3x","m3y","m3z",
                         "m31","m32","m33","m34","m35","m36","m37","m38","m39","m3a1","m3a2","m3a3","m3a4","m3a5","m3a6","m3a7","m3a8"))

extract.mod <- function(x) as.character(x@call[2])
xx <- lapply(mod.list,FUN='extract.mod')
a1$form <- unlist(xx)

a1$deltaAIC <- a1$AIC - min(a1$AIC)
a1$RL_AIC <- exp( -0.5 * a1$deltaAIC) 
a1$weightAIC <- a1$RL_AIC / sum(a1$RL_AIC)

a1 <- a1[order(a1$deltaAIC),c("form","df","AIC","deltaAIC","weightAIC","rsq.mod","rsq.fix","rsq.ran")]

#extract.coefs <- function(x) as.data.frame(broom::tidy(x))[,c("term","statistic")]
#xx <- lapply(mod.list,FUN='extract.coefs')
#x2 <- do.call(rbind,xx)

write.csv(a1,'./Figures 2021/Table1_DailyDistance_FullModelSelection-v20210421.csv')

# extract coeff estimates top model
coeffs <- as.data.frame(coef(summary(m3a8)))

write.csv(coeffs,'./Figures 2021/Table2a_DailyDistance_BestModel-effects_v20210421.csv')
rm(a1,ss,xx,model.average)
rm(m3,m3a,m3b,m3c,m3d,m3e,m3f,m3g,m3h,m3i,m3j,m3k,m3l,m3m,m3n,m3o,m3p,m3q,m3r,m3s,m3t,m3v,m3w)



#### DAILY MEAN SPEED
######################################
m3  <- lmer(log(speed.mean) ~ (1|dev) +  (1|yr),data=wind.days2,REML=FALSE)

m3a <- lmer(log(speed.mean) ~ trip + (1|dev) +  (1|yr),data=wind.days2,REML=FALSE)
m3b <- lmer(log(speed.mean) ~ daily.biome + (1|dev) +  (1|yr),data=wind.days2,REML=FALSE)
m3c <- lmer(log(speed.mean) ~ tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3d <- lmer(log(speed.mean) ~ abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3e <- lmer(log(speed.mean) ~ daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3f <- lmer(log(speed.mean) ~ trip + daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3g <- lmer(log(speed.mean) ~ trip + tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3h <- lmer(log(speed.mean) ~ trip + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3i <- lmer(log(speed.mean) ~ trip + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3j <- lmer(log(speed.mean) ~ daily.biome + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3k <- lmer(log(speed.mean) ~ daily.biome + tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3l <- lmer(log(speed.mean) ~ daily.biome + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3m <- lmer(log(speed.mean) ~ tailwind.r.mean + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3n <- lmer(log(speed.mean) ~ tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3o <- lmer(log(speed.mean) ~ abs(sidewind.r.mean) + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3p <- lmer(log(speed.mean) ~ trip + tailwind.r.mean + daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3q <- lmer(log(speed.mean) ~ trip + tailwind.r.mean + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3r <- lmer(log(speed.mean) ~ trip + tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3s <- lmer(log(speed.mean) ~ trip + daily.biome + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3t <- lmer(log(speed.mean) ~ trip + daily.biome + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3u <- lmer(log(speed.mean) ~ trip + abs(sidewind.r.mean) + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3v <- lmer(log(speed.mean) ~ daily.biome + tailwind.r.mean + daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3w <- lmer(log(speed.mean) ~ daily.biome + tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3x <- lmer(log(speed.mean) ~ daily.biome + daily.travel + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3x <- lmer(log(speed.mean) ~ daily.travel + tailwind.r.mean + abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3y <- lmer(log(speed.mean) ~ trip * daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3z <- lmer(log(speed.mean) ~ trip * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m31 <- lmer(log(speed.mean) ~ trip * tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m32 <- lmer(log(speed.mean) ~ trip * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m33 <- lmer(log(speed.mean) ~ daily.biome * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m34 <- lmer(log(speed.mean) ~ daily.biome * tailwind.r.mean + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m35 <- lmer(log(speed.mean) ~ daily.biome * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m36 <- lmer(log(speed.mean) ~ tailwind.r.mean * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m37 <- lmer(log(speed.mean) ~ tailwind.r.mean * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m38 <- lmer(log(speed.mean) ~ daily.travel * abs(sidewind.r.mean) + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m39 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a1 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a2 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3a3 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.biome + trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a4 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel + trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a5 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel + daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)

m3a6 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.biome * trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a7 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel * trip + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)
m3a8 <- lmer(log(speed.mean) ~ (tailwind.r.mean + abs(sidewind.r.mean)) * daily.travel * daily.biome + (1|dev) + (1|yr),data=wind.days2,REML=FALSE)


a1<- anova(m3,m3a,m3b,m3c,m3d,m3e,m3f,m3g,m3h,m3i,m3j,m3k,m3l,m3m,m3n,m3o,m3p,m3q,m3r,m3s,m3t,m3u,m3v,m3w,m3x,m3y,m3z,
           m31,m32,m33,m34,m35,m36,m37,m38,m39,m3a1,m3a2,m3a3,m3a4,m3a5,m3a6,m3a7,m3a8)
a1 <- as.data.frame(a1[,c(1:7,as.numeric(8))])
colnames(a1) <- c("df","AIC","BIC","logLik","dev","chisq","df_chisq","p")
a1$response <- rep("daily_spd")

mod.list <- list(m3,m3a,m3b,m3c,m3d,m3e,m3f,m3g,m3h,m3i,m3j,m3k,m3l,m3m,m3n,m3o,m3p,m3q,m3r,m3s,m3t,m3u,m3v,m3w,m3x,m3y,m3z,
                 m31,m32,m33,m34,m35,m36,m37,m38,m39,m3a1,m3a2,m3a3,m3a4,m3a5,m3a6,m3a7,m3a8)
rsq.m <- function(x) rsq(x,adj=TRUE)$model
rsq.f <- function(x) rsq(x,adj=TRUE)$fixed
rsq.r <- function(x) rsq(x,adj=TRUE)$random

a1$rsq.mod <- unlist(lapply(mod.list,FUN='rsq.m'))
a1$rsq.fix <- unlist(lapply(mod.list,FUN='rsq.f'))
a1$rsq.ran <- unlist(lapply(mod.list,FUN='rsq.r'))
a1$mod <- as.character(c("m3","m3a","m3b","m3c","m3d","m3e","m3f","m3g",'m3h',"m3i","m3j",'m3k',"m3l",'m3m','m3n','m3o','m3p','m3q','m3r',"m3s",'m3t',"m3u","m3v",'m3w',"m3x","m3y","m3z",
                         "m31","m32","m33","m34","m35","m36","m37","m38","m39","m3a1","m3a2","m3a3","m3a4","m3a5","m3a6","m3a7","m3a8"))

extract.mod <- function(x) as.character(x@call[2])
xx <- lapply(mod.list,FUN='extract.mod')
a1$form <- unlist(xx)

a1$deltaAIC <- a1$AIC - min(a1$AIC)
a1$RL_AIC <- exp( -0.5 * a1$deltaAIC) 
a1$weightAIC <- a1$RL_AIC / sum(a1$RL_AIC)

a1 <- a1[order(a1$deltaAIC),c("form","df","AIC","deltaAIC","weightAIC","rsq.mod","rsq.fix","rsq.ran")]

#extract.coefs <- function(x) as.data.frame(broom::tidy(x))[,c("term","statistic")]
#xx <- lapply(mod.list,FUN='extract.coefs')
#x2 <- do.call(rbind,xx)

write.csv(a1,'./Figures 2021/TableS7_DailyMeanSpeed_FullModelSelection-v20210421.csv')

# apply model averaging
#ss <- MuMIn::model.avg(m3w,m3v,beta=TRUE)
#xx <- summary(ss)
#model.average <- as.data.frame(xx$coefmat.full)
#write.csv(model.average,'./Figures 2021/Table2b_DailyMeanSpeed_ModAvg-effects.csv')

# extract coeff estimates top model
coeffs <- as.data.frame(coef(summary(m3a8)))

write.csv(coeffs,'./Figures 2021/Table2b_DailyMeanSpeed_BestModel-effects_v20210421.csv')


rm(a1,ss,xx,model.average)
rm(m3,m3a,m3b,m3c,m3d,m3e,m3f,m3g,m3h,m3i,m3j,m3k,m3l,m3m,m3n,m3o,m3p,m3q,m3r,m3s,m3t,m3v,m3w)

