library(rsq)

wind.days$yr <- as.factor(wind.days$yr)

# mixed models for  daily distance
m0 <- lmer(log(daily.dist/1000) ~ (1|dev) +  (1|yr),data=wind.days,REML=FALSE)
m0a <- lmer(log(daily.dist/1000) ~ trip + (1|dev) +  (1|yr),data=wind.days,REML=FALSE)
m0b <- lmer(log(daily.dist/1000) ~ daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0c <- lmer(log(daily.dist/1000) ~ tailwind.mean + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0d<- lmer(log(daily.dist/1000) ~ tailwind.mean  + trip + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0e<- lmer(log(daily.dist/1000) ~ tailwind.mean  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0f<- lmer(log(daily.dist/1000) ~ trip  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0g<- lmer(log(daily.dist/1000) ~ tailwind.mean + trip  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0h <- lmer(log(daily.dist/1000) ~ tailwind.mean  * trip + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0i <- lmer(log(daily.dist/1000) ~ tailwind.mean  * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0j <- lmer(log(daily.dist/1000) ~ trip  * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0k <- lmer(log(daily.dist/1000) ~ tailwind.mean  * trip * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)

a1 <- anova(m0,m0a,m0b,m0c,m0d,m0e,m0f,m0g,m0h,m0i,m0j,m0k)
#mods.full <- attributes(a1)$heading[-(1:2)]
mods <- attributes(a1)$row.names
#mods <- as.data.frame(cbind(mods.full,mods))

a1 <- as.data.frame(a1)

a2 <- c(rsq(m0)$model,rsq(m0a)$model,rsq(m0b)$model,rsq(m0c)$model,rsq(m0d)$model,rsq(m0e)$model,rsq(m0f)$model,rsq(m0g)$model,rsq(m0h)$model,rsq(m0i)$model,rsq(m0j)$model,rsq(m0k)$model)
a3 <- c(rsq(m0)$fixed,rsq(m0a)$fixed,rsq(m0b)$fixed,rsq(m0c)$fixed,rsq(m0d)$fixed,rsq(m0e)$fixed,rsq(m0f)$fixed,rsq(m0g)$fixed,rsq(m0h)$fixed,rsq(m0i)$fixed,rsq(m0j)$fixed,rsq(m0k)$fixed)
a4 <- c(rsq(m0)$random,rsq(m0a)$random,rsq(m0b)$random,rsq(m0c)$random,rsq(m0d)$random,rsq(m0e)$random,rsq(m0f)$random,rsq(m0g)$random,rsq(m0h)$random,rsq(m0i)$random,rsq(m0j)$random,rsq(m0k)$random)

a <- cbind(a1,mods,a2,a3,a4)
colnames(a) <- c("npar","AIC","BIC","logLik","dev","chisq","df_chisq","p","mod.abb","rsq.mod","rsq.fix","rsq.ran")
a$response <- rep("log(Daily distance)")

mod.abb <- c("m0","m0a","m0b","m0c","m0d","m0e","m0f","m0g","m0h","m0i","m0j","m0k")
mod.form <- c(" ","trip","daily.biome","tailwind.mean","tailwind.mean  + trip","tailwind.mean  + daily.biome","trip  + daily.biome",
              "tailwind.mean + trip  + daily.biome","tailwind.mean  * trip","tailwind.mean  * daily.biome","trip  * daily.biome ",
              "tailwind.mean  * trip * daily.biome")
mods2 <- as.data.frame(cbind(mod.abb,mod.form))
a <- merge(a,mods2,all.x=TRUE)

a$dAIC <- a$AIC - min(a$AIC)
a$RL_AIC <- exp( -0.5 * a$dAIC) 
a$weightAIC <- a$RL_AIC / sum(a$RL_AIC)

### mixed models for travle time
m0 <-  lmer(daily.travel ~ (1|dev) +  (1|yr),data=wind.days,REML=FALSE)
m0a <- lmer(daily.travel ~ trip + (1|dev) +  (1|yr),data=wind.days,REML=FALSE)
m0b <- lmer(daily.travel ~ daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0c <- lmer(daily.travel ~ tailwind.mean + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0d<-  lmer(daily.travel ~ tailwind.mean  + trip + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0e<-  lmer(daily.travel ~ tailwind.mean  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0f<-  lmer(daily.travel ~ trip  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0g<-  lmer(daily.travel ~ tailwind.mean + trip  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0h <- lmer(daily.travel ~ tailwind.mean  * trip + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0i <- lmer(daily.travel ~ tailwind.mean  * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0j <- lmer(daily.travel ~ trip  * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0k <- lmer(daily.travel ~ tailwind.mean  * trip * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)

b1 <- anova(m0,m0a,m0b,m0c,m0d,m0e,m0f,m0g,m0h,m0i,m0j,m0k)
#mods.full <- attributes(a1)$heading[-(1:2)]
mods <- attributes(b1)$row.names

#mods <- as.data.frame(cbind(mods.full,mods))

b1 <- as.data.frame(b1)

b2 <- c(rsq(m0)$model,rsq(m0a)$model,rsq(m0b)$model,rsq(m0c)$model,rsq(m0d)$model,rsq(m0e)$model,rsq(m0f)$model,rsq(m0g)$model,rsq(m0h)$model,rsq(m0i)$model,rsq(m0j)$model,rsq(m0k)$model)
b3 <- c(rsq(m0)$fixed,rsq(m0a)$fixed,rsq(m0b)$fixed,rsq(m0c)$fixed,rsq(m0d)$fixed,rsq(m0e)$fixed,rsq(m0f)$fixed,rsq(m0g)$fixed,rsq(m0h)$fixed,rsq(m0i)$fixed,rsq(m0j)$fixed,rsq(m0k)$fixed)
b4 <- c(rsq(m0)$random,rsq(m0a)$random,rsq(m0b)$random,rsq(m0c)$random,rsq(m0d)$random,rsq(m0e)$random,rsq(m0f)$random,rsq(m0g)$random,rsq(m0h)$random,rsq(m0i)$random,rsq(m0j)$random,rsq(m0k)$random)

b <- cbind(b1,mods,b2,b3,b4)
colnames(b) <- c("npar","AIC","BIC","logLik","dev","chisq","df_chisq","p","mod.abb","rsq.mod","rsq.fix","rsq.ran")
b$response <- rep("Daily Travel Hrs")

mod.abb <- c("m0","m0a","m0b","m0c","m0d","m0e","m0f","m0g","m0h","m0i","m0j","m0k")
mod.form <- c(" ","trip","daily.biome","tailwind.mean","tailwind.mean  + trip","tailwind.mean  + daily.biome","trip  + daily.biome",
              "tailwind.mean + trip  + daily.biome","tailwind.mean  * trip","tailwind.mean  * daily.biome","trip  * daily.biome ",
              "tailwind.mean  * trip * daily.biome")
mods2 <- as.data.frame(cbind(mod.abb,mod.form))
b <- merge(b,mods2,all.x=TRUE)

b$dAIC <- b$AIC - min(b$AIC)
b$RL_AIC <- exp( -0.5 * b$dAIC) 
b$weightAIC <- b$RL_AIC / sum(b$RL_AIC)

### mixed models for travle time
m0 <-  lmer(log(speed.mean) ~ (1|dev) +  (1|yr),data=wind.days,REML=FALSE)
m0a <- lmer(log(speed.mean) ~ trip + (1|dev) +  (1|yr),data=wind.days,REML=FALSE)
m0b <- lmer(log(speed.mean) ~ daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0c <- lmer(log(speed.mean) ~ tailwind.mean + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0d<-  lmer(log(speed.mean) ~ tailwind.mean  + trip + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0e<-  lmer(log(speed.mean) ~ tailwind.mean  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0f<-  lmer(log(speed.mean) ~ trip  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0g<-  lmer(log(speed.mean) ~ tailwind.mean + trip  + daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0h <- lmer(log(speed.mean) ~ tailwind.mean  * trip + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0i <- lmer(log(speed.mean) ~ tailwind.mean  * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0j <- lmer(log(speed.mean) ~ trip  * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)
m0k <- lmer(log(speed.mean) ~ tailwind.mean  * trip * daily.biome + (1|dev) + (1|yr),data=wind.days,REML=FALSE)

c1 <- anova(m0,m0a,m0b,m0c,m0d,m0e,m0f,m0g,m0h,m0i,m0j,m0k)
#mods.full <- attributes(a1)$heading[-(1:2)]
mods <- attributes(c1)$row.names
#mods <- as.data.frame(cbind(mods.full,mods))

c1 <- as.data.frame(c1)

c2 <- c(rsq(m0)$model,rsq(m0a)$model,rsq(m0b)$model,rsq(m0c)$model,rsq(m0d)$model,rsq(m0e)$model,rsq(m0f)$model,rsq(m0g)$model,rsq(m0h)$model,rsq(m0i)$model,rsq(m0j)$model,rsq(m0k)$model)
c3 <- c(rsq(m0)$fixed,rsq(m0a)$fixed,rsq(m0b)$fixed,rsq(m0c)$fixed,rsq(m0d)$fixed,rsq(m0e)$fixed,rsq(m0f)$fixed,rsq(m0g)$fixed,rsq(m0h)$fixed,rsq(m0i)$fixed,rsq(m0j)$fixed,rsq(m0k)$fixed)
c4 <- c(rsq(m0)$random,rsq(m0a)$random,rsq(m0b)$random,rsq(m0c)$random,rsq(m0d)$random,rsq(m0e)$random,rsq(m0f)$random,rsq(m0g)$random,rsq(m0h)$random,rsq(m0i)$random,rsq(m0j)$random,rsq(m0k)$random)

c <- cbind(c1,mods,c2,c3,c4)
colnames(c) <- c("npar","AIC","BIC","logLik","dev","chisq","df_chisq","p","mod.abb","rsq.mod","rsq.fix","rsq.ran")
c$response <- rep("log(Daily Mean Speed)")

mod.abb <- c("m0","m0a","m0b","m0c","m0d","m0e","m0f","m0g","m0h","m0i","m0j","m0k")
mod.form <- c(" ","trip","daily.biome","tailwind.mean","tailwind.mean  + trip","tailwind.mean  + daily.biome","trip  + daily.biome",
              "tailwind.mean + trip  + daily.biome","tailwind.mean  * trip","tailwind.mean  * daily.biome","trip  * daily.biome ",
              "tailwind.mean  * trip * daily.biome")
mods2 <- as.data.frame(cbind(mod.abb,mod.form))
c <- merge(c,mods2,all.x=TRUE)

c$dAIC <- c$AIC - min(c$AIC)
c$RL_AIC <- exp( -0.5 * c$dAIC) 
c$weightAIC <- c$RL_AIC / sum(c$RL_AIC)

stats <- rbind(b,a,c)
write.csv(stats,'./Figures 2021/TableS4_MME_Perf-vs-Tailwind-to-Goal-v20210421.csv')
rm(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,a,b,c,mods,mods.full,m0,m0a,m0b,m0c,m0d,m0e,m0f,m0g,m0h,m0i,m0j,stats,mods2)


