library(MuMIn)
library(lme4)

# define some funcitons to extract AIC for various models
calc.AIC <- function(x) {loglik <- logLik(x)
n   <- attributes(loglik)$nobs # following user20650 recommendation 
p   <- attributes(loglik)$df # following user20650 recommendation
dev <- -2*as.numeric(loglik)
(my_AIC  <- dev + 2 * p)
(my_AICc <- my_AIC + (2 * p^2 + 2 * p)/(n - p - 1))
(my_BIC  <- dev +  p * log(n))
as.data.frame(cbind(my_AIC,my_AICc,my_BIC,p,n,dev))
}

AIC.comp <- function(m1a,m1b,m1c,m1d){
AICm1a <- calc.AIC(m1a)
AICm1b <- calc.AIC(m1b)
AICm1c <- calc.AIC(m1c)
AICm1d <- calc.AIC(m1d)

rbind(AICm1a,AICm1b,AICm1c,AICm1d)}

##################################################################
#### mixed models for full dataset: performance vs detour and season (Table S2)
##################################################################

trip.summary$yr <- as.factor(trip.summary$yr)

# random models for trip duration vs detour and trip
m3 <- lmer(log(trip.restdays+1) ~ 1 + (1|dev) + (1|yr),data=trip.summary,REML='FALSE')
m3a <- lmer(log(trip.restdays+1) ~ trip + (1|dev) + (1|yr),data=trip.summary,REML='FALSE')
m3b <- lmer(log(trip.restdays+1) ~ detour + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
m3c <- lmer(log(trip.restdays+1) ~ detour + trip + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
m3d <- lmer(log(trip.restdays+1) ~ detour * trip + (1|dev) + (1|yr),data=trip.summary,REML='FALSE')
mcomp.rest <- anova(m3,m3a,m3b,m3c,m3d)
mcomp.rest <- as.data.frame(broom::tidy(mcomp.rest))
mcomp.rest$rsq.mod <- c(rsq(m3)$model,rsq(m3a)$model,rsq(m3b)$model,rsq(m3c)$model,rsq(m3d)$model)
mcomp.rest$rsq.fixed <- as.numeric(c(rsq(m3)$fixed,rsq(m3a)$fixed,rsq(m3b)$fixed,rsq(m3c)$fixed,rsq(m3d)$fixed))
mcomp.rest$rsq.random <- c(rsq(m3)$random,rsq(m3a)$random,rsq(m3b)$random,rsq(m3c)$random,rsq(m3d)$random)
p<- mcomp.rest$npar
n <- length(trip.summary$trip.restdays)
mcomp.rest$AICc <- mcomp.rest$AIC + (2 * p^2 + 2 * p)/(n - p - 1)
mcomp.rest$resp <- "Log(Stop days+1)"
mcomp.rest$predictors <- c("","trip","detour","detour+trip","detour*trip")
mcomp.rest$deltaAICc <- mcomp.rest$AICc - min(mcomp.rest$AICc)
mcomp.rest$RL_AICc <- exp( -0.5 * mcomp.rest$deltaAICc)
mcomp.rest$weightAICc <- mcomp.rest$RL_AICc/sum(mcomp.rest$RL_AICc)
mcomp.rest <- mcomp.rest[order(mcomp.rest$deltaAICc),]

ss <- MuMIn::model.avg(m3a,m3c,m3d,beta=TRUE)
xx <- summary(ss)
model.average.rest <- as.data.frame(xx$coefmat.full)
model.average.rest$resp <- rep("Log(Rest days +1)")

model.best.rest <- as.data.frame(coef(summary(m3c)))
model.best.rest$resp <- rep("Log(Rest days +1)")

# random models for trip duration vs detour and trip
m3  <- lmer(log(trip.traveldays) ~ 1 + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
m3a <- lmer(log(trip.traveldays) ~ trip + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
m3b <- lmer(log(trip.traveldays) ~ detour + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
m3c <- lmer(log(trip.traveldays) ~ detour + trip + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
m3d <- lmer(log(trip.traveldays) ~ detour * trip + (1|dev)+ (1|yr),data=trip.summary,REML='FALSE')
mcomp.trav <- anova(m3,m3a,m3b,m3c,m3d)
mcomp.trav <- as.data.frame(broom::tidy(mcomp.trav))
mcomp.trav$rsq.mod <- c(rsq(m3)$model,rsq(m3a)$model,rsq(m3b)$model,rsq(m3c)$model,rsq(m3d)$model)
mcomp.trav$rsq.fixed <- as.numeric(c(rsq(m3)$fixed,rsq(m3a)$fixed,rsq(m3b)$fixed,rsq(m3c)$fixed,rsq(m3d)$fixed))
mcomp.trav$rsq.random <- c(rsq(m3)$random,rsq(m3a)$random,rsq(m3b)$random,rsq(m3c)$random,rsq(m3d)$random)
p<- mcomp.trav$npar
n <- length(trip.summary$trip.traveldays)
mcomp.trav$AICc <- mcomp.trav$AIC + (2 * p^2 + 2 * p)/(n - p - 1)
mcomp.trav$resp <- "Log(Travel days)"
mcomp.trav$predictors <- c("","trip","detour","detour+trip","detour*trip")
mcomp.trav$deltaAICc <- mcomp.trav$AICc - min(mcomp.trav$AICc)
mcomp.trav$RL_AICc <- exp( -0.5 * mcomp.trav$deltaAICc)
mcomp.trav$weightAICc <- mcomp.trav$RL_AICc/sum(mcomp.trav$RL_AICc)
mcomp.trav <- mcomp.trav[order(mcomp.trav$deltaAICc),]

ss <- MuMIn::model.avg(m3c,m3d,beta=TRUE)
xx <- summary(ss)
model.average.trav <- as.data.frame(xx$coefmat.full)
model.average.trav$resp <- rep("Log(Travel days)")

model.best.trav <- as.data.frame(coef(summary(m3c)))
model.best.trav$resp <- rep("Log(Travel days)")

# random models for stop days vs detour and season
m3  <- lmer(log(trip.dur) ~ 1 + (1|dev),data=trip.summary,REML='FALSE')
m3a <- lmer(log(trip.dur) ~ trip + (1|dev),data=trip.summary,REML='FALSE')
m3b <- lmer(log(trip.dur) ~ detour + (1|dev),data=trip.summary,REML='FALSE')
m3c <- lmer(log(trip.dur) ~ detour + trip + (1|dev),data=trip.summary,REML='FALSE')
m3d <- lmer(log(trip.dur) ~ detour * trip + (1|dev),data=trip.summary,REML='FALSE')
mcomp.dur <- anova(m3,m3a,m3b,m3c,m3d)
mcomp.dur <- as.data.frame(broom::tidy(mcomp.dur))
mcomp.dur$rsq.mod <- c(rsq(m3)$model,rsq(m3a)$model,rsq(m3b)$model,rsq(m3c)$model,rsq(m3d)$model)
mcomp.dur$rsq.fixed <- as.numeric(c(rsq(m3)$fixed,rsq(m3a)$fixed,rsq(m3b)$fixed,rsq(m3c)$fixed,rsq(m3d)$fixed))
mcomp.dur$rsq.random <- c(rsq(m3)$random,rsq(m3a)$random,rsq(m3b)$random,rsq(m3c)$random,rsq(m3d)$random)
p<- mcomp.dur$npar
n <- length(trip.summary$trip.dur)
mcomp.dur$AICc <- mcomp.dur$AIC + (2 * p^2 + 2 * p)/(n - p - 1)
mcomp.dur$resp <- "Log(Trip duration)"
mcomp.dur$predictors <- c("","trip","detour","detour+trip","detour*trip")
mcomp.dur$deltaAICc <- mcomp.dur$AICc - min(mcomp.dur$AICc)
mcomp.dur$RL_AICc <- exp( -0.5 * mcomp.dur$deltaAICc)
mcomp.dur$weightAICc <- mcomp.dur$RL_AICc/sum(mcomp.dur$RL_AICc)
mcomp.dur <- mcomp.dur[order(mcomp.dur$deltaAICc),]

ss <- MuMIn::model.avg(m3b,m3c,m3d,beta=TRUE)
xx <- summary(ss)
model.average.dur <- as.data.frame(xx$coefmat.full)
model.average.dur$resp <- rep("Log(Duration)")

model.best.dur <- as.data.frame(coef(summary(m3b)))
model.best.dur$resp <- rep("Log(Trip Duration)")


# random models for stop days vs detour and season
m3  <- lmer(trip.tailwind.r ~ 1 + (1|dev),data=trip.summary,REML='FALSE')
m3a <- lmer(trip.tailwind.r ~ trip + (1|dev),data=trip.summary,REML='FALSE')
m3b <- lmer(trip.tailwind.r ~ detour + (1|dev),data=trip.summary,REML='FALSE')
m3c <- lmer(trip.tailwind.r ~ detour + trip + (1|dev),data=trip.summary,REML='FALSE')
m3d <- lmer(trip.tailwind.r ~ detour * trip + (1|dev),data=trip.summary,REML='FALSE')
mcomp.tw <- anova(m3,m3a,m3b,m3c,m3d)
mcomp.tw <- as.data.frame(broom::tidy(mcomp.tw))
mcomp.tw$rsq.mod <- c(rsq(m3)$model,rsq(m3a)$model,rsq(m3b)$model,rsq(m3c)$model,rsq(m3d)$model)
mcomp.tw$rsq.fixed <- as.numeric(c(rsq(m3)$fixed,rsq(m3a)$fixed,rsq(m3b)$fixed,rsq(m3c)$fixed,rsq(m3d)$fixed))
mcomp.tw$rsq.random <- c(rsq(m3)$random,rsq(m3a)$random,rsq(m3b)$random,rsq(m3c)$random,rsq(m3d)$random)
p<- mcomp.tw$npar
n <- length(trip.summary$trip.tailwind.r)
mcomp.tw$AICc <- mcomp.tw$AIC + (2 * p^2 + 2 * p)/(n - p - 1)
mcomp.tw$resp <- "Tailwind"
mcomp.tw$predictors <- c("","trip","detour","detour+trip","detour*trip")
mcomp.tw$deltaAICc <- mcomp.tw$AICc - min(mcomp.tw$AICc)
mcomp.tw$RL_AICc <- exp( -0.5 * mcomp.tw$deltaAICc)
mcomp.tw$weightAICc <- mcomp.tw$RL_AICc/sum(mcomp.tw$RL_AICc)
mcomp.tw <- mcomp.tw[order(mcomp.tw$deltaAICc),]

ss <- MuMIn::model.avg(m3a,m3c,m3d,beta=TRUE)
xx <- summary(ss)
model.average.tw <- as.data.frame(xx$coefmat.full)
model.average.tw$resp <- rep("Tailwind_track")

model.best.tw <- as.data.frame(coef(summary(m3d)))
model.best.tw$resp <- rep("Log(Tailwind)")

# bind together mixed model outputs
mcomp <- rbind(mcomp.dur,mcomp.rest,mcomp.trav,mcomp.tw)
write.csv(mcomp,'./Figures 2021/TableS2_MEE-Performance-vs-Detour-Trip_v20210421.csv')
rm(m3,m3a,m3b,m3c,m3d,p,n,mcomp.dur,mcomp.rest,mcomp.trav,mcomp.tw)

model.average <- rbind(model.average.dur,model.average.rest,model.average.trav,model.average.tw)
rm(model.average.dur,model.average.rest,model.average.trav,model.average.tw)

model.best <- rbind(model.best.dur,model.best.rest,model.best.trav,model.best.tw)
rm(model.best.dur,model.best.rest,model.best.trav,model.best.tw)

write.csv(model.average,'./Figures 2021/TableS3_ModAvg-effects_v20210421.csv')
write.csv(model.best,'./Figures 2021/TableS3_BestModel-effects_v20210421.csv')

rm(model.best,model.average,ss,xx,mcomp)
