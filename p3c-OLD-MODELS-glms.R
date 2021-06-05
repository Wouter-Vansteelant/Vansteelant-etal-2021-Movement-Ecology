
########################################
## OLD CODE: GLMS FOR linear models for averaged dataset: perf vs season and detour (OLD TABLES going with fig s1)
########################################
m1a <- glm(log(mean.trip.dur) ~ trip ,data=ind.summ)
m1b <- glm(log(mean.trip.dur) ~ mean.detour ,data=ind.summ)
m1c <- glm(log(mean.trip.dur) ~ mean.detour + trip,data=ind.summ)
m1d <- glm(log(mean.trip.dur) ~ mean.detour * trip,data=ind.summ)
AIC.dur <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.dur$response <- rep("Trip duration")
AIC.dur$predictors <- c("trip","detour","detour+trip","detour*trip")
AIC.dur$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

m1a <- glm(log(mean.trip.traveldays) ~ trip ,data=ind.summ)
m1b <- glm(log(mean.trip.traveldays) ~ mean.detour ,data=ind.summ)
m1c <- glm(log(mean.trip.traveldays) ~ mean.detour + trip,data=ind.summ)
m1d <- glm(log(mean.trip.traveldays) ~ mean.detour * trip,data=ind.summ)
AIC.trav <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.trav$response <- rep("Travel days")
AIC.trav$predictors <- c("trip","detour","detour+trip","detour*trip")
AIC.trav$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

m1a <- glm(log(mean.trip.restday+1) ~ trip ,data=ind.summ)
m1b <- glm(log(mean.trip.restday+1) ~ mean.detour ,data=ind.summ)
m1c <- glm(log(mean.trip.restday+1) ~ mean.detour + trip,data=ind.summ)
m1d <- glm(log(mean.trip.restday+1) ~ mean.detour * trip,data=ind.summ)
AIC.rest <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.rest$response <- rep("Stop days")
AIC.rest$predictors <- c("trip","detour","detour+trip","detour*trip")
AIC.rest$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))


m1a <- glm(mean.trip.tailwind.r ~ trip ,data=ind.stats)
m1b <- glm(mean.trip.tailwind.r ~ mean.detour ,data=ind.stats)
m1c <- glm(mean.trip.tailwind.r ~ mean.detour + trip,data=ind.stats)
m1d <- glm(mean.trip.tailwind.r ~ mean.detour * trip,data=ind.stats)
AIC.tw <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.tw$response <- rep("Tailwind")
AIC.tw$predictors <- c("trip","detour","detour+trip","detour*trip")
AIC.tw$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

AIC.all <- rbind(AIC.dur,AIC.rest,AIC.trav,AIC.tw)
AIC.all

write.csv(AIC.all,'./Figures 2021/TableS2_wtFigS1_LM-Averaged-Performance-vs-Detour-Trip_v20210110.csv')
rm(m1,m1a,m1b,m1c,m1d,p,n,AIC.dur,AIC.rest,AIC.trav,AIC.tw)
# take away: season explains bulk of variation in detour extent, trip duration and stop-over days (based 
# on higher R2, lower AIC and AICc for these models. Model inspection showed significant effects). ID explains considerable
# amount of additional variation in detour extent and trip duration (post-hoc analyses -not shown - suggest this pattern is driven mainly by  some particularly long
# spring journeys by just two birds). However, ID does not explain any additional variation in nr of rest days. 
# Season nor ID nor a combination of the two explain variation in travel days.

########################################
## OLD CODE: GLMS for full dataset split per season: performance vs detour and ID  (OLD Table S4)
########################################
# create models
ret <- subset(trip.summary,trip.summary$trip == 'return')
aut <- subset(trip.summary,trip.summary$trip == 'out')

### MODELS FOR TRIP DURATION
# compute for outward
m1a <- glm(log(trip.dur) ~ dev,data=aut)
m1b <- glm(log(trip.dur) ~ detour,data=aut)
m1c <- glm(log(trip.dur) ~ dev+detour,data=aut)
m1d <- glm(log(trip.dur) ~ dev*detour,data=aut)
AIC.out <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.out$trip <- rep("out")
AIC.out$response <- rep("trip.dur")
AIC.out$predictors <- c("dev","detour","dev+detour","dev*detour")
AIC.out$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

# compute for return
m1a <- glm(log(trip.dur) ~ dev,data=ret)
m1b <- glm(log(trip.dur) ~ detour,data=ret)
m1c <- glm(log(trip.dur) ~ dev+detour,data=ret)
m1d <- glm(log(trip.dur) ~ dev*detour,data=ret)
AIC.ret <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.ret$trip <- rep("return")
AIC.ret$response <- rep("trip.dur")
AIC.ret$predictors <- c("dev","detour","dev+detour","dev*detour")
AIC.ret$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

# combine seasons
AIC.table.trip.dur <- rbind(AIC.out,AIC.ret)

### MODELS FOR TRAVEL DAYS
# compute for outward
m1a <- glm(log(trip.traveldays) ~ dev,data=aut)
m1b <- glm(log(trip.traveldays) ~ detour,data=aut)
m1c <- glm(log(trip.traveldays) ~ dev+detour,data=aut)
m1d <- glm(log(trip.traveldays) ~ dev*detour,data=aut)
AIC.out <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.out$trip <- rep("out")
AIC.out$response <- rep("trip.traveldays")
AIC.out$predictors <- c("dev","detour","dev+detour","dev*detour")
AIC.out$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

# compute for return
m1a <- glm(log(trip.traveldays) ~ dev,data=ret)
m1b <- glm(log(trip.traveldays) ~ detour,data=ret)
m1c <- glm(log(trip.traveldays) ~ dev+detour,data=ret)
m1d <- glm(log(trip.traveldays) ~ dev*detour,data=ret)
AIC.ret <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.ret$trip <- rep("return")
AIC.ret$response <- rep("trip.traveldays")
AIC.ret$predictors <- c("dev","detour","dev+detour","dev*detour")
AIC.ret$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

# combine seasons
AIC.table.trip.traveldays <- rbind(AIC.out,AIC.ret)


### MODELS FOR REST DAYS
# compute for outward
m1a <- glm(log(trip.restdays + 1) ~ dev,data=aut)
m1b <- glm(log(trip.restdays + 1) ~ detour,data=aut)
m1c <- glm(log(trip.restdays + 1) ~ dev+detour,data=aut)
m1d <- glm(log(trip.restdays + 1) ~ dev*detour,data=aut)
AIC.out <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.out$trip <- rep("out")
AIC.out$response <- rep("trip.restdays")
AIC.out$predictors <- c("dev","detour","dev+detour","dev*detour")
AIC.out$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

# compute for return
m1a <- glm(log(trip.restdays + 1) ~ dev,data=ret)
m1b <- glm(log(trip.restdays + 1) ~ detour,data=ret)
m1c <- glm(log(trip.restdays + 1) ~ dev+detour,data=ret)
m1d <- glm(log(trip.restdays + 1) ~ dev*detour,data=ret)
AIC.ret <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.ret$trip <- rep("return")
AIC.ret$response <- rep("trip.restdays")
AIC.ret$predictors <- c("dev","detour","dev+detour","dev*detour")
AIC.ret$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

# combine seasons
AIC.table.trip.restdays <- rbind(AIC.out,AIC.ret)

### COMBINED TABLE FOR ALL PARAMETERS
AIC.table <- rbind(AIC.table.trip.dur,AIC.table.trip.traveldays,AIC.table.trip.restdays)
rm(AIC.table.trip.dur,AIC.table.trip.traveldays,AIC.table.trip.restdays)


write.csv(AIC.table,'./Figures 2021/OLD_TableS4_SeasonalPerformance-vs-ID&detour-v20210105.csv')


## OLD table 2: glms performance vs detour and season
m1a <- glm(trip.dur ~ dev,data=trip.summary,family='Gamma')
m1b <- glm(trip.dur ~ trip,data=trip.summary,family='Gamma')
m1c <- glm(trip.dur ~ dev+trip,data=trip.summary,family='Gamma')
m1d <- glm(trip.dur ~ dev*trip,data=trip.summary,family='Gamma')
AIC.dur <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.dur$response <- rep("trip.dur")
AIC.dur$predictors <- c("dev","trip","dev+trip","dev*trip")
AIC.dur$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

m1a <- glm(detour ~ dev,data=trip.summary,family='Gamma')
m1b <- glm(detour ~ trip,data=trip.summary,family='Gamma')
m1c <- glm(detour ~ dev+trip,data=trip.summary,family='Gamma')
m1d <- glm(detour ~ dev*trip,data=trip.summary,family='Gamma')
AIC.detour <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.detour$response <- rep("detour")
AIC.detour$predictors <- c("dev","trip","dev+trip","dev*trip")
AIC.detour$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

m1a <- glm(trip.traveldays ~ dev,data=trip.summary,family='Gamma')
m1b <- glm(trip.traveldays ~ trip,data=trip.summary,family='Gamma')
m1c <- glm(trip.traveldays ~ dev+trip,data=trip.summary,family='Gamma')
m1d <- glm(trip.traveldays ~ dev*trip,data=trip.summary,family='Gamma')
AIC.travel <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.travel$response <- rep("traveldays")
AIC.travel$predictors <- c("dev","trip","dev+trip","dev*trip")
AIC.travel$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

m1a <- glm(trip.restdays ~ dev,data=trip.summary)
m1b <- glm(trip.restdays ~ trip,data=trip.summary)
m1c <- glm(trip.restdays ~ dev+trip,data=trip.summary)
m1d <- glm(trip.restdays ~ dev*trip,data=trip.summary)
AIC.rest <- AIC.comp(m1a,m1b,m1c,m1d)
AIC.rest$response <- rep("restdays")
AIC.rest$predictors <- c("dev","trip","dev+trip","dev*trip")
AIC.rest$rsq <- c(rsq(m1a,adj=TRUE),rsq(m1b,adj=TRUE),rsq(m1c,adj=TRUE),rsq(m1d,adj=TRUE))

AIC.table <- rbind(AIC.detour,AIC.dur,AIC.travel,AIC.rest)
rm(AIC.detour,AIC.dur,AIC.travel,AIC.rest)
rm(m1a,m1b,m1c,m1d,dev,p,n,AICm1a,AICm1b,AICm1c,AICm1d,AIC.ret,AIC.out,AIC.detour,AIC.dur,AIC.travel,AIC.rest)
rm(aut,ret,AIC.table)

write.csv(AIC.table,'./Figures 2021/OLD_TableS2_Performance-vs-Season&ID-v20210105.csv')

