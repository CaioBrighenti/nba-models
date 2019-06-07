## HYPERPARAMETERS
SHORT_CUTOFF <- 0.75


#######################################################
############### PROCESS BBALLREF DATA #################
#######################################################

source("./repositories/nba-models/loadData.R")

# LOAD STANDINGS DATA
dat_std<-loadStandings(1990,2019)

# LOAD MVP DATA
dat_mvp<-loadMVP(1990,2018,dat_std)

# LOAD TOTAL PLAYER STATS
dat_totals<-loadTotals(1990,2018,dat_mvp,normalize=TRUE)

# LOAD PERGAME PLAYER STATS
dat_pergame<-loadPerGame(1990,2018,dat_mvp,normalize=TRUE)

# LOAD ADVANCED PLAYER STATS
dat_adv<-loadAdvanced(1990,2018,dat_mvp,normalize=FALSE)

# WRITE TO CSVs
write.csv(dat_std,file = "./repositories/nba-models/full-data/standings.csv",row.names=FALSE)
write.csv(dat_mvp,file = "./repositories/nba-models/full-data/mvp.csv",row.names=FALSE)
write.csv(dat_totals,file = "./repositories/nba-models/full-data/totals.csv",row.names=FALSE)
write.csv(dat_pergame,file = "./repositories/nba-models/full-data/pergame.csv",row.names=FALSE)
write.csv(dat_adv,file = "./repositories/nba-models/full-data/advanced.csv",row.names=FALSE)

#######################################################
################## LOAD MERGED CSVs ###################
#######################################################

# LOAD STANDINGS DATA
dat_std<-read.csv("./repositories/nba-models/full-data/standings.csv",header=TRUE)

# LOAD MVP DATA
dat_mvp<-read.csv("./repositories/nba-models/full-data/mvp.csv",header=TRUE)

# LOAD TOTAL PLAYER STATS
dat_totals<-read.csv("./repositories/nba-models/full-data/totals.csv",header=TRUE)

# LOAD PERGAME PLAYER STATS
dat_pergame<-read.csv("./repositories/nba-models/full-data/pergame.csv",header=TRUE)

# LOAD PERGAME PLAYER STATS
dat_adv<-read.csv("./repositories/nba-models/full-data/advanced.csv",header=TRUE)

## total prelim stats
library("dplyr")
library("corrplot")
dtot_numeric<-select_if(dat_totals, is.numeric)
corrplot(cor(dtot_numeric))

## MVP prelim stats
dmvp_numeric<-select_if(dat_mvp, is.numeric)
corrplot(cor(dmvp_numeric))

## advanced prelim stats
dadv_numeric<-select_if(dat_adv, is.numeric)
corrplot(cor(dadv_numeric,use="complete.obs"))
## visualize 
### VORP VS PER
plot(dat_adv$VORP, dat_adv$PER)
dat_adv_mvps <- dat_adv[which(dat_adv$MVP==TRUE),]
points(dat_adv_mvps$VORP,dat_adv_mvps$PER,pch=19,col="red")
### WS VS BPM
plot(dat_adv$WS, dat_adv$BPM)
dat_adv_mvps <- dat_adv[which(dat_adv$MVP==TRUE),]
points(dat_adv_mvps$WS,dat_adv_mvps$BPM,pch=19,col="red")
## weird PER values
PER_outliers <- dat_adv[which(abs(dat_adv$PER)>30),]

#######################################################
################# TOTAL STATS MODELS ##################
######### NEED TO ACTUALLY CHOOSE PREDICTORS ##########
#######################################################
## fit shortlist model
dat_totals$Shortlist<-dat_totals$Share!=0
tot.short.mod <- glm(Shortlist~G+MP+X3P+DRB+AST+BLK+TOV+PF+PTS+Team.Wins,data=dat_totals,family = binomial(link = "logit"))
## grab shortlist
tot.shortlist<-dat_totals[which(predict(tot.short.mod,type="response")>SHORT_CUTOFF),]

## fit linear
tot.lm <- lm(First~G+MP+X3P+DRB+AST+BLK+TOV+PF+PTS+Team.Wins,data=tot.shortlist)
## linear pred
tot.lm.pred<-predMVPs(tot.shortlist,tot.lm)
tot.lm.acc<-calcAccuracy(tot.lm.pred,FALSE)

## fit logit
tot.log <- glm(MVP~G+MP+X3P+DRB+AST+BLK+TOV+PF+PTS+Team.Wins,data=tot.shortlist,family = binomial(link = "logit"))
## logit pred
tot.log.pred<-predMVPs(tot.shortlist,tot.log)
tot.log.acc<-calcAccuracy(tot.log.pred,FALSE)


#######################################################
############### PERGAME STATS MODELS ##################
#######################################################
## fit shortlist model
dat_pergame$Shortlist<-dat_pergame$Share!=0
pg.short.mod <- glm(Shortlist~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,
                    data=dat_pergame,family = binomial(link = "logit"))
## grab shortlist
pg.shortlist<-dat_pergame[which(predict(pg.short.mod,type="response")>SHORT_CUTOFF),]

## fit linear
pg.lm <- lm(First~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins
            ,data=pg.shortlist)
## linear pred
pg.lm.pred<-predMVPs(pg.shortlist,pg.lm)
pg.lm.acc<-calcAccuracy(pg.lm.pred,FALSE)

## fit logit
pg.log <- glm(MVP~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins
              ,data=pg.shortlist,family = binomial(link = "logit"))
## logit pred
pg.log.pred<-predMVPs(pg.shortlist,pg.log)
pg.log.acc<-calcAccuracy(pg.log.pred,FALSE)

#######################################################
################ ADVANCED STATS MODELS ################
#######################################################
## fit shortlist model
dat_adv$Shortlist<-dat_adv$Share!=0
adv.short.mod <- glm(Shortlist~G+MP+WS+PER+VORP+Team.Wins,
                    data=dat_adv,family = binomial(link = "logit"))
## grab shortlist
adv.shortlist<-dat_adv[which(predict(adv.short.mod,type="response")>.9),]

## fit linear
adv.lm <- lm(First~(G+WS+PER+VORP+Team.Wins)^2,
              data=adv.shortlist)
## linear pred
adv.lm.pred<-predMVPs(adv.shortlist,adv.lm)
adv.lm.acc<-calcAccuracy(adv.lm.pred,FALSE)

## fit logit
adv.log <- glm(MVP~G+WS+PER+VORP+Team.Wins
              ,data=adv.shortlist,family = binomial(link = "logit"))
## logit pred
adv.log.pred<-predMVPs(adv.shortlist,adv.log)
adv.log.acc<-calcAccuracy(adv.log.pred,FALSE)

#######################################################
#################  PREDICT FOR 2019  ##################
#######################################################
# LOAD 2019 STATS
dat_2019<-loadCurrent(normalize = TRUE)
#dat_2019<-dat_totals[which(dat_totals$Season==2011),]

# predict shortlist
shortlist.pred<-predict(mod.shortlist,dat_2019)
shortlist_2019<-dat_2019[order(-shortlist.pred),]
shortlist_2019<-shortlist_2019[1:10,]


# predict winner - no shortlist
pred<-predict(mod.shortlist,dat_2019,type="response")
dat_2019_pred<-dat_2019
dat_2019_pred$Pred<-pred
dat_2019_pred<-dat_2019_pred[order(-dat_2019_pred$Pred),] 

# predict winner - with shortlist
pred<-predict(mod.shortlist,shortlist_2019,type="response")
shortlist_2019_pred<-shortlist_2019
shortlist_2019_pred$Pred<-pred
shortlist_2019_pred<-shortlist_2019_pred[order(-shortlist_2019_pred$Pred),] 

# turn shortlist predictions into percentages
shortlist_2019_pred$Pct<-shortlist_2019_pred$Pred-min(shortlist_2019_pred$Pred)
shortlist_2019_pred$Pct<-(shortlist_2019_pred$Pct/sum(shortlist_2019_pred$Pct))*100

## write predictions to csv
pred_dat<-data.frame(Player=shortlist_2019_pred$Player,Pct=shortlist_2019_pred$Pct)
write.csv(pred_dat, file = "./repositories/nba-models/html/2019Pred.csv",row.names=FALSE)


# mod refining ideas
## lockout seasons
## minimum games
## reverse recency bias
## partial seasons
## add advanced stats
## minimum advanced stats (PER)
## post all-star game numbers

#######################################################
################# HELPER FUNCTIONS #################### 
#######################################################

predMVPs <- function(param.dat,param.mod){
  # make predictions
  pred<-predict(param.mod,type="response")
  # create empty MVPs dataframe
  mvps<-param.dat[0,]
  for (year in levels(as.factor(param.dat$Season))) {
    dat_temp<-param.dat[which(param.dat$Season==year),]
    temp_preds<-pred[which(param.dat$Season==year)]
    mvp<-dat_temp[which(temp_preds==max(temp_preds)),]
    mvps<-data.frame(rbind(as.matrix(mvps), as.matrix(mvp)))
  }
  return(mvps)
}

# $Rank versus $MVP is hacky right now, needs better system
calcAccuracy <- function(mvps,ranks) {
  if (ranks){
    errs<-mvps[which(mvps$Rank!=1),]
  } else {
    errs<-mvps[which(mvps$MVP==FALSE),]
  }
  print(errs)
  return(1-(dim(errs)[1]/dim(mvps)[1]))
}
