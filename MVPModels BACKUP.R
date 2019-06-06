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
dat_pergame<-read.csv("./repositories/nba-models/full-data/advanced.csv",header=TRUE)

## total prelim stats
#library("dplyr")
dtot_numeric<-select_if(dat_totals, is.numeric)
corrplot(cor(dtot_numeric))

## MVP prelim stats
dmvp_numeric<-select_if(dat_mvp, is.numeric)
corrplot(cor(dmvp_numeric))

## advanced prelim stats
dadv_numeric<-select_if(dat_adv, is.numeric)
corrplot(cor(dadv_numeric,use="complete.obs"))

#######################################################
################# PRELIMINARY MODELS ##################
#######################################################

# models
## MVP data only
mod.mvp<-lm(First~Age+G+MP+PTS+TRB+AST+STL+BLK+FG.+X3P.+FT.+Team.Wins,data=dat_mvp)
summary(mod.mvp)
mod.mvp.red<-lm(First~G+PTS+TRB+AST+Team.Wins,data=dat_mvp)
summary(mod.mvp.red)

## total data
mod.totals<-lm(First~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,data=dat_totals)
summary(mod.totals)

# pergame data
mod.pergame<-lm(First~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,data=dat_pergame)
summary(mod.pergame)

# advanced data
mod.adv<-lm(First~Age+G+MP+PER+TSP+X3PAr+FTr+ORBP+DRBP+TRBP+ASTP+STLP+BLKP+TOVP+USGP+OWS+DWS+WS+
                  WS.48+OBPM+DBPM+BPM+VORP,data=dat_adv)
summary(mod.adv)

# actual winners for comparison
truevals<-dat_mvp[which(dat_mvp$Rank==1),]

# make predictions & calc accuracy
## MVP data
### full model
MVPs.mvp<-predMVPs(dat_mvp,mod.mvp)
acc.mvp<-calcAccuracy(MVPs.mvp,TRUE)
### reduced model
MVPs.mvp.red<-predMVPs(dat_mvp,mod.mvp.red)
acc.mvp.red<-calcAccuracy(MVPs.mvp.red,TRUE)
## total stats data
MVPs.totals<-predMVPs(dat_totals,mod.totals)
acc.totals<-calcAccuracy(MVPs.totals,FALSE)
## pergame stats data
MVPs.pergame<-predMVPs(dat_pergame,mod.pergame)
acc.pergame<-calcAccuracy(MVPs.pergame,FALSE)

# try logit model
## MVP data
mod_logit<-glm(MVP~Age+G+MP+PTS+TRB+AST+STL+BLK+FG.+X3P.+FT.+Team.Wins,data=dat_mvp,family = binomial(link = "logit"))
summary(mod_logit)

# McFadden's R^2
#install.packages("pscl")
library(pscl)
pR2(mod_logit)

## MVP logit model
MVPs.logit<-predMVPs(dat_mvp,mod_logit)
acc.logit<-calcAccuracy(MVPs.logit,FALSE)


#######################################################
################# SHORTLIST -> TOTAL ##################
#######################################################
dat_totals$Shortlist<-dat_totals$Share!=0
mod.shortlist<-glm(Shortlist~G+MP+X3P+DRB+AST+BLK+TOV+PF+PTS+Team.Wins,data=dat_totals,family = binomial(link = "logit"))
summary(mod.shortlist)

# McFadden's R^2
library(pscl)
pR2(mod.shortlist)

# grab shortlist
shortlist<-dat_totals[which(predict(mod.shortlist,type="response")>0.5),]

# shortlist final logit
mod.shortlist<-glm(MVP~G+MP+X3P+DRB+AST+BLK+TOV+PF+PTS+Team.Wins,data=shortlist,family = binomial(link = "logit"))
summary(mod.shortlist)

# McFadden's R^2
library(pscl)
pR2(mod.shortlist)

## MVP logit model
MVPs.shortlist<-predMVPs(shortlist,mod.shortlist)
acc.shortlist<-calcAccuracy(MVPs.shortlist,FALSE)
acc.shortlist

## shortlist - linear regression
mod.shortlist.lm<-lm(First~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,data=shortlist)
summary(mod.shortlist.lm)
## total stats data
MVPs.shortlist.lm<-predMVPs(shortlist,mod.shortlist.lm)
acc.shortlist.lm<-calcAccuracy(MVPs.shortlist.lm,FALSE)

#######################################################
#################  PREDICT FOR 2018  ##################
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
