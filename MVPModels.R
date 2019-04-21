source("./repositories/nba-models/loadData.R")

# LOAD STANDINGS DATA
dat_std<-loadStandings(2000,2019)

# LOAD MVP DATA
dat_mvp<-loadMVP(2000,2017,dat_std)

# LOAD TOTAL PLAYER STATS
dat_totals<-loadTotals(2000,2018,dat_mvp,normalize=TRUE)

# LOAD PERGAME PLAYER STATS
dat_pergame<-loadPerGame(2000,2017,dat_mvp,normalize=TRUE)


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

# shortlist final logic
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
# LOAD 2018 STATS
dat_2018<-loadCurrent(normalize = TRUE)
#dat_2018<-dat_totals[which(dat_totals$Season==2011),]

# predict shortlist
shortlist.pred<-predict(mod.shortlist,dat_2018)
shortlist_2018<-dat_2018[order(-shortlist.pred),]
shortlist_2018<-shortlist_2018[1:10,]


# predict winner - no shortlist
pred<-predict(mod.shortlist.lm,dat_2018)
dat_2018_pred<-dat_2018
dat_2018_pred$Pred<-pred
dat_2018_pred<-dat_2018_pred[order(-dat_2018_pred$Pred),] 

# predict winner - with shortlist
## same result
pred<-predict(mod.shortlist.lm,shortlist_2018)
shortlist_2018_pred<-shortlist_2018
shortlist_2018_pred$Pred<-pred
shortlist_2018_pred<-shortlist_2018_pred[order(-shortlist_2018_pred$Pred),] 

# turn shortlist predictions into percentages
shortlist_2018_pred$Pct<-shortlist_2018_pred$Pred-min(shortlist_2018_pred$Pred)
shortlist_2018_pred$Pct<-(shortlist_2018_pred$Pct/sum(shortlist_2018_pred$Pct))*100

## write predictions to csv
pred_dat<-data.frame(Player=shortlist_2018_pred$Player,Pct=shortlist_2018_pred$Pct)
write.csv(pred_dat, file = "./Repositories/nba-models/html/2018Pred.csv",row.names=FALSE)


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
