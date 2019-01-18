#source("C:/Users/Caio/repos/nba-models/loadData.R")

# LOAD STANDINGS DATA
dat_std<-loadStandings(1977,2017)

# LOAD MVP DATA
dat_mvp<-loadMVP(2000,2017,dat_std)

# LOAD TOTAL PLAYER STATS
dat_totals<-loadTotals(2000,2017,dat_mvp,normalize=FALSE)

# LOAD PERGAME PLAYER STATS
#dat_pergame<-loadPerGame(2000,2017)


# models
## MVP data only
mod.mvp<-lm(Pts.Won~Age+G+MP+PTS+TRB+AST+STL+BLK+FG.+X3P.+FT.+Team.Wins,data=dat_mvp)
summary(mod.mvp)
mod.mvp.red<-lm(First~G+PTS+TRB+AST+Team.Wins,data=dat_mvp)
summary(mod.mvp.red)

## total data
mod.totals<-lm(First~Age+G+GS+MP+FG+FG.+X3P+X3P.+X2P.+FT.+ORB+DRB+AST+STL+BLK+TOV+PF+PTS+Team.Wins,data=dat_totals)
summary(mod.totals)

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

# mod refining ideas
## lockout seasons
## minimum games
## reverse recency bias
## partial seasons
## add advanced stats
## minimum advanced stats (PER)
## post all-star game numbers

# make prediction
# load 2018 data
#dat_2018<-loadTotals(2018,2018,dat_mvp,normalize=FALSE)

#######################################################
################# HELPER FUNCTIONS #################### 
#######################################################

predMVPs <- function(param.dat,param.mod){
  # make predictions
  pred<-fitted(param.mod)
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
