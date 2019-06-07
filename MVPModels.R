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
dat_adv<-loadAdvanced(1990,2018,dat_mvp,normalize=TRUE)

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
#library("dplyr")
#library("corrplot")
par(mfrow=c(1,1))
dtot_numeric<-select_if(dat_totals, is.numeric)
corrplot(cor(dtot_numeric))

## MVP prelim stats
dmvp_numeric<-select_if(dat_mvp, is.numeric)
corrplot(cor(dmvp_numeric))

## advanced prelim stats
dadv_numeric<-select_if(dat_adv, is.numeric)
corrplot(cor(dadv_numeric,use="complete.obs"))
## visualize 
par(mfrow=c(1,2))
### VORP VS PER
plot(dat_adv$VORP, dat_adv$PER)
dat_adv_mvps <- dat_adv[which(dat_adv$MVP==TRUE),]
points(dat_adv_mvps$VORP,dat_adv_mvps$PER,pch=19,col="red")
### WS VS BPM
plot(dat_adv$WS, dat_adv$BPM)
dat_adv_mvps <- dat_adv[which(dat_adv$MVP==TRUE),]
points(dat_adv_mvps$WS,dat_adv_mvps$BPM,pch=19,col="red")

#######################################################
################# TOTAL STATS MODELS ##################
######### NEED TO ACTUALLY CHOOSE PREDICTORS ##########
#######################################################
## fit shortlist model
dat_totals$Shortlist<-dat_totals$Share!=0
tot.short.mod <- glm(Shortlist~G+MP+X3P+DRB+AST+
                       BLK+TOV+PF+PTS+Team.Wins,data=dat_totals,family = binomial(link = "logit"))
## grab shortlist
tot.shortlist<-dat_totals[which(predict(tot.short.mod,type="response")>SHORT_CUTOFF),]

## fit linear
tot.lm <- lm(First~G+X3P+DRB+AST+BLK+PF+PTS+Team.Wins,data=tot.shortlist)
## linear pred
tot.lm.pred<-predMVPs(tot.shortlist,tot.lm)
tot.lm.acc<-calcAccuracy(tot.lm.pred,FALSE)

## fit logit
tot.log <- glm(MVP~G+X3P+DRB+AST+BLK+PF+PTS+Team.Wins,data=tot.shortlist,family = binomial(link = "logit"))
## logit pred
tot.log.pred<-predMVPs(tot.shortlist,tot.log)
tot.log.acc<-calcAccuracy(tot.log.pred,FALSE)

## LASSO 
#library(glmnet)
x = model.matrix(First~G+MP+X3P+DRB+AST+BLK+TOV+PF+PTS+Team.Wins,data=tot.shortlist)
y = tot.shortlist$First
tot.cv.out <- cv.glmnet(x,y,alpha=1)
lasso.coef=predict(tot.cv.out,type="coefficients",s=tot.cv.out$lambda.min)


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
adv.short.mod <- glm(Shortlist~G+MP+PER+TSP+X3PAr+FTr+ORBP+DRBP+TRBP+ASTP+STLP+BLKP+
                     TOVP+USGP+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP+Team.Wins,
                    data=dat_adv,family = binomial(link = "logit"))
## grab shortlist
adv.shortlist<-dat_adv[which(predict(adv.short.mod,type="response")>SHORT_CUTOFF),]

## fit linear
adv.lm <- lm(First~G+PER+TSP+X3PAr+FTr+TRBP+ASTP+STLP+BLKP+
              TOVP+USGP+WS+BPM+VORP+Team.Wins,
              data=adv.shortlist)
## linear pred
adv.lm.pred<-predMVPs(adv.shortlist,adv.lm)
adv.lm.acc<-calcAccuracy(adv.lm.pred,FALSE)

## fit logit
adv.log <- glm(MVP~PER+TSP+X3PAr+FTr+TRBP+ASTP+STLP+BLKP+
                 TOVP+USGP+WS+BPM+VORP+Team.Wins
                ,data=adv.shortlist,family = binomial(link = "logit"))
## logit pred
adv.log.pred<-predMVPs(adv.shortlist,adv.log)
adv.log.acc<-calcAccuracy(adv.log.pred,FALSE)

## LASSO 
#library(glmnet)
x = model.matrix(First~G+MP+PER+TSP+X3PAr+FTr+ORBP+DRBP+TRBP+ASTP+STLP+BLKP+
                   TOVP+USGP+OWS+DWS+WS+WS.48+OBPM+DBPM+BPM+VORP+Team.Wins
                  ,data=adv.shortlist)
y = adv.shortlist$First
adv.cv.out <- cv.glmnet(x,y,alpha=1)
adv.lasso.coef=predict(adv.cv.out,type="coefficients",s=adv.cv.out$lambda.min)

#######################################################
##################### MERGED DATA  ####################
#######################################################
## merge data
#PER+TSP+X3PAr+FTr+TRBP+ASTP+STLP+BLKP+
#TOVP+USGP+WS+BPM+VORP+Team.Wins
dat_merge <- dat_totals
adv_subset <- dat_adv[,c(2,3,12,13,14,15,18,19,20,21,22,23,26,30,31)]
dat_merge <- merge(dat_merge,adv_subset,by=c("Player","Season"))

## grab shortlist
merge.shortlist<-dat_merge[which(predict(tot.short.mod,newdata = dat_merge,type="response")>SHORT_CUTOFF),]

## predict for each model
merge.tot.pred<-predict(tot.log,newdata=merge.shortlist,type="response")
merge.adv.pred<-predict(adv.log,newdata=merge.shortlist,type="response")
merge.pred <- (merge.tot.pred + merge.adv.pred) / 2
## select MVPS
### create empty MVPs dataframe
merge.mvps<-merge.shortlist[0,]
for (year in levels(as.factor(merge.shortlist$Season))) {
  dat_temp<-merge.shortlist[which(merge.shortlist$Season==year),]
  temp_preds<-merge.pred[which(merge.shortlist$Season==year)]
  mvp<-dat_temp[which(temp_preds==max(temp_preds)),]
  merge.mvps<-data.frame(rbind(as.matrix(merge.mvps), as.matrix(mvp)))
}
merge.acc<-calcAccuracy(merge.mvps,FALSE)

## fit linear
merge.lm <- lm(First~G+PER+TSP+X3PAr+FTr+TRBP+ASTP+STLP+BLKP+
                TOVP+USGP+WS+BPM+VORP+Team.Wins+PER+TSP+X3PAr+
                FTr+TRBP+ASTP+STLP+BLKP+TOVP+USGP+WS+BPM+VORP,
                data=merge.shortlist)
## linear pred
merge.lm.pred<-predMVPs(merge.shortlist,merge.lm)
merge.lm.acc<-calcAccuracy(merge.lm.pred,FALSE)

## fit logit
merge.log <- glm(MVP~G+PER+TSP+X3PAr+FTr+TRBP+ASTP+STLP+BLKP+
                  TOVP+USGP+WS+BPM+VORP+Team.Wins+PER+TSP+X3PAr+
                  FTr+TRBP+ASTP+STLP+BLKP+TOVP+USGP+WS+BPM+VORP,
                  data=merge.shortlist,family = binomial(link = "logit"))
## logit pred
merge.log.pred<-predMVPs(merge.shortlist,merge.log)
merge.log.acc<-calcAccuracy(merge.log.pred,FALSE)

#######################################################
################## CROSS VALIDATION ###################
#######################################################
tlm.cv.acc<-accuracyCV(tot.shortlist,tot.lm)
tlog.cv.acc<-accuracyCV(tot.shortlist,tot.log)
plm.cv.acc<-accuracyCV(pg.shortlist,pg.lm)
plog.cv.acc<-accuracyCV(pg.shortlist,pg.log)
alm.cv.acc<-accuracyCV(adv.shortlist,adv.lm)
alog.cv.acc<-accuracyCV(adv.shortlist,adv.log)
mlm.cv.acc<-accuracyCV(merge.shortlist,merge.lm)
mlog.cv.acc<-accuracyCV(merge.shortlist,merge.log)

#######################################################
#################### SUMMARY PLOTS ####################
#######################################################
accs <- data.frame(Data=c("Totals","Per Game","Advanced","Tot. + Adv."),Linear=c(tot.lm.acc,pg.lm.acc,adv.lm.acc,merge.lm.acc),
                   Logistic=c(tot.log.acc,pg.log.acc,adv.log.acc,merge.log.acc),
                   LOOCV.lm=c(tlm.cv.acc,plm.cv.acc,alm.cv.acc,mlm.cv.acc),
                   LOOCV.log=c(tlog.cv.acc,plog.cv.acc,alog.cv.acc,mlog.cv.acc))
par(mfrow=c(1,1))
#library(RColorBrewer)
xx=barplot(c(accs$Linear,accs$LOOCV.lm,accs$Logistic,accs$LOOCV.log), 
           names.arg=rep(accs$Data,times=4),
           ylim=c(0,1),
           las=2,
           col=c(rep("blue",4),rep("red",4),rep("green",4),rep("yellow",4)),
           ylab="Accuracy", 
           main="Model Accuracies"
          )
text(x=xx,
     y=c(accs$Linear,accs$LOOCV.lm,accs$Logistic,accs$LOOCV.log),
     label=round(c(accs$Linear,accs$LOOCV.lm,accs$Logistic,accs$LOOCV.log),digits=3),
     pos=3
    ) 
legend("topright", 
       c("Linear","Linear LOOCV","Logistic","Logistic LOOCV"), 
       fill=c("blue","red","green","yellow"), 
       cex=0.8
    )

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

predMVPs <- function(param.dat,param.mod,lasso=FALSE,bestlam=0){
  # make predictions
  if (lasso == TRUE) {
    ## lasso case
    predict(param.mod,s=bestlam,newx=param.dat)
  } else {
    ## linear/logistic cases
    pred<-predict(param.mod,newdata=param.dat,type="response")
  }
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

accuracyCV <- function(param.dat,param.mod){
  # extract formula from model
  formula <- param.mod$call[2]
  mvps<-param.dat[0,]
  for (year in min(param.dat$Season):max(param.dat$Season)) {
    # leave out one year
    test <- param.dat[which(param.dat$Season == year),]
    train <- param.dat[which(param.dat$Season != year),]
    # train model on remaining
    ## fit linear
    cv <- lm(formula,data=train)
    ## linear pred
    cv.pred<-predict(cv,newdata=test,type="response")
    cv.mvp<-test[which(cv.pred == max(cv.pred)),]
    mvps <- data.frame(rbind(as.matrix(mvps), as.matrix(cv.mvp)))
    # test on leaveout
  }
  return(calcAccuracy(mvps,FALSE))
}
