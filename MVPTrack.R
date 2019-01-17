# constants
YEAR_START<-2000
YEAR_END<-2017

#######################################################
#################### MVP DATA ######################### 
#######################################################
# GRAB MVP DATA FROM EACH SEASON
mvp_dat<-data.frame(Rank=integer(),Player=character(),Age=double(),Tm=character(),
                    First=double(),Pts.Won=double(),Pts.Max=double(),Share=double(),
                    G=double(),MP=double(),PTS=double(),TRB=double(),AST=double(),
                    STL=double(),BLK=double(),FG.=double(),X3P.=double(),FT.=double(),
                    WS=double(),WS.48=double(),Season=double())
for (year in YEAR_START:YEAR_END) {
  str<-paste("C:/Users/Caio/repos/nba-models/award-stats/",year,".csv",sep="")
  dat_temp<-read.csv(str, header = TRUE,stringsAsFactors=FALSE)
  dat_temp$Season<-year
  # normalize stats
  ## G
  dat_temp$G<-dat_temp$G/max(dat_temp$G)
  ## MP
  dat_temp$MP<-dat_temp$MP/max(dat_temp$MP)
  ## PTS
  dat_temp$PTS<-dat_temp$PTS/max(dat_temp$PTS)
  ## TRB
  dat_temp$TRB<-dat_temp$TRB/max(dat_temp$TRB)
  ## AST
  dat_temp$AST<-dat_temp$AST/max(dat_temp$AST)
  ## STL
  dat_temp$STL<-dat_temp$STL/max(dat_temp$STL)
  ## BLK
  dat_temp$BLK<-dat_temp$BLK/max(dat_temp$BLK)
  ## FG.
  dat_temp$FG.<-dat_temp$FG./max(dat_temp$FG.)
  ## FT.
  dat_temp$FT.<-dat_temp$FT./max(dat_temp$FT.)
  ## WS.48
  dat_temp$WS.48<-dat_temp$WS.48/max(dat_temp$WS.48)
  
  # clean player names
  dat_temp$Player<-as.character(dat_temp$Player)
  names<-strsplit(as.character(dat_temp$Player),"[\\\\]")
  for (idx in 1:dim(dat_temp)[1]) {
    dat_temp$Player[idx]<-names[[idx]][1]
  }
  
  mvp_dat<-data.frame(rbind.data.frame(as.matrix(mvp_dat), as.matrix(dat_temp)))
}
head(mvp_dat)

# set each column to appropriate data type
levels(mvp_dat$Rank)<-c(as.numeric(levels(mvp_dat$Rank)))
mvp_dat$Age<-type.convert(mvp_dat$Age)
#class(as.numeric(levels(mvp_dat$Age))[mvp_dat$Age])
mvp_dat$Pts.Won<-type.convert(mvp_dat$Pts.Won)
mvp_dat$First<-type.convert(mvp_dat$First)
mvp_dat$G<-type.convert(mvp_dat$G)
mvp_dat$MP<-type.convert(mvp_dat$MP)
mvp_dat$PTS<-type.convert(mvp_dat$PTS)
mvp_dat$TRB<-type.convert(mvp_dat$TRB)
mvp_dat$AST<-type.convert(mvp_dat$AST)
mvp_dat$STL<-type.convert(mvp_dat$STL)
mvp_dat$BLK<-type.convert(mvp_dat$BLK)
mvp_dat$FG.<-type.convert(mvp_dat$FG.)
mvp_dat$FT.<-type.convert(mvp_dat$FT.)
mvp_dat$WS.48<-type.convert(mvp_dat$WS.48)
mvp_dat$Season<-type.convert(mvp_dat$Season)

# full mod
mvp.mod<-lm(First~Age+G+MP+PTS+TRB+AST+STL+BLK+FG.+WS.48,data=mvp_dat)
summary(mvp.mod)

#reduced mod
mvp.mod.red<-lm(First~G+PTS+AST+WS.48,data=mvp_dat)
summary(mvp.mod.red)

# make predictions
pred<-fitted(mvp.mod)
mvps<-data.frame(Rank=integer(),Player=character(),Age=double(),Tm=character(),
                 First=double(),Pts.Won=double(),Pts.Max=double(),Share=double(),
                 G=double(),MP=double(),PTS=double(),TRB=double(),AST=double(),
                 STL=double(),BLK=double(),FG.=double(),X3P.=double(),FT.=double(),
                 WS=double(),WS.48=double(),Season=double())
for (year in levels(as.factor(mvp_dat$Season))) {
  dat_temp<-mvp_dat[which(mvp_dat$Season==year),]
  temp_preds<-pred[which(mvp_dat$Season==year)]
  mvp<-dat_temp[which(temp_preds==max(temp_preds)),]
  mvps<-data.frame(rbind(as.matrix(mvps), as.matrix(mvp)))
}


# calculate accuracy
## hack to fix seasons like 1995,2009,2016 where rank is messed up
#levels(mvps$Rank)<-c(as.numeric(levels(mvps$Rank)))
truevals<-mvp_dat[which(mvp_dat$Rank==1),]
errs<-mvps[which(mvps$Rank!=1),]
accuracy<-1-(dim(errs)[1]/dim(mvps)[1])
accuracy

#######################################################
################ SEASON PLAYER TOTALS ################
#######################################################
# GRAB DATA FROM EACH SEASON
dat_totals<-data.frame(Rk=integer(),Player=character(),Pos=character(),Age=double(),Tm=character(),
                    G=double(),GS=double(),MP=double(),FG=double(),FGA=double(),FG.=double(),
                    X3P=double(),X3PA=double(),X3P.=double(),X2P=double(),X2PA=double(),
                    X2P.=double(),eFG.=double(),FT=double(),FTA=double(),FT.=double(),ORB=double(),
                    DRB=double(),TRB=double(),AST=double(),STL=double(),BLK=double(),TOV=double(),
                    PF=double(),PTS=double(),Season=integer())
for (year in YEAR_START:YEAR_END) {
  # read data
  str<-paste("C:/Users/Caio/repos/nba-models/season-stats-totals/",year,".csv",sep="")
  dat_temp<-read.csv(str, header = TRUE)
  
  # add season column
  dat_temp$Season<-year
  
  # clean player names
  dat_temp$Player<-as.character(dat_temp$Player)
  names<-strsplit(as.character(dat_temp$Player),"[\\\\]")
  for (idx in 1:dim(dat_temp)[1]) {
    name<-names[[idx]][1]
    name<-gsub("[*]","",name)
    dat_temp$Player[idx]<-name
  }
  
  # might consider lockout seasons
  
  ### NEED TO ADD CODE TO CLEAN PARTIAL SEASONS
  
  # add season to main dataframe
  dat_totals<-data.frame(rbind(as.matrix(dat_totals), as.matrix(dat_temp)))
}
head(dat_totals)

## add MVP winning seasons
dat_totals$MVP<-FALSE
for (idx in 1:dim(truevals)[1]) {
  dat_totals[which(as.character(dat_totals$Player)==as.character(truevals[idx,]$Player)&dat_totals$Season==truevals[idx,]$Season),]$MVP<-TRUE
}

# add first place votes
dat_totals$First<-0
for (idx in 1:dim(mvp_dat)[1]) {
  dat_totals[which(as.character(dat_totals$Player)==as.character(mvp_dat[idx,]$Player)&dat_totals$Season==mvp_dat[idx,]$Season),]$First<-mvp_dat[idx,]$First
}

#######################################################
################ SEASON PLAYER PER GAME################ 
#######################################################
# GRAB DATA FROM EACH SEASON
dat_pg<-data.frame(Rk=integer(),Player=character(),Pos=character(),Age=double(),Tm=character(),
                     G=double(),GS=double(),MP=double(),FG=double(),FGA=double(),FG.=double(),
                     X3P=double(),X3PA=double(),X3P.=double(),X2P=double(),X2PA=double(),
                     X2P.=double(),eFG.=double(),FT=double(),FTA=double(),FT.=double(),ORB=double(),
                     DRB=double(),TRB=double(),AST=double(),STL=double(),BLK=double(),TOV=double(),
                     PF=double(),PS.G=double(),Season=integer())
for (year in YEAR_START:YEAR_END) {
  # read data
  str<-paste("C:/Users/Caio/repos/nba-models/season-stats-pergame/",year,".csv",sep="")
  dat_temp<-read.csv(str, header = TRUE)
  
  # add season column
  dat_temp$Season<-year
  
  # clean player names
  dat_temp$Player<-as.character(dat_temp$Player)
  names<-strsplit(as.character(dat_temp$Player),"[\\\\]")
  for (idx in 1:dim(dat_temp)[1]) {
    name<-names[[idx]][1]
    name<-gsub("[*]","",name)
    dat_temp$Player[idx]<-name
  }
  
  # might consider lockout seasons
  
  ### NEED TO ADD CODE TO CLEAN PARTIAL SEASONS
  
  # add season to main dataframe
  dat_pg<-data.frame(rbind(as.matrix(dat_pg), as.matrix(dat_temp)))
}
head(dat_pg)

## add MVP winning seasons
dat_pg$MVP<-FALSE
for (idx in 1:dim(truevals)[1]) {
  dat_pg[which(as.character(dat_pg$Player)==as.character(truevals[idx,]$Player)&dat_pg$Season
               ==truevals[idx,]$Season),]$MVP<-TRUE
}

# add first place votes
dat_pg$First<-0
for (idx in 1:dim(mvp_dat)[1]) {
  dat_pg[which(as.character(dat_pg$Player)==as.character(mvp_dat[idx,]$Player)&
                 dat_pg$Season==mvp_dat[idx,]$Season),]$First<-mvp_dat[idx,]$First
  
}

# test truevals
dat_pg[which(dat_pg$MVP==TRUE),]

# fix data.frame classes
for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)) {
  class(dat_pg[,idx])
  dat_pg[,idx]<-as.numeric(type.convert(dat_pg[,idx]))
  class(dat_pg[,idx])
}


# clean missing observations
dat_pg_clean<-dat_pg[complete.cases(dat_pg), ]

#subset data
dat.mod<-dat_pg_clean[which(dat_pg_clean$G>60),]

# normalize data
for (year in levels(as.factor(dat.mod$Season))) {
  for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)) {
    dat.mod[,idx]<-dat.mod[,idx]/max(dat.mod[,idx])
  }
}

# baseline model
mod<-lm(First~Pos+Age+G+GS+MP+FG+FGA+FG.+X3P+X3PA+X3P+X2P+X2PA+X2P.+eFG.+FT+FTA+FT.+ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PS.G,data=dat.mod)
summary(mod)

# reduced model
mod.red<-lm(First~Pos+G+MP+eFG.+ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PS.G,data=dat.mod)
summary(mod.red)

# make predictions
pred<-fitted(mod.red)
mvps<-data.frame(Rk=integer(),Player=character(),Pos=character(),Age=double(),Tm=character(),
                  G=double(),GS=double(),MP=double(),FG=double(),FGA=double(),FG.=double(),
                  X3P=double(),X3PA=double(),X3P.=double(),X2P=double(),X2PA=double(),
                  X2P.=double(),eFG.=double(),FT=double(),FTA=double(),FT.=double(),ORB=double(),
                  DRB=double(),TRB=double(),AST=double(),STL=double(),BLK=double(),TOV=double(),
                  PF=double(),PS.G=double(),Season=integer(),MVP=logical(),First=double())
for (year in levels(as.factor(dat.mod$Season))) {
  dat_temp<-dat.mod[which(dat.mod$Season==year),]
  temp_preds<-pred[which(dat.mod$Season==year)]
  mvp<-dat_temp[which(temp_preds==max(temp_preds)),]
  mvps<-data.frame(rbind(as.matrix(mvps), as.matrix(mvp)))
}
mvps

# calculate accuracy
truevals<-dat_pg_clean[which(dat_pg_clean$MVP==TRUE),]
errs<-mvps[which(mvps$MVP==FALSE),]
accuracy<-1-(dim(errs)[1]/dim(mvps)[1])
accuracy
