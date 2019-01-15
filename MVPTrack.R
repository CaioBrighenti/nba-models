# GRAB DATA FROM EACH SEASON
main_dat<-read.csv("C:/Users/Caio/repos/nba-models/season-stats/1976.csv", header = TRUE)
main_dat$Season<-1976
for (year in 1977:2017) {
  str<-paste("C:/Users/Caio/repos/nba-models/season-stats/",year,".csv",sep="")
  temp_dat<-read.csv(str, header = TRUE)
  temp_dat$Season<-year
  names(temp_dat)<-names(main_dat)
  ### NEED TO ADD CODE TO CLEAN PARTIAL SEASONS
  main_dat<-data.frame(rbind(as.matrix(main_dat), as.matrix(temp_dat)))
}
head(main_dat)


# GRAB MVP DATA FROM EACH SEASON
mvp_dat<-data.frame(Rank=integer(),Player=character(),Age=double(),Tm=character(),
                    First=double(),Pts.Won=double(),Pts.Max=double(),Share=double(),
                    G=double(),MP=double(),PTS=double(),TRB=double(),AST=double(),
                    STL=double(),BLK=double(),FG.=double(),X3P.=double(),FT.=double(),
                    WS=double(),WS.48=double(),Season=double())
for (year in 2000:2017) {
  str<-paste("C:/Users/Caio/repos/nba-models/award-stats/",year,".csv",sep="")
  temp_dat<-read.csv(str, header = TRUE,stringsAsFactors=FALSE)
   temp_dat$Season<-year
  # normalize stats & set class
  # Model is more accurate without this
  ## G
  temp_dat$G<-temp_dat$G/max(temp_dat$G)
  ## MP
  temp_dat$MP<-temp_dat$MP/max(temp_dat$MP)
  ## PTS
  temp_dat$PTS<-temp_dat$PTS/max(temp_dat$PTS)
  ## TRB
  temp_dat$TRB<-temp_dat$TRB/max(temp_dat$TRB)
  ## AST
  temp_dat$AST<-temp_dat$AST/max(temp_dat$AST)
  ## STL
  temp_dat$STL<-temp_dat$STL/max(temp_dat$STL)
  ## BLK
  temp_dat$BLK<-temp_dat$BLK/max(temp_dat$BLK)
  ## FG.
  temp_dat$FG.<-temp_dat$FG./max(temp_dat$FG.)
  ## FT.
  temp_dat$FT.<-temp_dat$FT./max(temp_dat$FT.)
  ## WS.48
  temp_dat$WS.48<-temp_dat$WS.48/max(temp_dat$WS.48)
  #names(temp_dat)<-names(mvp_dat)
  mvp_dat<-data.frame(rbind.data.frame(as.matrix(mvp_dat), as.matrix(temp_dat)))
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
  temp_dat<-mvp_dat[which(mvp_dat$Season==year),]
  temp_preds<-pred[which(mvp_dat$Season==year)]
  mvp<-temp_dat[which(temp_preds==max(temp_preds)),]
  mvps<-data.frame(rbind(as.matrix(mvps), as.matrix(mvp)))
}


# calculate accuracy
## hack to fix seasons like 1995,2009,2016 where rank is messed up
#levels(mvps$Rank)<-c(as.numeric(levels(mvps$Rank)))
errs<-mvps[which(mvps$Rank!=1),]
accuracy<-1-(dim(errs)[1]/dim(mvps)[1])
accuracy
