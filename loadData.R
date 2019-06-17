# constants
YEAR_START<-1980
YEAR_END<-2018
NORM<-TRUE

#######################################################
############### PROCESS BBALLREF DATA #################
#######################################################

#source("loadData.R")

# LOAD STANDINGS DATA
dat_std<-loadStandings(YEAR_START,YEAR_END)

# LOAD MVP DATA
dat_mvp<-loadMVP(YEAR_START,YEAR_END,dat_std)

# LOAD TOTAL PLAYER STATS
dat_totals<-loadTotals(YEAR_START,YEAR_END,dat_mvp,normalize=NORM)

# LOAD PERGAME PLAYER STATS
dat_pergame<-loadPerGame(YEAR_START,YEAR_END,dat_mvp,normalize=NORM)

# LOAD ADVANCED PLAYER STATS
dat_adv<-loadAdvanced(YEAR_START,YEAR_END,dat_mvp,normalize=NORM)

# WRITE TO CSVs
write.csv(dat_std,file = "full-data/standings.csv",row.names=FALSE)
write.csv(dat_mvp,file = "full-data/mvp.csv",row.names=FALSE)
write.csv(dat_totals,file = "full-data/totals.csv",row.names=FALSE)
write.csv(dat_pergame,file = "full-data/pergame.csv",row.names=FALSE)
write.csv(dat_adv,file = "full-data/advanced.csv",row.names=FALSE)


#######################################################
#################### MVP DATA ######################### 
#######################################################
loadMVP <- function(yr_start,yr_end,dat_std) {
  mvp_dat<-data.frame(Rank=integer(),Player=character(),Age=double(),Tm=character(),
                      First=double(),Pts.Won=double(),Pts.Max=double(),Share=double(),
                      G=double(),MP=double(),PTS=double(),TRB=double(),AST=double(),
                      STL=double(),BLK=double(),FG.=double(),X3P.=double(),FT.=double(),
                      WS=double(),WS.48=double(),Season=double())
  for (year in yr_start:yr_end) {
    str<-paste("award-stats/",year,".csv",sep="")
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
  
  # fix data.frame classes
  mvp_dat$Rank<-as.numeric(gsub("[^0-9]", "", as.character(mvp_dat$Rank)))
  for (idx in c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)) {
    mvp_dat[,idx]<-as.numeric(type.convert(mvp_dat[,idx]))
  }
  
  ## needs a better fix eventually
  mvp_dat<-mvp_dat[which(mvp_dat$Tm!="TOT"),]
  mvp_dat<-mvp_dat[complete.cases(mvp_dat), ]
  
  # fix team strings
  ## standings full team name, player stats abbreviated
  abbrev<-c("ATL","BOS","BRK","BUF","CHA","CHH","CHI","CHO","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL",
            "MEM","MIA","MIL","MIN","NJN","NOH","NOJ","NOK","NOP","NYK","NYN","OKC","ORL","PHI","PHO","POR","SAC","SAS",
            "SDC","SEA","TOR","UTA","VAN","WAS","WSB","KCK")
  fullnames<-c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Buffalo Braves","Charlotte Bobcats",
               "Charlotte Hornets","Chicago Bulls","Charlotte Hornets","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
               "Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers",
               "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
               "New Jersey Nets","New Orleans Hornets","New Orleans Jazz","New Orleans/Oklahoma City Hornets","New Orleans Pelicans",
               "New York Knicks","New York Nets","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
               "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","San Diego Clippers","Seattle SuperSonics",
               "Toronto Raptors","Utah Jazz","Vancouver Grizzlies","Washington Wizards","Washington Bullets","Kansas City Kings")
  names(fullnames)<-abbrev
  
  # add team wins
  mvp_dat$Team.Wins<-0
  for (team_abbrev in levels(as.factor(mvp_dat$Tm))) {
    for (season in levels(as.factor(mvp_dat$Season))) {
      team_name<-fullnames[team_abbrev]
      roster<-mvp_dat[which(as.character(mvp_dat$Tm)==team_abbrev & as.character(mvp_dat$Season)==season),]
      if (dim(roster)[1]!=0) {
        roster$Team.Wins<-dat_std[which(as.character(dat_std$Team)==team_name & as.character(dat_std$Season)==season),]$Wins
      }
      mvp_dat[which(as.character(mvp_dat$Tm)==team_abbrev & as.character(mvp_dat$Season)==season),]<-roster
    }
  }
  mvp_dat$Team.Wins<-type.convert(mvp_dat$Team.Wins)
  
  # add whether won MVP
  mvp_dat$MVP<-mvp_dat$Rank==1
  
  return(mvp_dat)
}


#######################################################
################   SEASON STANDINGS    ################ 
#######################################################
loadStandings <- function(yr_start,yr_end) {
  # GRAB DATA FROM EACH SEASON
  dat_std<-data.frame(Rk=integer(),Team=character(),Overall=character(),Home=character(),Road=character(),
                      X3=character(),X10=character(),Oct=character(),
                      Nov=character(),Dec=character(),Jan=character(),Feb=character(),Mar=character(),
                      Apr=character(),May=character(),Season=factor())
  for (year in yr_start:yr_end) {
    # read data
    str<-paste("season-standings/",year,".csv",sep="")
    dat_temp<-read.csv(str, header = TRUE)
    
    # add season column
    dat_temp$Season<-year
    
    # fix missing Oct data
    if(!("Oct" %in% colnames(dat_temp)))
    {
      dat_temp$Oct<-NA
    }
    if(!("Nov" %in% colnames(dat_temp)))
    {
      dat_temp$Nov<-NA
    }
    if(!("Dec" %in% colnames(dat_temp)))
    {
      dat_temp$Dec<-NA
    }
    if(!("Jan" %in% colnames(dat_temp)))
    {
      dat_temp$Jan<-NA
    }
    if(!("Feb" %in% colnames(dat_temp)))
    {
      dat_temp$Feb<-NA
    }
    if(!("Mar" %in% colnames(dat_temp)))
    {
      dat_temp$Mar<-NA
    }
    if(!("Apr" %in% colnames(dat_temp)))
    {
      dat_temp$Apr<-NA
    }
    if(!("May" %in% colnames(dat_temp)))
    {
      dat_temp$May<-NA
    }
    
    # fix columns
    ## removed conference records and pre/post allstar break for simplicity, worth trying again
    dat_temp<-data.frame(Rk=dat_temp$Rk,Team=dat_temp$Team,Overall=dat_temp$Overall,Home=dat_temp$Home,
                        Road=dat_temp$Road,X3=dat_temp$X3,X10=dat_temp$X10,
                        Oct=dat_temp$Oct,Nov=dat_temp$Nov,Dec=dat_temp$Dec,Jan=dat_temp$Jan,Feb=dat_temp$Feb,
                        Mar=dat_temp$Mar,Apr=dat_temp$Apr,May=dat_temp$May,Season=dat_temp$Season)
    
    # add season to main dataframe
    dat_std<-data.frame(rbind(as.matrix(dat_std), as.matrix(dat_temp)))
  }
  head(dat_std)
  
  # add wins and losses
  wl<-strsplit(as.character(dat_std$Overall), "-")
  dat_std$Wins<-0
  dat_std$Losses<-0
  for (idx in 1:dim(dat_std)[1]) {
    dat_std[idx,]$Wins<-wl[[idx]][1]
    dat_std[idx,]$Losses<-wl[[idx]][2]
  }
  
  return(dat_std)
}

#######################################################
################ SEASON PLAYER TOTALS ################
#######################################################
loadTotals <- function(yr_start,yr_end,dat_mvp,normalize) {
  # GRAB DATA FROM EACH SEASON
  dat_totals<-data.frame(Rk=integer(),Player=character(),Pos=character(),Age=double(),Tm=character(),
                      G=double(),GS=double(),MP=double(),FG=double(),FGA=double(),FG.=double(),
                      X3P=double(),X3PA=double(),X3P.=double(),X2P=double(),X2PA=double(),
                      X2P.=double(),eFG.=double(),FT=double(),FTA=double(),FT.=double(),ORB=double(),
                      DRB=double(),TRB=double(),AST=double(),STL=double(),BLK=double(),TOV=double(),
                      PF=double(),PTS=double(),Season=integer())
  for (year in yr_start:yr_end) {
    # read data
    str<-paste("season-stats-totals/",year,".csv",sep="")
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
    
    # add season to main dataframe
    dat_totals<-data.frame(rbind(as.matrix(dat_totals), as.matrix(dat_temp)))
  }
  head(dat_totals)
  
  ## add MVP winning seasons
  truevals<-dat_mvp[which(dat_mvp$Rank==1),]
  dat_totals$MVP<-FALSE
  for (idx in 1:dim(truevals)[1]) {
    dat_totals[which(as.character(dat_totals$Player)==as.character(truevals[idx,]$Player)&dat_totals$Season==truevals[idx,]$Season),]$MVP<-TRUE
  }
  
  # add first place votes
  dat_totals$First<-0
  for (idx in 1:dim(dat_mvp)[1]) {
    dat_totals[which(as.character(dat_totals$Player)==as.character(dat_mvp[idx,]$Player)&dat_totals$Season==dat_mvp[idx,]$Season),]$First<-dat_mvp[idx,]$First
  }
  
  # add point share
  dat_totals$Share<-0
  for (idx in 1:dim(dat_mvp)[1]) {
    dat_totals[which(as.character(dat_totals$Player)==as.character(dat_mvp[idx,]$Player)&dat_totals$Season==dat_mvp[idx,]$Season),]$Share<-dat_mvp[idx,]$Share
  }
  
  # fix data.frame classes
  for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,33)) {
    class(dat_totals[,idx])
    dat_totals[,idx]<-as.numeric(type.convert(dat_totals[,idx]))
    class(dat_totals[,idx])
  }
  
  # remove TOT seasons & missing observations
  ## needs a better fix eventually
  dat_totals<-dat_totals[which(dat_totals$Tm!="TOT"),]
  dat_totals<-dat_totals[complete.cases(dat_totals), ]
  # remove observations without minimum games
  dat_totals <- dat_totals[which(dat_totals$G > 41),]
  
  # normalize data
  if (normalize==TRUE){
    for (year in levels(as.factor(dat_totals$Season))) {
      for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)) {
        dat_year <- dat_totals[which(dat_totals$Season==year),]
        dat_totals[which(dat_totals$Season==year),][,idx]<-dat_year[,idx]/max(dat_year[,idx])
        dat_totals[,idx]<-round(dat_totals[,idx], digits = 3)
      }
    }
  }
  
  # fix team strings
  ## standings full team name, player stats abbreviated
  abbrev<-c("ATL","BOS","BRK","BUF","CHA","CHH","CHI","CHO","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL",
            "MEM","MIA","MIL","MIN","NJN","NOH","NOJ","NOK","NOP","NYK","NYN","OKC","ORL","PHI","PHO","POR","SAC","SAS",
            "SDC","SEA","TOR","UTA","VAN","WAS","WSB","KCK")
  fullnames<-c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Buffalo Braves","Charlotte Bobcats",
               "Charlotte Hornets","Chicago Bulls","Charlotte Hornets","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
               "Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers",
               "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
               "New Jersey Nets","New Orleans Hornets","New Orleans Jazz","New Orleans/Oklahoma City Hornets","New Orleans Pelicans",
               "New York Knicks","New York Nets","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
               "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","San Diego Clippers","Seattle SuperSonics",
               "Toronto Raptors","Utah Jazz","Vancouver Grizzlies","Washington Wizards","Washington Bullets","Kansas City Kings")
  names(fullnames)<-abbrev
  
  # add team wins
  dat_totals$Team.Wins<-0
  for (team_abbrev in levels(as.factor(dat_totals$Tm))) {
    for (season in levels(as.factor(dat_totals$Season))) {
      team_name<-fullnames[team_abbrev]
      roster<-dat_totals[which(as.character(dat_totals$Tm)==team_abbrev & as.character(dat_totals$Season)==season),]
      if (dim(roster)[1]!=0) {
        roster$Team.Wins<-dat_std[which(as.character(dat_std$Team)==team_name & as.character(dat_std$Season)==season),]$Wins
      }
      dat_totals[which(as.character(dat_totals$Tm)==team_abbrev & as.character(dat_totals$Season)==season),]<-roster
    }
  }
  dat_totals$Team.Wins<-type.convert(dat_totals$Team.Wins)
  
  dat_totals <- dat_totals[,c(1,2,seq(31,34),seq(3,30),35),]
  
  return(dat_totals)
}

#######################################################
############## SHOULD MERGE WITH TOTALS ###############
############## SEASON PLAYER ADVANCED  ################ 
#######################################################
loadAdvanced <- function(yr_start,yr_end,dat_mvp,normalize) {
  dat_adv<-data.frame(Rk=integer(),Player=character(),Pos=character(),Age=double(),Tm=character(), G=integer(),
                        MP=double(),PER=double(),TS.=double(),X3PAr=double(),FTr=double(),ORB.=double(),
                        DRB.=double(),TRB.=double(),AST.=double(),STL.=double(),BLK.=double(),TOV.=double(),USG.=double(),
                        X=double(),OWS=double(),DWS=double(),WS=double(),WS.48=double(),X.1=double(),OBPM=double(),DBPM=double(),
                        BPM=double(),VORP=double(),Season=integer())
  for (year in yr_start:yr_end) {
    # read data
    str<-paste("season-stats-advanced/",year,".csv",sep="")
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
    
    # add season to main dataframe
    dat_adv<-data.frame(rbind(as.matrix(dat_adv), as.matrix(dat_temp)))
  }
  head(dat_adv)
  
  # drop X and X.1 columns
  dat_adv <- subset(dat_adv, select = -c(X, X.1))
  
  ## add MVP winning seasons
  truevals<-dat_mvp[which(dat_mvp$Rank==1),]
  dat_adv$MVP<-FALSE
  for (idx in 1:dim(truevals)[1]) {
    dat_adv[which(as.character(dat_adv$Player)==as.character(truevals[idx,]$Player)&dat_adv$Season==truevals[idx,]$Season),]$MVP<-TRUE
  }
  
  # add first place votes
  dat_adv$First<-0
  for (idx in 1:dim(dat_mvp)[1]) {
    dat_adv[which(as.character(dat_adv$Player)==as.character(dat_mvp[idx,]$Player)&dat_adv$Season==dat_mvp[idx,]$Season),]$First<-dat_mvp[idx,]$First
  }
  
  # add point share
  dat_adv$Share<-0
  for (idx in 1:dim(dat_mvp)[1]) {
    dat_adv[which(as.character(dat_adv$Player)==as.character(dat_mvp[idx,]$Player)&dat_adv$Season==dat_mvp[idx,]$Season),]$Share<-dat_mvp[idx,]$Share
  }
  
  # fix data.frame classes
  for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30)) {
    class(dat_adv[,idx])
    dat_adv[,idx]<-as.numeric(type.convert(dat_adv[,idx]))
    class(dat_adv[,idx])
  }
  
  # remove TOT seasons & missing observations
  ## needs a better fix eventually
  dat_adv<-dat_adv[which(dat_adv$Tm!="TOT"),]
  #dat_adv<-dat_adv[complete.cases(dat_adv), ]
  
  # remove observations without minimum games
  dat_adv <- dat_adv[which(dat_adv$G > 41),]
  
  # normalize data
  if (normalize==TRUE){
    for (year in levels(as.factor(dat_adv$Season))) {
      for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)) {
        dat_year <- dat_adv[which(dat_adv$Season==year),]
        dat_adv[which(dat_adv$Season==year),][,idx]<-dat_year[,idx]/max(dat_year[,idx],na.rm=T)
        dat_adv[,idx]<-round(dat_adv[,idx], digits = 3)
      }
    }
  }
  
  # fix team strings
  ## standings full team name, player stats abbreviated
  abbrev<-c("ATL","BOS","BRK","BUF","CHA","CHH","CHI","CHO","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL",
            "MEM","MIA","MIL","MIN","NJN","NOH","NOJ","NOK","NOP","NYK","NYN","OKC","ORL","PHI","PHO","POR","SAC","SAS",
            "SDC","SEA","TOR","UTA","VAN","WAS","WSB","KCK")
  fullnames<-c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Buffalo Braves","Charlotte Bobcats",
               "Charlotte Hornets","Chicago Bulls","Charlotte Hornets","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
               "Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers",
               "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
               "New Jersey Nets","New Orleans Hornets","New Orleans Jazz","New Orleans/Oklahoma City Hornets","New Orleans Pelicans",
               "New York Knicks","New York Nets","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
               "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","San Diego Clippers","Seattle SuperSonics",
               "Toronto Raptors","Utah Jazz","Vancouver Grizzlies","Washington Wizards","Washington Bullets","Kansas City Kings")
  names(fullnames)<-abbrev
  
  # add team wins
  dat_adv$Team.Wins<-0
  for (team_abbrev in levels(as.factor(dat_adv$Tm))) {
    for (season in levels(as.factor(dat_adv$Season))) {
      team_name<-fullnames[team_abbrev]
      roster<-dat_adv[which(as.character(dat_adv$Tm)==team_abbrev & as.character(dat_adv$Season)==season),]
      if (dim(roster)[1]!=0) {
        roster$Team.Wins<-dat_std[which(as.character(dat_std$Team)==team_name & as.character(dat_std$Season)==season),]$Wins
      }
      dat_adv[which(as.character(dat_adv$Tm)==team_abbrev & as.character(dat_adv$Season)==season),]<-roster
    }
  }
  dat_adv$Team.Wins<-type.convert(dat_adv$Team.Wins)
  
  dat_adv <- dat_adv[,c(1,2,seq(28,31),seq(3,27),32)]
  
  return(dat_adv)
}

#######################################################
############## SHOULD MERGE WITH TOTALS ###############
################ SEASON PLAYER PER GAME################ 
#######################################################
loadPerGame <- function(yr_start,yr_end,dat_mvp,normalize) {
  # GRAB DATA FROM EACH SEASON
  dat_pg<-data.frame(Rk=integer(),Player=character(),Pos=character(),Age=double(),Tm=character(),
                     G=double(),GS=double(),MP=double(),FG=double(),FGA=double(),FG.=double(),
                     X3P=double(),X3PA=double(),X3P.=double(),X2P=double(),X2PA=double(),
                     X2P.=double(),eFG.=double(),FT=double(),FTA=double(),FT.=double(),ORB=double(),
                     DRB=double(),TRB=double(),AST=double(),STL=double(),BLK=double(),TOV=double(),
                     PF=double(),PTS=double(),Season=integer())
  for (year in yr_start:yr_end) {
    # read data
    str<-paste("season-stats-pergame/",year,".csv",sep="")
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
    
    # add season to main dataframe
    dat_pg<-data.frame(rbind(as.matrix(dat_pg), as.matrix(dat_temp)))
  }
  head(dat_pg)
  
  ## add MVP winning seasons
  truevals<-dat_mvp[which(dat_mvp$Rank==1),]
  dat_pg$MVP<-FALSE
  for (idx in 1:dim(truevals)[1]) {
    dat_pg[which(as.character(dat_pg$Player)==as.character(truevals[idx,]$Player)&dat_pg$Season==truevals[idx,]$Season),]$MVP<-TRUE
  }
  
  # add first place votes
  dat_pg$First<-0
  for (idx in 1:dim(dat_mvp)[1]) {
    dat_pg[which(as.character(dat_pg$Player)==as.character(dat_mvp[idx,]$Player)&dat_pg$Season==dat_mvp[idx,]$Season),]$First<-dat_mvp[idx,]$First
  }
  
  # add point share
  dat_pg$Share<-0
  for (idx in 1:dim(dat_mvp)[1]) {
    dat_pg[which(as.character(dat_pg$Player)==as.character(dat_mvp[idx,]$Player)&dat_pg$Season==dat_mvp[idx,]$Season),]$Share<-dat_mvp[idx,]$Share
  }
  
  # fix data.frame classes
  for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,33)) {
    class(dat_pg[,idx])
    dat_pg[,idx]<-as.numeric(type.convert(dat_pg[,idx]))
    class(dat_pg[,idx])
  }
  
  # remove TOT seasons & missing observations
  ## needs a better fix eventually
  dat_pg<-dat_pg[which(dat_pg$Tm!="TOT"),]
  dat_pg<-dat_pg[complete.cases(dat_pg), ]
  # remove observations without minimum games
  dat_pg <- dat_pg[which(dat_pg$G > 41),]
  
  # normalize data
  if (normalize==TRUE){
    for (year in levels(as.factor(dat_pg$Season))) {
      for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)) {
        dat_year <- dat_pg[which(dat_pg$Season==year),]
        dat_pg[which(dat_pg$Season==year),][,idx]<-dat_year[,idx]/max(dat_year[,idx])
        dat_pg[,idx]<-round(dat_pg[,idx], digits = 3)
      }
    }
  }
  
  # fix team strings
  ## standings full team name, player stats abbreviated
  abbrev<-c("ATL","BOS","BRK","BUF","CHA","CHH","CHI","CHO","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL",
            "MEM","MIA","MIL","MIN","NJN","NOH","NOJ","NOK","NOP","NYK","NYN","OKC","ORL","PHI","PHO","POR","SAC","SAS",
            "SDC","SEA","TOR","UTA","VAN","WAS","WSB","KCK")
  fullnames<-c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Buffalo Braves","Charlotte Bobcats",
               "Charlotte Hornets","Chicago Bulls","Charlotte Hornets","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
               "Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers",
               "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
               "New Jersey Nets","New Orleans Hornets","New Orleans Jazz","New Orleans/Oklahoma City Hornets","New Orleans Pelicans",
               "New York Knicks","New York Nets","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
               "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","San Diego Clippers","Seattle SuperSonics",
               "Toronto Raptors","Utah Jazz","Vancouver Grizzlies","Washington Wizards","Washington Bullets","Kansas City Kings")
  names(fullnames)<-abbrev
  
  # add team wins
  dat_pg$Team.Wins<-0
  for (team_abbrev in levels(as.factor(dat_pg$Tm))) {
    for (season in levels(as.factor(dat_pg$Season))) {
      team_name<-fullnames[team_abbrev]
      roster<-dat_pg[which(as.character(dat_pg$Tm)==team_abbrev & as.character(dat_pg$Season)==season),]
      if (dim(roster)[1]!=0) {
        roster$Team.Wins<-dat_std[which(as.character(dat_std$Team)==team_name & as.character(dat_std$Season)==season),]$Wins
      }
      dat_pg[which(as.character(dat_pg$Tm)==team_abbrev & as.character(dat_pg$Season)==season),]<-roster
    }
  }
  dat_pg$Team.Wins<-type.convert(dat_pg$Team.Wins)
  
  
  return(dat_pg)
}

#######################################################
#################### 2019 DATA ######################## 
#######################################################
loadCurrent <- function(normalize) {
  # read data
  str<-paste("season-stats-totals/",2019,".csv",sep="")
  dat_2019<-read.csv(str, header = TRUE)
  
  # add season column
  dat_2019$Season<-2019
  
  # clean player names
  dat_2019$Player<-as.character(dat_2019$Player)
  names<-strsplit(as.character(dat_2019$Player),"[\\\\]")
  for (idx in 1:dim(dat_2019)[1]) {
    name<-names[[idx]][1]
    name<-gsub("[*]","",name)
    dat_2019$Player[idx]<-name
  }
  
  ## add MVP winning seasons
  dat_2019$MVP<-FALSE
  
  # add first place votes
  dat_2019$First<-0
  
  # fix data.frame classes
  for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,33)) {
    class(dat_2019[,idx])
    dat_2019[,idx]<-as.numeric(type.convert(dat_2019[,idx]))
    class(dat_2019[,idx])
  }
  
  # remove TOT seasons & missing observations
  ## needs a better fix eventually
  dat_2019<-dat_2019[which(dat_2019$Tm!="TOT"),]
  dat_2019<-dat_2019[complete.cases(dat_2019), ]
  
  # normalize data
  if (normalize==TRUE){
    for (year in levels(as.factor(dat_2019$Season))) {
      for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)) {
        dat_2019[,idx]<-dat_2019[,idx]/max(dat_2019[,idx])
        dat_2019[,idx]<-round(dat_2019[,idx], digits = 3)
      }
    }
  }
  
  # fix team strings
  ## standings full team name, player stats abbreviated
  abbrev<-c("ATL","BOS","BRK","BUF","CHA","CHH","CHI","CHO","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL",
            "MEM","MIA","MIL","MIN","NJN","NOH","NOJ","NOK","NOP","NYK","NYN","OKC","ORL","PHI","PHO","POR","SAC","SAS",
            "SDC","SEA","TOR","UTA","VAN","WAS","WSB","KCK")
  fullnames<-c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Buffalo Braves","Charlotte Bobcats",
               "Charlotte Hornets","Chicago Bulls","Charlotte Hornets","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
               "Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers",
               "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
               "New Jersey Nets","New Orleans Hornets","New Orleans Jazz","New Orleans/Oklahoma City Hornets","New Orleans Pelicans",
               "New York Knicks","New York Nets","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
               "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","San Diego Clippers","Seattle SuperSonics",
               "Toronto Raptors","Utah Jazz","Vancouver Grizzlies","Washington Wizards","Washington Bullets","Kansas City Kings")
  names(fullnames)<-abbrev
  
  # add team wins
  dat_2019$Team.Wins<-0
  for (team_abbrev in levels(as.factor(dat_2019$Tm))) {
    for (season in levels(as.factor(dat_2019$Season))) {
      team_name<-fullnames[team_abbrev]
      roster<-dat_2019[which(as.character(dat_2019$Tm)==team_abbrev & as.character(dat_2019$Season)==season),]
      if (dim(roster)[1]!=0) {
        roster$Team.Wins<-dat_std[which(as.character(dat_std$Team)==team_name & as.character(dat_std$Season)==season),]$Wins
      }
      dat_2019[which(as.character(dat_2019$Tm)==team_abbrev & as.character(dat_2019$Season)==season),]<-roster
    }
  }
  dat_2019$Team.Wins<-type.convert(dat_2019$Team.Wins)
  
  return(dat_2019)
}

#######################################################
#################### 2019 ADV  ######################## 
#######################################################
loadCurrentAdv <- function(normalize) {
  # read data
  str<-paste("season-stats-advanced/",2019,".csv",sep="")
  dat_2019<-read.csv(str, header = TRUE)
  
  # add season column
  dat_2019$Season<-2019
  
  # clean player names
  dat_2019$Player<-as.character(dat_2019$Player)
  names<-strsplit(as.character(dat_2019$Player),"[\\\\]")
  for (idx in 1:dim(dat_2019)[1]) {
    name<-names[[idx]][1]
    name<-gsub("[*]","",name)
    dat_2019$Player[idx]<-name
  }
  
  ## add MVP winning seasons
  dat_2019$MVP<-FALSE
  
  # add first place votes
  dat_2019$First<-0
  
  # fix data.frame classes
  for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,30)) {
    class(dat_2019[,idx])
    dat_2019[,idx]<-as.numeric(type.convert(dat_2019[,idx]))
    class(dat_2019[,idx])
  }
  
  # remove TOT seasons & missing observations
  ## needs a better fix eventually
  dat_2019<-dat_2019[which(dat_2019$Tm!="TOT"),]
  #dat_2019<-dat_2019[complete.cases(dat_2019), ]
  dat_2019<-dat_2019[which(dat_2019$G>41),]
  
  # normalize data
  if (normalize==TRUE){
    for (year in levels(as.factor(dat_2019$Season))) {
      for (idx in c(4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)) {
        dat_2019[,idx]<-dat_2019[,idx]/max(dat_2019[,idx])
        dat_2019[,idx]<-round(dat_2019[,idx], digits = 3)
      }
    }
  }
  
  # fix team strings
  ## standings full team name, player stats abbreviated
  abbrev<-c("ATL","BOS","BRK","BUF","CHA","CHH","CHI","CHO","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL",
            "MEM","MIA","MIL","MIN","NJN","NOH","NOJ","NOK","NOP","NYK","NYN","OKC","ORL","PHI","PHO","POR","SAC","SAS",
            "SDC","SEA","TOR","UTA","VAN","WAS","WSB","KCK")
  fullnames<-c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Buffalo Braves","Charlotte Bobcats",
               "Charlotte Hornets","Chicago Bulls","Charlotte Hornets","Cleveland Cavaliers","Dallas Mavericks","Denver Nuggets",
               "Detroit Pistons","Golden State Warriors","Houston Rockets","Indiana Pacers","Los Angeles Clippers",
               "Los Angeles Lakers","Memphis Grizzlies","Miami Heat","Milwaukee Bucks","Minnesota Timberwolves",
               "New Jersey Nets","New Orleans Hornets","New Orleans Jazz","New Orleans/Oklahoma City Hornets","New Orleans Pelicans",
               "New York Knicks","New York Nets","Oklahoma City Thunder","Orlando Magic","Philadelphia 76ers","Phoenix Suns",
               "Portland Trail Blazers","Sacramento Kings","San Antonio Spurs","San Diego Clippers","Seattle SuperSonics",
               "Toronto Raptors","Utah Jazz","Vancouver Grizzlies","Washington Wizards","Washington Bullets","Kansas City Kings")
  names(fullnames)<-abbrev
  
  # add team wins
  dat_2019$Team.Wins<-0
  for (team_abbrev in levels(as.factor(dat_2019$Tm))) {
    for (season in levels(as.factor(dat_2019$Season))) {
      team_name<-fullnames[team_abbrev]
      roster<-dat_2019[which(as.character(dat_2019$Tm)==team_abbrev & as.character(dat_2019$Season)==season),]
      if (dim(roster)[1]!=0) {
        roster$Team.Wins<-dat_std[which(as.character(dat_std$Team)==team_name & as.character(dat_std$Season)==season),]$Wins
      }
      dat_2019[which(as.character(dat_2019$Tm)==team_abbrev & as.character(dat_2019$Season)==season),]<-roster
    }
  }
  dat_2019$Team.Wins<-type.convert(dat_2019$Team.Wins)
  
  dat_2019 <- dat_2019[,c(1,2,seq(30,32),seq(3,29),33)]
  
  return(dat_2019)
}
