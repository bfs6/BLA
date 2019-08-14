####Read in Libraries####
library(data.table)
library(dplyr)
library(Hmisc)
library(glmnet)
library(tidyr)


####Helper Functions####
remove_zero_var_cols <- function(df){
  colsToRemove <- c()
  for(i in 1:ncol(df)){
    if(length(unique(df[,i])) == 1){
      colsToRemove <- c(i, colsToRemove)
    }
  }
  if(length(colsToRemove) >= 1){
    df <- df[,-colsToRemove]
  }
  return(df)
}


####Read in Data####
exampleRanks <- fread("ExampleRankings.csv", sep = ",", stringsAsFactors = F)
ACCGames <- fread("ACCGames1819.csv", sep = ",", stringsAsFactors = F)


####Cleaning Data####
ACCGames$GameDate <- as.Date(ACCGames$GameDate, format = "%m/%d/%Y")
teamIDRefs <- data.frame(team_name = sort(unique(c(ACCGames$AwayTeam, ACCGames$HomeTeam))),
                         team_id = c(1:length(sort(unique(c(ACCGames$AwayTeam, ACCGames$HomeTeam))))))
ACCGames <- merge(ACCGames, teamIDRefs, by.x = "AwayTeam", by.y = "team_name", all.x = T)
names(ACCGames)[which(names(ACCGames) == "team_id")] = "AwayTeamID"
ACCGames <- merge(ACCGames, teamIDRefs, by.x = "HomeTeam", by.y = "team_name", all.x = T)
names(ACCGames)[which(names(ACCGames) == "team_id")] = "HomeTeamID"
gameIDRefs <- unique(ACCGames[,c("HomeTeam", "AwayTeam", "GameDate", "NeutralSite", "AwayTeamID", "HomeTeamID")])
gameIDRefs <- arrange(gameIDRefs, GameDate, HomeTeamID, AwayTeamID)
gameIDRefs$gameID <- c(1:nrow(gameIDRefs))
ACCGames <- merge(ACCGames, gameIDRefs, by = c("HomeTeam", "AwayTeam", "GameDate", "NeutralSite", "AwayTeamID", "HomeTeamID"), all.x = T)


####Feature Engineering####
##Calculate Rebound Pct
ACCGames$HomeRebOpps <- (ACCGames$AwayFTA + ACCGames$HomeFTA) * 0.44 + (ACCGames$AwayFGA + ACCGames$HomeFGA) + (ACCGames$AwayBLK + ACCGames$HomeBLK)
ACCGames$AwayRebOpps <- (ACCGames$AwayFTA + ACCGames$HomeFTA) * 0.44 + (ACCGames$AwayFGA + ACCGames$HomeFGA) + (ACCGames$AwayBLK + ACCGames$HomeBLK)
ACCGames$HomeOffRebOpps <- (ACCGames$HomeFTA) * 0.44 + ACCGames$HomeFGA + ACCGames$AwayBLK
ACCGames$AwayOffRebOpps <- (ACCGames$AwayFTA) * 0.44 + ACCGames$AwayFGA + ACCGames$HomeBLK
ACCGames$AwayDefRebOpps <- (ACCGames$HomeFTA) * 0.44 + ACCGames$HomeFGA + ACCGames$AwayBLK
ACCGames$HomeDefRebOpps <- (ACCGames$AwayFTA) * 0.44 + ACCGames$AwayFGA + ACCGames$HomeBLK
ACCGames$HomeTRBPct <- ACCGames$HomeRebounds/ACCGames$HomeRebOpps
ACCGames$AwayTRBPct <- ACCGames$AwayRebounds/ACCGames$AwayRebOpps
ACCGames$HomeORBPct <- ACCGames$HomeORB/ACCGames$HomeOffRebOpps
ACCGames$AwayORBPct <- ACCGames$AwayORB/ACCGames$AwayOffRebOpps
ACCGames$HomeDRBPct <- ACCGames$HomeDRB/ACCGames$HomeDefRebOpps
ACCGames$AwayDRBPct <- ACCGames$AwayDRB/ACCGames$AwayDefRebOpps

##Creat Per Game Averages and SDs for Each Team  
awayVars <- names(ACCGames)[which(grepl("Away", names(ACCGames)) == T)]
homeVars <- names(ACCGames)[which(grepl("Home", names(ACCGames)) == T)]
homeTeamDT <- ACCGames[,c("GameDate", "NeutralSite", "gameID", homeVars), with = F]
awayTeamDT <- ACCGames[,c("GameDate", "NeutralSite", "gameID", awayVars), with = F]
names(homeTeamDT) <- gsub("Home", "", names(homeTeamDT))
names(awayTeamDT) <- gsub("Away", "", names(awayTeamDT))
homeTeamDT$HomeAway <- rep(1, nrow(homeTeamDT))
awayTeamDT$HomeAway <- rep(0, nrow(awayTeamDT))
fullTeamDT <- rbind(homeTeamDT, awayTeamDT)
fullTeamDT <- setDT(arrange(fullTeamDT, gameID, -HomeAway))
names(fullTeamDT) <- gsub("3FGM", "FG3M", names(fullTeamDT))
names(fullTeamDT) <- gsub("3FGA", "FG3A", names(fullTeamDT))
fullTeamDT$FGPct <- fullTeamDT$FGM/fullTeamDT$FGA
fullTeamDT$FG3Pct <- fullTeamDT$FG3M/fullTeamDT$FG3A
fullTeamDT$FTPct <- fullTeamDT$FTM/fullTeamDT$FTA
fullTeamDT$FTARate <- fullTeamDT$FTA/fullTeamDT$FGA
fullTeamDT$FG3ARate <- fullTeamDT$FG3A/fullTeamDT$FGA
fullTeamDT$DefActivity <- (fullTeamDT$STL + fullTeamDT$BLK)/fullTeamDT$Fouls
teamVars <- names(fullTeamDT)[which(names(fullTeamDT) %nin% c("GameDate", "NeutralSite", "gameID", "Team", "TeamID", "HomeAway"))]
fullTeamDT <- fullTeamDT[, paste0(teamVars, "Diff") := lapply(.SD, function(x) c(-1,1)*diff(x)), by = "gameID", .SDcols = c(teamVars)]
teamVars <- c(teamVars, paste0(teamVars, "Diff"))
fullTeamDT <- fullTeamDT[, paste0(teamVars, "PerGame") := lapply(.SD, mean, na.rm = T), by = "TeamID", .SDcols = c(teamVars)]
fullTeamDT <- fullTeamDT[, paste0(teamVars, "SD") := lapply(.SD, sd, na.rm = T), by = "TeamID", .SDcols = c(teamVars)]
teamVars <- c(teamVars, paste0(teamVars, "PerGame"), paste0(teamVars, "SD"))
ACCGamesFull <- unique(ACCGames[,c("HomeTeam", "AwayTeam", "GameDate", "NeutralSite", "AwayTeamID", "HomeTeamID", "gameID")])
ACCGamesFull <- merge(ACCGamesFull, fullTeamDT[,c("TeamID", "gameID", teamVars), with = F], by.x = c("gameID", "HomeTeamID"), by.y = c("gameID", "TeamID"), all.x = T)
names(ACCGamesFull)[which(names(ACCGamesFull) %in% teamVars)] <- paste0("Home", teamVars)
ACCGamesFull <- merge(ACCGamesFull, fullTeamDT[,c("TeamID", "gameID", teamVars), with = F], by.x = c("gameID", "AwayTeamID"), by.y = c("gameID", "TeamID"), all.x = T)
names(ACCGamesFull)[which(names(ACCGamesFull) %in% teamVars)] <- paste0("Away", teamVars)


####Create Model Matrix with Teams, Home/Away, and Margin####
##Home and Away
TeamMat <- matrix(0, ncol = nrow(teamIDRefs), nrow = nrow(ACCGames))
colnames(TeamMat) <- paste0("Team_ID", teamIDRefs$team_id)
for(i in 1:nrow(teamIDRefs)){
  TeamMat[,i] <- TeamMat[,i] + as.numeric(ACCGames$HomeTeamID == teamIDRefs$team_id[i])
  TeamMat[,i] <- TeamMat[,i] - as.numeric(ACCGames$AwayTeamID == teamIDRefs$team_id[i])
}


##Margin
Margin <- ACCGamesFull$HomeScore - ACCGamesFull$AwayScore


####CV Ridge Regression####
impVars <- c("FGPct", "FG3Pct", "FTPct", "FG3ARate", "FTARate", "DefActivity", "TRBPct", "ORBPct", "DRBPct")
impVars <- c(impVars, paste0(impVars, "PerGame"))
x <- cbind(Constant = 1, TeamMat, NeutralSite = ACCGamesFull$NeutralSite, as.matrix(ACCGamesFull[,paste0(c("Home", "Away"), impVars), with = F]))
y <- Margin
lambdas <- seq(50, 8000, 50)
fit <- glmnet(x, y, alpha = 1, lambda = lambdas)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, standardize = T, standardize.response = T)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
allBetas <- cv_fit$glmnet.fit$beta[,which(cv_fit$glmnet.fit$lambda == opt_lambda)]
allBetas <- allBetas[which(grepl("_ID", names(allBetas)) == T)]
marginResults <- as.data.frame(do.call(rbind, strsplit(names(allBetas), "_ID")))
names(marginResults) <- c("margin_type", "team_id")
marginResults$beta <- allBetas
marginResults <- merge(marginResults, teamIDRefs, by = "team_id", all.x = T)
marginResults <- remove_zero_var_cols(marginResults)
marginResults <- arrange(marginResults, -beta)





