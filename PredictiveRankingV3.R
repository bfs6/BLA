####Read in Libraries####
library(data.table)
library(dplyr)
library(Hmisc)
library(glmnet)
library(tidyr)


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
teamVars <- names(fullTeamDT)[which(names(fullTeamDT) %nin% c("GameDate", "NeutralSite", "gameID", "Team", "TeamID", "HomeAway"))]
fullTeamDT <- fullTeamDT[, paste0(teamVars, "Diff") := lapply(.SD, function(x) c(-1,1)*diff(x)), by = "gameID", .SDcols = c(teamVars)]
teamVars <- c(teamVars, paste0(teamVars, "Diff"))
fullTeamDT <- fullTeamDT[, paste0(teamVars, "PerGame") := lapply(.SD, mean, na.rm = T), by = "TeamID", .SDcols = c(teamVars)]
fullTeamDT <- fullTeamDT[, paste0(teamVars, "SD") := lapply(.SD, sd, na.rm = T), by = "TeamID", .SDcols = c(teamVars)]
teamVars <- c(teamVars, paste0(teamVars, "PerGame"), paste0(teamVars, "SD"))
ACCGames <- merge(ACCGames, fullTeamDT[,c("TeamID", "gameID", teamVars), with = F], by.x = c("gameID", "HomeTeamID"), by.y = c("gameID", "TeamID"), all.x = T)


####Create Model Matrix with Teams, Home/Away, and Margin####
##Home and Away
HomeMat <- model.matrix(~ as.character(HomeTeamID) + 0, ACCGames)
AwayMat <- model.matrix(~ as.character(AwayTeamID) + 0, ACCGames)
colnames(HomeMat) <- paste0("Home_", gsub(" |as.character[(]HomeTeamID[)]|[(]|[)]", "", colnames(HomeMat)))
colnames(AwayMat) <- paste0("Away_", gsub(" |as.character[(]AwayTeamID[)]|[(]|[)]", "", colnames(AwayMat)))
TeamMat <- cbind(HomeMat, -AwayMat)

##Margin
Margin <- ACCGames$HomeScore - ACCGames$AwayScore

