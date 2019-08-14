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
ACCGamesFull <- unique(ACCGames[,c("HomeTeam", "AwayTeam", "GameDate", "NeutralSite", "AwayTeamID", "HomeTeamID", "gameID")])

ACCGamesFull <- merge(ACCGamesFull, fullTeamDT[,c("TeamID", "gameID", teamVars), with = F], by.x = c("gameID", "HomeTeamID"), by.y = c("gameID", "TeamID"), all.x = T)
names(ACCGamesFull)[which(names(ACCGamesFull) %in% teamVars)] <- paste0("Home", teamVars)
ACCGamesFull <- merge(ACCGamesFull, fullTeamDT[,c("TeamID", "gameID", teamVars), with = F], by.x = c("gameID", "AwayTeamID"), by.y = c("gameID", "TeamID"), all.x = T)
names(ACCGamesFull)[which(names(ACCGamesFull) %in% teamVars)] <- paste0("Away", teamVars)


####Create Model Matrix with Teams, Home/Away, and Margin####
##Home and Away
HomeMat <- model.matrix(~ as.character(HomeTeamID) + 0, ACCGamesFull)
AwayMat <- model.matrix(~ as.character(AwayTeamID) + 0, ACCGamesFull)
colnames(HomeMat) <- paste0("Home_ID", gsub(" |as.character[(]HomeTeamID[)]|[(]|[)]", "", colnames(HomeMat)))
colnames(AwayMat) <- paste0("Away_ID", gsub(" |as.character[(]AwayTeamID[)]|[(]|[)]", "", colnames(AwayMat)))
TeamMat <- cbind(HomeMat, -AwayMat)

##Margin
Margin <- ACCGamesFull$HomeScore - ACCGamesFull$AwayScore

##Different Vars
teamVarsFull <- teamVars[which(teamVars != "Score")]
awayVarsFull <- paste0("Away", teamVarsFull)
homeVarsFull <- paste0("Home", teamVarsFull)

####CV Ridge Regression####
x <- cbind(Constant = 1, TeamMat, NeutralSite = ACCGamesFull$NeutralSite, as.matrix(ACCGamesFull[, homeVarsFull, with = F]), -as.matrix(ACCGamesFull[, awayVarsFull, with = F]))
y <- Margin
lambdas <- seq(50, 8000, 50)
fit <- glmnet(x, y, alpha = 1, lambda = lambdas)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, standardize = F, standardize.response = F)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
allBetas <- cv_fit$glmnet.fit$beta[,which(cv_fit$glmnet.fit$lambda == opt_lambda)]
allBetas <- allBetas[which(grepl("_ID", names(allBetas)) == T)]
marginResults <- as.data.frame(do.call(rbind, strsplit(names(allBetas), "_ID")))
names(marginResults) <- c("margin_type", "team_id")
marginResults$beta <- allBetas
marginResults <- merge(marginResults, teamIDRefs, by = "team_id", all.x = T)
marginResults <- spread(marginResults, margin_type, beta)
marginResults$Total <- marginResults$Home + marginResults$Away
marginResults <- arrange(marginResults, -Total)

####Manual Ridge Regression####
lambda <- 2000
xTx <- t(x) %*% x
xTy <- t(x) %*% y
I <- diag(nrow(xTy))
xTx_lambda <- xTx + lambda * I
mean <- solve(xTx_lambda) %*% xTy
meanResults <- as.data.frame(do.call(rbind, strsplit(row.names(mean), "_ID")))
names(meanResults) <- c("margin_type", "team_id")
meanResults$beta <- mean
meanResults <- filter(meanResults, margin_type %in% c("Home", "Away"))
meanResults <- merge(meanResults, teamIDRefs, by = "team_id", all.x = T)
meanResults <- spread(meanResults, margin_type, beta)
meanResults$Total <- meanResults$Home + meanResults$Away
meanResults <- arrange(meanResults, -Total)

