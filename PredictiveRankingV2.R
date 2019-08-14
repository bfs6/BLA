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


####Create Model Matrix with Teams, Home/Away, and Margin####
##Home and Away
HomeAwayDF <- filter(ACCGames, NeutralSite == 0)
NeutralDF <- filter(ACCGames, NeutralSite == 1)
HomeMat <- model.matrix(~ as.character(HomeTeamID) + 0, HomeAwayDF)
AwayMat <- model.matrix(~ as.character(AwayTeamID) + 0, HomeAwayDF)
colnames(HomeMat) <- paste0("Home_ID", gsub(" |as.character[(]HomeTeamID[)]|[(]|[)]", "", colnames(HomeMat)))
colnames(AwayMat) <- paste0("Away_ID", gsub(" |as.character[(]AwayTeamID[)]|[(]|[)]", "", colnames(AwayMat)))
TeamMat <- cbind(HomeMat, -AwayMat)
TeamMatEmpty <- matrix(0, nrow = nrow(NeutralDF), ncol = ncol(TeamMat))
TeamMat <- rbind(TeamMat, TeamMatEmpty)

##Neutral
NeutralMat <- matrix(0, nrow = nrow(NeutralDF), ncol = nrow(teamIDRefs))
colnames(NeutralMat) <- gsub("Home_ID", "Neutral_ID", colnames(HomeMat))
for(i in 1:nrow(teamIDRefs)){
  NeutralMat[,i] <- NeutralMat[,i] + as.numeric(rowSums(NeutralDF[,c("HomeTeamID", "AwayTeamID")] == teamIDRefs$team_id[i]))
}
NeutralMatEmpty <- matrix(0, nrow = nrow(HomeAwayDF), ncol = ncol(NeutralMat))
NeutralMat <- rbind(NeutralMatEmpty, NeutralMat)
TeamMatFull <- cbind(TeamMat, NeutralMat)

##Margin
Margin <- HomeAwayDF$HomeScore - HomeAwayDF$AwayScore
Margin2 <- NeutralDF$HomeScore - NeutralDF$AwayScore
Margin <- c(Margin, Margin2)


##CV Ridge Regression
x <- cbind(1, TeamMatFull)
colnames(x)[1] <- "Constant"
y <- Margin
lambdas <- seq(50, 8000, 50)
fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, nfolds = 10)
opt_lambda <- cv_fit$lambda.min
fit <- cv_fit$glmnet.fit
allBetas <- cv_fit$glmnet.fit$beta[,which(cv_fit$glmnet.fit$lambda == opt_lambda)]
allBetas <- allBetas[which(grepl("_ID", names(allBetas)) == T)]
marginResults <- as.data.frame(do.call(rbind, strsplit(names(allBetas), "_ID")))
names(marginResults) <- c("margin_type", "team_id")
marginResults$beta <- allBetas
marginResults <- merge(marginResults, teamIDRefs, by = "team_id", all.x = T)
marginResults <- spread(marginResults, margin_type, beta)
HomeTeamCount <- HomeAwayDF %>% group_by(HomeTeamID) %>% dplyr::summarise(count_home = n())
AwayTeamCount <- HomeAwayDF %>% group_by(AwayTeamID) %>% dplyr::summarise(count_away = n())
NeutralTeamCount <- data.frame(TeamID = c(NeutralDF$HomeTeam, NeutralDF$AwayTeam)) %>% group_by(TeamID) %>% dplyr::summarize(count_neutral = n())
names(HomeTeamCount)[which(names(HomeTeamCount) == "HomeTeamID")] = "TeamID"
names(AwayTeamCount)[which(names(AwayTeamCount) == "AwayTeamID")] = "TeamID"
gameCounts <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = "TeamID"), list(HomeTeamCount, AwayTeamCount, NeutralTeamCount))
marginResults <- merge(marginResults, gameCounts, by.x = "team_id", by.y = "TeamID", all.x = T)
marginResults$WeightedAvgScore <- ((marginResults$Away * marginResults$count_away) + (marginResults$Home * marginResults$count_home) + (marginResults$Neutral * marginResults$count_neutral))/
  (marginResults$count_home + marginResults$count_away + marginResults$count_neutral)
marginResults <- arrange(marginResults, -WeightedAvgScore)


##Manual Ridge Regression
lambda <- 2000
xTx <- t(x) %*% x
xTy <- t(x) %*% y
I <- diag(nrow(xTy))
xTx_lambda <- xTx + lambda * I
mean <- solve(xTx_lambda) %*% xTy
meanResults <- as.data.frame(do.call(rbind, strsplit(row.names(mean), "_ID")))
names(meanResults) <- c("margin_type", "team_id")
meanResults <- filter(meanResults, margin_type %in% c("Home", "Away", "Neutral"))
mean <- mean[which(grepl("_ID", row.names(mean)) == T)]
meanResults$beta <- mean
meanResults <- merge(meanResults, teamIDRefs, by = "team_id", all.x = T)
meanResults <- spread(meanResults, margin_type, beta)
HomeTeamCount <- HomeAwayDF %>% group_by(HomeTeamID) %>% dplyr::summarise(count_home = n())
AwayTeamCount <- HomeAwayDF %>% group_by(AwayTeamID) %>% dplyr::summarise(count_away = n())
NeutralTeamCount <- data.frame(TeamID = c(NeutralDF$HomeTeam, NeutralDF$AwayTeam)) %>% group_by(TeamID) %>% dplyr::summarize(count_neutral = n())
names(HomeTeamCount)[which(names(HomeTeamCount) == "HomeTeamID")] = "TeamID"
names(AwayTeamCount)[which(names(AwayTeamCount) == "AwayTeamID")] = "TeamID"
gameCounts <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by = "TeamID"), list(HomeTeamCount, AwayTeamCount, NeutralTeamCount))
meanResults <- merge(meanResults, gameCounts, by.x = "team_id", by.y = "TeamID", all.x = T)
meanResults$WeightedAvgScore <- ((meanResults$Away * meanResults$count_away) + (meanResults$Home * meanResults$count_home) + (meanResults$Neutral * meanResults$count_neutral))/
  (meanResults$count_home + meanResults$count_away + meanResults$count_neutral)
meanResults <- arrange(meanResults, -WeightedAvgScore)

