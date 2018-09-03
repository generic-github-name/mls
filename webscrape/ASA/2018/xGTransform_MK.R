#Kevin Minkus
#transform raw shots into expected goals

library(dplyr)
season = 2018
setwd("~/../Dropbox/ASA Blog Data")

shots <- data.frame(read.csv(paste0(season, " Stats/raw shots.csv"), stringsAsFactors=FALSE))
shots$passID <- NULL
if(!file.exists(paste0(season, " Stats/shots with xG.csv"))){
  shots.with.xg <- data.frame()
}else{
  shots.with.xg <- data.frame(read.csv(paste0(season, " Stats/shots with xG.csv"), stringsAsFactors = FALSE))
  shots.with.xg <- shots.with.xg %>%
    mutate(X = NULL,
           date = as.Date(date))
  
}

shots <- shots %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y')) %>%
  arrange(date, gameID, half, eventID)

shotloc <- function(data, fwidth = 80, flength = 115){
        X <- data[1]
        Y <- data[2]
	X <- (100 - X)*flength/100
	Y <- (Y-50)*fwidth/100
	distance <- sqrt(X^2 + Y^2)
	theta <- atan(Y/X)*180/pi
	slope1 <- -abs(Y)/X
	slope2 <- X/(abs(Y) + 4)
	xpoint <- -4*(slope1 + slope2)/(slope2 - slope1)
	ypoint <- flength - slope2*(xpoint + 4)
	available <- sqrt((4-xpoint)^2 + (flength-ypoint)^2)
	return(c(distance, theta, available))
}

output <- apply(cbind(shots$x,shots$y),1,shotloc)
shots$distance <- output[seq(1,length(output),3)]
shots$angle <- output[seq(2,length(output),3)]
shots$available <- output[seq(3,length(output),3)]

gmloc <- function(data, flength = 115, fwidth = 80){ # Here is where we could input stadium-specific dimensions someday
    X <- data[1]
    Y <- data[2]
    gmX <- data[3]
    X <- (100 - X)*flength/100
    Y <- (Y-50)*fwidth/100
    ang1 <- ifelse(Y == -4, pi/2, ifelse(Y < -4, pi + atan(X/(4+Y)), atan(X/(4+Y))))
    ang2 <- ifelse(Y == 4, pi/2, ifelse(Y < 4, atan(X/(4-Y)), pi + atan(X/(4-Y))))
    angshot <- apply(data.frame(ang1, pi-ang2), 1, mean, na.rm = T)

    slope1 <- -1/tan(angshot) ##slope from near post perpendicular to shot angle
    slope2 <- ifelse(Y > 0, X/(Y+4), X/(Y-4)) ##slope to far post
	
    xpoint <- ifelse(Y == 0, -4, sign(Y)*4*(slope1 + slope2)/(slope1 - slope2)) 
    ypoint <- ifelse(angshot > pi/2, flength - (4-xpoint)*abs(slope2), flength - (xpoint + 4)*abs(slope2))
    xmid <- (xpoint + sign(Y)*4)/2
    ymid <- (ypoint + flength)/2

    GmX <- (gmX - 50)*fwidth/100
    xshot <- array(dim = length(X))
    yshot <- array(dim = length(X))
	
    xshot[GmX == Y & is.na(GmX) == F] <- GmX[GmX == Y & is.na(GmX) == F]
    xshot[GmX != Y & is.na(GmX) == F] <- c(ifelse(Y < GmX, (4*slope1 - GmX*X/abs(Y-GmX))/(-X/abs(Y-GmX)-slope1),(-4*slope1+GmX*abs(X/(Y-GmX)))/(-slope1 + X/(Y-GmX))))[GmX != Y & is.na(GmX) == F]
	
    yshot[GmX == Y & is.na(GmX) == F] <- (-abs(slope1)*abs(abs(xshot) - 4) + flength)[GmX == Y & is.na(GmX) == F]
    yshot[GmX != Y & is.na(GmX) == F] <- ifelse(Y < 0, flength - slope1*(xshot+4), flength - slope1*(xshot-4))[GmX != Y & is.na(GmX) == F]
	
    keepreach <- sqrt((xmid - xshot)^2 + (ymid - yshot)^2)
    dive.dir <- sign(xshot - xmid)
    dive.dir[is.na(GmX) == T] <- 0
    return(c(keepreach, dive.dir))
}

gmOutput <- apply(cbind(shots$x,shots$y,shots$gmlocy),1,gmloc)

shots$keepreach <- gmOutput[seq(1,length(gmOutput),2)]
shots$dive <- gmOutput[seq(2,length(gmOutput),2)]
shots$outcome <- shots$result == 'Goal'
shots$gmlocz <- 8*shots$gmlocz/37.5

shots <- shots %>%
  mutate(patternOfPlay.model = ifelse(patternOfPlay == 'Throw in', 
                                      'Regular', 
                                      patternOfPlay),
         distance = ifelse(patternOfPlay == 'Penalty', 
                           12, 
                           distance),
         available = ifelse(patternOfPlay == 'Penalty', 
                            8, 
                            available))
#########################
# Apply xGoal models ####
#########################

load("ShootingShinyApp/IgnoreList/UpdatedModels_2018-02-03.Rdata") # Fix filepath accordingly
source("ShootingShinyApp/TeamxGoalAdjustmentFunction.R") # Fix filepath accordingly

shots[["xGvalueP"]] <- predict(xgoal.model,
                    newdata = shots %>%
                      mutate(year = as.character(season - 1)),
                    type='response')
shots[["xGvalueT"]] <- predict(xgoal.model, 
                    shots %>%
                      mutate(patternOfPlay.model = ifelse(patternOfPlay.model == 'Penalty', 
                                                          'Regular', 
                                                          patternOfPlay.model),
                             year = as.character(season - 1)), 
                    type = 'response')
shots[["xGTeam"]] <- shots[["xGvalueT"]]

shots[['xGKeeper']][shots$result %in% c('Goal', 'Saved')] <- predict(xgoal.model.keeper, 
                                                                           shots[shots$result %in% c('Goal', 'Saved'),] %>%
                                                                       mutate(year = as.character(season - 1)), 
                                                                           type = 'response')

shots <- team.xgoal.adj(shots, 5/60)

shots <- shots %>%
  mutate(shooter = str_replace_all(shooter, 
                                   c('Kazaishvili' = 'Qazaishvili', 
                                     'Jorge Villafaña' = 'Jorge Villafana',
                                     "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
         shooter = ifelse(row_number() %in% grep("Boniek", shooter), "Oscar Boniek Garcia", shooter),
         passer = str_replace_all(passer, 
                                  c('Kazaishvili' = 'Qazaishvili', 
                                    'Jorge Villafaña' = 'Jorge Villafana',
                                    "Antonio Mlinar Dalamea" = "Antonio Mlinar Delamea")),
         passer = ifelse(row_number() %in% grep("Boniek", passer), "Oscar Boniek Garcia", passer))

output <- bind_rows(shots.with.xg, shots) %>%
  filter(!duplicated(data.frame(gameID, eventID)))

write.csv(output, paste0(season, " Stats/shots with xG.csv"), row.names = F)

# Old stuff ####
# shooterFormula <- as.formula("outcome~I(log(distance))+I((available - mean(available))^2)+available + I(bodypart == \'Head\') + cross + through + I(patternOfPlay == \'Corner\') + I(patternOfPlay == \'Free kick\') + I(patternOfPlay == \'Set piece\') + I(patternOfPlay == \'Fastbreak\') + I(patternOfPlay == \'Penalty\')")
# 
# #teamModel <- as.formula("outcome ~ I(log(distance)) + I((available - mean(available))^2) + available + ( == "Head") + Cross + Through + I(Pattern.of.Play == "Corner") + I(Pattern.of.Play == "Free kick") + I(Pattern.of.Play == "Set piece") + I(Pattern.of.Play == "Fastbreak") + I(Pattern.of.Play == "Penalty")
# 
# keeperFormula <- as.formula("outcome~ I(log(distance)) + I((available - mean(available))^2) + available + I(bodypart == \'Head\') + cross + through + I(patternOfPlay == \'Corner\') + I(patternOfPlay == \'Free kick\') + I(patternOfPlay == \'Set piece\') + I(patternOfPlay == \'Fastbreak\') + I(patternOfPlay == \'Penalty\') + keepreach + I(abs(gmlocz - 3.5)) + gmlocz")
# 
# #### Really need to fix this##########
# #print(tail(shots[1:196, ]))
# shooterMode <- glm(shooterFormula,family=binomial,data=shots)
# ##print('here')
# keeperMode <- glm(keeperFormula,family=binomial,data=filter(shots, result == 'Saved' | result == 'Goal'))
# 
# #now input our model probabilities
# shooterModelCoef <- c(4.172,-2.353,-.026,0.069,-.648,-.380,.909,-.622,.539,-.192,.680,2.735)
# 
# keeperModelCoef <- c(5.233,-2.774,-.025,-.138,-.896,-.146,.516,-.115,.377,.010,.507,2.038,1.093,.266,.129)
# for(i in 1:length(shooterMode$coef)){
#     shooterMode$coefficients[i] <- shooterModelCoef[i]
# }
# 
# for(i in 1:length(keeperModelCoef)){
#     keeperMode$coefficients[i] <- keeperModelCoef[i]
# }
# 
# #these are good
# xGvalueP <- predict(shooterMode,newdata=shots,type='response')
# xGvalueT <- xGvalueP
# xGvalueT[shots$patternOfPlay == "Penalty"] <- 0.15
# 
# shots$xGvalueP <- xGvalueP
# shots$xGvalueT <- xGvalueT
# 
# #this isn't yet
# xGvalueGK <- predict(keeperMode,newdata=shots,type='response')
# xGvalueGK[shots$result != 'Saved' & shots$result != 'Goal'] <- 0
# 
# shots$xGvalueGK <- xGvalueGK
# output <- rbind(shots.with.xg,shots)
# write.csv(output,'shots with xG.csv')
