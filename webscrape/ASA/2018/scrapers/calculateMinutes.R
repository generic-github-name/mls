#Script to calculate minutes played for everybody

library(dplyr)
library(lubridate)

lineups <- data.frame(read.csv('Starting Lineups.csv',stringsAsFactors=FALSE))
gameInfo <- data.frame(read.csv('Game Information.csv',stringsAsFactors = FALSE))
infoKeep <- c('gameID','firstHalfTime','secondHalfTime')
gameInfo <- gameInfo[,infoKeep]


playerChanges <- data.frame(read.csv('playerChanges.csv',stringsAsFactors=FALSE))
pcKeep <- c('gameID','time','half','team','player','on','off')
playerChanges <- playerChanges[,pcKeep]

#playerChanges <- filter(playerChanges,gameID == 914827)

#first, add every player in our lineup who is starting to the playerChanges data frame, and do it with a friggin for loop cause i'm tired and its 1:30 am
for(i in 1:nrow(lineups)){
    row <- lineups[i,]
    gameID <- row$gameID
    team <- row$team
    time <- "0:00"
    on <- 1
    half <- 1
    off <-  0
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player1,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player2,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player3,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player4,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player5,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player6,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player7,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player8,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player9,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player10,on,off))
    playerChanges <- rbind(playerChanges,c(gameID,time,half,team,row$player11,on,off))
}
    
#checked the new number of rows in playerChanges is as it should be

#then, split the playerChanges df into players on and players off
playersOn <- filter(playerChanges, on == 1)
playersOff <- filter(playerChanges, off == 1)

playerMinutes <- c()
#iterate through the players on, and find out whether there is a corresponding record in the off df
for(index in 1:nrow(playersOn)){
    row <- playersOn[index,]
    gameIDOn <- row$gameID
    timeOn <- ms(row$time)
    halfOn <- row$half
    playerOn <- row$player
    teamOn <- row$team
    # get the row that player leaves play, if it exists
    offRow <- filter(playersOff,gameID == gameIDOn, player == playerOn)

    #first, handle the case where that player doesn't come off
    if(nrow(offRow) == 0){
        if(halfOn == 1){
            #first half minutes played is end of first half minus time on
            firstHalfEnd <- ms(filter(gameInfo,gameID==gameIDOn)$firstHalfTime)
            firstHalfTime <- as.period(firstHalfEnd - timeOn,"minute")
            #second half minutes played is end of 2nd half - 45
            secondHalfEnd <- ms(filter(gameInfo,gameID==gameIDOn)$secondHalfTime)
            secondHalfTime <- as.period(secondHalfEnd - ms("45:00"),"minute")
           totalTime <- minute(firstHalfTime + secondHalfTime)
        }

    
        else{
            secondHalfEnd <- ms(filter(gameInfo,gameID==gameIDOn)$secondHalfTime)
            totalTime <- minute(secondHalfEnd - timeOn)
        }
    }

    else{
        #how to handle the person coming off during the game
        halfOff <- offRow$half
        if(halfOff == 1){
            totalTime <- minute(ms(offRow$time) - timeOn)
        }
        else{
            firstHalfEnd <- ms(filter(gameInfo,gameID == gameIDOn)$firstHalfTime)
            firstHalfTime <- as.period(firstHalfEnd - timeOn,"minute")
            secondHalfTime <- as.period(ms(offRow$time) - ms("45:00"),"minute")
            totalTime <- minute(firstHalfTime + secondHalfTime)
        }
    }
        playerMinutes <- rbind(playerMinutes,c(gameIDOn,playerOn,totalTime, teamOn))

 }       

playerMin <- data.frame(playerMinutes)
colnames(playerMin) <- c('gameID','player','minutes', 'team')

write.csv(playerMin,"minutes played by game.csv")
