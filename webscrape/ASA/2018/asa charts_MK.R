#Kevin Minkus
#file to compile ASA charts for website

library(dplyr)
library(xtable)
year = 2018
#setwd(paste0("~/../Dropbox/ASA Blog Data/", year, " Stats/"))
setwd("./")

#load in the requisite data
shooting <- data.frame(read.csv('shots with xG.csv', stringsAsFactors=FALSE))
minutesPlayed <- data.frame(read.csv('minutes played by game.csv',stringsAsFactors=FALSE))
touches <- data.frame(read.csv('touches.csv',stringsAsFactors=FALSE))

shooting[shooting == 'Antonio Mlinar Dalamea'] <- 'Antonio Mlinar Delamea'
minutesPlayed[minutesPlayed == 'Antonio Mlinar Dalamea'] <- 'Antonio Mlinar Delamea'
touches[touches == 'Antonio Mlinar Dalamea'] <- 'Antonio Mlinar Delamea'
# minutesPlayed[minutesPlayed == 'Matt VanOekel'] <- 'Matt Van Oekel'
# touches[touches == 'Matt VanOekel'] <- 'Matt Van Oekel'
# shooting[shooting == 'Matt VanOekel'] <- 'Matt Van Oekel'
minutesPlayed[minutesPlayed == 'Valeri Kazaishvili'] <- 'Valeri Qazaishvili'
touches[touches == 'Valeri Kazaishvili'] <- 'Valeri Qazaishvili'
shooting[shooting == 'Valeri Kazaishvili'] <- 'Valeri Qazaishvili'
minutesPlayed[minutesPlayed == 'Boniek Garcia'] <- 'Oscar Boniek Garcia'
touches[touches == 'Boniek Garcia'] <- 'Oscar Boniek Garcia'
shooting[shooting == 'Boniek Garcia'] <- 'Oscar Boniek Garcia'

#get total minutes for each player
playerMin <- group_by(minutesPlayed,player) %>% 
  summarise(totalMinutes = sum(minutes))

################ now do stuff specifically for player xG###########################
#get shot statistics for each shooter
shotStats <- group_by(shooting,shooter) %>%
    summarise(shots = n(),
              unassisted = 1-sum(assisted)/shots,
              goals = sum(result == 'Goal'),
              xG = sum(xGvalueP),
              goalsMinusXG = goals - xG)

#get assist stats for each passer
assistStats <- group_by(shooting,passer) %>%
     filter(!(is.na(passer))) %>%
    summarise(keyPasses = n(), 
              assists = sum(result == 'Goal'), 
              xA = sum(xGvalueP))

#get touch stats for each player
teamTouches <- group_by(touches,team,gameID) %>% 
  summarise(total = sum(touches))
playerTouches <- group_by(touches,player,team,gameID) %>% 
  summarise(totalTouches = sum(touches))
combined <- merge(teamTouches,playerTouches)
percent <- group_by(combined,player) %>%
    summarise(percentTouches = sum(totalTouches)/sum(total))

assistsAndShots <- merge(shotStats,
                         assistStats,
                         by.x=c('shooter'),
                         by.y=c('passer'),
                         all=TRUE)
colnames(assistsAndShots)[1] <- 'player'

totalStats <- merge(assistsAndShots,
                    percent,
                    by.x=c('player'),
                    by.y=c('player'))
withMinutes <- merge(totalStats,playerMin)

#now convert is.na to 0's
withMinutes[is.na(withMinutes)] <- 0
#and include xG + xA
withMinutes$xGxA <- withMinutes$xG + withMinutes$xA
#reorder columns
withMinutes <- withMinutes[,c('player','totalMinutes','shots','unassisted','goals','xG','goalsMinusXG','keyPasses','assists','xA','xGxA','percentTouches')]

withMinutes <- mutate(withMinutes, shotsp96 = round(shots/totalMinutes*96,2),
                                   xgp96    = round(xG/totalMinutes*96,2),
                                   kp96     = round(keyPasses/totalMinutes*96,2),
                                   xap96    = round(xA/totalMinutes*96,2),
                                   xgxap96  = round(xGxA/totalMinutes*96,2))
                                   

#now write our total stats out
write.csv(withMinutes,'player xg.csv')

#write HTML table out
write.table(print.xtable(xtable(withMinutes, 
                                digits = c(0, 0, 0, 0, 2, 0, 2, 2, 0, 0, 2, 2, 3, 2, 2, 2, 2, 2), 
                                align = rep("c", 18)), 
                         type = "html", 
                         include.rownames = F), 
            "player xg_HTML.txt",
            quote = F)

##########################################################################

# now handle goalie charts ####################
#we've already got player minutes

saves <- group_by(shooting, goalie) %>%
    summarise(saves = sum(result == 'Saved'),goals = sum(result == 'Goal'), sog = saves + goals, xGA = sum(xGKeeper), gMinusxG = goals - xGA)

totalGKStats <- merge(saves,playerMin,by.x=c('goalie'),by.y=c('player'))

write.csv(totalGKStats,'goalie xg.csv')

#write HTML table out
write.table(print.xtable(xtable(totalGKStats, 
                                digits = c(0, 0, 0, 0, 0, 2, 2, 0), 
                                align = rep("c", 8)), 
                         type = "html", 
                         include.rownames = F), 
            "goalie xg_HTML.txt",
            quote = F)

#########################################################

# Now get passing ratio numbers for each team
#passes <- data.frame(read.csv('raw passes.csv'))
#passPercFor <- group_by(passes, team) %>%
 #   summarize(finalThirdPer = sum((x > 66.7 | endX > 66.7) & success == 1)/sum(success == 1))

#passPercAgainst <- group_by(passes, team.1) %>%
#    summarize(finalThirdAPer = sum((x > 66.7 | endX > 66.7) & success == 1)/sum(success == 1))


#write.csv(passPerc, 'passing_percentages.csv')x


# Now handle the team pages
teamStatsFor <- group_by(shooting, team) %>%
    summarise(gp = length(unique(gameID)), xGF = sum(xGvalueP), gf = sum(result == 'Goal'))

teamStatsAgainst <- group_by(shooting, team.1) %>%
    summarise(xGA = sum(xGvalueP), ga = sum(result == 'Goal'))

teamStats <- merge(teamStatsFor, teamStatsAgainst, by.x = 'team', by.y = 'team.1')

teamStats <- mutate(teamStats, xGD = xGF - xGA, gd = gf - ga, gdMinusxGD = gd - xGD)[, c('team','gp','xGF','xGA','xGD','gf','ga','gd','gdMinusxGD')]

write.csv(teamStats, 'team_table.csv')

#write HTML table out
write.table(print.xtable(xtable(teamStats, 
                                digits = c(0, 0, 0, 2, 2, 2, 0, 0, 0, 2), 
                                align = rep("c", 10)), 
                         type = "html", 
                         include.rownames = F), 
            "team_table_HTML.txt",
            quote = F)

# Now handle the game xG pages

homeStats <- group_by(shooting, gameID) %>%
    summarise(hg = sum(ifelse(team == hteam, result == 'Goal', 0)), hxg = sum(ifelse(team == hteam, xGvalueP, 0)))

awayStats <- group_by(shooting, gameID) %>%
    summarise(ag = sum(ifelse(team == ateam, result == 'Goal', 0)), axg = sum(ifelse(team == ateam, xGvalueP, 0)))

stats <- merge(homeStats, awayStats, by.x = 'gameID', by.y = 'gameID')
statsWithTeams <- unique(merge(stats, shooting, by.x = 'gameID', by.y = 'gameID')[, c('date', 'hteam', 'hg', 'hxg', 'ateam', 'ag', 'axg')])

statsWithTeams$xGD <- statsWithTeams$hxg - statsWithTeams$axg

write.csv(statsWithTeams, 'by_game_xg.csv')

#write HTML table out
write.table(print.xtable(xtable(statsWithTeams, 
                                digits = c(0, 0, 0, 0, 2, 0, 0, 2, 2), 
                                align = rep("c", 9)), 
                         type = "html", 
                         include.rownames = F), 
            "by_game_xg_HTML.txt",
            quote = F)
