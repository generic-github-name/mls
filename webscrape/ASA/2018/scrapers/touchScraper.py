#Date: 2/12/2016
#Kevin Minkus
#Scrape whoscored json for aggregated touches actions

#TODO can I traverse things in order like this? Or do I need time stamps? I'll have to evaluate this
#import necessary packages
from __future__ import print_function
from unidecode import unidecode
#this handles our accented characters
import json
import re
import csv


def main():
	with open('gameIDs.csv', 'r') as fi:
    		reader = csv.reader(fi)
    		idList = list(reader)
	
	
	f = open('touches.csv','w+')
		#s = open('tracker.csv','w+')
	header= "date,gameID,player,touches,team,team.1,averageX,averageY,hteam,ateam,hfinal,afinal"
	print(header,file=f)
	
	for gameIDL in idList:
		gameID = gameIDL[0]
		with open('/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/Game ' + str(gameID) + '.json','rb') as data_file:    
    			data = json.load(data_file)
	
		
		count = 0
	
		#now get to the actual scraping part
		home = data["home"]#can use this later to get good stuff
		homeID = home["teamId"]
		homeTeam = home["name"]
		away = data["away"]#can use this later to get good stuff
		awayID = away["teamId"]
		awayTeam = away["name"]
		playerDict = data["playerIdNameDictionary"]
		#handle alvas powell error
		playerDict['133484'] = 'Alvas Powell'
		#handle micheal azira error
		playerDict['144299'] = 'Micheal Azira'
		#handle phanuel kavita error
		playerDict['275975'] = 'Phanuel Kavita'
		date = data["startDate"].split(" ")[0]
		if len(date.split("T")) > 1:
			date = date.split("T")[0]
			date = date[5:7] + "/" + date[8:10] + "/" + date[0:4]
		events = data["events"]
		hfinal = data["score"][0]
		afinal = data["score"][4] #assumes no double digit score
		#this dictionary relates players with their team, and the other team
		playerTeamDict = {}
		#this dictionary relates players with their touches, and sum location of those touches
		playerTouchDict = {}
		#initialize that dictionary
		for person in playerDict.values():
			playerTouchDict[person] = [0,0,0]
			
		for e in events:
			try: 
				if e["isTouch"]:
					
					player = playerDict[str(e["playerId"])]
					if e['teamId'] == homeID:
						team = homeTeam
						team1 = awayTeam
					else:
						team = awayTeam
						team1 = homeTeam
					xCoord = e["x"] 
					yCoord = e["y"]
					#update our two dictionaries
					playerTeamDict[player] = [team,team1]
					current = playerTouchDict[player]
					playerTouchDict[player] = [current[0]+1,current[1]+xCoord,current[2]+yCoord]
					
					
			except Exception as exc:
				print(gameID)
				print(e["eventId"])
				print(exc)		
		#print(playerTeamDict)		
		for p in playerTouchDict:
			if playerTouchDict[p][0] != 0:
				team = playerTeamDict[p][0]
				team1 = playerTeamDict[p][1]
				touches = playerTouchDict[p][0]
				aveX = playerTouchDict[p][1]/touches
				aveY = playerTouchDict[p][2]/touches		
				line = str(date)+','+str(gameID) +','+unidecode(p)+','+ str(touches) + ',' + str(team) + ',' +str(team1)+',' +str(aveX)+','+str(aveY) +','+str(homeTeam)+','+str(awayTeam)+','+str(hfinal)+','+str(afinal)
				print(line,file=f)
			
			
		#print(str(count)+','+str(firstHalfEndMin) + ":" + str(firstHalfEndSec)+','+str(secondHalfEndMin) + ":" + str(secondHalfEndSec))
	
	f.close()	
	#print results
			

			
if __name__ == '__main__':
	main()

