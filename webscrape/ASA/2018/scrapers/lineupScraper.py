#Date: 1/16/2016
#Kevin Minkus
#Scrape whoscored json for team lineups

#import necessary packages
#this reads stuff off the webpages
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
	
	#idList = [[908236]]
	
	f = open('Starting Lineups.csv','w+')
		#s = open('tracker.csv','w+')
	header= "gameID,team,home,formation,player1,player2,player3,player4,player5,player6,player7,player8,player9,player10,player11,bench1,bench2,bench3,bench4,bench5,bench6,bench7"
	print(header,file=f)
	try:
		for gameIDL in idList:
			gameID = gameIDL[0]
			with open('/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/Game ' + str(gameID) + '.json','rb') as data_file:    
    				data = json.load(data_file)
	

			#now get to the actual scraping part
			playerDict = data["playerIdNameDictionary"]
			#handle andres correa error, micheal azira error
			#TODO just make a csv dictionary to do these lookups, so i dont need to fix this in multiple spots
			playerDict["278240"] = 'Andres Correa'
			playerDict['144299'] = 'Micheal Azira'
			playerDict['275975'] = 'Phanuel Kavita'
			playerDict['133484'] = 'Alvas Powell'

			#handle the home team
			home = data["home"]#can use this later to get good stuff
			hformation = home["formations"][0]["formationName"]
			homeTeam = home["name"]	
			homeInd = 1
			lineup = str(gameID) + "," + homeTeam + "," + str(homeInd) + "," + hformation
			for playerId in home["formations"][0]["playerIds"]:
				lineup += "," + unidecode(playerDict[str(playerId)])
			print(lineup,file=f)	
	
			#handle the away team
			away = data["away"]
			aformation = away["formations"][0]["formationName"]
			awayTeam = away["name"]
			homeInd = 0
			lineup = str(gameID) + "," + awayTeam + "," + str(homeInd) + "," + aformation
			for playerId in away["formations"][0]["playerIds"]:
				lineup += "," + unidecode(playerDict[str(playerId)])
			print(lineup,file=f)
		

		#print(line,file=f)
	except Exception as exc:
		print(gameID)
		print(exc)

	f.close()	
	#print results
			

			
if __name__ == '__main__':
	main()
