#Date: 3/9/2016
#Kevin Minkus
#Scrape whoscored json for minutes played by each person

#TODO can I traverse things in order like this? Or do I need time stamps? I'll have to evaluate this
#TODO Handle rebounds on the R side of things
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
	#idList = [["914831"]]
	
	f = open('playerChanges.csv','w+')
		#s = open('tracker.csv','w+')
	header= "date,gameID,time,half,team,player,on,off,action"
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
		#get the home goalie Assumes the hgoalie is in the first position here
		hgoalie = data["home"]["formations"][0]["playerIds"][0]
		hgoaliename = playerDict[str(hgoalie)]
		#get the away goalie
		agoalie = data["away"]["formations"][0]["playerIds"][0]
		agoaliename = playerDict[str(agoalie)]
		events = data["events"]
		#now iterate over the events
		hscore = 0
		ascore = 0
		hplayers = 11
		aplayers = 11
		hfinal = data["score"][0]
		afinal = data["score"][4] #assumes no double digit score
		final = int(hfinal)-int(afinal)
		firstHalfEndMin = 0
		firstHalfEndSec = 0
		secondHalfEndMin = 0
		secondHalfEndSec = 0
		index = 0
		for e in events:
			try: 
				eID = e["type"]["value"]
				eventID = e["id"]
			

				#handle a substitution
				if eID == 18:
					half = e["period"]["value"]
					minute = str(e["minute"])
					second = str(e["second"])
					if len(second) == 1:
						second = '0'+str(second)
					time = minute + ":" + second
					### handle the substituion off
					if e['teamId'] == homeID:
						team = homeTeam
					else:
						team = awayTeam
					player = playerDict[str(e["relatedPlayerId"])]
					off = 0
					on = 1
					action = "Subbed on"
					print(str(date) + ',' + str(gameID) + ',' + time + ',' + str(half) + ',' + team + ',' + unidecode(player)+','+str(on)+','+str(off)+','+action,file=f)
					### handle the substitution on
					
					player = playerDict[str(e["playerId"])]
					on = 0
					off = 1
					action = "Subbed off"
					print(str(date) + ',' + str(gameID) + ',' + time + ',' + str(half) + ',' + team + ',' + unidecode(player)+','+str(on)+','+str(off)+','+action,file=f)
				#handle the rare case a player leaves early and no subs are left############### haven't tested this yet
				if eID == 20:
					half = e["period"]["value"]
					minute = str(e["minute"])
					second = str(e["second"])
					if len(second) == 1:
						second = '0'+str(second)
					time = minute + ":" + second
					if e['teamId'] == homeID:
						team = homeTeam
					else:
						team = awayTeam
					player = playerDict[str(e["relatedPlayerId"])]
					off = 1
					on = 0
					action = "Taken off"
					print(str(date) + ',' + str(gameID) + ',' + time + ',' + str(half) + ',' + team + ',' + unidecode(player)+','+str(on)+','+str(off)+','+action,file=f)
					
				#handle a red card or second yellow#####################################
				if eID == 17:
					half = e["period"]["value"]
					minute = str(e["minute"])
					second = str(e["second"])
					if len(second) == 1:
						second = '0'+str(second)
					time = minute + ":" + second
					if e['teamId'] == homeID:
						team = homeTeam
					else:
						team = awayTeam
					#check what type of card it is
					for q in e['qualifiers']:
						#second yellow or red, respectively, and check to make sure its not the coach- TODO what if sub?
						if (q['type']['value'] == 32 or q['type']['value'] == 33) and "playerId" in e.keys():
							player = playerDict[str(e["playerId"])]
							off = 1
							on = 0
							if (q['type']['value'] == 32):
								action = 'secondYellow'
							else:
								action = 'red'
							print(str(date) + ',' + str(gameID) + ',' + time + ',' + str(half) + ',' + team + ',' + unidecode(player)+','+str(on)+','+str(off)+','+action,file=f)
				###############################################################################

				
					
			except Exception as exc:
				print(gameID)
				print(eventID)
				print(exc)
			index = index + 1
		#print(str(count)+','+str(firstHalfEndMin) + ":" + str(firstHalfEndSec)+','+str(secondHalfEndMin) + ":" + str(secondHalfEndSec))
	f.close()	
	#print results
			

			
if __name__ == '__main__':
	main()
