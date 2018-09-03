#Date: 7/27/2018
#Jamon Moore based on scraper code written by Kevin Minkus
#Scrape whoscored json for raw player errors leading to goal or shot

#TODO can I traverse things in order like this? Or do I need time stamps? I'll have to evaluate this
#import necessary packages
from __future__ import print_function
from unidecode import unidecode
#this handles our accented characters
import json
import re
import csv


def main():
	with open('../gameIDs.csv', 'r') as fi:
    		reader = csv.reader(fi)
    		idList = list(reader)
	
	
	f = open('raw errors.csv','w+')
		#s = open('tracker.csv','w+')
	header= "date,gameID,eventID,time,half,player,team,action,team.1,outcome,x,y,hteam,ateam,hscore,ascore,hplayers,aplayers,hfinal,afinal,final,errorType"
	print(header,file=f)
	
	for gameIDL in idList:
		gameID = gameIDL[0]
		#Kevin Minkus does this
		with open('/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/Game '+ str(gameID) + '.json','rb') as data_file:
		
		#Jamon Moore had to do this with Python 3.6
		#with open('C:/Users/jpmoore.ORADEV/Documents/Personal/Soccer/Analysis/ASA/Scraping/2018/Opta JSON Data/Game '+ str(gameID) + '.json', encoding='ISO-8859-1') as data_file:
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
		count = 0
		for e in events:
			count = count + 1
			try: 
				eID = e["type"]["value"]
				eventID = e["id"]
			
				#figure out when the end of a half is
				if eID == 30:
					half = e["period"]["value"]
					if half == 1:
						firstHalfEndMin = e["minute"]
						firstHalfEndSec = e["second"]
						if len(str(firstHalfEndSec)) == 1:
							firstHalfEndSec = '0' + str(firstHalfEndSec)
					if half == 2:
						secondHalfEndMin = e["minute"]
						secondHalfEndSec = e["second"]
						if len(str(secondHalfEndSec)) == 1:
							secondHalfEndSec = '0' + str(secondHalfEndSec)

				#handle a goalie substitution
				if eID == 18 and e["playerId"] == hgoalie:
					hgoalie = e["relatedPlayerId"]
					hgoaliename = playerDict[str(hgoalie)]

				if eID == 18 and e["playerId"] == agoalie:
					agoalie = e["relatedPlayerId"]
					agoaliename = playerDict[str(agoalie)]
			
				#handle the rare case a player leaves early and no subs are left############### haven't tested this yet
				#also handle goalies getting sent off- TODO figure this part out
				if eID == 20:
					if e['teamId'] == homeID:
						hplayers = hplayers - 1
					else:
						aplayers = aplayers - 1
				#handle a red card or second yellow#####################################
				if eID == 17:
					teamid = e['teamId']
					#check what type of card it is
					for q in e['qualifiers']:
						#second yellow or red, respectively, and check to make sure its not the coach- TODO what if sub?
						if (q['type']['value'] == 32 or q['type']['value'] == 33) and "playerId" in e.keys():
							if teamid == homeID:
								hplayers = hplayers - 1
							else:
								aplayers = aplayers - 1

				#handle updates for scoring
				if eID == 16:
					ownGoal = 0
					for q in e["qualifiers"]:
						if q["type"]["value"] == 28:
							ownGoal = 1
					if e['teamId'] == homeID:
						if ownGoal == 0:
							hscore = hscore + 1
						else:
							ascore = ascore + 1
							
					else:
						if ownGoal == 0:
							ascore = ascore + 1
						else:
							hscore = hscore + 1

				#handle the error and type of error

				action = ""
				errorType = 0

				if eID == 51:
					action = "playerError"
					for q in e["qualifiers"]:
						#error led to shot
						if q["type"]["value"] == 169:
							errorType = 2
						#error led to goal
						elif q["type"]["value"] == 170:
							errorType = 1
							
					if e['teamId'] == homeID:
						team = homeTeam
						team1 = awayTeam
					else:
						team = awayTeam
						team1 = homeTeam

					minute = e["minute"]
					second = e["second"]
					if len(str(second)) == 1:
						second = '0'+str(second)

					half = e["period"]["value"]
					xCoord = e["x"] 
					yCoord = e["y"]
					success = e["outcomeType"]["value"]
					player = playerDict[str(e["playerId"])]
					
					if e['teamId'] == homeID:
						team = homeTeam
						team1 = awayTeam
					else:
						team = awayTeam
						team1 = homeTeam
					
					
					
				
				
					line = str(date)+','+str(gameID) +','+ str(eventID) +','+ str(minute)+':'+str(second)+','+str(half)+','+unidecode(player)+','+str(team)+ ',' + action + ',' +str(team1)+','+str(success)+ ',' +str(xCoord)+','+str(yCoord) +','+str(homeTeam)+','+str(awayTeam)+','+str(hscore)+','+str(ascore)+','+str(hplayers)+','+str(aplayers)+','+str(hfinal)+','+str(afinal)+','+str(final)+','+str(errorType)
					print(line,file=f)
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

