#Date: 1/16/2016
#Kevin Minkus
#Scrape whoscored json for raw shots

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
	
	f = open('raw shots.csv','w+')
		#s = open('tracker.csv','w+')
	header= "date,gameID,eventID,time,half,shooter,team,goalie,team.1,passer,assisted,passID,through,cross,x,y,gmlocy,gmlocz,bodypart,result,patternOfPlay,hteam,ateam,hscore,ascore,hplayers,aplayers,hfinal,afinal,final,bigchance"
	print(header,file=f)
	#idList = [[1041735]]
	for gameIDL in idList:
		gameID = gameIDL[0]
		with open('/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/Game '+ str(gameID) + '.json','rb') as data_file:    
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
				if (gameID == "1256416") and (e['minute'] >= 33):
					hgoalie = 357065
					hgoaliename = 'Paul Christensen'

				if (gameID == "1256721") and (e['minute'] >= 19):
					agoalie = 99246
					agoaliename = 'Zac MacMath'
				
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
				###############################################################################

				#if else eID == 15: #shot saved, qualifier 82 for blocked shot
				if eID == 16 or eID == 13 or eID == 14 or eID == 15:
					gmlocy = 'NA'
					gmlocz = 'NA'
					result = ''
					if eID == 16:
						result = "Goal"
				
					if eID == 15:
						result = "Saved"
						for q in e["qualifiers"]:
							if q['type']['value'] == 82:
								result = "Blocked"
				
					if eID == 13:
						result = "Miss"
				
					if eID == 14:
						result = "Post"
				
					goalie = ""
					minute = str(e["minute"])
					second = str(e["second"])
					if len(second) == 1:
						second = '0'+str(second)
					half = e["period"]["value"] #either a 1 or 2
					player = playerDict[str(e["playerId"])]
					xCoord = e["x"]
					yCoord = e["y"]
					if e['teamId'] == homeID:
						team = homeTeam
						team1 = awayTeam
						goalie = agoaliename
						#handle the score update
						if eID == 16:
							hscore = hscore + 1
							result = "Goal"
					else:
						team = awayTeam
						team1 = homeTeam
						goalie = hgoaliename
						if eID == 16:
							ascore = ascore + 1
							result = "Goal"
					qualifiers = e["qualifiers"]
					assisted = 0
					patternofPlay = ""
					bodyPart = ""
					ownGoal = 0
					bigChance = 0
					passer = "NA"
					cross = 0
					throughball = 0
					passId = 0
					for q in qualifiers:
						#handle if it was an assist
						if q["type"]["value"] == 29:
							assisted = 1
							passId = e['relatedEventId']
							try:
								passer = playerDict[str(e["relatedPlayerId"])]
							except Exception as problem:
								passer = 'NA'
							pastPass = events[index-1] #find the pass that led to this shot
							if pastPass["type"]["value"] != 1:
								pastPass = events[index-2] #in case the prior event is the save or block
							if pastPass["type"]["value"] != 1:
								pastPass = events[index-3] #in case it was the result of an aerial
							for qual in pastPass["qualifiers"]:
								if qual["type"]["value"] == 2:
									cross = 1
								if qual["type"]["value"] == 4:
									throughball = 1
						#handle pattern of play
						elif q["type"]["value"] == 22:
							patternOfPlay = "Regular"
						elif q["type"]["value"] == "23":
							patternofPlay = "Fastbreak"
						elif q["type"]["value"] == 24:
							patternOfPlay = "Set piece" 
						elif q["type"]["value"] == 25:
							patternOfPlay = "Corner"
						elif q["type"]["value"] == 26:
							patternOfPlay = "Free kick"
						elif q["type"]["value"] == 160:
							patternOfPlay = "Throw in"
						elif q["type"]["value"] == 9:
							patternOfPlay = "Penalty"
						elif q["type"]["value"] == 23:
							patternOfPlay = "Fastbreak"
						#handle body part
						elif q["type"]["value"] == 15:
							bodyPart = "Head"
						elif q["type"]["value"] == 20:
							bodyPart = "Right foot"
						elif q["type"]["value"] == 72:
							bodyPart = "Left foot"
						elif q["type"]["value"] == 21:
							bodyPart = "Other"
						#handle own goal
						elif q["type"]["value"] == 28:
							ownGoal = 1
							hscore = hscore-1
							ascore = ascore + 1
						#handle big chance
						elif q["type"]["value"] == 214:
							bigChance = 1
						#handle where it crossed the goal line
						elif q["type"]["value"] == 102:
							gmlocy = q["value"]
						elif q["type"]["value"] == 103:
							gmlocz = q["value"]
				
					#clumsy fix for handling scoring
					if result == 'Goal':
						if team == homeTeam:
							hOutScore = hscore -1
							aOutScore = ascore
						else:
							aOutScore = ascore - 1
							hOutSocre = hscore
						
					else:
						hOutScore = hscore
						aOutScore = ascore
				
					line = str(date)+','+str(gameID) +','+ str(eventID) +','+ str(minute)+':'+str(second)+','+str(half)+','+unidecode(player)+','+str(team)+','+unidecode(goalie)+','+str(team1)+','+unidecode(passer)+','+str(assisted)+',' + str(passId) + ',' +str(throughball)+','+str(cross)+','+str(xCoord)+','+str(yCoord)+','+str(gmlocy)+','+str(gmlocz)+','+bodyPart+','+str(result)+','+str(patternOfPlay)+','+str(homeTeam)+','+str(awayTeam)+','+str(hOutScore)+','+str(aOutScore)+','+str(hplayers)+','+str(aplayers)+','+str(hfinal)+','+str(afinal)+','+str(final)+','+str(bigChance)
					if ownGoal == 0:
						#TODO handle own goals
						print(line,file=f)
						count = count + 1
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
