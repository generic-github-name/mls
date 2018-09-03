#Date: 7/27/2018
#Jamon Moore based on scraper code written by Kevin Minkus
#Scrape whoscored json for raw keeper actions

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
	
	
	f = open('raw keeper actions.csv','w+')
		#s = open('tracker.csv','w+')
	header= "date,gameID,eventID,time,half,player,team,action,team.1,outcome,x,y,hteam,ateam,hscore,ascore,hplayers,aplayers,hfinal,afinal,final,location,highClaim,keeperDiveDeflect,catchDiveSave,collected,standingSave,divingSave,handsSave,eventScored,eventSaved,eventSavedOffTarget,oppositeEvent"
	print(header,file=f)
	
	for gameIDL in idList:
		gameID = gameIDL[0]
		#Kevin Minkus does this
		with open('/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/Game '+ str(gameID) + '.json','rb') as data_file:
		
		#Jamon Moore needed to do this with Python 3.6
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
				block = 0
				#handle the defensive action
				#handle blocks (which get counted as saves)
				if eID == 10:
					for q in e["qualifiers"]:
						if q["type"]["value"] == 94:
							block = 1
				
				#handle blocked/cleared crosses (which get counted as clearances)
				qualifier = 0
				action = ""
				
				if eID in [10,11,41,52,53,54,58,59]: 
					
					if eID == 10:
						if block == 1:
							action = "playerSave"
						else:
							action = "keeperSave"
					elif eID == 11:
						action = "claim"
					elif eID == 41:
						action = "punch"
					elif eID == 52:
						action = "pick-up"
					elif eID == 53:
						action = "no-claim"
					elif eID == 54:
						action = "smother"
					#elif eID == 55:
					#	action = "offsideProvoked"
					#elif eID == 56:
					#	action = "shieldBall"					
					elif eID == 58:
						action = "penaltyFaced"
					elif eID == 59:
						action = "sweeper"
					else:
						action = str(eID)

					#post = 0
					#playerReturns = 0
					shieldBall = 0
					location = ''
					highClaim = 0
					#keeperDeflect = 0
					keeperDiveDeflect = 0
					#catchSave = 0
					catchDiveSave = 0
					#parry = 0
					#parryOutcome = 0
					fingertip = 0
					caught = 0
					collected = 0
					standingSave = 0
					divingSave = 0
					#stoopingSave = 0
					#reachingSave = 0
					handsSave = 0
					#feetSave = 0
					eventScored = 0
					#eventMissed = 0
					eventSaved = 0
					eventSavedOffTarget = 0
					oppositeEvent = 0
					#unknown = ""
					
					for q in e["qualifiers"]:
						
						qualifier = q["type"]["value"]
						
						#if qualifier == 14:
						#	post = 1
						#elif qualifier == 21:
						#	playerReturns = 1
						if qualifier == 56:
							shieldBall = 1
						elif qualifier == 73:
							location = 'Left'
						elif qualifier == 74:
							location = 'High'				
						elif qualifier == 75:
							location = 'Right' 							
						elif qualifier == 76:
							location = 'Low left'
						elif qualifier == 77:
							location = 'High left'
						elif qualifier == 78:
							location = 'Low center'
						elif qualifier == 79:
							location = 'High center'
						elif qualifier == 80:
							location = 'Low right'
						elif qualifier == 81:
							location = 'High right'						
						elif qualifier == 82:
							location = 'Blocked'						
						elif qualifier == 83:
							location = 'Close left'						
						elif qualifier == 84:
							location = 'Close right'						
						elif qualifier == 85:
							location = 'Close high'
						elif qualifier == 86:
							location = 'Close left and high'
						elif qualifier == 87:
							location = 'Close right and high'
						elif qualifier == 88:
							highClaim = 1
						#elif qualifier == 91:
						#	keeperDeflect = 1
						elif qualifier == 92:
							keeperDiveDeflect = 1
						elif qualifier == 93:
							catchSave = 1
						elif qualifier == 94:
							catchDiveSave = 1
						elif qualifier == 100:
							location = 'Six yard blocked'
						elif qualifier == 101:
							location = 'Saved off line'
						elif qualifier == 173:
							parry = 1
							parryOutcome = 1
						elif qualifier == 174:
							parryOutcome = 2
						elif qualifier == 175:
							fingertip = 1
						elif qualifier == 176:
							caught = 1
						elif qualifier == 177:
							collected = 1
						elif qualifier == 178:
							standingSave = 1
						elif qualifier == 179:
							divingSave = 1
						elif qualifier == 180:
							stoopingSave = 1
						elif qualifier == 181:
							reachingSave = 1
						elif qualifier == 182:
							handsSave = 1
						elif qualifier == 183:
							feetSave = 1
						elif qualifier == 186:
							eventScored = 1
						elif qualifier == 187:
							eventSaved = 1
						elif qualifier == 188:
							eventMissed = 1
						elif qualifier == 190:
							eventSavedOffTarget = 1
						elif qualifier == 198:
							dropKick = 1
						elif qualifier == 199:
							punt = 1
						elif qualifier == 233:
							oppositeEvent = 1
						#elif qualifier == 11115:
						#	unknown = unknown + ',11115'
						#elif qualifier == 11116:
						#	unknown = unknown + ',11116'
						#elif qualifier == 11117:
						#	unknown = unknown = ',11117'

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
				
					if action != "playerSave":
						line = str(date)+','+str(gameID) +','+ str(eventID) +','+ str(minute)+':'+str(second)+','+str(half)+','+unidecode(player)+','+str(team)+ ',' + action + ',' +str(team1)+','+str(success)+ ',' +str(xCoord)+','+str(yCoord) +','+str(homeTeam)+','+str(awayTeam)+','+str(hscore)+','+str(ascore)+','+str(hplayers)+','+str(aplayers)+','+str(hfinal)+','+str(afinal)+','+str(final)+','+str(location)+','+str(highClaim)+','+str(keeperDiveDeflect)+','+str(catchDiveSave)+','+str(collected)+','+str(standingSave)+','+str(divingSave)+','+str(handsSave)+','+str(eventScored)+','+str(eventSaved)+','+str(eventSavedOffTarget)+','+str(oppositeEvent)
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

