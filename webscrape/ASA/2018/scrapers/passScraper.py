#Date: 1/16/2016
#Kevin Minkus
#Scrape whoscored json for raw passes

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
	
	
	f = open('raw passes.csv','w+')
		#s = open('tracker.csv','w+')
	header= "date,gameID,eventID,teamEventId,time,half,passer,team,recipient,team.1,success,x,y,endX,endY,longball,cross,headpass,throughball,freekick,corner,throwin,assist,secondAssist,keyPass,bigChanceCreated,hteam,ateam,hscore,ascore,hplayers,aplayers,hfinal,afinal,final"
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
		count = 0
		for e in events:
			count = count + 1
			try: 
				eID = e["type"]["value"]
				eventID = e["id"]
				teamEventID = e['eventId']
			
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
				
				#handle passes
				if eID == 1:
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
					endX = e["endX"]
					endY = e["endY"]
					# figure out, roughly, who received the pass
					
					recipient = "NA"
					if success == 1:
						try:
							for ev in events[count:]:
								if (ev['teamId'] == homeID and team == homeTeam) or (ev['teamId'] == awayID and team == awayTeam):
									recipient = playerDict[str(ev["playerId"])]
									break
						except Exception as exce:
							recipient = "NA"
							print(exce)
					
					longball = 0
					cross = 0
					headpass = 0
					throughball = 0
					freekick = 0
					corner = 0
					throwin = 0
					assist = 0
					secondAssist = 0
					bigChanceCreated = 0
					keyPass = 0
					for q in e["qualifiers"]:
						if q["type"]["value"] == 1:
							longball = 1
						elif q["type"]["value"] == 2:
							cross = 1
						elif q["type"]["value"] == 3:
							headpass = 1
						elif q["type"]["value"] == 4:
							throughball = 1
						elif q["type"]["value"] == 5:
							freekick = 1
						elif q["type"]["value"] == 6:
							corner = 1
						elif q["type"]["value"] == 107:
							throwin = 1
						elif q["type"]["value"] == 210 or q["type"]["value"] == 154:
							assist = 1
						elif q["type"]["value"] == 218:
							secondAssist = 1
						elif q["type"]["value"] == 11113:
							keyPass = 1
						elif q["type"]["value"] == 11112:
							bigChanceCreated = 1
							
					
				
				
					line = str(date)+','+str(gameID) +','+ str(eventID) + ',' + str(teamEventID)+','+ str(minute)+':'+str(second)+','+str(half)+','+unidecode(player)+','+str(team)+','+ unidecode(recipient) + ',' +str(team1)+','+str(success)+ ',' +str(xCoord)+','+str(yCoord)+ ',' + str(endX) + ',' +str(endY) + ','+str(longball)+','+str(cross)+','+str(headpass) + ',' + str(throughball) + ',' + str(freekick) + ',' + str(corner) + ',' + str(throwin) + ',' + str(assist) + ',' + str(secondAssist) +',' + str(keyPass) +',' + str(bigChanceCreated) +','+str(homeTeam)+','+str(awayTeam)+','+str(hscore)+','+str(ascore)+','+str(hplayers)+','+str(aplayers)+','+str(hfinal)+','+str(afinal)+','+str(final)
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

