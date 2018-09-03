#Date: 1/16/2016
#Kevin Minkus
#Scrape whoscored json for general game information

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
	
	#idList = [[914828]]
	
	f = open('Game Information.csv','w+')
		#s = open('tracker.csv','w+')
	header= "gameID,date,startTime,firstHalfTime,secondHalfTime,ref,attendance,hteam,hformation,ateam,aformation,hfinal,afinal,hhalf,ahalf"
	print(header,file=f)
	try:
		for gameIDL in idList:
			gameID = gameIDL[0]
			with open('/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/Game ' + str(gameID) + '.json','rb') as data_file:    
    				data = json.load(data_file)

			#handle the home team
			home = data["home"]#can use this later to get good stuff
			hformation = home["formations"][0]["formationName"]
			homeTeam = home["name"]	
			
			#handle the away team
			away = data["away"]
			aformation = away["formations"][0]["formationName"]
			awayTeam = away["name"]
			
			#get the general stuff
			date = data["startDate"].split(" ")[0]
			if len(date.split("T")) > 1:
				date = date.split("T")[0]
				date = date[5:7] + "/" + date[8:10] + "/" + date[0:4]
			starttime = data["startTime"][11:]
			#assumes no double digit score
			hfinal = data["score"][0]
			afinal = data["score"][4]
			hhalf = data["htScore"][0]
			ahalf = data["htScore"][4]
			attendance = data["attendance"]
			try:
				ref = data["refereeName"]
			except Exception as exp:
				try:
					ref = data["referee"]["name"]
				except Exception as exp:
					ref = data['referee']['firstName'] + ' ' + data['referee']['lastName']
			
			#walk through the events to get half time and full time
			half = 1
			for e in data["events"]:
				eID = e["type"]["value"]
				eventID = e["id"]
				#figure out when the end of a half is
				if eID == 30:
					half = e["period"]["value"]
					if half == 1:
						firstHalfEndMin = e["minute"]
						firstHalfEndSec = e["second"]
						half = 2
						if len(str(firstHalfEndSec)) == 1:
							firstHalfEndSec = '0' + str(firstHalfEndSec)
					if half == 2:
						secondHalfEndMin = e["minute"]
						secondHalfEndSec = e["second"]
						if len(str(secondHalfEndSec)) == 1:
							secondHalfEndSec = '0' + str(secondHalfEndSec) 
			
			line =  str(gameID)+ ',' + date + ',' + starttime + ',' + str(firstHalfEndMin) + ":" + str(firstHalfEndSec) + "," + str(secondHalfEndMin) + ":" + str(secondHalfEndSec) + ',' + unidecode(ref) + ',' + str(attendance) + ',' + homeTeam + ',' + str(hformation) + ',' + awayTeam + ',' + str(aformation) + ',' + str(hfinal) + ',' + str(afinal) + ',' + str(hhalf) + ',' + str(ahalf)
			print(line,file=f)
		

		#print(line,file=f)
	except Exception as exc:
		print(gameID)
		print(exc)

	f.close()	
	#print results
			

			
if __name__ == '__main__':
	main()
