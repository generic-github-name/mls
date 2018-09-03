#Date: 1/6/2016
#Kevin Minkus
#Grab the JSONs I need for each game file

#import necessary packages
#this reads stuff off the webpages
from __future__ import print_function
from bs4 import BeautifulSoup
from unidecode import unidecode
#this handles our accented characters
from selenium import webdriver
import json
import os.path
import csv
from os import listdir
from os.path import isfile, join
#TODO- figure out unicode encoding


def main():
	#read in the gameIDs, put them into a list to iterate over
	with open('gameIDs.csv', 'r') as fi:
    		reader = csv.reader(fi)
    		idList = list(reader)
	
	save_path = '/home/kcm30/Documents/PracticeR/Whoscored Scraping/2018/Opta JSON Data/'
	#get the games we've already grabbed jsons for
	gamesScraped = [f for f in listdir(save_path) if isfile(join(save_path, f))]
	#remove those games from the list of gameIDs to scrape
	for games in gamesScraped:
		if [games[5:12]] in idList:
			idList.remove([games[5:12]])

	
	#idList = [[1041735]]
	for gameIDL in idList:
		gameID = gameIDL[0]
		try:
			URL = "http://www.whoscored.com/Matches/" + str(gameID) + "/Live"
			driver = webdriver.Chrome()
			#driver.set_page_load_timeout(15)
			driver.get(URL)
			

			html = driver.page_source
			driver.close()
			soup = BeautifulSoup(html)

			#check to make sure the game file is for the right league
			#this href will change depending on the season and league (and if its playoffs)
			#this is the specific href for MLS regular season 2016
			
			script = soup.find_all('script')


			#set up the appropriate file path
			name_of_file = "Game " + str(gameID)
			completeName = os.path.join(save_path, name_of_file+".json")
			#open the document to save the game JSON to
			f = open(completeName,'w+')
			for s in script:
				#this is clunky, but for now it works
				if len(str(s)) > 100000:
					#this minus 4958 is hard coded, too, but for now it works. it might change in the future
					#also figure out how to handle accents
					dataAsString = str(s)[1:len(str(s))-4958] #use unidecode(string,'utf-8') to later decode this properly
					jsonString = '{%s}' % (dataAsString.split('{', 1)[1].rsplit('}', 1)[0],)
					print(jsonString,file=f)
		except Exception as exc:
			print(gameID)
			print(exc)
			



if __name__ == '__main__':
	main()
