# Replace mlssoccer.com standard team names with the names they use in URLs
# Input: inName (character) : standard name
# Output: outName (character) : url name
convertTeamNames = function(inName) { 
	outName = NA
	if(inName=='san-jose') outName='san-jose-earthquakes'
	if(inName=='columbus') outName='columbus-crew-sc'
	if(inName=='la-galaxy') outName='la-galaxy'
	if(inName=='sporting-kc') outName='sporting-kansas-city'
	if(inName=='tampa-bay-mutiny') outName=''
	if(inName=='fc-dallas') outName='fc-dallas'
	if(inName=='d.c.-united') outName='dc-united'
	if(inName=='ny-red-bulls') outName='new-york-red-bulls'
	if(inName=='colorado') outName='colorado-rapids'
	if(inName=='new-england') outName='new-england-revolution'
	if(inName=='chicago') outName='chicago-fire'
	if(inName=='chivas-usa') outName='cd-chivas-usa'
	if(inName=='real-salt-lake') outName='real-salt-lake'
	if(inName=='houston') outName='houston-dynamo'
	if(inName=='toronto-fc') outName='toronto-fc'
	if(inName=='seattle') outName='seattle-sounders-fc'
	if(inName=='philadelphia') outName='philadelphia-union'
	if(inName=='vancouver') outName='vancouver-whitecaps-fc'
	if(inName=='portland') outName='portland-timbers'
	if(inName=='montreal') outName='montreal-impact'
	if(inName=='orlando') outName='orlando-city-sc'
	if(inName=='nycfc') outName='new-york-city-fc'
	if(inName=='minnesota') outName='minnesota-united-fc'
	if(inName=='atlanta') outName='atlanta-united-fc'
	if(inName=='lafc') outName='los-angeles-football-club'
	return(outName)
}
