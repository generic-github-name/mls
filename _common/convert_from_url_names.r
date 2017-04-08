# Replace mlssoccer.com URL team names with the short standard ones
# Input: inName (character) : url name
# Output: outname (character) : standard name
convertTeamNamesRev = function(inName) { 
	outname = NA
	if(inName=='san-jose-earthquakes')	outname = 'san-jose'
	if(inName=='columbus-crew-sc')	outname = 'columbus'
	if(inName=='la-galaxy')	outname = 'la-galaxy'
	if(inName=='sporting-kansas-city')	outname = 'sporting-kc'
	if(inName=='')	outname = 'tampa-bay-mutiny'
	if(inName=='fc-dallas')	outname = 'fc-dallas'
	if(inName=='dc-united')	outname = 'd.c.-united'
	if(inName=='new-york-red-bulls')	outname = 'ny-red-bulls'
	if(inName=='colorado-rapids')	outname = 'colorado'
	if(inName=='new-england-revolution')	outname = 'new-england'
	if(inName=='chicago-fire')	outname = 'chicago'
	if(inName=='cd-chivas-usa')	outname = 'chivas-usa'
	if(inName=='real-salt-lake')	outname = 'real-salt-lake'
	if(inName=='houston-dynamo')	outname = 'houston'
	if(inName=='toronto-fc')	outname = 'toronto-fc'
	if(inName=='seattle-sounders-fc')	outname = 'seattle'
	if(inName=='philadelphia-union')	outname = 'philadelphia'
	if(inName=='vancouver-whitecaps-fc')	outname = 'vancouver'
	if(inName=='portland-timbers')	outname = 'portland'
	if(inName=='montreal-impact')	outname = 'montreal'
	if(inName=='orlando-city-sc')	outname = 'orlando'
	if(inName=='new-york-city-fc')	outname = 'nycfc'
	if(inName=='minnesota-united-fc')	outname = 'minnesota'
	if(inName=='atlanta-united-fc')	outname = 'atlanta'
	return(outname)
}
