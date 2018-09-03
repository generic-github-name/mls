# Replace mlssoccer.com standard team names with more formal names
# Input: dt (data.table) : data.table containing variables to replace
# Output: tmp (data.table) : data.table with variables replaced
# Variables that get replaced are specified in object 'vars'
formalizeTeamNames = function(dt) { 
	tmp = copy(dt)
	vars = c('team', 'team_away', 'team_home', 'winner', 'hteam', 'ateam')
	for (var in vars) {
		if (!var %in% names(dt)) next
		tmp[, tmpVar:=get(var)]
		tmp[tmpVar=='san-jose' | tmpVar=='San Jose Earthquakes', tmpVar:='San Jose']
		tmp[tmpVar=='columbus' | tmpVar=='Columbus Crew', tmpVar:='Columbus']
		tmp[tmpVar=='la-galaxy' | tmpVar=='L.A. Galaxy' | tmpVar=='Los Angeles Galaxy', tmpVar:='LA Galaxy']
		tmp[tmpVar=='sporting-kc' | tmpVar=='Kansas City' | tmpVar=='Sporting Kansas City', tmpVar:='Sporting KC']
		tmp[tmpVar=='tampa-bay-mutiny', tmpVar:='Tampa Bay Mutiny']
		tmp[tmpVar=='fc-dallas', tmpVar:='FC Dallas']
		tmp[tmpVar=='d.c.-united', tmpVar:='DC United']
		tmp[tmpVar=='ny-red-bulls' | tmpVar=='New York' | tmpVar=='New York Red Bulls', tmpVar:='NY Red Bulls']
		tmp[tmpVar=='colorado' | tmpVar=='Colorado Rapids', tmpVar:='Colorado']
		tmp[tmpVar=='new-england' | tmpVar=='New England Revolution', tmpVar:='New England']
		tmp[tmpVar=='miami', tmpVar:='Miami']
		tmp[tmpVar=='chicago' | tmpVar=='Chicago Fire', tmpVar:='Chicago']
		tmp[tmpVar=='chivas-usa', tmpVar:='Chivas USA']
		tmp[tmpVar=='real-salt-lake' | tmpVar=='Salt Lake', tmpVar:='Real Salt Lake']
		tmp[tmpVar=='houston' | tmpVar=='Houston Dynamo', tmpVar:='Houston']
		tmp[tmpVar=='toronto-fc' | tmpVar=='Toronto', tmpVar:='Toronto FC']
		tmp[tmpVar=='seattle' | tmpVar=='Seattle', tmpVar:='Seattle Sounders']
		tmp[tmpVar=='philadelphia' | tmpVar=='Philadelphia Union', tmpVar:='Philadelphia'] 
		tmp[tmpVar=='vancouver' | tmpVar=='Vancouver Whitecaps', tmpVar:='Vancouver']
		tmp[tmpVar=='portland' | tmpVar=='Portland Timbers', tmpVar:='Portland']
		tmp[tmpVar=='montreal' | tmpVar=='Montreal Impact', tmpVar:='Montreal']
		tmp[tmpVar=='orlando' | tmpVar=='Orlando City', tmpVar:='Orlando']
		tmp[tmpVar=='nycfc' | tmpVar=='New York City FC' | tmpVar=='New York City', tmpVar:='NYCFC']
		tmp[tmpVar=='atlanta', tmpVar:='Atlanta United']
		tmp[tmpVar=='minnesota' | tmpVar=='Minnesota', tmpVar:='Minnesota United']
		tmp[, (var):=tmpVar]
		tmp$tmpVar = NULL
	}
	return(tmp)
}
