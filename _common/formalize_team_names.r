# Replace mlssoccer.com standard team names with more formal names
# Input: dt (data.table) : data.table containing variables to replace
# Output: tmp (data.table) : data.table with variables replaced
# Variables that get replaced are specified in object 'vars'
formalizeTeamNames = function(dt) { 
	tmp = copy(dt)
	vars = c('team', 'team_away', 'team_home', 'winner')
	for (var in vars) {
		if (!var %in% names(dt)) next
		tmp[, tmpVar:=get(var)]
		tmp[tmpVar=='san-jose', tmpVar:='San Jose']
		tmp[tmpVar=='columbus', tmpVar:='Columbus']
		tmp[tmpVar=='la-galaxy', tmpVar:='LA Galaxy']
		tmp[tmpVar=='sporting-kc', tmpVar:='Sporting KC']
		tmp[tmpVar=='tampa-bay-mutiny', tmpVar:='Tampa Bay Mutiny']
		tmp[tmpVar=='fc-dallas', tmpVar:='FC Dallas']
		tmp[tmpVar=='d.c.-united', tmpVar:='DC United']
		tmp[tmpVar=='ny-red-bulls', tmpVar:='NY Red Bulls']
		tmp[tmpVar=='colorado', tmpVar:='Colorado']
		tmp[tmpVar=='new-england', tmpVar:='New England']
		tmp[tmpVar=='miami', tmpVar:='Miami']
		tmp[tmpVar=='chicago', tmpVar:='Chicago']
		tmp[tmpVar=='chivas-usa', tmpVar:='Chivas USA']
		tmp[tmpVar=='real-salt-lake', tmpVar:='Real Salt Lake']
		tmp[tmpVar=='houston', tmpVar:='Houston']
		tmp[tmpVar=='toronto-fc', tmpVar:='Toronto FC']
		tmp[tmpVar=='seattle', tmpVar:='Seattle Sounders']
		tmp[tmpVar=='philadelphia', tmpVar:='Philadelphia'] 
		tmp[tmpVar=='vancouver', tmpVar:='Vancouver']
		tmp[tmpVar=='portland', tmpVar:='Portland']
		tmp[tmpVar=='montreal', tmpVar:='Montreal']
		tmp[tmpVar=='orlando', tmpVar:='Orlando']
		tmp[tmpVar=='nycfc', tmpVar:='NYCFC']
		tmp[, (var):=tmpVar]
		tmp$tmpVar = NULL
	}
	return(tmp)
}
