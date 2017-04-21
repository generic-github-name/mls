# Replace ASA team names with the short standard ones
# Input: dt (data.table) : data.table containing variables to replace
# Output: tmp (data.table) : data.table with variables replaced
# Variables that get replaced are specified in object 'vars'
convertTeamNamesASA = function(dt) { 
	tmp = copy(dt)
	vars = c('team', 'team_away', 'team_home', 'winner')
	outname = NA
	for (var in vars) {
		if (!var %in% names(dt)) next
		tmp[, tmpVar:=get(var)]
		tmp[tmpVar=='San Jose', tmpVar:='san-jose']
		tmp[tmpVar=='Columbus', tmpVar:='columbus']
		tmp[tmpVar=='L.A. Galaxy', tmpVar:='la-galaxy']
		tmp[tmpVar=='Kansas City', tmpVar:='sporting-kc']
		tmp[tmpVar=='FC Dallas', tmpVar:='fc-dallas']
		tmp[tmpVar=='DC United', tmpVar:='d.c.-united']
		tmp[tmpVar=='New York', tmpVar:='ny-red-bulls']
		tmp[tmpVar=='Colorado', tmpVar:='colorado']
		tmp[tmpVar=='New England', tmpVar:='new-england']
		tmp[tmpVar=='Chicago', tmpVar:='chicago']
		tmp[tmpVar=='Salt Lake', tmpVar:='real-salt-lake']
		tmp[tmpVar=='Houston', tmpVar:='houston']
		tmp[tmpVar=='Toronto', tmpVar:='toronto-fc']
		tmp[tmpVar=='Seattle', tmpVar:='seattle']
		tmp[tmpVar=='Philadelphia', tmpVar:='philadelphia']
		tmp[tmpVar=='Vancouver', tmpVar:='vancouver']
		tmp[tmpVar=='Portland', tmpVar:='portland']
		tmp[tmpVar=='Montreal', tmpVar:='montreal']
		tmp[tmpVar=='Orlando City', tmpVar:='orlando']
		tmp[tmpVar=='New York City FC', tmpVar:='nycfc']
		tmp[tmpVar=='Minnesota United', tmpVar:='minnesota']
		tmp[tmpVar=='Atlanta United', tmpVar:='atlanta']
		tmp[, (var):=tmpVar]
		tmp$tmpVar = NULL
	}
	return(tmp)
}
