TDALogin = function(source, version, user, pass, ..., url='https://apis.tdameritrade.com/apps/100/LogIn?'){
	url = paste(url, 'source=', source, '&version=', version, sep='')
	xmlResult = postForm(url, userid=user, password=pass, source=source, version=version, style="post")
	resultsOK(xmlResult)
	
	return(xmlToList(gsub("-",".",xmlResult))$xml.log.in$session.id)


}

TDALogout = function(source, ..., url='https://apis.tdameritrade.com/apps/100/LogOut?source='){
	xmlResult = postForm(paste(url, source, sep=''), style='post')
	return(xmlResult)
}
