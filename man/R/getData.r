getSnapQuotesList = function(sessionid, source, stocks){
	
	xmlResult = postForm(paste('https://apis.tdameritrade.com/apps/100/Quote;jsessionid=',sessionid,'?source=',source,'&symbol=',paste(stocks,  collapse=','),sep=''), style='post')
	#put it into a better list format
	resultsOK(xmlResult)
	xmlList = xmlToList(gsub("-", ".", xmlResult))
	quotes = which(names(xmlList$quote.list)=="quote")
	quotesList = vector("list", length(quotes))
	quotesNames = rep("", length(quotes))
	for(q in quotes){
		quotesList[[which(quotes==q)]] = formatQuote(xmlList$quote.list[[q]])
		quotesNames[which(quotes==q)] = xmlList$quote.list[[q]]$symbol
		
	}
	names(quotesList) = quotesNames
	return(quotesList)

}

getNewsQuotesList = function(sessionid, source,stocks){

	xmlResult = postForm(paste('https://apis.tdameritrade.com/apps/100/QuoteNews;jsessionid=',sessionid,'?source=',source,'&symbol=',paste(stocks, collapse=","),sep=''), style='post')
}
getAccountBalances = function(sessionid, source){

	xmlResult = postForm(paste('https://apis.tdameritrade.com/apps/100/BalancesAndPositions;jsessionid=',sessionid,'?source=',source,'&symbol=',paste(stocks, collapse=","),sep=''), style='post')
	resultsOK(xmlResult)
	return(xmlToList(gsub("-",".",xmlResult)))

}

formatQuote = function(xmlResponse){
        xmlS = unlist(xmlResponse)
        bidask = strsplit(xmlS["bid.ask.size"], "X")
        xmlS["last.trade.date"] = gsub("\\.", "-", xmlS["last.trade.date"])
        namesS = c(names(xmlS), "bid.size", "ask.size")
        xmlS = c(xmlS, bidask$bid.ask.size[1])
        xmlS = c(xmlS, bidask$bid.ask.size[2])
        xmlS = xts(matrix(xmlS, nrow=1), order.by=as.POSIXct(xmlS["last.trade.date"]))
        colnames(xmlS) = namesS
        return( xmlS[-c(19,20)])

}

resultsOK = function(xmlResult){
	if(xmlToList(xmlResult)$result=="OK"){
		return(T)
	}else{
		stop('error in result retrieval')
	}
	
}

getHistory = function(sessionid, source, stocks, startdate, enddate,..., url='https://apis.tdameritrade.com/apps/100/PriceHistory;jsession=sessionid?source=sourceid&startdate=datestart&enddate=dateend&requestvalue=stocks&intervaltype=MINUTE&extended=true&intervalduration=1&requestidentifiertype=SYMBOL', cl=F){
	hist = vector("list", length(stocks))
	names(hist) = stocks
		getHistData = function(stock){
			cat('getting data for ', stock, '...')
			url2 = url
			url2=gsub("sessionid",sessionid, url2)
			url2=gsub("sourceid",source,url2)
			url2=gsub("stocks",paste(stock, collapse=", "), url2)
			url2=gsub("dateend",gsub("-","",enddate),url2)
			url2=gsub("datestart",gsub("-","",startdate),url2)
			byteResult=postForm(url2, style='post')
			#and now for the real fun... parsing
			symlen = hextobindec(byteResult[5:6]) 
			sym =hextochar(byteResult[7:(7+symlen-1)])
			symerr = byteResult[7+symlen]
			if(symerr!=0){
				stop('error retrieving data')
			} 		
			barcount = hextobindec(byteResult[(7+symlen+1):(7+symlen+1+3)])
			barstart = 7+symlen+1+4
			data = matrix(rep(0,6*barcount), nrow=barcount)
			colnames(data) = c("close","high","low","first","volume","timestamp")
			barseq =seq(barstart, length(byteResult)-2, by=28)
			###get the repeating price data
			cat('parsing data ... ')
			for(i in 1:(barcount)){
				datatemp=try(getBarData(byteResult[barseq[i]:(barseq[i]+27)]))
				if(class(datatemp)!="try-error"){
					data[i,] = datatemp
				}
				
			}	
			#rudimentary data cleaning
			cat('cleaning data ... \n')
			off = which(data[,6]<1.26e+12)
			#off = off[-which(off==1 | off==nrow(data))]
			#datad[off,6] = (datad[off+1,6]+datad[off-1,6])/2
			data = data[-off,]
			datad = xts(data, order.by=as.POSIXct(data[,6]/1000, origin="1970-01-01", tz="GMT"))
			return(datad)
		}
		#datad = datad[-which(index(datad)<as.POSIXct(startdate, tz="GMT")),]	
		if(class(cl)!="logical"){
			results = clusterApplyLB(cl, stocks, getHistData)
		}else{
			results = lapply(stocks, getHistData)
		}	
		for(r in 1:length(results)){
			datad = results[[r]]
			if(class(datad)!="try-error"){
				hist[[stocks[r]]] = datad
			}
		}
	
	return(hist)
}
getHistoryBigMem = function(sessionid, source, stocks, startdate, enddate,..., url='https://apis.tdameritrade.com/apps/100/PriceHistory;jsession=sessionid?source=sourceid&startdate=datestart&enddate=dateend&requestvalue=stocks&intervaltype=MINUTE&extended=true&intervalduration=1&requestidentifiertype=SYMBOL', cl=F){
	hist = vector("list", length(stocks))
	names(hist) = stocks
		getHistData = function(stock){
			cat('getting data for ', stock, '...')
			url2 = url
			url2=gsub("sessionid",sessionid, url2)
			url2=gsub("sourceid",source,url2)
			url2=gsub("stocks",paste(stock, collapse=", "), url2)
			url2=gsub("dateend",gsub("-","",enddate),url2)
			url2=gsub("datestart",gsub("-","",startdate),url2)
			byteResult=postForm(url2, style='post')
			#and now for the real fun... parsing
			symlen = hextobindec(byteResult[5:6]) 
			sym =hextochar(byteResult[7:(7+symlen-1)])
			symerr = byteResult[7+symlen]
			if(symerr!=0){
				stop('error retrieving data')
			} 		
			barcount = hextobindec(byteResult[(7+symlen+1):(7+symlen+1+3)])
			barstart = 7+symlen+1+4
			data = matrix(rep(0,6*barcount), nrow=barcount)
			colnames(data) = c("close","high","low","first","volume","timestamp")
			barseq =seq(barstart, length(byteResult)-2, by=28)
			###get the repeating price data
			cat('parsing data ... ')
			for(i in 1:(barcount)){
				datatemp=try(getBarData(byteResult[barseq[i]:(barseq[i]+27)]))
				if(class(datatemp)!="try-error"){
					data[i,] = datatemp
				}
				
			}	
			#rudimentary data cleaning
			cat('cleaning data ... \n')
			off = which(data[,6]<1.26e+12)
			#off = off[-which(off==1 | off==nrow(data))]
			#datad[off,6] = (datad[off+1,6]+datad[off-1,6])/2
			data = data[-off,]
			datad = xts(data, order.by=as.POSIXct(data[,6]/1000, origin="1970-01-01", tz="GMT"))
			return(datad)
		}
		#datad = datad[-which(index(datad)<as.POSIXct(startdate, tz="GMT")),]	
		if(class(cl)!="logical"){
			results = clusterApplyLB(cl, stocks, getHistData)
		}else{
			results = lapply(stocks, getHistData)
		}	
		for(r in 1:length(results)){
			datad = results[[r]]
			if(class(datad)!="try-error"){
				hist[[stocks[r]]] = datad
			}
		}
	
	return(hist)
}
hexTo32bitfloat = function(byteArray){
	bin = as.numeric(strsplit(paste(digitsBase(strtoi(byteArray, base=16L)), collapse=""),"")[[1]])
	s = (-1)^bin[1]
	exponent = sum(bin[2:9]*(2^(7:0)))-127
	dec = 1+sum(bin[10:32]*(.5^(1:23)))
	float = s*dec*2^exponent
	return(float)
}
hextobindec = function(byteArray){
	bin = as.numeric(strsplit(paste(digitsBase(strtoi(byteArray, base=16L)), collapse=""),"")[[1]])
	return(sum(bin*(2^((length(bin)-1):0))))
}
hextochar = function(byteArray){
	return(rawToChar(as.raw(strtoi(byteArray, base=16L))))

}

getBarData = function(byteArray){
	cl = hexTo32bitfloat(byteArray[1:4])
	hi = hexTo32bitfloat(byteArray[5:8])
	lo = hexTo32bitfloat(byteArray[9:12])
	op = hexTo32bitfloat(byteArray[13:16])
	vol = hexTo32bitfloat(byteArray[17:20])
	time=hextobindec(byteArray[21:28])
	return(abs(c(cl, hi, lo, op, vol, time)))	
}
