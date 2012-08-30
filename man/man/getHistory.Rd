% File man/getHistory.Rd
     \name{getHistory}
     \alias{getHistory}
     \title{Get full history for a symbol}
     \description{
      Take the session id, and a list of symbols, fetch and parse the bytecode data into a list suitable for using in R.
     }
     \usage{
     getHistory(sessionid, source, stocks, startdate, enddate, ..., url='https://apis.tdameritrade.com/apps/100/PriceHistory;jsession=sessionid?source=sourceid&startdate=datestart&enddate=dateend&requestvalue=stocks&intervaltype=MINUTE&extended=true&intervalduration=1&requestidentifiertype=SYMBOL', cl=F)
     }
     \arguments{
       \item{source}{The TD Ameritrde supplied source, similar to a user name but for the API.}
       \item{sessionid}{The current session id returned from login.}
       \item{stocks}{A collection of symbols to fetch data for.}
       \item{startdate}{start date string Y-m-d e.g. 2012-01-01}
       \item{enddate}{end date string Y-m-d e.g. 2012-02-01}
       \item{...}{extended arguments not needed, but can be changed.}
       \item{url}{url of the TD Ameritrade API to call for the data.}
       \item{cl}{a cluster instance if wanting to perform in parallel.}
     }
     \seealso{
       \code{\link{TDALogin}}.
     }
     \examples{
     ##print the session id returned from the login try
     
     \dontrun{
     user="username"
     pass="password"
     source="TSLU"
     version="1.0"
     log = try(TDALogin(source, version, user, pass))
     print(log)
     
     stocks = c("RYAAY","WEC","HRL","SRCL","DVA","FDO","SPY","DIA", "QQQ")
     
     library(snow)
     cl = makeCluster(8, type="SOCK")
     clusterExport(cl, list("user","pass","source","version","startdate","enddate","url", "log"))
     clusterCall(cl, prepareSlave)

     hist = getHistory(log, source, stocks, startdate=startdate, enddate=enddate, cl=cl)
     }
     }
     \keyword{login}
