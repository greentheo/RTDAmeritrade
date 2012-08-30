% File man/getSnapQuotesList.Rd
     \name{getSnapQuotesList}
     \alias{getSnapQuotesList}
     \title{Retrieve a list of current quotes from the API}
     \description{
      Given a set of symbols, retrieve the current quote for them.
     }
     \usage{
     getSnapQuotesList(sessionid, source, stocks)
     }
     \arguments{
       \item{sessionid}{sessionid returned from TDALogin.}
       \item{source}{The TD Ameritrde supplied source, similar to a user name but for the API.}
       \item{stocks}{A collection of stock symbol strings.}
     }
     \seealso{
       \code{\link{getHistory}}.
     }
     \examples{
     ##login and get snap quotes
     \dontrun{
     user="username"
     pass="password"
     source="TSLU"
     version="1.0"
     log = try(TDALogin(source, version, user, pass))
     print(log)
     
     stocks = c("RYAAY","WEC","HRL","SRCL","DVA","FDO","SPY","DIA", "QQQ")
     quotesList = getSnapQuotesList(log, source, stocks)
     
     print(quotesList)
     }
     }
     \keyword{quotes}