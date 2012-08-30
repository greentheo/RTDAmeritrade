% File man/TDALogin.Rd
     \name{TDALogin}
     \alias{TDALogin}
     \title{Login to TDA Ameritrade API}
     \description{
      Perform authentication and return a session ID with the TD Ameritrade API
       .
     }
     \usage{
     TDALogin(source, version, user, pass, ..., url='https://apis.tdameritrade.com/apps/100/LogIn?')
     }
     \arguments{
       \item{source}{The TD Ameritrde supplied source, similar to a user name but for the API}
       \item{version}{Version in the form of "1.0", for TDAmeritrades app logging purposes.}
       \item{user}{Your username}
       \item{pass}{Your password}
       \item{...}{The url is set, but you can change it if needed}
       \item{url}{In case the API addresses change and a new one is needed}
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
     }
     }
     \keyword{login}
