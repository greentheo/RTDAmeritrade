% File man/TDALogout.Rd
     \name{TDALogout}
     \alias{TDALogout}
     \title{Close a TDA Ameritrade API session}
     \description{
      Close the TD Ameritrade API session which invalidates the current session ID.
     }
     \usage{
     TDALogout(source, ..., url='https://apis.tdameritrade.com/apps/100/LogOut?source=')
     }
     \arguments{
       \item{source}{The TD Ameritrde supplied source, similar to a user name but for the API}
       \item{...}{The URL is set, but can be changed if needed}
       \item{url}{The URL used to complete the logout and close the session}
     }
     \seealso{
       \code{\link{TDALogout}}.
     }
     \examples{
     
     \dontrun{
     source="TSLU"
     TDALogout(source)
     print(log)
     }
     }
     \keyword{login}
