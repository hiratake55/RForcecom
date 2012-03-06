\name{rforcecom.login}
\alias{rforcecom.login}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sign in Force.com (Salesforce.com) and retrieve a session ID.
}
\description{
Sign in Force.com (Salesforce.com) and retrieve a session ID.
}
\usage{
rforcecom.login(username, password, instanceURL, apiVersion)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{
Your username for login to Salesforce.com. In many cases, username is your E-mail address.
}
  \item{password}{
Your password for login to Salesforce.com. 
Note that DO NOT FORGET your Security Token.
(Ex.) If your password is "Pass1234" and your security token is "XYZXYZXYZXYZ", you should set "Pass1234XYZXYZXYZXYZ".
}
  \item{instanceURL}{
Your SFDC domain name.
}
  \item{apiVersion}{
Version of the REST API and SOAP API that you want to use.
Current API version is "24.0" (As of Feb. 2012).
}
}
\details{
}
\value{
  \item{sessionID }{Session ID. }
  \item{instanceURL }{Instance URL. }
  \item{apiVersion }{API Version. }
}
\references{

}
\author{
 Takekatsu Hiramura <thira@plavox.info>
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{rforcecom.query}}
 \code{\link{rforcecom.search}}
 \code{\link{rforcecom.create}}
}
\examples{
# Sign in to the Force.com
username <- "yourname@yourcompany.com"
password <- "YourPasswordSECURITY_TOKEN"
instanceURL <- "https://xxx.salesforce.com/"
apiVersion <- "24.0"
\dontrun{rforcecom.login(username, password, instanceURL, apiVersion)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Login }
\keyword{ ~Session }
