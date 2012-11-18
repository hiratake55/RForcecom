\name{rforcecom.login}
\alias{rforcecom.login}

\title{
Sign in Force.com (Salesforce.com) and retrieve a session ID.
}

\description{
Sign in Force.com (Salesforce.com) and retrieve a session ID.
}

\usage{
rforcecom.login(username, password, instanceURL, apiVersion)
}

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
If your SFDC instance http://na14.salesforce.com/ , set https://na14.salesforce.com/.
}

\item{apiVersion}{
Version of the REST API and SOAP API that you want to use.
Current API version is "25.0" (As of Jul. 2012).
}
}

\value{
  \item{sessionID }{ Session ID. }
  \item{instanceURL }{ Instance URL. }
  \item{apiVersion }{ API Version. }
}

\author{
 Takekatsu Hiramura <thira@plavox.info>
}

\seealso{
 \code{\link{rforcecom.query}}
 \code{\link{rforcecom.search}}
 \code{\link{rforcecom.create}}
 \code{\link{rforcecom.delete}}
 \code{\link{rforcecom.retrieve}}
 \code{\link{rforcecom.update}}
 \code{\link{rforcecom.upsert}}
}

\examples{
\dontrun{
 # Sign in to the Force.com
 username <- "yourname@yourcompany.com"
 password <- "YourPasswordSECURITY_TOKEN"
 instanceURL <- "https://xx99.salesforce.com/"
 apiVersion <- "24.0"
 session <- rforcecom.login(username, password, instanceURL, apiVersion)
 }
}


\keyword{connection}
