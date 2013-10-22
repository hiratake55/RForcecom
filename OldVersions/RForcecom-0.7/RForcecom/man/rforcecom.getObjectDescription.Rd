\name{rforcecom.getObjectDescription}
\alias{rforcecom.getObjectDescription}

\title{
Retrieve object descriptions
}

\description{
Retrieve object descriptions
}

\usage{
rforcecom.getObjectDescription(session, objectName)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (ex: "Account", "Contact", "CustomObject__c") }
}

\value{
Object descriptions
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Retrieve object descriptions
  objectName <- "Account"
  rforcecom.getObjectDescription(session, objectName)
 }
}

\keyword{connection}


