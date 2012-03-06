\name{rforcecom.retrieve}
\alias{rforcecom.retrieve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieving a record
}
\description{
Retrieving a record
}
\usage{
rforcecom.retrieve(session, objectName, fields, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{session}{ A session data. It can be retrieve from \code{\link{rforcecom.login()}}. }
  \item{objectName}{ A object name. (Ex.) Account, Contact, CustomObject__c }
  \item{fields}{ Field names. (Ex.) Id, Name, Industry, AnnualRevenue }
  \item{\dots}{
   \item{limit}{ Number of the records to retrieve. }
   \item{id}{ Record ID to retrieve. }
 }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Result dataset.
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
 \code{\link{rforcecom.login()}}
}
\examples{
 \dontrun{
 # Retrieving a record
 objectName <- "Account"
 fields <- c("name", "Industry", "AnnualRevenue")
 rforcecom.retrieve(session, objectName, fields)
 rforcecom.retrieve(session, objectName, fields, limit = 5)
 rforcecom.retrieve(session, objectName, fields, id = "999x000000xxxxxZZZ")
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
