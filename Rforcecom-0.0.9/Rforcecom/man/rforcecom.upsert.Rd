\name{rforcecom.upsert}
\alias{rforcecom.upsert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Upsert a record
}
\description{
Upsert a record
}
\usage{
rforcecom.upsert(session, objectName, externalIdField, externalId, fields)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{session}{
%%     ~~Describe \code{session} here~~
}
  \item{objectName}{
%%     ~~Describe \code{objectName} here~~
}
  \item{externalIdField}{
%%     ~~Describe \code{externalIdField} here~~
}
  \item{externalId}{
%%     ~~Describe \code{externalId} here~~
}
  \item{fields}{
%%     ~~Describe \code{fields} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Takekatsu Hiramura <thira@plavox.info>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Upsert a record
objectName <- "Account";
externalIdField <- "AccountMaster__c"
externalId <- "AM-00000151"
fields <- c(Name="ABC Network Company", Phone="3333-3333-3333")
\dontrun{rforcecom.upsert(session, objectName, externalIdField, externalId, fields)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
