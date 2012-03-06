\name{rforcecom.query}
\alias{rforcecom.query}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Execute a SOQL
}
\description{
Execute a SOQL
}
\usage{
rforcecom.query(session, soqlQuery)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{session}{
%%     ~~Describe \code{session} here~~
}
  \item{soqlQuery}{
%%     ~~Describe \code{soqlQuery} here~~
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

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

}
\examples{
# Execute a SOQL
soqlQuery <- "SELECT Id, Name, Industry, AnnualRevenue FROM Account"
\dontrun{rforcecom.query(session, soqlQuery)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
