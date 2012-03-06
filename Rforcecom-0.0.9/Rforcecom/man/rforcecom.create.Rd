\name{rforcecom.create}
\alias{rforcecom.create}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating a record
}
\description{
Creating a record
}
\usage{
rforcecom.create(session, objectName, fields)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{session}{
%%     ~~Describe \code{session} here~~
}
  \item{objectName}{
%%     ~~Describe \code{objectName} here~~
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

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

}
\examples{
objectName <- "Account"
fields <- c(Name="R Analytics Service Ltd", Phone="5555-5555-5555")
\dontrun{rforcecom.create(session, objectName, fields)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
