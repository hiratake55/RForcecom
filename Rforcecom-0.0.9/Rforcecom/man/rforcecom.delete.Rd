\name{rforcecom.delete}
\alias{rforcecom.delete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deleting a record
}
\description{
Deleting a record
}
\usage{
rforcecom.delete(session, objectName, id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{session}{
%%     ~~Describe \code{session} here~~
}
  \item{objectName}{
%%     ~~Describe \code{objectName} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
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
# Deleting a record
objectName <- "Account";
id <- "999x000000xxxxxZZZ" # Record's Id
\dontrun{rforcecom.delete(session, objectName, id)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
