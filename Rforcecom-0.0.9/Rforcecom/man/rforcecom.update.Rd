\name{rforcecom.update}
\alias{rforcecom.update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Updating a record
}
\description{
Updating a record
}
\usage{
rforcecom.update(session, objectName, id, fields)
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
  \item{fields}{
%%     ~~Describe \code{fields} here~~
}
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

}
\examples{
# Updating a record
objectName <- "Account"
id <- "999x000000xxxxxZZZ"
fields <- c(Phone="9999-9999-9999")
\dontrun{rforcecom.update(session, objectName, id, fields)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
