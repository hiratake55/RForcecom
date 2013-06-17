\name{rforcecom.setProxy}
\alias{rforcecom.setProxy}

\title{
Proxy setting
}

\description{
Proxy setting for network environments require HTTP proxy.
}

\usage{
rforcecom.setProxy(proxyhost, port=NULL, username=NULL, password=NULL)
}

\arguments{
  \item{proxyhost}{ IP address or hostname of your proxy server. (ex: "192.168.1.254") }
  \item{port}{ Port number. (ex: "8080") }
  \item{username}{ If your proxy environment requires username, set your username. (ex: "yourusername") }
  \item{password}{ If your proxy environment requires username, set your username. (ex: "yourpassword") }
}


\value{
No data.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
 rforcecom.setProxy("192.168.1.254", "8080")
 rforcecom.setProxy("proxy-server", "8080", "username", "password")
}

\keyword{connection}

