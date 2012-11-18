\name{rforcecom.api}
\alias{rforcecom.api.getSoapEndpoint}
\alias{rforcecom.api.getRestEndpoint}
\alias{rforcecom.api.getSoqlEndpoint}
\alias{rforcecom.api.getSoslEndpoint}
\alias{rforcecom.api.getObjectEndpoint}
\alias{rforcecom.api.getRecordEndpoint}
\alias{rforcecom.api.getFieldEndpoint}
\alias{rforcecom.api.getExternalIdFieldEndpoint}

\title{
Functions to connect API.
}

\description{
For internal use.
}

\usage{

rforcecom.api.getSoapEndpoint(apiVersion)

rforcecom.api.getRestEndpoint(apiVersion)

rforcecom.api.getSoqlEndpoint(apiVersion)

rforcecom.api.getSoslEndpoint(apiVersion)

rforcecom.api.getObjectEndpoint(apiVersion, objectName)

rforcecom.api.getRecordEndpoint(apiVersion, objectName, id)

rforcecom.api.getFieldEndpoint(apiVersion, objectName, field)

rforcecom.api.getExternalIdFieldEndpoint(apiVersion, objectName, field, id)

}

\arguments{
  \item{apiVersion}{ An REST API or SOAP API version to connect. (e.g. "24.0". As of Feb 2012) }
  \item{objectName}{ An object name. (e.g. Account, Contact, CustomObject__c) }
  \item{field}{ A field name. (e.g. Id, Name, Industry, AnnualRevenue) }
  \item{id}{ A record ID. (e.g. "999x000000xxxxxZZZ") }
}

\value{
  Return values depends on the function used.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\keyword{connection}

