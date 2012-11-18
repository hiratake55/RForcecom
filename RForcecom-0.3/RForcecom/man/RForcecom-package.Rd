\name{RForcecom-package}
\alias{RForcecom-package}
\alias{RForcecom}
\docType{package}
\title{
RForcecom provides the connection to Force.com and Salesforce.com from R.
}
\description{
RForcecom provides the connection to Force.com and Salesforce.com from R.
}
\details{
\tabular{ll}{
Package: \tab RForcecom\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2012-07-30\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr
}
}
\author{
Takekatsu Hiramura <thira@plavox.info>
}
\references{
Force.com REST API Developer's Guide
http://www.salesforce.com/us/developer/docs/api_rest/

Web Services API Developer's Guide
http://www.salesforce.com/us/developer/docs/api/
}
\keyword{ package }
\seealso{
XML
RCurl
}
\examples{
\dontrun{
# Sign in to the Force.com
username <- "yourname@yourcompany.com"
password <- "YourPasswordSECURITY_TOKEN"
instanceURL <- "https://xxx.salesforce.com/"
apiVersion <- "25.0"
session <- rforcecom.login(username, password, instanceURL, apiVersion)

# Execute a SOQL
soqlQuery <- "SELECT Id, Name, Industry, AnnualRevenue FROM Account"
rforcecom.query(session, soqlQuery)

# Execute a SOSL
queryString <- "United"
rforcecom.search(session, queryString)

# Creating a record
objectName <- "Account"
fields <- c(Name="R Analytics Service Ltd", Phone="5555-5555-5555")
rforcecom.create(session, objectName, fields)

# Retrieving a record
objectName <- "Account"
fields <- c("name", "Industry", "AnnualRevenue")
rforcecom.retrieve(session, objectName, fields)

# Updating a record
objectName <- "Account"
id <- "999x000000xxxxxZZZ"
fields <- c(Phone="9999-9999-9999")
rforcecom.update(session, objectName, id, fields)

# Upsert a record
objectName <- "Account";
externalIdField <- "AccountMaster__c"
externalId <- "AM-00000151"
fields <- c(Name="ABC Network Company", Phone="3333-3333-3333")
rforcecom.upsert(session, objectName, externalIdField, externalId, fields)

# Deleting a record
objectName <- "Account";
id <- "999x000000xxxxxZZZ"
rforcecom.delete(session, objectName, id)

# Retrieve a server timestamp
rforcecom.getServerTimestamp(session)

 }
}
\keyword{connection}


