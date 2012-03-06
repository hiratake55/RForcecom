pkgname <- "RForcecom"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('RForcecom')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("RForcecom-package")
### * RForcecom-package

flush(stderr()); flush(stdout())

### Name: RForcecom-package
### Title: RForcecom provides the connection to Force.com (Salesforce.com)
###   from R.
### Aliases: RForcecom-package RForcecom
### Keywords: package connection

### ** Examples

## Not run: 
##D # Sign in to the Force.com
##D username <- "yourname@yourcompany.com"
##D password <- "YourPasswordSECURITY_TOKEN"
##D instanceURL <- "https://xxx.salesforce.com/"
##D apiVersion <- "24.0"
##D session <- rforcecom.login(username, password, instanceURL, apiVersion)
##D 
##D # Execute a SOQL
##D soqlQuery <- "SELECT Id, Name, Industry, AnnualRevenue FROM Account"
##D rforcecom.query(session, soqlQuery)
##D 
##D # Execute a SOSL
##D queryString <- "United"
##D rforcecom.search(session, queryString)
##D 
##D # Creating a record
##D objectName <- "Account"
##D fields <- c(Name="R Analytics Service Ltd", Phone="5555-5555-5555")
##D rforcecom.create(session, objectName, fields)
##D 
##D # Retrieving a record
##D objectName <- "Account"
##D fields <- c("name", "Industry", "AnnualRevenue")
##D rforcecom.retrieve(session, objectName, fields)
##D 
##D # Updating a record
##D objectName <- "Account"
##D id <- "999x000000xxxxxZZZ"
##D fields <- c(Phone="9999-9999-9999")
##D rforcecom.update(session, objectName, id, fields)
##D 
##D # Upsert a record
##D objectName <- "Account";
##D externalIdField <- "AccountMaster__c"
##D externalId <- "AM-00000151"
##D fields <- c(Name="ABC Network Company", Phone="3333-3333-3333")
##D rforcecom.upsert(session, objectName, externalIdField, externalId, fields)
##D 
##D # Deleting a record
##D objectName <- "Account";
##D id <- "999x000000xxxxxZZZ"
##D rforcecom.delete(session, objectName, id)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.create")
### * rforcecom.create

flush(stderr()); flush(stdout())

### Name: rforcecom.create
### Title: Creating a record
### Aliases: rforcecom.create
### Keywords: connection

### ** Examples

## Not run: 
##D  objectName <- "Account"
##D  fields <- c(Name="R Analytics Service Ltd", Phone="5555-5555-5555")
##D  rforcecom.create(session, objectName, fields)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.delete")
### * rforcecom.delete

flush(stderr()); flush(stdout())

### Name: rforcecom.delete
### Title: Deleting a record
### Aliases: rforcecom.delete
### Keywords: connection

### ** Examples

## Not run: 
##D  # Deleting a record
##D  objectName <- "Account";
##D  id <- "999x000000xxxxxZZZ" # Record's Id
##D  rforcecom.delete(session, objectName, id)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.login")
### * rforcecom.login

flush(stderr()); flush(stdout())

### Name: rforcecom.login
### Title: Sign in Force.com (Salesforce.com) and retrieve a session ID.
### Aliases: rforcecom.login
### Keywords: connection

### ** Examples

## Not run: 
##D  # Sign in to the Force.com
##D  username <- "yourname@yourcompany.com"
##D  password <- "YourPasswordSECURITY_TOKEN"
##D  instanceURL <- "https://xxx.salesforce.com/"
##D  apiVersion <- "24.0"
##D  session <- rforcecom.login(username, password, instanceURL, apiVersion)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.query")
### * rforcecom.query

flush(stderr()); flush(stdout())

### Name: rforcecom.query
### Title: Execute a SOQL
### Aliases: rforcecom.query
### Keywords: connection

### ** Examples

## Not run: 
##D  # Execute a SOQL
##D  soqlQuery <- "SELECT Id, Name, Industry, AnnualRevenue FROM Account"
##D  rforcecom.query(session, soqlQuery)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.retrieve")
### * rforcecom.retrieve

flush(stderr()); flush(stdout())

### Name: rforcecom.retrieve
### Title: Retrieving a record
### Aliases: rforcecom.retrieve
### Keywords: connection

### ** Examples

## Not run: 
##D  # Retrieving a record
##D  objectName <- "Account"
##D  fields <- c("name", "Industry", "AnnualRevenue")
##D  rforcecom.retrieve(session, objectName, fields)
##D  rforcecom.retrieve(session, objectName, fields, limit = 5)
##D  rforcecom.retrieve(session, objectName, fields, id = "999x000000xxxxxZZZ")
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.search")
### * rforcecom.search

flush(stderr()); flush(stdout())

### Name: rforcecom.search
### Title: Execute a SOSL
### Aliases: rforcecom.search
### Keywords: connection

### ** Examples

## Not run: 
##D  # Execute a SOSL
##D  queryString <- "United"
##D  rforcecom.search(session, queryString)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.update")
### * rforcecom.update

flush(stderr()); flush(stdout())

### Name: rforcecom.update
### Title: Updating a record
### Aliases: rforcecom.update
### Keywords: connection

### ** Examples

## Not run: 
##D  # Updating a record
##D  objectName <- "Account"
##D  id <- "999x000000xxxxxZZZ"
##D  fields <- c(Phone="9999-9999-9999")
##D  rforcecom.update(session, objectName, id, fields)
##D  
## End(Not run)



cleanEx()
nameEx("rforcecom.upsert")
### * rforcecom.upsert

flush(stderr()); flush(stdout())

### Name: rforcecom.upsert
### Title: Upsert a record
### Aliases: rforcecom.upsert
### Keywords: connection

### ** Examples

## Not run: 
##D  # Upsert a record
##D  objectName <- "Account";
##D  externalIdField <- "AccountMaster__c"
##D  externalId <- "AM-00000151"
##D  fields <- c(Name="ABC Network Company", Phone="3333-3333-3333")
##D  rforcecom.upsert(session, objectName, externalIdField, externalId, fields)
##D  
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
