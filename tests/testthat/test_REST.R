
context("Testing REST API Operations")

username <- Sys.getenv("RFORCECOM_EMAIL")
password <- Sys.getenv("RFORCECOM_PASSWORD")
session <- rforcecom.login(username, password)

# Create a record
objectName <- "Account"
fields <- c(Name="R API Test Suite")
create_result <- rforcecom.create(session, objectName, fields)

# Update a record
objectName <- "Account"
id <- as.character(create_result$id)
unique_suffix <- paste0(sample(c(LETTERS,0:9), 5, replace=T), collapse="")
fields <- c(Name=paste0("R API Test Suite - ", unique_suffix, " - (after Name update)"))
update_result <- rforcecom.update(session, objectName, id, fields)

# check that the update worked by using SOQL
soqlQuery <- paste0("SELECT Id, Name FROM Account where Id='", as.character(create_result$id), "'")
soql_result <- rforcecom.query(session, soqlQuery)

# check that the update worked by using SOSL
queryString <- paste0("R API Test Suite \\- ", unique_suffix)
sosl_result <- rforcecom.search(session, queryString)

# Delete a record
objectName <- "Account";
id <- as.character(create_result$id)
delete_result <- rforcecom.delete(session, objectName, id)

# check that queryall works to find deleted record
soqlQuery <- paste0("SELECT Id, Name FROM Account where Id='", as.character(create_result$id), "'")
deleted_soql_result1 <- rforcecom.query(session, soqlQuery, queryAll=FALSE)
soqlQuery <- paste0("SELECT Id, Name FROM Account where Id='", as.character(create_result$id), "'")
deleted_soql_result2 <- rforcecom.query(session, soqlQuery, queryAll=TRUE)

# TODO - Determine if this external field will always exist. 
#        Possibly by creating metadata if it doesnt so that the test passes.

# Upsert a record
# objectName <- "Account";
# externalIdField <- "AccountMaster__c"
# externalId <- "AM-00000151"
# fields <- data.frame(Name=c("R API Testing Suite (after Name upsert)",
#                             "R API Testing Suite (created on upsert)")
# rforcecom.upsert(session, objectName, externalIdField, externalId, fields)


test_that("rforcecom.create", {
  expect_is(create_result, "data.frame")
  expect_equal(nrow(create_result), 1)
  expect_equal(names(create_result), c('id', 'success'))
  expect_equal(as.character(create_result$success), 'true')
})

test_that("rforcecom.update", {
  expect_equal(update_result, NULL)
})

test_that("rforcecom.delete", {
  expect_equal(delete_result, NULL)
})

test_that("rforcecom.query", {
  expect_is(soql_result, "data.frame")
  expect_equal(nrow(soql_result), 1)
  expect_equal(names(soql_result), c('Id', 'Name'))
})

test_that("rforcecom.search", {
  
  # check a bad sosl where we forgot to escape dash with backslash
  queryString <- paste0("R API Test Suite - ", unique_suffix)
  expect_error(rforcecom.search(session, queryString), 'MALFORMED_SEARCH')
  
  # check a sosl that returns no records
  queryString <- paste0("R API Test Suite \\- ", unique_suffix, 'MakeResultsTooNarrow')
  expect_equal(rforcecom.search(session, queryString), NULL)
  
  # check the valid sosl
  expect_is(sosl_result, "data.frame")
  expect_equal(nrow(sosl_result), 1)
  expect_equal(names(sosl_result), c('Id', 'type', 'url'))
})

test_that("rforcecom.queryall", {
  expect_equal(nrow(deleted_soql_result1), 0)
  expect_is(deleted_soql_result2, "data.frame")
  expect_equal(nrow(deleted_soql_result2), 1)
  expect_equal(names(deleted_soql_result2), c('Id', 'Name'))
})

context("Enhanced Testing of Query Results")

test_that("Simple query returns expected columns", {
  
  tryCatch(columnTest <- rforcecom.query(session, "select Id, Name, Amount, CloseDate, IsWon from Opportunity limit 1"))
  expect_named(columnTest, c("Id", "Name", "Amount", "CloseDate", "IsWon"))
  expect_equal(nchar(as.character(columnTest[,'Id'])), 18)  # IDs should be 18 characters long
  
  tryCatch(lookupFieldTest <- rforcecom.query(session, "select Id, Owner.Id, Owner.Name from Opportunity limit 1"))
  expect_named(lookupFieldTest, c("Id", "Owner.Id", "Owner.Name"))
  expect_equal(nchar(as.character(lookupFieldTest[,'Id'])), 18)  # IDs should be 18 characters long
  expect_equal(nchar(as.character(lookupFieldTest[,'Owner.Id'])), 18)  # IDs should be 18 characters long
  
})
