library(RForcecom)

context("Testing login feature")

test_that("Checking login is successful", {
  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  instanceURL <- "https://login.salesforce.com/"
  apiVersion <- "27.0"
  session <- NULL
  tryCatch(session <- rforcecom.login(username, password, instanceURL, apiVersion))
  expect_true(length(session) > 0)
})

context("Testing query results")

test_that("Simple query returns expected columns", {
  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  instanceURL <- "https://login.salesforce.com/"
  apiVersion <- "27.0"
  session <- NULL
  tryCatch(session <- rforcecom.login(username, password, instanceURL, apiVersion))
  
  tryCatch(columnTest <- rforcecom.query(session, "select Id, Name, Amount, CloseDate, IsWon from Opportunity limit 1"))
  expect_named(columnTest, c("Id", "Name", "Amount", "CloseDate", "IsWon"))
  expect_equal(nchar(as.character(columnTest[,'Id'])), 18)  # IDs should be 18 characters long
  
  tryCatch(lookupFieldTest <- rforcecom.query(session, "select Id, Owner.Id, Owner.Name from Opportunity limit 1"))
  expect_named(lookupFieldTest, c("Id", "Owner.Id", "Owner.Name"))
  expect_equal(nchar(as.character(lookupFieldTest[,'Id'])), 18)  # IDs should be 18 characters long
  expect_equal(nchar(as.character(lookupFieldTest[,'Owner.Id'])), 18)  # IDs should be 18 characters long
  
})
