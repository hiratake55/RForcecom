library(RForcecom)

context("Testing login feature")

test_that("Checking login has sucess", {
  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  instanceURL <- "https://na14.salesforce.com/"
  apiVersion <- "27.0"
  session <- NULL
  tryCatch(session <- rforcecom.login(username, password, instanceURL, apiVersion))
  expect_true(length(session) > 0)
  }
)
