library(RForcecom)

context("Testing Utility Functions")

test_that("Checking login and logout", {
  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  session <- NULL
  
  # test successful login
  tryCatch(session <- rforcecom.login(username, password))
  expect_true(length(session) > 0)
  
  # test successful logout
  tryCatch(logout_result <- rforcecom.logout(session))
  expect_true(logout_result)
  
  # test unsuccessful login
  expect_error(rforcecom.login(username='FailingTest', password='WrongPassword'))
  
  # test unsuccessful logout
  expect_error(rforcecom.logout(session='FailingTest'))
})

test_that("rforcecom.getServerTimestamp", {

  username <- Sys.getenv("RFORCECOM_EMAIL")
  password <- Sys.getenv("RFORCECOM_PASSWORD")
  session <- NULL
  
  # test for timestamp equality within 3 seconds
  returned_timestamp <- rforcecom.getServerTimestamp(session)
  expect_equal(returned_timestamp, Sys.time(), tolerance=3)

})

