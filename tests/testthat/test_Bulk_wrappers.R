
context("Testing Bulk API CRUD Operations - Wrapper Functions")

# This testing suite will perform a set of operations that
# proceed to create 3 new account records then query and delete that data.

username <- Sys.getenv("RFORCECOM_EMAIL")
password <- Sys.getenv("RFORCECOM_PASSWORD")
session <- rforcecom.login(username, password)

# create a sample data.frame of 3 records
n <- 3
data <- data.frame(Name=paste('New Record:', 1:n), 
                   stringsAsFactors=FALSE)

# run an insert job into the Account object
job_result_insert <- rforcecom.bulkAction(session, 
                                   operation='insert', 
                                   data, object='Account', 
                                   multiBatch=TRUE, batchSize=2)

## QUERY FOR INSERTED RECORDS

# format the data
batch_details_together <- plyr::ldply(job_result_insert)
new_ids <- data.frame(id=batch_details_together[,"Id"], 
                   stringsAsFactors=FALSE)
id_string <- paste(paste0("'", new_ids[, 1], "'"), collapse = ",")

# run a query on the Account object
soqlQuery <- paste0("SELECT Id, Name FROM Account WHERE Id IN (", id_string, ")")
job_result_query <- rforcecom.bulkQuery(session, soqlQuery, object='Account')

## BULK DELETE THE PRIOR INSERT

job_result_delete <- rforcecom.bulkAction(session,
                                          operation='delete',
                                          data=new_ids, object='Account')

test_that("rforcecom.bulkAction", {
  
  # check insert operation
  expect_is(job_result_insert, "list")
  expect_that(length(job_result_insert), equals(2))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% names(job_result_insert[[1]])))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% names(job_result_insert[[2]])))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% 
                    names(job_result_insert[[1]])))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% 
                    names(job_result_insert[[2]])))
  
  # check delete operation
  expect_is(job_result_delete, "list")
  expect_that(length(job_result_delete), equals(1))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% names(job_result_delete[[1]])))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% 
                    names(job_result_delete[[1]])))
  
})

test_that("rforcecom.bulkQuery", {
  
  # check query operation
  expect_is(job_result_query, "data.frame")
  expect_true(all(c('Id', 'Name') %in% names(job_result_query)))
  expect_that(nrow(job_result_query), equals(3))
  
})






