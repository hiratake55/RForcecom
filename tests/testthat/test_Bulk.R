
context("Testing Bulk API CRUD Operations")

# This testing suite will perform a set of operations that
# proceed to create 3 new account records then delete that data.

username <- Sys.getenv("RFORCECOM_EMAIL")
password <- Sys.getenv("RFORCECOM_PASSWORD")
session <- rforcecom.login(username, password)

# create a sample data.frame of 3 records
n <- 3
data <- data.frame(Name=paste('New Record:', 1:n), 
                   stringsAsFactors=FALSE)

# run an insert job into the Account object
job_info <- rforcecom.createBulkJob(session, 
                                    operation='insert', 
                                    object='Account')

# split into batch sizes of 2 to test multibatch option
batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=job_info$id, 
                                          data, 
                                          multiBatch = TRUE, 
                                          batchSize=2)

# check on status of each batch
batches_status <- lapply(batches_info, 
                         FUN=function(x){
                           rforcecom.checkBatchStatus(session, 
                                                      jobId=x$jobId, 
                                                      batchId=x$id)
                         })

# pause a moment for the operation to complete
Sys.sleep(3)
# get details on each batch
batches_detail <- lapply(batches_info, 
                         FUN=function(x){
                           rforcecom.getBatchDetails(session, 
                                                     jobId=x$jobId, 
                                                     batchId=x$id)
                         })
# close the job
close_job_info <- rforcecom.closeBulkJob(session, jobId=job_info$id)


## BULK DELETE THE PRIOR INSERT

# format the data
batch_details_together <- plyr::ldply(batches_detail)
delete_ids <- data.frame(id=batch_details_together[,"Id"], 
                         stringsAsFactors=FALSE)

delete_job_info <- rforcecom.createBulkJob(session, operation='delete', object='Account')
delete_batches_info <- rforcecom.createBulkBatch(session, 
                                          jobId=delete_job_info$id, 
                                          data=delete_ids)

# check on status of each batch
delete_batches_status <- lapply(delete_batches_info, 
                         FUN=function(x){
                           rforcecom.checkBatchStatus(session, 
                                                      jobId=x$jobId, 
                                                      batchId=x$id)
                         })
# pause a moment for the operation to complete
Sys.sleep(3)
# get details on each batch
delete_batches_detail <- lapply(delete_batches_info, 
                                 FUN=function(x){
                                   rforcecom.getBatchDetails(session, 
                                                             jobId=x$jobId, 
                                                             batchId=x$id)
                                 })
# close the job
close_delete_job_info <- rforcecom.closeBulkJob(session, jobId=delete_job_info$id)




test_that("rforcecom.createBulkJob", {
  
  # check insert operation
  expect_is(job_info, "list")
  expect_true(all(c('id', 'operation', 'object', 'state', 
                    'concurrencyMode', 'numberBatchesQueued', 
                    'totalProcessingTime') %in% names(job_info)))
  expect_equal(job_info$state, 'Open')
  expect_equal(job_info$operation, 'insert')
  expect_equal(job_info$object, 'Account')
  
  # check delete operation
  expect_is(delete_job_info, "list")
  expect_true(all(c('id', 'operation', 'object', 'state', 
                    'concurrencyMode', 'numberBatchesQueued', 
                    'totalProcessingTime') %in% names(delete_job_info)))
  expect_equal(delete_job_info$state, 'Open')
  expect_equal(delete_job_info$operation, 'delete')
  expect_equal(delete_job_info$object, 'Account')
})

test_that("rforcecom.createBulkBatch", {
  
  expect_is(batches_info, "list")
  expect_equal(length(batches_info), 2)
  expect_true(all(c('id', 'jobId', 'state', 
                    'numberRecordsProcessed',
                    'totalProcessingTime') %in% names(batches_info[[1]])))
  expect_equal(batches_info[[1]]$state, 'Queued')
  
  expect_is(delete_batches_info, "list")
  expect_equal(length(delete_batches_info), 1)
  expect_true(all(c('id', 'jobId', 'state', 
                    'numberRecordsProcessed',
                    'totalProcessingTime') %in% names(delete_batches_info[[1]])))
  expect_equal(delete_batches_info[[1]]$state, 'Queued')
})

test_that("rforcecom.checkBatchStatus", {
  expect_is(batches_status, "list")
  expect_equal(length(batches_status), 2)
  expect_true(all(c('id', 'jobId', 'state', 
                    'numberRecordsProcessed',
                    'totalProcessingTime') %in% names(batches_status[[1]])))
  
  expect_is(delete_batches_status, "list")
  expect_equal(length(delete_batches_status), 1)
  expect_true(all(c('id', 'jobId', 'state', 
                    'numberRecordsProcessed',
                    'totalProcessingTime') %in% names(delete_batches_status[[1]])))
})

test_that("rforcecom.getBatchDetails", {
  expect_is(batches_detail, "list")
  expect_equal(length(batches_detail), 2)
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% 
                    names(batches_detail[[1]])))
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% 
                    names(batches_detail[[2]])))
  
  
  expect_is(delete_batches_detail, "list")
  expect_equal(length(delete_batches_detail), 1)
  expect_true(all(c('Id', 'Success', 'Created', 'Error') %in% 
                    names(delete_batches_detail[[1]])))
  
})

test_that("rforcecom.closeBulkJob", {
  expect_is(close_job_info, "list")
  expect_true(all(c('id', 'operation', 'object', 'state', 
                    'concurrencyMode', 'numberBatchesQueued', 
                    'totalProcessingTime') %in% names(close_job_info)))
  expect_equal(close_job_info$state, 'Closed')
  expect_equal(close_job_info$numberBatchesTotal, '2')
  expect_equal(close_job_info$numberRecordsProcessed, '3')
  
  expect_is(close_delete_job_info, "list")
  expect_true(all(c('id', 'operation', 'object', 'state', 
                    'concurrencyMode', 'numberBatchesQueued', 
                    'totalProcessingTime') %in% names(close_delete_job_info)))
  expect_equal(close_delete_job_info$state, 'Closed')
  expect_equal(close_delete_job_info$numberBatchesTotal, '1')
  expect_equal(close_delete_job_info$numberRecordsProcessed, '3')
})
