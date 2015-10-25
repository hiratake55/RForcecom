# Custom Object "account_dummy__c" has 3 columns.
#   - Name
#   - category_id__c
#   - insert_test_use__c

rforcecom.create.testdata<-function(){
  rforcecom.create(session, "account_dummy__c", c(Name="America Railway", category_id__c="10", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="Brazil Bank", category_id__c="20", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="China Television", category_id__c="10", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="Denmark Police", category_id__c="30", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="England Electronics", category_id__c="10", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="Finland University", category_id__c="30", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="Germany Research", category_id__c="30", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="Hong Kong Airline", category_id__c="10", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="India Industries", category_id__c="10", insert_test_use__c="False"))
  rforcecom.create(session, "account_dummy__c", c(Name="Japan Automotive", category_id__c="10", insert_test_use__c="False"))
  for(i in 1:450){
    name<-paste0("Acme Corporation ", i)
    rforcecom.create(session, "account_dummy__c", c(Name=name, category_id__c="99", insert_test_use__c="False"))
  }
}