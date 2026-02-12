library(wiNBS)

test_that("Password set and get", {
  new_password<-paste0('testpassword',round(runif(1)*1000))
  nbs_password_set('test',new_password)
  expect_equal(new_password, nbs_password_get('test'))
})


test_that("Browser Opened", {
  browser_open()
  expect_true(exists('remDr'))
})

test_that("Prod login", {
  nbs_load('dc49p74', process='wiNBS tests')
  expect_match(unlist(remDr$getCurrentUrl()), 'production')
  expect_equal(unlist(remDr$getTitle()), "NBS Dashboard")
})


test_that("Staging login", {
  nbs_load('dc49p74', environment = 'NBS Staging', process='wiNBS tests')
  expect_match(unlist(remDr$getCurrentUrl()), 'staging')
  expect_equal(unlist(remDr$getTitle()), "NBS Dashboard")
})


test_that("Release - Staging logout", {
  remDr$findElement('xpath','//*[@id="bd"]/form[1]/table/tbody/tr/td[2]/table/tbody/tr/td[3]/a')$clickElement()
  expect_equal(unlist(remDr$getTitle()), "GetAccess Resource Menu")
})

test_that("Release - Entrust logout", {
  remDr$findElement('xpath','//*[@id="login_header"]/div/div[2]/a')$clickElement()
  expect_equal(unlist(remDr$getTitle()), "Entrust GetAccess")
})

test_that("Staging login 2", {
  nbs_load('dc49p74', environment = 'NBS Staging', process='wiNBS tests')
  expect_match(unlist(remDr$getCurrentUrl()), 'staging')
  expect_equal(unlist(remDr$getTitle()), "NBS Dashboard")
})


test_that('Go to patient', {
  nbs_go_to(6519295)
  expect_equal(unlist(remDr$getTitle()), "View Patient File")
})

test_that('Go to inv', {
  expect_true(nbs_go_to('CAS17409156TN01'))
})

test_that('Go to home page from patient', {
  nbs_home_page()
  expect_match(unlist(remDr$getCurrentUrl()), 'loadHomePage')
})

test_that('Go to lab', {
  expect_true(nbs_go_to('OBS95948068TN01'))
})

test_that('Go to home page from lab', {
  nbs_home_page()
  expect_match(unlist(remDr$getCurrentUrl()), 'loadHomePage')
})

test_that('Go to morb', {
  expect_true(nbs_go_to('OBS98272013TN01'))
})

test_that('Go to home page from morb', {
  nbs_home_page()
  expect_match(unlist(remDr$getCurrentUrl()), 'loadHomePage')
})

test_that('Go to case report', {
  expect_true(nbs_go_to('DOC11133087TN01'))
})

test_that('Go to home page from case report', {
  nbs_home_page()
  expect_match(unlist(remDr$getCurrentUrl()), 'loadHomePage')
})


test_that('Patient Search by demo', {
  edit_text_field('DEM102','test','id','')
  edit_text_field('DEM104','test','id','')
  edit_date('DEM115', Sys.Date())
  remDr$findElement('xpath','//*[@id="patientSearchByDetails"]/table[2]/tbody/tr[8]/td[2]/input[1]')$clickElement()
  
  expect_equal(unlist(remDr$getTitle()), "NBS: Search Results")
})

test_that('Patient add from search', {
  remDr$findElement('name','Submit')$clickElement()
  expect_equal(unlist(remDr$getTitle()), "NBS:Add Patient - Basic")
})


test_that('Patient Created', {
  remDr$findElement('name','Submit')$clickElement()
  expect_equal(unlist(remDr$getTitle()), "View Patient File")
})


test_that('Search for new patient', {
  new_patient_id<<-unlist(remDr$findElement('xpath','//*[@id="bd"]/table[2]/tbody/tr/td[2]/span[2]')$getElementText())
  nbs_home_page()
  nbs_patient_search(new_patient_id)
  expect_equal(new_patient_id,unlist(remDr$findElement('xpath','//*[@id="bd"]/table[2]/tbody/tr/td[2]/span[2]')$getElementText()))
})


test_that('Create HIV Case from patient page', {
  condition<-'AIDS'
  pre_submit_fields<-list(referralBasis_textbox='A1 - Associate 1'
                          ,reviewReason_textbox='Field Follow-up')
  initial_data<-list(DEM196='Created via script'
                     ,INV111=as.character(Sys.Date())
                     ,INV107='Central Office'
                     ,NBS111='Tennessee'
                     ,NBS124='Associate'
                     ,NBS143='6-Yes, Notifiable'
                     ,NBS161='AA'
                     ,NBS162=as.character(Sys.Date()))
  new_hiv_case<-nbs_investigation_from_patient(condition
                                               ,pre_submit_fields
                                               ,initial_data)
  
  expect_equal(new_hiv_case,'Investigation has been successfully saved in the system.')
  remDr$findElement('link text','Return To File: Events')$clickElement()
})


test_that('Create Covid Case from patient page', {
  new_cov_case<-nbs_investigation_from_patient('COVID-19', initial_data=list(INV107='Central Office'))
  expect_equal(new_cov_case,'Investigation has been successfully saved in the system.')
  remDr$findElement('link text','Return To File: Events')$clickElement()
})

test_that('Create Campy (legacy) Case from patient page', {
  initial_data_campy<-list('proxy.publicHealthCaseVO_s.thePublicHealthCaseDT.jurisdictionCd_textbox'='Central Office')
  new_campy_case<-nbs_investigation_from_patient('Campylobacteriosis', initial_data=initial_data_campy)
  expect_true(is.na(new_campy_case))
  expect_match(unlist(remDr$findElement('xpath','/html/body/table/tbody/tr/td/table/tbody/tr[3]/td/table/thead/tr[1]/td/table/tbody/tr/td[1]/span[2]/b')$getElementText()),'CAS')
  remDr$findElement('link text','Return To File: Events')$clickElement()
})

test_that('Create Ebola lab from patient page', {
  
  fields<<-list(
    NBS_LAB365='mcj-child'
    ,INV108='General Communicable Disease'
    ,INV107='Central Office'
    ,NBS_LAB220='Ebola virus - Result'
    ,NBS_LAB208='SUPER EBOLA'
    ,Add='Add'
  )
  ebola_id<-nbs_lab_create(fields)
  expect_match(ebola_id,'OBS')
  
})


test_that('Create Ebola lab from patient page with HIV inv', {

  pre_submit_fields<-list()
  initial_data<-list(DEM196='Created via script'
                      ,INV111=as.character(Sys.Date())
                      #,INV107='Central Office'
                      #,NBS111='Tennessee'
                      #,NBS124='Associate'
                      #,NBS143='6-Yes, Notifiable'
                      #,NBS161='AA'
                      #,NBS162=as.character(Sys.Date())
                      )
  
  ebola_id<-nbs_lab_create(fields,inv_create=T,condition=condition, pre_submit_fields=list(), initial_data=initial_data)
  
  lab_table<-nbs_tables('person')$eventLabReport
  lab_assoc<-lab_table$associated_with[lab_table$event_id==ebola_id]
  
  expect_match(ebola_id,'OBS')
  expect_match(lab_assoc,'CAS')
})


test_that('Create Ebola lab from patient page with Campy inv', {
  
  condition<-'Campylobacteriosis'
  pre_submit_fields<-list()
  initial_data<-list()
  
  ebola_id<-nbs_lab_create(fields,inv_create=T,condition=condition, pre_submit_fields=list(), initial_data=initial_data)
  
  lab_table<-nbs_tables('person')$eventLabReport
  lab_assoc<-lab_table$associated_with[lab_table$event_id==ebola_id]
  
  expect_match(ebola_id,'OBS')
  expect_match(lab_assoc,'CAS')
})

test_that('Create Ebola lab from patient page with Ebola inv', {
  
  condition<-'Ebola hemorrhagic fever'
  pre_submit_fields<-list()
  initial_data<-list()
  
  ebola_id<<-nbs_lab_create(fields,inv_create=T,condition=condition, pre_submit_fields=list(), initial_data=initial_data)
  
  lab_table<-nbs_tables('person')$eventLabReport
  lab_assoc<-lab_table$associated_with[lab_table$event_id==ebola_id]
  
  expect_match(ebola_id,'OBS')
  expect_match(lab_assoc,'CAS')
})

test_that('Go to lab from patient page', {
  expect_true(nbs_lab_go_to(ebola_id, patient_page = T))
})

test_that('Edit lab data', {
  remDr$findElement('name','edit')$clickElement()
  nbs_field_set('NBS_LAB197',Sys.Date(),metadata = 'LAB_Lab_Report')
  
  expect_true(nbs_investigation_submit()=='')
  expect_equal(nbs_field_get('NBS_LAB197')==format(Sys.Date(),"%m/%d/%Y"))
  nbs_queue_return()
})


test_that('Go to DRSA', {
  nbs_queue_load('Documents Requiring Security')
  expect_equal(unlist(remDr$getTitle()),"Documents Requiring Security Assignment")
})

test_that('Go to SRQ', {
  nbs_queue_load('Supervisor Review')
  expect_equal(unlist(remDr$getTitle()),"Supervisor Review Queue")
})



test_that('Go to DRR', {
  nbs_queue_load('Documents Requiring Review')
  expect_equal(unlist(remDr$getTitle()),"Documents Requiring Review")
})


test_that('View eICR from DRR', {
  nbs_queue_filter(1,'Case Report', select_all = T)
  nbs_queue_row_click(1,2)
  expect_equal(unlist(remDr$getTitle()),"View Document")
})

test_that('Open eICR Document (DIBBS)', {
  remDr$findElement('xpath','//*[@id="docBtnBar"]/tbody/tr/td[2]/a[1]/font')$clickElement()
  expect_length(remDr$getWindowHandles(),2)
})

test_that('Switch to new window', {
  expect_true(window_switch())
})

test_that('View eICR Document (DIBBS)', {
  expect_equal(unlist(remDr$getTitle()),"DIBBs eCR Viewer")
})

test_that('Switch to new window, close old', {
  expect_true(window_switch(close_old = T))
  expect_length(remDr$getWindowHandles(),1)
})



test_that('Go to OIQ', {
  nbs_queue_load('Open Investigations')
  expect_equal(unlist(remDr$getTitle()),"Open Investigations Queue")
})

test_that('Determine queue size', {
  size_1<<-nbs_queue_size()
  expect_gt(size_1,1)
})

test_that('Filter Queue', {
  nbs_queue_filter(6,'Confirmed')
  size_2<-nbs_queue_size()
  nbs_queue_filter(6,'Confirmed')
  nbs_queue_filter(6,'Confirmed', select_all = T)
  size_3<-nbs_queue_size()
  expect_equal(size_1,size_2 + size_3)
  expect_lt(size_2,size_1)
})

test_that('Get queue row info', {
  expect_match(nbs_queue_row_info()[['Investigation ID']],'CAS')
})

test_that('Click queue checkbox', {
  nbs_queue_row_click()
  expect_true(unlist(remDr$findElements('name','selectCheckBox')[[1]]$isElementSelected()))
})

test_that('Click into inv from queue', {
  nbs_queue_row_click(1,6)
  expect_in(unlist(remDr$getTitle()),c("NBS:Investigation: Generic",'NBS'))
})

test_that('Return to OIQ', {
  nbs_queue_return()
  expect_equal(unlist(remDr$getTitle()),"Open Investigations Queue")
})





test_that('nbs_investigation_go_to', {
  expect_true(nbs_investigation_go_to('CAS17086668TN01'))
  expect_equal(unlist(remDr$getTitle()),"NBS:Investigation: Generic")
})

test_that('nbs_field_get', {
  expect_equal(nbs_field_get('DEM104'),'Daffy')
})

test_that('nbs_field_get from other tab', {
  expect_equal(nbs_field_get('INV108'),'General Communicable Disease')
})

test_that('nbs_investigation_edit', {
  nbs_investigation_edit()
  expect_match(unlist(remDr$getCurrentUrl()),"Edit")
})

test_that('Get page metadata', {
  expect_gt(nrow(nbs_page_metadata_get()),0)
})


test_that('nbs_field_get from edit page', {
  test_string<<-paste0('Test ',Sys.Date())
  nbs_field_set('DEM196',test_string)
  expect_equal(nbs_field_get('DEM104'),'Daffy')
})


test_that('race check 1', {
  nbs_investigation_race('Unknown', uncheck_others = T)
  nbs_investigation_race('White', uncheck_others = T)
  race_w1<- unlist(remDr$findElement('name','pageClientVO.whiteRace')$isElementSelected())
  race_u1<- unlist(remDr$findElement('name','pageClientVO.unKnownRace')$isElementSelected())
  
  expect_true(race_w1)
  expect_false(race_u1)
})

test_that('race check 2', {
  nbs_investigation_race('Unknown', uncheck_others = T)
  race_w2<- unlist(remDr$findElement('name','pageClientVO.whiteRace')$isElementSelected())
  race_u2<- unlist(remDr$findElement('name','pageClientVO.unKnownRace')$isElementSelected())
  
  expect_true(race_u2)
  expect_false(race_w2)
})

test_that('race check 3', {
  nbs_investigation_race('White')
  race_w3<- unlist(remDr$findElement('name','pageClientVO.whiteRace')$isElementSelected())
  race_u3<- unlist(remDr$findElement('name','pageClientVO.unKnownRace')$isElementSelected())
  
  expect_true(race_w3)
  expect_true(race_u3)
})

test_that('race check 4', {
  nbs_investigation_race('Unknown', uncheck_others = T)
  race_w4<- unlist(remDr$findElement('name','pageClientVO.whiteRace')$isElementSelected())
  race_u4<- unlist(remDr$findElement('name','pageClientVO.unKnownRace')$isElementSelected())
  
  expect_true(race_u4)
  expect_false(race_w4)
})

test_that('nbs_investigation_submit', {
  expect_equal(nbs_investigation_submit(),"Investigation has been successfully saved in the system.")
})

test_that('nbs_field_set', {
  expect_equal(nbs_field_get('DEM196'),test_string)
})

test_that('Edit multiple field types', {
  nbs_investigation_edit()
  # Date field
  nbs_field_set('INV147',Sys.Date(), check_tab = T)
  random_text1<<-round(runif(1)*1000)
  # Text box
  nbs_field_set('INV173',random_text1)
  # Comment
  nbs_field_set('DEM196',random_text1, check_tab = T)
  # Dropdown
  nbs_field_set('NOT113','Davidson County', check_tab = T)
  expect_equal(nbs_investigation_submit(),"Investigation has been successfully saved in the system.")
})

test_that('nbs_field_set on date 1', {
  expect_equal(TNTools::tn_clean_date(nbs_field_get('INV147')),Sys.Date(), ignore_attr =T)
})

test_that('nbs_field_set on text 1', {
  expect_equal(nbs_field_get('INV173'),as.character(random_text1))
})

test_that('nbs_field_set on comment 1', {
  expect_equal(nbs_field_get('DEM196'),as.character(random_text1))
})

test_that('nbs_field_set on dropdown 1', {
  expect_equal(nbs_field_get('NOT113'),'Davidson County')
})



test_that('Edit multiple field types', {
  nbs_investigation_edit()
  # Date field
  nbs_field_set('INV147',Sys.Date(), check_tab = T)
  random_text2<<-round(runif(1)*1000)
  # Text box
  nbs_field_set('INV173',random_text2)
  # Comment
  nbs_field_set('DEM196',random_text2, check_tab = T)
  # Dropdown
  nbs_field_set('NOT113','Shelby County', check_tab = T)
  expect_equal(nbs_investigation_submit(),"Investigation has been successfully saved in the system.")
})

test_that('nbs_field_set on date 2', {
  expect_equal(TNTools::tn_clean_date(nbs_field_get('INV147')),Sys.Date(), ignore_attr =T)
})

test_that('nbs_field_set on text 2', {
  expect_equal(nbs_field_get('INV173'),as.character(random_text2))
})

test_that('nbs_field_set on comment 2', {
  expect_equal(nbs_field_get('DEM196'),as.character(random_text2))
})

test_that('nbs_field_set on dropdown 2', {
  expect_equal(nbs_field_get('NOT113'),'Shelby County')
})




 
