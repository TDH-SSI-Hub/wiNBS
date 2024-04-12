#' Store password for an NBS account
#'
#' Uses the keyring package to store credentials locally.
#'
#' @param username Username for account
#' @param password Password for account
#' 
#' @return Nothing
#' @export
nbs_password_set<-function(username,password=''){
  message('Password set. Delete lines containing the password from this file.')
  keyring::key_set_with_value('NBS',username,password)
}


#' Retrieve NBS password for an account
#'
#' Retrieves a password set with nbs_password_set().
#'
#' @param username NBS username
#' 
#' @return NBS password
#' @export
nbs_password_get<-function(username){
  if(username %in% keyring::key_list('NBS')$username){
    message('Do not store your password in a script.')
    return(keyring::key_get('NBS',username))
  }else{
    message(paste0('No credentials found for ',username,'. Use nbs_password_set() to create credentials.'))
    return(NA)
  }
  return(NA)
  
}



#' Search NBS for a given ID
#'
#' Starting from the NBS home screen, select the correct type of event and
#' search by event ID.
#'
#' @param ID_value Value to search for
#' @param ID_type Event ID type to select from dropdown
#' @return Nothing
#' @export
nbs_search <- function(ID_value, ID_type = "Investigation ID") {
  remDr$findElements("name", "ESR100_textbox")[[1]]$sendKeysToElement(list(ID_type, key = "tab"))
  remDr$findElements("name", "patientSearchVO.actId")[[1]]$sendKeysToElement(list(ID_value))
  remDr$executeScript("searchPatient();")
  remDr$findElements("id", "searchResultsTable")[[1]]$findChildElements("tag name", "a")[[7]]$clickElement()
}

#' Load NBS home screen
#'
#' Navigate to the NBS login page, enter username and password
#' , and enter the specified environment (defaults to production)
#'
#' @param u NBS Username
#' @param environment Environment to enter
#' @param url url for NBS login page (e.g., 'https://hssi.tn.gov/auth/login')
#' @return Nothing
#' @export
nbs_load <- function(u = "", environment = "NBS Production", url = "https://hssi.tn.gov/auth/login") {
  password<-nbs_password_get(u)
  if(is.na(password)){
    return(NA)  
    }
  remDr$navigate(url)
  remDr$findElement(using = "name", value = "usr_name")$clearElement()
  remDr$findElement(using = "name", value = "usr_name")$sendKeysToElement(list(u))
  remDr$findElement(using = "name", value = "usr_password")$clearElement()
  remDr$findElement(using = "name", value = "usr_password")$sendKeysToElement(list(password))
  remDr$findElements("tag name", "button")[[2]]$clickElement()
  if (remDr$getTitle() == "HSSI Welcome") {
    remDr$findElements("class", "btn")[[1]]$clickElement()
  }
  Sys.sleep(.5)
  remDr$findElements("link text", environment)[[1]]$clickElement()
}

#' Download a file from NBS
#'
#' Download documents accessed through links in NBS (e.g., attachments).
#'
#' @param element HTML element for file
#' @param outputname Desired name for downloaded file without extension
#' @param url Base url for NBS instance
#' (e.g., 'https://nbsproduction.tn.gov/nbs/')
#' @return T/F if file downloaded
#' @export
nbs_file_download <- function(element, outputname, url = "https://nbsproduction.tn.gov/nbs/") {
  old.exact.name <- tryCatch(unlist(element$getElementText()), error = function(e) {
    return(NA)
  })
  filetype <- tools::file_ext(old.exact.name)
  new.name <- paste0(outputname, ".", filetype)
  if (!is.na(old.exact.name)) {
    attachment.exists <- T
    old.url <- unlist(element$getElementAttribute("href"))
    attachment.uid <- substr(old.url, str_locate(old.url, "AttachmentUid=")[2] + 1, stringr::str_locate(old.url, "fileNmTxt")[1] - 2)
    remDr$navigate(paste0(url, "InvDownloadFile.do?ContextAction=doDownload&nbsAttachmentUid=", attachment.uid, "&fileNmTxt=", new.name))
  } else {
    (attachment.exists <- F)
  }
  return(attachment.exists)
}


#' Filter queue dropdown
#'
#' Open a queue's dropdown filter and select specified elements.
#' When grepl = T, many results may be selected using regex pattern matching.
#' When select_all = T, the 'Select all' option is clicked, which reverses the
#' selection
#'
#' @param dropdown Number representing which dropdown to use (1 is far left)
#' @param search_for Pattern to match in dropdown options
#' @param grepl T/F whether to match multiple options by regex pattern, or
#' match one option exactly
#' @param select_all T/F whether to uncheck select all prior to matching
#' @return None
#' @export
nbs_queue_filter <- function(dropdown, search_for, grepl = F, select_all = F) {
  remDr$findElements("id", "queueIcon")[[dropdown]]$clickElement()
  options <- remDr$findElements("class", "sortable")[[dropdown]]$findChildElements("class", "pText")
  if (select_all) {
    remDr$findElements("class", "sortable")[[dropdown]]$findChildElement("class", "selectAll")$clickElement()
  }
  for (r in 1:length(options)) {
    if (!grepl) {
      if (unlist(options[[r]]$getElementText()) == search_for) {
        options[[r]]$clickElement()
        break
      }
    } else {
      if (grepl(search_for, unlist(options[[r]]$getElementText()), ignore.case = T)) {
        options[[r]]$clickElement()
      }
    }
  }
  remDr$executeScript("selectfilterCriteria();")
}

#' Print the current page as a pdf
#'
#' Prints the current NBS page as a pdf. The pdf is initially saved in the
#'  default download directory (which must be specified in
#'  chrome_open_browser()).
#'  The pdf is then renamed and possibly moved to a different folder.
#'  The default printing method pdf is not exactly equivalent to the pdf
#'  generated by using the 'Print'
#'  button in NBS, but performance is improved. Setting legacy_print to TRUE
#'  will use aspects of the NBS print button, but this method is slower and
#'  more error prone and should only be used when needed
#'  (e.g., better formatting for printing legacy cases)
#'
#' @param pdfname Desired name for the pdf.
#' @param folder (Optional) Folder location for the pdf after download. Usually
#' unnecessary since pdf will download in the location specified in 'print_to'
#' parameter of chrome_open_browser().
#' @param legacy_print Print using the NBS print button. Better quality for
#' legacy pages, but sometimes causes errors and is slower.
#'
#' @return None
#' @export
nbs_pdf_print <- function(pdfname, folder = NA, legacy_print = F) {
  download_location <- remDr$extraCapabilities$chromeOptions$prefs$savefile.default_directory
  if (is.null(download_location)) {
    message("ERROR: No download directory set. Set print_to argument in chrome_open_browser()")
  } else {

    if (legacy_print) {
      home.window <- remDr$getCurrentWindowHandle()[[1]]
      remDr$findElement("name", "Print")$clickElement()
      Sys.sleep(.2)
      all.window <- remDr$getWindowHandles()
      preview.Window <- all.window[!all.window %in% home.window][[1]]
      chrome_window_switch(preview.Window)
      Sys.sleep(.2)
    }

    page.name <- unlist(remDr$getTitle())
    if (page.name == "") page.name <- stringr::str_extract(remDr$getCurrentUrl(),"Load.*\\.do")

    old.name <- paste0(download_location, "\\", page.name, ".pdf")
    if (!is.na(folder)) {
      if (!grepl("[\\|/]$", folder)) folder <- paste0(folder, "/")
      pdfname <- paste0(folder, pdfname)
    } else {
      pdfname <- paste0(download_location, "/", pdfname)
    }


    if (file.exists(old.name)) {
      file.remove(old.name)
    }
    Sys.sleep(1)
    remDr$executeScript("window.print();")

    transferred <- F
    for (w in 1:60) {
      if (file.exists(old.name)) {
        file.rename(old.name, pdfname)
        transferred <- T
        break
      } else {
        Sys.sleep(1)
      }
    }


    if (legacy_print) {
      Sys.sleep(.2)
      remDr$closeWindow()
      Sys.sleep(.2)
      chrome_window_switch(home.window)
    }

    return(transferred)
  }
}

#' Mark an event as reviewed
#'
#' @return "Marked as Reviewed" or "Already marked"
#' @export
nbs_lab_mark_as_reviewed<-function(){
  if('markReviewd' %in% html_attr(html_nodes(read_html(remDr$getPageSource()[[1]]),'input'),'name')){
    remDr$findElement('name','markReviewd')$clickElement()
    return('Marked as Reviewed')
  }else{
    return('Already marked')
  }
}


#' Determine if the current page is a legacy page
#'
#' @return T/F
#' @export
nbs_page_is_legacy <- function() {
  length(rvest::html_node(rvest::read_html(remDr$getPageSource()[[1]]), '.cellColor'))!=2
}



#' Edit the quick code on an investigation
#' 
#' @param id HTML ID for quick code field
#' @param val Quick code to enter
#' @param pre_clear T/F. Should the quick code be cleared before attempting to enter the new value?
#' @param ... Arguments (environment) passed on to nbs_quick_code_get() 
#'
#' @return NULL
#' @export
edit_quick_code<-function(id,val, pre_clear=F, ...){
  
  if(pre_clear){
    if(remDr$findElement('id',paste0('clear',id))$getElementAttribute('class')!='none'){
      remDr$findElement('id',paste0('clear',id))$clickElement()
    }
  }
  
  
  if(nchar(val)>10){
   val<- nbs_quick_code_get(val,...)
  }
  
  
  remDr$findElement('name',paste0("pageClientVO.answer(",id,")"))$sendKeysToElement(list("\uE009a\uE009",val))
  Sys.sleep(.3)
  remDr$findElement('id',paste0(id,"CodeLookupButton"))$clickElement()
}



#' Edit a text field
#' 
#' @param id String. HTML ID for text field. Can vary based on ID type.
#' @param val String. Text to enter. Can be a list of strings.
#' @param id_type String. HTML attribute used to find the element. Usually one of id, name, xpath, etc. 
#' @param id_suffix String. Suffix for the element ID.
#' @param pre_clear T/F. Should the existing text be highlighted prior to new text being entered. If TRUE, existing text will be cleared.
#' @param post_key string. One of tab, enter, etc. to be pressed after the text has been entered. Tab (the default) shifts the cursor to the next element, and can help when you need the text field data read in to activate page logic.
#'
#' @return NULL
#' @export
edit_text_field<-function(id, val,id_type='name', id_suffix='_textbox' ,pre_clear=T,post_key='tab'){
  new_val<-list()
  if(pre_clear) new_val<- append(new_val,"\uE009a\uE009")
  new_val<- append(new_val,val)
  if(!is.na(post_key)) new_val<- append(new_val,list(key=post_key))
  
  remDr$findElement(id_type,paste0(id,id_suffix))$sendKeysToElement(new_val)
  
}


#' Edit a date field
#' 
#' @param id String. HTML ID for date field.
#' @param val String. A date or value that can be coerced into a date.
#'
#' @return NULL
#' @export
edit_date<-function(id, val){
  edit_text_field(id,as.character(TNTools::tn_clean_date(val, format = '%m/%d/%Y')), id_suffix = '', id_type = 'id')
}

#' Edit a numeric field
#' 
#' @param id String. HTML ID for numeric field.
#' @param val String. A number or value that can be coerced into a number.
#'
#' @return NULL
#' @export
edit_numeric<-function(id, val){
  remDr$findElement('id',id)$clearElement()
  edit_text_field(id,as.character(val), id_suffix = '', id_type = 'id')
}


#' Retrieve page metadata for a given page
#' 
#' @param page String. Page name found in url. If NA, the correct page is determined using the current browser page.
#'
#' @return Dataframe of page metadata
#' @export
nbs_page_metadata_get<-function(page=NA){
  if(is.na(page)){
    page<-remDr$getCurrentUrl()
    if(grepl('invFormCd=',page)){
      page<-substr(page,str_locate(page,'invFormCd=')[2]+1,nchar(page))
    }else{
      page<-gsub('View Investigation: ','',remDr$findElement('xpath','//*[@id="bd"]/h1/table/tbody/tr[1]/td[1]/a')$getElementText())
      page<-condition_metadata$investigation_form_cd[condition_metadata$condition_short_nm==page]
    }
  } else if(grepl('invFormCd=',page)){
    page<-substr(page,str_locate(page,'invFormCd=')[2]+1,nchar(page))
  } else if(page %in% page_metadata$investigation_form_cd){
    
  } else if(page %in% condition_metadata$condition_short_nm){
    page<-condition_metadata$investigation_form_cd[condition_metadata$condition_short_nm==page]
  }else{
    message('No valid page selected or detected')
    return(NA)
  }
  
  page_metadata[page_metadata$investigation_form_cd==page,]
}

#' Go to the NBS home page
#'
#' @return Nothing
#' @export
nbs_home_page<-function(){
  remDr$findElements("class", "navLink")[[1]]$clickElement()
}

#' Submit changes to an investigation
#'
#' @return Nothing
#' @export
nbs_investigation_submit<-function(){
  remDr$findElement("id", "SubmitTop")$clickElement()
}

#' Go to an investigation from a patient page
#' 
#' @param ID A case ID. Required if this function is called from outside the patient page.
#' @param uid The case uid. Providing a uid may be slightly faster.
#' @param environment What environment does this occur in? Only needed if this may be called from outside NBS (requiring login) or if uid is supplied.
#'
#' @return Nothing
#' @export
nbs_investigation_go_to<-function(ID=NA,uid=NA,environment='NBS Production'){
  ID<-as.character(ID)
  uid<-as.character(uid)
  if(sum(is.na(c(ID,uid)))==2){
    message('Error: ID or uid must be supplied')
    return(NA)
  }
  
  if(remDr$getTitle()!="View Patient File"){
  
  if(!is.na(ID)){
  if (!grepl('nbs',remDr$getCurrentUrl())){
    nbs_load(username,environment)
  }
    if(remDr$getTitle()!="NBS Dashboard") nbs_home_page()
      nbs_search(ID)
  }else{
    message('ID must be supplied if not called from patient file')
    return(NA)
  }
  }
  
  if(is.na(uid)){
    case_index<-remDr$getPageSource() %>%
      unlist() %>%
      read_html() %>%
      html_element(xpath='//*[@id="eventSumaryInv"]/tbody')  %>%
      html_text() %>%
      str_split('\nT\n') %>% unlist() %>%
      str_which(ID)
    url<-remDr$findElement('xpath',paste0('//*[@id="eventSumaryInv"]/tbody/tr[',case_index,']/td[2]/a'))$getElementAttribute('href')
    remDr$navigate(unlist(url))
  }else{
    base_url<-"https://nbsproduction.tn.gov/nbs/ViewFile1.do?ContextAction=InvestigationIDOnEvents&publicHealthCaseUID="
    if(environment=='NBS Staging') base_url<-gsub('production','staging',base_url)
    remDr$navigate(paste0(base_url,uid))
  }

}

#' Edit an investigation
#' 
#' If a notification exists, accept the alert
#'
#' @return Nothing
#' @export
nbs_investigation_edit<-function(){
  notification<-remDr$findElement("id", 'patientSummaryJSP_view_notificationStatus')$getElementText()!=''
  # This is the edit button for most accounts, not the delete button
  remDr$findElement("id", 'delete')$clickElement()
  # Accept the alert if there is one
  if(notification){remDr$acceptAlert()
  Sys.sleep(.5)
  }
}

#' Retrieve field from an investigation page
#' 
#' @param id String. NBS question_identifier for the field.
#' @param page_source Page html. If NA, will pull current browser page (slower). For longer queries, use remDr$getPageSource() to pull the html once.
#'
#' @return string
#' @export
nbs_field_get<-function(id,page_source=NA){
  if(is.na(page_source)){
    page_source <- remDr$getPageSource() %>% unlist() %>% rvest::read_html()
  }
  
  if('list' %in% class(page_source)){
    page_source<-unlist(page_source)
  }
  
  if('character' %in% class(page_source)){
    page_source<-rvest::read_html(page_source)
  }
  
  
  page_source %>% html_element(paste0('#',id)) %>% html_text2()
}

#' Set the value of a field on the edit investigation page
#' 
#' Given an ID, a desired value, and the page metadata, this function will determine what type of field needs to be updated, which helper function should be used, and then update the field. It can also check to be sure the correct tab is selected prior to updating the field, although organizing your code to minimize the number of checks needed is highly recommended for speed purposes.
#' 
#' @param id String. HTML ID or metadata label for field.
#' @param value String. Value to send to the field.
#' @param page String. The url for the edit investigation page, the condition name, or the page name. If NA, will attempt to autodetect (slower).
#' @param check_tab T/F. If FALSE, assumes the element is visible currently (faster). If TRUE, a check is run to see if the correct tab is selected, then selects the tab if not (slower).
#' @param ... Arguments passed on to field setting function (currently just environment for edit_quick_code())
#' 
#' @return NULL
#' @export
nbs_field_set<-function(id, value, page=NA, check_tab=F, ...){
    metadata<-nbs_page_metadata_get(page)
  
  metadata_row<-metadata$question_identifier==id|metadata$question_label==id
  
  field_type<-toupper(metadata$data_type[metadata_row])
  field_id<-metadata$question_identifier[metadata_row]
  
  if(check_tab){
    ps<-remDr$getPageSource() %>% unlist() %>% rvest::read_html()
    test_x<-ps %>%
      rvest::html_nodes(xpath=paste0('//*[@id="',field_id,'"]'))
    tab_id<-gsub('tabs0tab','tabs0head',find_ancestor(test_x,'id','tabs0tab\\d'))
    tab_index<-as.numeric(substr(tab_id,nchar(tab_id),nchar(tab_id)))+1
    tab_info<-ps %>% 
      html_element(xpath=paste0('//*[@id="',tab_id,'"]')) %>% 
      html_attr('class')
    if(tab_info!='ongletTextEna'){
      #message(paste0('Switching to tab ',tab_index))
      remDr$findElement('xpath',paste0('//*[@id="',tab_id,'"]'))$clickElement()
    }
  }
  
  if(field_type=='CODED'){
    edit_text_field(id=field_id, val=value)
  }else if(field_type=='TEXT'){
    edit_text_field(id=field_id, val=as.character(value), id_type = 'id', id_suffix = '')
  }else if(field_type=='PART'){
    edit_quick_code(field_id,value,...)
  }else if(field_type=='NUMERIC'){
    edit_numeric(field_id,value)
  }else if(field_type=='DATE'){
    edit_date(field_id,value)
  }else{
    message('Unrecognized field type. Self destruct sequence initiated')
  }
  
}


#' Find an NBS quick code using the displayed demographic details
#' 
#' This function reverse searches for a quick code, given the information found in an NBS profile. This requires ODSE access and a properly formatted ODBC connection.
#' If a code can't be found, it is set to 'nbs-bot'. Code searches and their results are stored in the qlist object, so subsequent searches for the same profile can use the archived info (faster).
#' 
#' @param full_text String. The text found in an NBS profile, generated by a quick code.
#'  
#' @return Quick code
#' @export
nbs_quick_code_get<-function(full_text='nbs bot, ESQ Tennessee', odbc_name = 'NBS_Prod'){
  if(!exists('qlist')){
    qlist<<-list('nbs bot, ESQ Tennessee'='nbs-bot')
  }
  
  if(full_text=='') return(qlist[['nbs bot, ESQ Tennessee']])
  
  if(!full_text %in% names(qlist)){
    split_text<-str_split(full_text,'\n')[[1]]
    name<-split_text[1]
    
    names<-str_split(name,' ')[[1]]
    p.first_nm<-names[1]
    p.last_nm<-names[2]
    
    if(length(split_text)>1){
      is.phone<-grepl('\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d',split_text)
      t.phone_nbr_txt<-phone<-split_text[is.phone]
      t.phone_nbr_txt<-str_extract(phone,'\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d')
      if(length(t.phone_nbr_txt)>0){
        t.extension_txt<-str_extract(phone,'Ext\\. \\d*$') %>% str_replace('Ext\\. ','')
        if(is.na(t.extension_txt)) t.extension_txt<-character(0)
      }
      is.address<-grepl('^\\d.*\\D',split_text) & !is.phone
      pl.street_addr1<-split_text[is.address]
      is.location<-!is.phone & !is.address & c(F,rep(T,length(split_text)-1))
      
      location<-split_text[is.location]
      if(length(location)>=1){
        location<-location[[1]] %>% str_split(',')
        pl.city_desc_txt<-location[[1]][1]
        
        s.code_desc_txt<-paste0(str_extract_all(location[[1]][2],'[[:alpha:]]')[[1]],collapse='')
        pl.zip_cd<-paste0(str_extract_all(location[[1]][2],'\\d')[[1]],collapse='')
      }
    }
    where_clauses<-c('p.first_nm','p.last_nm','t.phone_nbr_txt','t.extension_txt','pl.street_addr1','pl.city_desc_txt','s.code_desc_txt','pl.zip_cd')
    #rm('p.first_nm','p.last_nm','t.phone_nbr_txt','pl.street_addr1','pl.city_desc_txt','s.code_desc_txt','pl.zip_cd','t.extension_txt')
    base_query<-"select root_extension_txt
  ,p.record_status_cd
from nbs_odse..person p
left join nbs_odse..Person_name pn
on pn.person_uid = p.person_uid
left join nbs_odse..entity_id AS ei
on p.person_uid=ei.entity_uid
left join nbs_odse..Entity_locator_participation elp
on p.person_uid = elp.entity_uid
and elp.class_cd='Tele'
left join nbs_odse..Tele_locator t
on elp.locator_uid = t.tele_locator_uid
left join nbs_odse..Entity_locator_participation elp2
on p.person_uid = elp2.entity_uid
and elp2.class_cd='PST'
left join nbs_odse..Postal_locator pl
on elp2.locator_uid = pl.postal_locator_uid
left join nbs_srte..state_code s
on s.state_cd = pl.state_cd
where ei.type_cd='QEC' and p.record_status_cd = 'ACTIVE'"
    w<- where_clauses[1]
    for(w in where_clauses)  {
      if(!exists(w)) next
      current_w<-get(w)
      if(length(current_w)==1){
        base_query<-paste0(base_query,' and ',w," = '",current_w,"'")
      }
    }
    
    base_query<-paste0(base_query,' order by p.record_status_time desc')
    
    message(paste0('Adding Quick Code for ',name))
    
    prod<-odbcConnect(odbc_name)
    qcode<-sqlQuery(prod,base_query)
    if(nrow(qcode)==0){
      qlist[[full_text]]<<-'nbs-bot'
    }else{
      qlist[[full_text]]<<-qcode[1,1]
    }
    return(qlist[full_text])
  }else{
    return(qlist[full_text] )
  }
}
