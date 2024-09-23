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
#' Retrieves a password set with nbs_password_set.
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
#' , and enter the specified environment - defaults to production
#'
#' @param u NBS Username
#' @param environment Environment to enter
#' @param url url for NBS login page - e.g., 'https://hssi.tn.gov/auth/login'
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
#' Download documents accessed through links in NBS - e.g., attachments.
#'
#' @param element HTML element for file
#' @param outputname Desired name for downloaded file without extension
#' @param url Base url for NBS instance - e.g., 'https://nbsproduction.tn.gov/nbs/'
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

#' Navigate to a queue
#'
#' Navigate to a queue
#'
#' @param queue Name of the queue to load
#' @return Nothing
#' @export
nbs_queue_load<-function(queue){
  if(remDr$getTitle()!="NBS Dashboard"){nbs_home_page()}
  remDr$findElement('partial link text',queue)$clickElement()
}

#' Filter queue
#'
#' Open a queue's dropdown filter and select specified elements.
#' When grepl = T, many results may be selected using regex pattern matching.
#' When select_all = T, the 'Select all' option is clicked, which reverses the
#' selection. If the dropdown is a text match, grepl and select_all are ignored.
#'
#' @param dropdown Number representing which dropdown to use (1 is far left)
#' @param search_for Pattern to match in dropdown options
#' @param grepl T/F whether to match multiple options by regex pattern, or
#' match one option exactly
#' @param select_all T/F whether to uncheck select all prior to matching
#' @return None
#' @export
nbs_queue_filter <- function(dropdown, search_for, grepl = F, select_all = F) {
  ps<-remDr$getPageSource() %>%
    unlist() %>%
    read_html()
  
  if(is.numeric(dropdown)){
    
  }else{
    filter_names<-ps %>% html_elements('.sortable') %>% html_elements('a') %>% html_text()
    dropdown<-which(filter_names==dropdown)
  }
  
  if(length(dropdown)!=1){
    message('Filter not found')
    return(NA)
  }
  
  hps<-ps %>% html_elements('.sortable') %>% .[dropdown]
  
  hclass<- hps %>%
    html_elements('div') %>% html_attr('class')
  
  if('multiTextOptions' %in% hclass){
    queue_contains(dropdown,search_for,ps)
  }else{
    
    remDr$findElements("id", "queueIcon")[[dropdown]]$clickElement()
    options <- remDr$findElements("class", "sortable")[[dropdown]]$findChildElements("class", "pText")
    option_text<-hps %>% html_elements('.pText') %>% html_text()
    if (select_all) {
      remDr$findElements("class", "sortable")[[dropdown]]$findChildElement("class", "selectAll")$clickElement()
    }
    for (r in 1:length(options)) {
      if (!grepl) {
        if (option_text[r]== search_for) {
          options[[r]]$clickElement()
          break
        }
      } else {
        if (grepl(search_for, option_text[r], ignore.case = T)) {
          options[[r]]$clickElement()
        }
      }
    }
    remDr$executeScript("selectfilterCriteria();")
  }
}

#' Filter queue dropdown from supervisor queue
#'
#' Open a queue's dropdown filter and select specified elements.
#' When grepl = T, many results may be selected using regex pattern matching.
#' When select_all = T, the 'Select all' option is clicked, which reverses the
#' selection. If you are attempting to select 1 or more options, 
#' but no options are actually in the dropdown list, 
#' all options will remain selected and be included in the shown results.
#' To prevent this, an error is generated by default if no option is selected.
#'
#' @param dropdown Number representing which dropdown to use (1 is far left)
#' @param search_for Pattern to match in dropdown options
#' @param grepl T/F whether to match multiple options by regex pattern, or
#' match one option exactly
#' @param select_all T/F whether to uncheck select all prior to matching
#' @param noclick_error T/F Should an error be generated when no option is clicked (aside from select all). Defaults to the opposite of select_all.
#' @return None
#' @export
nbs_queue_filter_supervisor<-function (dropdown, search_for, grepl = F, select_all = F, noclick_error=!select_all) {
  dd <- remDr$findElements("class", "multiSelect")[[dropdown]]
  dd_id <- gsub("ig", "", unlist(dd$getElementAttribute("id")))
  dd$clickElement()
  option_names <- unlist(str_split(remDr$findElement("id", 
                                                     dd_id)$getElementText(), "\n "))
  options <- remDr$findElement("id", dd_id)$findChildElements("tag name", 
                                                              "input")
  clicked_once<-F
  if (select_all) {
    options[[1]]$clickElement()
  }
  for (r in 2:length(options)) {
    if (!grepl) {
      if (option_names[r] == search_for) {
        options[[r]]$clickElement()
        clicked_once<-T
        break
      }
    }
    else {
      if (grepl(search_for, option_names[r], ignore.case = T)) {
        options[[r]]$clickElement()
        clicked_once<-T
      }
    }
  }
  if(!clicked_once & noclick_error){
    stop('Failed to select any dropdown options')
  }
  remDr$executeScript("selectfilterCriteria();")
}

#' Filters queue columns that use a contains string search
#'
#' Filters queue columns that use a contains string search
#'
#' @param dropdown Number representing which dropdown to use (1 is far left)
#' @param search_for Pattern to match in dropdown options
#' 
#' @return None
queue_contains<-function(dropdown,search_for,pagesource){
  dropdowns<-pagesource %>%
    html_nodes('#queueIcon')
  
  axpath<-find_ancestor_xpath(dropdowns[dropdown],'class','sortable')
  axsub <- as.numeric(str_extract(str_extract(axpath,"table\\[\\d\\]" ),'\\d'))
  naxsub<-paste0('table[',axsub-1,']')
  axpath <- gsub(paste0("table\\[",axsub,"\\]"), naxsub, axpath)
  remDr$findElements("id", "queueIcon")[[dropdown]]$clickElement()
  remDr$findElement('xpath',axpath )$findChildElements('tag name','input')[[3]]$sendKeysToElement(list(search_for))
  remDr$executeScript("selectfilterCriteria();")
}

#' Select a link or checkbox in a queue
#'
#' Select a link or checkbox in a queue
#'
#' @param row Row to select
#' @param column Column to select (includes checkbox column)
#' 
#' @return None
#' @export
nbs_queue_row_click<-function(row=1, col=1){
  
  if(row%%2==0){
    row_class<-'even'
    row<-row/2
  }else{
    row_class<-'odd'
    row<-row/2+1
  }
  
  cur_element<-remDr$getPageSource() %>% unlist() %>% read_html() %>% 
    html_elements(paste0('.',row_class)) %>% 
    .[row] %>% 
    html_children() %>% 
    .[col] %>% html_children()
  cur_el_attr<-unlist(html_attrs(cur_element))
  if('checkbox' %in% cur_el_attr | '#' %in% cur_el_attr) {
    remDr$findElement('xpath',gsub('table\\[4\\]','table[3]',xml_path(cur_element)[1]))$clickElement()
  }else{
    message('Nothing to click')
  }
}

#' Get information from a queue row
#'
#' Get information from a queue row
#'
#' @param row Row to select
#' 
#' @return Named character vector of row data
#' @export
nbs_queue_row_info<-function(row=1){
  if(row%%2==0){
    row_class<-'even'
    row<-row/2
  }else{
    row_class<-'odd'
    row<-row/2+1
  }
  cnames<-remDr$findElements('class','sortable')
  cnames<-unlist(sapply(cnames, function(x) x$getElementText()))
  rdata<-unlist(lapply(remDr$findElements('class',row_class)[[row]]$findChildElements('tag name','td'), function(x) x$getElementText()))
  names(rdata)<-c('checkbox',cnames)
  rdata
}


#' Return to a queue
#'
#' Return to a queue
#' 
#' @return None
#' @export
nbs_queue_return<-function(){
  remDr$findElement('partial link text','Return to')$clickElement()
}


#' Create a notification
#'
#' Create a notification
#' 
#' @param comment Comment to add to notification
#' 
#' @return None
#' @export
nbs_notification_create<- function(comment='Created by automated bot'){
  remDr$executeScript('return createPamNotification();')
  window_switch()
  remDr$findElement('id','NTF137')$sendKeysToElement(list(comment))
  remDr$executeScript('var opener = getDialogArgument();
				var comments = getElementByIdOrByName("NTF137").value;
				opener.createNotifications(comments);
				var pview = getElementByIdOrByNameNode("pageview", opener.document)
				pview.style.display = "none";')
  window_switch(close_old = T)
  Sys.sleep(.5)
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
    message("ERROR: No download directory set. Set print_to parameter.")
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

#' Edit a note field
#' 
#' @param id String. HTML ID for note field.
#' @param val String. Text to enter
#'
#' @return NULL
#' @export
edit_note<-function(id, val){
  edit_text_field(id,as.character(val), id_suffix = '', id_type = 'id')
  #Sys.sleep(.2)
  oku<-unlist(remDr$findElement('id',id)$getElementAttribute('onchange'))
  button_ui<-substr(oku, str_locate(oku,"unhideBatchImg\\('")[2]+1,nchar(oku)-3)
  remDr$findElement('id',paste0('AddButtonToggle',button_ui))$findChildElement('tag name','input')$clickElement()
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

#' Edit a multiselect field
#' 
#' 
#' @param id String. HTML ID for numeric field.
#' @param values A vector of values to select
#' @param options A vector of the possible options, in order. Autodetected if not provided, but this is slower. Make sure the first value for a provided vector is "".
#' @param pre_clear T/F. Should current values be deselected prior to selection of values? Does not affect the other option
#' @param other_clear T/F. Should the other option be deselected if pre_clear is TRUE? This will delete data in the other text field.
#'
#' @return NULL
#' @export
edit_multiselect<-function(id, values, options=NA, pre_clear=F, other_clear=F){
  current_values<-unlist(remDr$findElement('id',paste0(id,'-selectedValues'))$getElementText()) %>% 
    str_replace('Selected Values: ','') %>% 
    str_split(', ') %>% 
    unlist()
  
  current_values<-current_values[current_values!='Selected Values:']
  
  if(identical(options,NA)){
    options<-remDr$getPageSource() %>% 
      unlist() %>% 
      read_html() %>% 
      html_node(paste0('#',id)) %>% 
      html_nodes('option') %>% 
      html_text()
  }
  v<-values[1]
  olist<-remDr$findElement('id',id)$findChildElements('tag name','option')
  
  other_index<-grep('other',options, ignore.case = T)[1]
  other_option<-options[other_index]
  
  
  if(pre_clear & length(current_values)>0){
    for(cv in current_values){
      if(cv==other_option & !other_clear) next
      olist[[(1:length(options))[options==cv]]]$clickElement()
    }
  }
  
  for(v in values){
    if(v %in% current_values & !pre_clear) next
    if(!v %in% options){
      if(!other_option %in% current_values){
        olist[[other_index]]$clickElement()
      }
      Sys.sleep(.5)
      remDr$findElement('id',paste0(id,'Oth'))$sendKeysToElement(list(paste0(' ',v)))
    }else{
      olist[[(1:length(options))[options==v]]]$clickElement()
    }
  }
  
  unlist(remDr$findElement('id',paste0(id,'-selectedValues'))$getElementText())
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
#' @param check_legacy T/F. If TRUE, checks if the page is legacy before clicking the home button, since legacy pages need slightly different code.
#'
#' @return Nothing
#' @export
nbs_home_page<-function(check_legacy=F){
  pclass<- "navLink"
  if(check_legacy){
    if(nbs_page_is_legacy()){
      pclass<-'navBar'
    }
  }
  remDr$findElements("class", pclass)[[1]]$clickElement()
}

#' Submit changes to an investigation
#'
#' @param check_submission T/F. Should the feedback bar text be returned?
#'
#' @return Nothing
#' @export
nbs_investigation_submit<-function(check_submission=T){
  remDr$findElement("id", "SubmitTop")$clickElement()
  if(check_submission){
  remDr$getPageSource() %>% unlist() %>% 
    read_html() %>% 
    html_element('#globalFeedbackMessagesBar') %>% 
    html_text() %>% str_replace_all('\n','') %>% str_trim() %>% unlist()
  }
}

#' Go to an investigation from a patient page
#' 
#' @param ID A case ID. Required if this function is called from outside the patient page.
#' @param uid The case uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#'
#' @return Nothing
#' @export
nbs_investigation_go_to<-function(ID=NA,uid=NA,patient_page=F){
  
  ID<-as.character(ID)
  uid<-as.character(uid)
  if(sum(is.na(c(ID,uid)))==2){
    message('Error: ID or uid must be supplied')
    return(NA)
  }
  
  if(patient_page){
  
  }else{
    if(remDr$getTitle()!="NBS Dashboard"){nbs_home_page()}
    nbs_search(ID)
  }
  
  if(is.na(uid)){
    case_index<-remDr$getPageSource() %>%
      unlist() %>%
      read_html() %>%
      html_element(xpath='//*[@id="eventSumaryInv"]/tbody')  %>%
      html_text() %>%
      str_split('\nT\n|\nF\n') %>% unlist() %>%
      str_which(ID)
    url<-remDr$findElement('xpath',paste0('//*[@id="eventSumaryInv"]/tbody/tr[',case_index,']/td[2]/a'))$getElementAttribute('href')
    remDr$navigate(unlist(url))
  }else{
    remDr$getCurrentUrl()
    base_url<-"https://nbsproduction.tn.gov/nbs/ViewFile1.do?ContextAction=InvestigationIDOnEvents&publicHealthCaseUID="
    if(grepl('staging',unlist(remDr$getCurrentUrl()))) base_url<-gsub('production','staging',base_url)
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
    notification<-remDr$findElement("id", 'patientSummaryJSP_view_notificationStatus')$getElementText()
    # This is the edit button for most accounts, not the delete button
    remDr$findElement("id", 'delete')$clickElement()
    # Accept the alert if there is one
    if(!notification %in% c('','REJECTED')){remDr$acceptAlert()
      Sys.sleep(.5)
    }
}

#' Retrieve info from an note field
#' 
#' @param id String. NBS question_identifier for the field.
#' @param page_source Page html. If NA, will pull current browser page (slower). For longer queries, use remDr$getPageSource() to pull the html once.
#' @param row_start Numeric. First note row to be returned. Set to Inf to start at last row.
#' @param row_end Numeric. Last note row to be returned. Set to Inf to end at last row.
#' @param columns Numeric. Which columns (1-3) to include in output
#' @param col_separator String. When specified column data will be combined into one string for each row, with the col_separator between fields. Set to NA to return a dataframe.
#' @param row_separator String. When specified alongside a col_separator, data will be combined into one string, with the col_separator between fields and the row_separator between rows. If col_separator is not NA, and row_separator is NA, a vector will be returned.
#' @return string, vector, or dataframe
#' @export
get_note<-function(id, page_source=NA, row_start=1, row_end=Inf , columns=1:3, row_separator = ' ^ ', col_separator= ' | '){
  if(is.na(page_source)){
    page_source <- remDr$getPageSource() %>% unlist() %>% rvest::read_html()
  }
  
  if('list' %in% class(page_source)){
    page_source<-unlist(page_source)
  }
  
  if('character' %in% class(page_source)){
    page_source<-rvest::read_html(page_source)
  }
  table_text<-page_source %>% 
    html_elements(xpath=paste0("//*[starts-with(@id, 'table",id,"')]")) %>% 
    html_text2() %>% 
    matrix(ncol=3, byrow = T) %>% 
    as.data.frame()
  
  colnames(table_text)<-c('Note','Date','By')
  table_row<-nrow(table_text)-1
  
  if(table_row>0){
    table_text<-as_tibble(table_text[1:table_row,columns])
    if(row_end == Inf) row_end <- max(table_row,1)
    if(row_start == Inf) row_start <- max(table_row,1)
    if(!is.na(col_separator)){
      table_text<-apply(table_text[row_start:row_end,], 1, paste0, collapse=col_separator)
      if(!is.na(row_separator)){
        table_text<-paste0(table_text, collapse=row_separator)
      }
    }
  }else{
    if(is.na(col_separator)){
      return(table_text)
    }else{
      return('')
    }
    
  }
  table_text
}

#' Retrieve field from an investigation page
#' 
#' @param id String. NBS question_identifier for the field.
#' @param page_source Page html. If NA, will pull current browser page (slower). For longer queries, use remDr$getPageSource() to pull the html once.
#' @param note T/F. Is the field a note field (with repeating note blocks)
#' @param ... Additional arguments passed on to get_note() which determine what info is returned for notes. By default returns the text of the last note.
#'
#' @return string
#' @export
nbs_field_get<-function(id,page_source=NA, note = F, ...){
  if(is.na(page_source)){
    page_source <- remDr$getPageSource() %>% unlist() %>% rvest::read_html()
  }
  
  if('list' %in% class(page_source)){
    page_source<-unlist(page_source)
  }
  
  if('character' %in% class(page_source)){
    page_source<-rvest::read_html(page_source)
  }
  
  if(note){
    get_note(id, page_source, Inf, Inf,1)
  }else{
    page_source %>% html_element(paste0('#',id)) %>% html_text2()
  }
  
  
}

#' Set the value of a field on the edit investigation page
#' 
#' Given an ID, a desired value, and the page metadata, this function will determine what type of field needs to be updated, which helper function should be used, and then update the field. It can also check to be sure the correct tab is selected prior to updating the field, although organizing your code to minimize the number of checks needed is highly recommended for speed purposes.
#' 
#' @param id String. HTML ID or metadata label for field.
#' @param value String. Value to send to the field.
#' @param metadata Can be one of several things. A dataframe pulled from nbs_page_metadata_get(), the url for the edit investigation page, the condition name, or the page name. If NA, will attempt to autodetect (slower).
#' @param check_tab T/F. If FALSE, assumes the element is visible currently (faster). If TRUE, a check is run to see if the correct tab is selected, then selects the tab if not (slower).
#' 
#' 
#' @return NULL
#' @export
nbs_field_set<-function(id,value,metadata=NA, check_tab=F){
  
  if(class(metadata)=='data.frame'){
    
  }else if(class(metadata)=='character'){
    metadata<-nbs_page_metadata_get(metadata)
  }else{
    metadata<-nbs_page_metadata_get()
  }
  
  metadata<-metadata[!is.na(metadata$question_identifier),]
  metadata_row<-metadata$question_identifier==id
  
  field_type<-toupper(metadata$data_type[metadata_row])
  input_type<-toupper(metadata$type_cd_desc[metadata_row])
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
  if(length(field_type)==0){
    message('Could not find question in metadata.')
  }else if(is.na(field_type)){
    message('No field type.')
  }else if(grepl('^MULTI-LINE NOTES WITH',input_type)){
    edit_note(id=field_id, val=as.character(value))
  }else if(field_type=='TEXT'){
    edit_text_field(id=field_id, val=as.character(value), id_type = 'id', id_suffix = '')
  }else if(input_type=='MULTI-SELECT (LIST BOX)'){
    edit_multiselect(id=field_id,values=value)
  }else if(field_type=='CODED'){
    edit_text_field(id=field_id, val=value)
  }else if(field_type=='PART'){
    edit_quick_code(field_id,value)
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
#' This function reverse searches for a quick code, given the information found in an NBS profile. This requires ODSE access and a properly formatted ODBC connection. If a code can't be found, it is set to 'nbs-bot'. Code searches and their results are stored in the qlist object, so subsequent searches for the same profile can use the archived info (faster).
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


#' Search for an NBS entity using demographic details
#' 
#' This function uses the built in NBS search UI to find a matching entity. If no entity is found, it can add one.
#' 
#' @param id The NBS element ID
#' @param value The demographic details, as found in a completed quick code
#' @param id_type What type of entity is being searched for?
#'  
#' @return Quick code
#' @export
nbs_quick_code_search<-function(id,value,id_type='Person', add=T){
  dl<-metadata$part_type_cd[metadata$question_identifier==id]
  if(id_type==Person){
    dt<-'person'
    
    
    split_text<-str_split(value,'\n')[[1]]
    name<-split_text[1]
    
    names<-str_split(name,' ')[[1]]
    providerSearch.firstName<-names[1]
    providerSearch.lastName<-names[2]
    
    if(length(split_text)>1){
      is.phone<-grepl('\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d',split_text)
      t.phone_nbr_txt<-phone<-split_text[is.phone]
      providerSearch.phoneNbrTxt<-str_extract(phone,'\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d')
      if(length(t.phone_nbr_txt)>0){
        t.extension_txt<-str_extract(phone,'Ext\\. \\d*$') %>% str_replace('Ext\\. ','')
        if(is.na(t.extension_txt)) t.extension_txt<-character(0)
      }
      is.address<-grepl('^\\d.*\\D',split_text) & !is.phone
      providerSearch.streetAddr1<-split_text[is.address]
      is.location<-!is.phone & !is.address & c(F,rep(T,length(split_text)-1))
      
      location<-split_text[is.location]
      if(length(location)>=1){
        location<-location[[1]] %>% str_split(',')
        providerSearch.cityDescTxt<-trimws(gsub('\\d','',location[[1]][1]))
        
        stoperator_textbox<-paste0(str_extract_all(location[[1]][2],'[[:alpha:]]')[[1]],collapse='')
        providerSearch.zipCd<-paste0(str_extract_all(location[[1]][2],'\\d')[[1]],collapse='')
      }
    }
    
    
    home.window <- remDr$getCurrentWindowHandle()[[1]]
    remDr$executeScript(paste0("getProvider('",id,"');"))
    Sys.sleep(.2)
    all.window <- remDr$getWindowHandles()
    preview.Window <- all.window[!all.window %in% home.window][[1]]
    chrome_window_switch(preview.Window)
    
    where_clauses<-c('providerSearch.firstName','providerSearch.lastName','providerSearch.phoneNbrTxt','providerSearch.streetAddr1','providerSearch.cityDescTxt','stoperator_textbox','providerSearch.zipCd')
    w<- where_clauses[1]
    for(w in where_clauses)  {
      if(!exists(w)) next
      if(get(w)=='NA') next
      remDr$findElement('name',w)$sendKeysToElement(list(get(w)))
    }
    
    remDr$executeScript(paste0("return findProvider('",id,"');"))
    
    rows_returned<-remDr$findElement('xpath','//*[@id="section1"]/div/div[2]/b')$getElementText()
    
    
    
    if(rows_returned=='0'){
      if(add){
        new_q<-paste0(paste0(unlist(str_extract_all(str_to_title(paste(providerSearch.firstName,providerSearch.lastName)),'[A-Z]')), collapse = ''),sample(100:999,1))
        message(paste0('No matching entity. Adding new entity with quick code ',new_q))
        
        remDr$findElement('name','quickCode')$sendKeysToElement(list(new_q))
        
        remDr$executeScript(paste0("return addEntityProvider('",id,"');"))
        chrome_window_switch(home.window)
      }else{
        message('No matched entities and add=F. To add entities, set add=T')
        remDr$executeScript('closePopup();')
        chrome_window_switch(home.window)
        new_q<-NA
      }
      #return(new_q)
    }else if(rows_returned=='1'){
      new_q<-remDr$findElement('xpath','//*[@id="parent"]/tbody/tr/td[5]/table/tbody/tr[2]/td')$getElementText()
      remDr$findElement('xpath','//*[@id="parent"]/tbody/tr/td[1]/a')$clickElement()
      #return(qcode)
    }else{
      message('Multiple entities matched. You done goofed. But I will just choose the first one.')
      new_q<-remDr$findElement('xpath','//*[@id="parent"]/tbody/tr/td[5]/table/tbody/tr[2]/td')$getElementText()
      remDr$findElement('xpath','//*[@id="parent"]/tbody/tr/td[1]/a')$clickElement()
      
    }
    return(new_q)
    
  }
  
}

#' Search for a patient by ID
#' 
#' This function searches for a patient from the home page.
#' 
#' @param id The patient ID
#'  
#' @return NULL
#' @export
nbs_patient_search<-function(id){
  remDr$findElement('id','DEM229')$sendKeysToElement(list(as.character(id)))
  remDr$executeScript('searchPatient();')
  remDr$findElement('xpath','//*[@id="searchResultsTable"]/tbody/tr/td[1]/a')$clickElement()
}


#' Go to and export an NBS report
#' 
#' This function will find an NBS report by name, then enter basic filters and column selections before exporting the report results.
#' Will first attempt to load the home page if not already there.
#' Advanced filtering not added yet, but possible.
#' 
#' @param report String. The name of the report.
#' @param basic Named list. Values to enter/select in the basic filtering page, named by their HTML ID. 
#' To click an element, use 'elementID'=NA. 
#' To enter text, use 'elementID'='text_to_enter'. 
#' To select from a dropdown, use 'elementID'=c('option1','option2'). To select just one dropdown option, you will need to provide a vector of options with a length greater than 1, so provide an additional fake option (will generate a message), or list the same option 3 times (first 2 cancel out).
#' @param columns NA/character or numeric vector. If NA, will export all columns. Otherwise, will export the columns named or specified via index.
#'  
#' @return NULL
#' @export
nbs_report<-function(report
                     , basic=list('id_T_T01a'=Sys.Date()-30, 'id_T_T01b'=Sys.Date())
                     , columns=NA
){

    ct<-unlist(remDr$getCurrentUrl())
    if(grepl('managereports',ct, ignore.case = T)){
      
    }else if(grepl('nbs/report/',ct, ignore.case = T)){
      remDr$findElement('id','id_cancel_top_ToolbarButtonGraphic')$clickElement()
    } else {
      nbs_home_page()
      remDr$navigate('https://nbsproduction.tn.gov/nbs/ManageReports.do')
    }

  
  
  # Find all dropdowns
  dropdown_boxes<-remDr$findElements('tag name','img')
  # If they are closed, open them
  for (db in dropdown_boxes){
    if(grepl('plus',db$getElementAttribute('src'))) db$clickElement()
  }
  
  ps<-remDr$getPageSource() %>% unlist() %>% rvest::read_html()
  
  tab_num<-ps %>%  html_elements(xpath='//*[@id="frm"]/table[2]/tbody/tr[2]/td/table/tbody/tr[3]/td/table[5]/tbody/tr[2]/td/table/tbody/tr/td') %>% 
    length()
  
  
  dtext<- ps %>%
    rvest::html_elements('.hoverDescLink') %>% 
    rvest::html_text() %>% 
    tolower() %>% trimws()
  
  rfound<-which(tolower(report)==dtext)
  
  if(length(rfound)==0){
    message('Report not found; check for typos')
    return(NULL)
  }
  
  if(length(rfound)>1) message('Multiple reports matched, choosing first')
  
  remDr$findElements('link text','Run')[[rfound[1]]]$clickElement()
  
  ps<-remDr$getPageSource() %>% unlist() %>% rvest::read_html()
  
  tab_num<-ps %>%  html_elements(xpath='//*[@id="frm"]/table[2]/tbody/tr[2]/td/table/tbody/tr[3]/td/table[5]/tbody/tr[2]/td/table/tbody/tr/td') %>% 
    length()
  
  if(length(basic)>0){
  for(b in 1:length(basic)){
    if(any(is.na(basic[[b]]))){
      remDr$findElement('id',names(basic[b]))$clickElement()
      next
    }
    
    if(length(basic[[b]])>1){
      bb<-tolower(basic[[b]])
      
      
      
      dlist<-ps %>%
        rvest::html_nodes(xpath=paste0('//*[@id="',names(basic[b]),'"]')) %>% 
        rvest::html_nodes('option') 
      dtext<-dlist %>% rvest::html_text() %>% tolower()
      
      for (sb in bb){
        copt<-which(sb==dtext)
        
        if(length(copt)==0){
          message(paste0("Can't find option ",sb))
          next
        }else{
          remDr$findElement('xpath',paste0('//*[@id="',names(basic[b]),'"]/option[',copt[1],']'))$clickElement()
        }
        
      }
      next
    }
    
    if('Date' %in% class(basic[[b]])){
      basic[[b]]<-TNTools::tn_clean_date(basic[[b]],'%m%d%Y')
    }
    
    remDr$findElement('id',names(basic[b]))$sendKeysToElement(list("\uE009a\uE009",basic[[b]]))
  }
  }
  
  if(tab_num>2){
  remDr$findElement('xpath','//*[@id="frm"]/table[2]/tbody/tr[2]/td/table/tbody/tr[3]/td/table[5]/tbody/tr[2]/td/table/tbody/tr/td[3]/table/tbody/tr/td/table/tbody/tr/td[2]/a')$clickElement()
  
  if(any(is.na(columns))){
    remDr$findElement('id','id_AddAll')$clickElement()
  }else if(length(columns==1)&any(columns=='')){
    
    }else{
    option_list<-remDr$findElement('id','id_AVAILABLE_COLUMNS_list')
    col_options<-option_list$getElementText()
    
    option_list_selections<-option_list$findChildElements('tag name','option')
    
    col_options<-unlist(stringr::str_split(col_options,'\n'))
    
    if('integer' %in% class(columns)){
      col_selected<-col_options[columns]
    }else{
      col_selected<-columns
    }
    
    for (cl in col_selected){
      cindex<-(1:length(col_options))[cl==col_options]
      option_list_selections[[cindex]]$clickElement()
    }
    
    remDr$findElement('id','id_Add')$clickElement()
  }
  }
  # Hit export button
  remDr$findElement('id','id_export_top_ToolbarButtonGraphic')$clickElement()
  
}

