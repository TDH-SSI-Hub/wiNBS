#' Mark an event as reviewed
#' 
#' @param processing_decision String with a processing decision (necessary for certain program areas). Leave NA is not required.
#'
#' @return "Marked as Reviewed" or "Already marked"
#' @export
nbs_lab_mark_as_reviewed<-function(processing_decision=NA){
    
    buttons<-html_attr(html_nodes(read_html(remDr$getPageSource()[[1]]), 
                                  "input"), "name")
    if ("markReviewd" %in% buttons | "Mark as Reviewed" %in% buttons) {
      bname<-ifelse("markReviewd" %in% buttons,'markReviewd',"Mark as Reviewed")
      remDr$findElement("name", bname)$clickElement()
      if (!is.na(processing_decision)) {
        Sys.sleep(.2)
        if (window_switch()) {
          Sys.sleep(0.1)
          edit_text_field("reviewReason_textbox", processing_decision, 
                          id_suffix = "")
          Sys.sleep(0.1)
          remDr$executeScript("\tvar opener = getDialogArgument();\n      var reason = null;\n      if(getElementByIdOrByName(\"reviewReason\")!=null)\n    \t  reason =  getElementByIdOrByName(\"reviewReason\").value;\n      if(getElementByIdOrByNameNode(\"markAsReviewReason\", opener.document)!=null && reason!=null)\n    \t  getElementByIdOrByNameNode(\"markAsReviewReason\", opener.document).value=reason;\n      var markAsReviewButtonHref=\"\";\n      if(getElementByIdOrByNameNode(\"markAsReviewButtonHref\", opener.document)!=null){\n        markAsReviewButtonHref = getElementByIdOrByNameNode(\"markAsReviewButtonHref\", opener.document).value;\n      }\n      if(checkRequired()){\n        return false;\n      }\n      if(markAsReviewButtonHref!=\"\"){\n        opener.getPage(markAsReviewButtonHref+\"&markAsReviewReason=\"+reason);\n      }\n       else{\n    \t   opener.markAsReviewed(reason);\n      }\n      var invest = getElementByIdOrByNameNode(\"blockparent\", opener.document);\n      if(invest==null || invest==\"undefined\")\n    \t  invest = getElementByIdOrByNameNode(\"pageview\", opener.document);\n      invest.style.display = \"none\";\n      window.returnValue =\"true\";\n    ")
          Sys.sleep(0.1)
          window_switch(close_old = T)
        }
        else {
          warning("Unused processing decision")
        }
      }
      return("Marked as Reviewed")
    }
    else {
      return("Already marked")
    }
  }



#' Go to a lab from anywhere
#' 
#' @param ID A lab ID. Required if this function is called from outside the patient page.
#' @param uid The lab uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#' @param verbose T/F. Should some extra messages be printed?
#'
#' @return Nothing
#' @export
nbs_lab_go_to_deprecated<-function(ID=NA,uid=NA,patient_page=F, verbose=T){
  
  ID<-as.character(ID)
  uid<-as.character(uid)
  if(sum(is.na(c(ID,uid)))==2){
    message('Error: ID or uid must be supplied')
    return(NA)
  }
  
  if(patient_page){
    
  }else{
    if(remDr$getTitle()!="NBS Dashboard"){nbs_home_page(check_legacy = T)}
    if(nbs_search(ID, 'Lab ID', verbose=verbose)){
      
    }else{
      return(F)
    }
  }

  
  if(is.na(uid)){
    lab_list<-remDr$getPageSource() %>%
      unlist() %>%
      read_html() %>%
      html_element(xpath='//*[@id="eventLabReport"]/tbody') 
    lab_table<-html_table(lab_list) 
    lab_index<-str_which(unlist(lab_table[,8]),ID)

    
    url<-remDr$findElement('xpath',paste0('//*[@id="eventLabReport"]/tbody/tr[',lab_index,']/td[1]/a'))$getElementAttribute('href')
    remDr$navigate(unlist(url))
  }else{
    remDr$navigate(paste0(nbs_url,"ViewFile1.do?ContextAction=ObservationLabIDOnSummary&observationUID=",uid))
  }
  remDr$executeScript('hideBackButtonMessage()')
  return(T)
}




#' Go to a lab from anywhere
#' 
#' @param ID A lab ID. Required if this function is called from outside the patient page.
#' @param uid The lab uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#' @param verbose T/F. Should some extra messages be printed?
#'
#' @return Nothing
#' @export
nbs_lab_go_to<-function(ID=NA,uid=NA,patient_page=F, verbose=T){
  
  go_to_event('Lab ID' , ID, uid, patient_page, verbose)
  
}



#' Associate a lab to one or more investigations
#' 
#' @param case_uids A vector of public health case uids. Not local IDs
#' @param disassociate_unlisted T/F Should cases not found in uid be unchecked?
#' 
#' @return Status message bar, or 'No association' as appropriate
#' @export
nbs_lab_associate<-function(case_uids, disassociate_unlisted=F){
  legacy<-nbs_page_is_legacy()
  if(legacy){
    button_name<-'Associate Investigation'
  }else{
    button_name<-'Associate AssociateInvestigations'
  }
  remDr$findElement('name',button_name)$clickElement() 
  
  # Find table with possible associations
  inv.table<-remDr$findElements("class", 'dtTable')[[1]]$findChildElements('tag name', 'tr')
  inv_rows<-length(inv.table)
  changed_something<-F
  anything_checked<-F
  uids<-paste0(case_uids,collapse='|')
  
  # Row 1 is the header, so need 2+ rows
  if(inv_rows>1){
    for(r in 2:inv_rows){
      # Find case uid in onclick attribute
      onclick<-inv.table[[r]]$findChildElements('tag name','a')[[1]]$getElementAttribute('onclick')
      checkbox<-remDr$findElement('xpath',paste0('//*[@id="parent"]/tbody/tr[',r-1,']/td[1]/div/input'))
      checked<-checkbox$isElementSelected()[[1]]
      if(grepl(uids,onclick )){
        checkbox<-remDr$findElement('xpath',paste0('//*[@id="parent"]/tbody/tr[',r-1,']/td[1]/div/input'))
        anything_checked<-T
        if(!checked){
          checkbox$clickElement()
          changed_something<-T
        }else{
          #remDr$findElement('id','Cancel')$clickElement()
          #remDr$acceptAlert()
          #return('Already associated')
          
        }
      }else if(disassociate_unlisted){ # Uncheck things if they arent listed and disassociate_unlisted = T
        if(checked){
          checkbox$clickElement()
          changed_something<-T
          checked<-F
        }
      }
      if(checked) anything_checked<-T
    }
  }
  
  # If something changed, click submit, otherwise click cancel and accept the alert
  if(changed_something){
    remDr$findElement('id','Submit')$clickElement()
    if(!anything_checked) remDr$acceptAlert()
    other_windows<-length(remDr$getWindowHandles())
    if(other_windows==2){
      remDr$findElement('name','theProcessingDecision_textbox')$sendKeysToElement(list('Administrative Closure'))
      remDr$executeScript('var opener = getDialogArgument();
      var reason = getElementByIdOrByName("theProcessingDecision").value;
      getElementByIdOrByNameNode("processingDecisionReason", opener.document).value=reason;
 
      if(checkRequired()){
        return false;
      }

      var invest = getElementByIdOrByNameNode("blockparent", opener.document);
      invest.style.display = "none";
      window.returnValue ="true";')
      Sys.sleep(1)
      window_switch(close_old=T)
      Sys.sleep(1)
      remDr$findElement('id','Submit')$clickElement()
      Sys.sleep(1)
      window_switch()
      window_switch(close_old=T)
    }
    if(legacy){
      return(unlist(remDr$findElement('id','error1')$getElementText()))
    }else{
      return(unlist(remDr$findElement('id','globalFeedbackMessagesBar')$getElementText()))
    }
  }else{
    remDr$findElement('id','Cancel')$clickElement()
    remDr$acceptAlert()
    return('No association')
  }
}



#' Create a lab from the patient page
#' 
#' @param fields A list of data fields to populate with information. 
#' @param inv_create T/F Should an investigation be created from this lab?
#' @param ...  If inv_create=TRUE, ... must contain parameters needed by nbs_investigation_from_patient()
#' 
#' @return Local ID of created observation
#' @export
nbs_lab_create<-function(fields,inv_create=F,...){
  if(remDr$getTitle()!='View Patient File') stop('This function can only be called from the patient page.')
  existing_data<-nbs_tables('person')
  if('eventLabReport' %in%  names(existing_data)){
    old_ids<-existing_data[['eventLabReport']]$event_id
  }else{
    old_ids<-''
  }
  
  remDr$executeScript("getWorkUpPage('/nbs/ViewFile1.do?ContextAction=AddLab');")
  wiNBS:::nbs_back_button_error_dismiss()
  lab_metadata<-page_metadata[page_metadata$investigation_form_cd=='LAB_Lab_Report',]
  
  for(f in names(fields)){
    if(tolower(f)=='add'){
      remDr$executeScript("if (pgRESULTED_TEST_CONTAINERBatchAddFunction()) writeQuestion('RESULTED_TEST_CONTAINER','patternRESULTED_TEST_CONTAINER','questionbodyRESULTED_TEST_CONTAINER');")
    }else{
      nbs_field_set(f,fields[[f]], metadata = lab_metadata, check_tab = T)
    }
  }
  
  if(inv_create){
    Sys.sleep(1)
    remDr$findElement('name','SubmitAndCreateInvestiation')$clickElement()
    nbs_investigation_from_patient(...)
    remDr$findElement('link text','View File')$clickElement()
  }else{
    nbs_investigation_submit()
  }
  
  new_ids<-nbs_tables('person')[['eventLabReport']]$event_id
  return(new_ids[!new_ids %in% old_ids])
}
