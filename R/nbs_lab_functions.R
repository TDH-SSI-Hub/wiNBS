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


