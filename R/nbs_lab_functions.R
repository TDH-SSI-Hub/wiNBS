#' Mark an event as reviewed
#' 
#' @param processing_decision String with a processing decision (necessary for certain program areas). Leave NA is not required.
#'
#' @return "Marked as Reviewed" or "Already marked"
#' @export
nbs_lab_mark_as_reviewed<-function(processing_decision=NA){
  if('markReviewd' %in% html_attr(html_nodes(read_html(remDr$getPageSource()[[1]]),'input'),'name')){
    remDr$findElement('name','markReviewd')$clickElement()
    if(!is.na(processing_decision)){
      if(window_switch()){
      Sys.sleep(.1)
      edit_text_field('reviewReason_textbox',processing_decision, id_suffix = '')
      Sys.sleep(.1)
      remDr$executeScript('	var opener = getDialogArgument();
      var reason = null;
      if(getElementByIdOrByName("reviewReason")!=null)
    	  reason =  getElementByIdOrByName("reviewReason").value;
      if(getElementByIdOrByNameNode("markAsReviewReason", opener.document)!=null && reason!=null)
    	  getElementByIdOrByNameNode("markAsReviewReason", opener.document).value=reason;
      var markAsReviewButtonHref="";
      if(getElementByIdOrByNameNode("markAsReviewButtonHref", opener.document)!=null){
        markAsReviewButtonHref = getElementByIdOrByNameNode("markAsReviewButtonHref", opener.document).value;
      }
      if(checkRequired()){
        return false;
      }
      if(markAsReviewButtonHref!=""){
        opener.getPage(markAsReviewButtonHref+"&markAsReviewReason="+reason);
      }
       else{
    	   opener.markAsReviewed(reason);
      }
      var invest = getElementByIdOrByNameNode("blockparent", opener.document);
      if(invest==null || invest=="undefined")
    	  invest = getElementByIdOrByNameNode("pageview", opener.document);
      invest.style.display = "none";
      window.returnValue ="true";
    ')
      Sys.sleep(.1)
      window_switch(close_old = T)
      }else{
        warning('Unused processing decision')
      }
    }
    return('Marked as Reviewed')
  }else{
    return('Already marked')
  }
}



#' Go to a lab from anywhere
#' 
#' @param ID A lab ID. Required if this function is called from outside the patient page.
#' @param uid The lab uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#'
#' @return Nothing
#' @export
nbs_lab_go_to<-function(ID=NA,uid=NA,patient_page=F){
  
  ID<-as.character(ID)
  uid<-as.character(uid)
  if(sum(is.na(c(ID,uid)))==2){
    message('Error: ID or uid must be supplied')
    return(NA)
  }
  
  if(patient_page){
    
  }else{
    if(remDr$getTitle()!="NBS Dashboard"){nbs_home_page()}
    nbs_search(ID, 'Lab ID')
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
    remDr$navigate(paste0(nbs_url(),"ViewFile1.do?ContextAction=ObservationLabIDOnSummary&observationUID=",uid))
  }
  remDr$executeScript('hideBackButtonMessage()')
  
}


