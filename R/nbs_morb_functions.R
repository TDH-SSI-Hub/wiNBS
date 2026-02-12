#' Go to a morb report from anywhere
#' 
#' @param ID A morb report ID. Required if this function is called from outside the patient page.
#' @param uid The morb report uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#' @param verbose T/F. Should some extra messages be printed?
#'
#' @return Nothing
#' @export
nbs_morb_go_to<-function(ID=NA,uid=NA,patient_page=F, verbose=T){
  
  go_to_event('Morbidity Report ID' , ID, uid, patient_page, verbose)
  
}





#' Create a morbidity report from the patient page
#' 
#' @param fields A list of data fields to populate with information. 
#' @param inv_create T/F Should an investigation be created from this lab?
#' @param processing_decision For STI/HIV only
#' @param initial_data Optional. Named list with data elements and their desired values.
#' 
#' @return Local ID of created morbidity report
#' @export
nbs_morb_create<-function(fields,inv_create=F,processing_decision='Field Follow-up', initial_data=NA){
  if(remDr$getTitle()!='View Patient File') stop('This function can only be called from the patient page.')
  existing_data<-nbs_tables('person')
  if('eventMorbReport' %in%  names(existing_data)){
    old_ids<-existing_data[['eventMorbReport']]$event_id
  }else{
    old_ids<-''
  }
  
  remDr$executeScript("getWorkUpPage('/nbs/ViewFile1.do?ContextAction=AddMorb');")
  wiNBS:::nbs_back_button_error_dismiss()
  
  
  for(f in names(fields)){
    if(tolower(f)=='click'){
      remDr$findElement('xpath',fields[[f]])$clickElement()
    }else{
      nbs_field_set(f,fields[[f]], legacy = T)
    }
    Sys.sleep(.2)
  }
  
  if(inv_create){
    Sys.sleep(1)
    remDr$findElement('name','Submit and Create Investigation')$clickElement()
    
    window_switch()
    
    remDr$findElement('name','reviewReason_textbox')$sendKeysToElement(list(processing_decision))
    remDr$executeScript("markProcessingDecisionSubmitandCreateInv('MorbCreate','New')")
    Sys.sleep(1)
    window_switch()

    metadata<-nbs_page_metadata_get()
    if(nrow(metadata)>0){
    for(d in names(initial_data)){
      legacy<-F
      nbs_field_set(d,initial_data[[d]], metadata = metadata, check_tab = T)
      Sys.sleep(.5)
    }
    }else{
      legacy<-T
      for(d in names(initial_data)){
        edit_legacy(d,initial_data[[d]])
      }
    }
    
    nbs_investigation_submit(legacy=legacy)
    remDr$findElement('link text','View File')$clickElement()
  }else{
    nbs_investigation_submit(legacy=T)
  }
  
  new_ids<-nbs_tables('person')[['eventMorbReport']]$event_id
  return(new_ids[!new_ids %in% old_ids])
 
}

