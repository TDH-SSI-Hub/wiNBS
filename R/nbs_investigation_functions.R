

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
  if(pre_clear) {
    remDr$findElement(id_type,paste0(id,id_suffix))$clearElement()
    new_val<- append(new_val,"\uE009a\uE009")
  }
  new_val<- append(new_val,as.character(val))
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

#' Edit a legacy field
#' 
#' @param name String. HTML name attribute for  field.
#' @param val String. Text to enter.
#'
#' @return NULL
#' @export
edit_legacy<-function(name, val){
  edit_text_field(name,as.character(val), id_suffix = '', id_type = 'name')
}

#' Edit a note field
#' 
#' @param id String. HTML ID for note field.
#' @param val String. Text to enter.
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
      page<-gsub('View Investigation: |Add Investigation: ','',remDr$findElement('xpath','//*[@id="bd"]/h1/table/tbody/tr[1]/td[1]/a')$getElementText())
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



#' Submit changes to an investigation
#'
#' @param check_submission T/F. Should the feedback bar text be returned (page builder pages only)?
#' @param legacy T/F. Is this for a legacy page?
#'
#' @return Submission status for page-builder pages. NA for legacy pages.
#' @export
nbs_investigation_submit<-function(check_submission=T, legacy=F){
  if(legacy){
    remDr$findElement('id','submit')$clickElement()
    return(NA)
  }else{
  remDr$findElement("id", "SubmitTop")$clickElement()
  if(check_submission){
    remDr$getPageSource() %>% unlist() %>% 
      read_html() %>% 
      html_element('#globalFeedbackMessagesBar') %>% 
      html_text() %>% str_replace_all('\n','') %>% str_trim() %>% unlist()
  }
  }
}

#' Go to an invetigation from anywhere
#' 
#' @param ID An investigation ID. Required if this function is called from outside the patient page.
#' @param uid The investigation uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#' @param verbose T/F. Should some extra messages be printed?
#'
#' @return T/F
#' @export
nbs_investigation_go_to<-function(ID=NA,uid=NA,patient_page=F, verbose=T){
  
  go_to_event('Investigation ID' , ID, uid, patient_page, verbose)
  
}

#' Go to an investigation from a patient page
#' 
#' @param ID A case ID. Required if this function is called from outside the patient page.
#' @param uid The case uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#'
#' @return Nothing
#' @export
nbs_investigation_go_to_deprecated<-function(ID=NA,uid=NA,patient_page=F){
  
  ID<-as.character(ID)
  uid<-as.character(uid)
  if(sum(is.na(c(ID,uid)))==2){
    message('Error: ID or uid must be supplied')
    return(NA)
  }
  
  if(patient_page){
    
  }else{
    if(remDr$getTitle()!="NBS Dashboard"){nbs_home_page(check_legacy = T)}
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
    remDr$navigate(paste0(nbs_url,"ViewFile1.do?ContextAction=InvestigationIDOnEvents&publicHealthCaseUID=",uid))
  }
  remDr$executeScript('hideBackButtonMessage()')
  
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
  if(length(remDr$findElements('id','delete'))==4){
    notification<-remDr$findElement("id", 'patientSummaryJSP_view_notificationStatus')$getElementText()
    notification<-!notification %in% c('','REJECTED')
    remDr$findElement("id", 'delete')$clickElement()
  }else{
    notification<-grepl('COMPLETED|Approved',remDr$findElement('id','notificationHistoryTable')$getElementText(), ignore.case = T)
    
  remDr$findElement("id", 'edit')$clickElement()
  }
  # Accept the alert if there is one
  if(notification){remDr$acceptAlert()
    Sys.sleep(.5)
  }
}


#' Cancel out of investigation edits
#' 
#'
#' @return Nothing
#' @export
nbs_investigation_cancel<-function(){
  remDr$findElement("id", 'Cancel')$clickElement()
  remDr$acceptAlert()
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
#' Works best from the view page, might work from edit page. Dropdowns from edit page return all options, not the selected option.
#' 
#' @param id String. NBS question_identifier for the field.
#' @param page_source Page html. If NA, will pull current browser page (slower). For longer queries, use remDr$getPageSource() to pull the html once.
#' @param note T/F. Is the field a note field (with repeating note blocks)
#' @param legacy T/F. Is this field on a legacy page?
#' @param ... Additional arguments passed on to get_note() which determine what info is returned for notes. By default returns the text of the last note.
#'
#' @return string
#' @export
nbs_field_get<-function(id,page_source=NA, note = F, legacy=F, ...){
  
  if(legacy){
    return(unlist(remDr$findElement('id',id)$getElementText()))
  }
  
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
    text_extracted<-get_note(id, page_source, Inf, Inf,1)
  }else{
    if(grepl('Edit',page_source %>% html_element('[name="pageTop"]') %>% html_text2())){
      text_extracted<-page_source %>% html_element(paste0('#',id)) %>% html_attr('value')
      if(is.na(text_extracted)){
        text_extracted<-page_source %>% html_element(paste0('#',id)) %>% xml2::xml_text()
      }
      
    }else{
      text_extracted<-page_source %>% html_element(paste0('#',id)) %>% html_text2()
    }
    
  }
  return(text_extracted)
  
}

#' Set the value of a field on the edit investigation page
#' 
#' Given an ID, a desired value, and the page metadata, this function will determine what type of field needs to be updated, which helper function should be used, and then update the field. It can also check to be sure the correct tab is selected prior to updating the field, although organizing your code to minimize the number of checks needed is highly recommended for speed purposes.
#' 
#' @param id String. HTML ID or metadata label for field.
#' @param value String. Value to send to the field.
#' @param metadata Can be one of several things. A dataframe pulled from nbs_page_metadata_get(), the url for the edit investigation page, the condition name, or the page name. If NA, will attempt to autodetect (slower).
#' @param check_tab T/F. If FALSE, assumes the element is visible currently (faster). If TRUE, a check is run to see if the correct tab is selected, then selects the tab if not (slower).
#' @param legacy T/F. Is this field on a legacy page?
#' 
#' @return NULL
#' @export
nbs_field_set<-function(id,value,metadata=NA, check_tab=F, legacy=F){
  
  if(legacy){
    edit_legacy(id,value)
  }else{
  
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
  if(length(input_type)==0) input_type<-toupper(metadata$ui_display_type[metadata_row])
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


#' Edit the race field in an investigation
#'
#' Check (and possibly uncheck) various race checkboxes from the investigation edit page.
#' Does not check for the correct tab currently.
#' 
#' @param race String with race(s) to select. Must match race option exactly!
#' You can specify multiple race options by separating them with commas (e.g.,"Black or African American, Other")
#' @param uncheck_others T/F Should races not found in race parameter be unselected? Should usually be FALSE. 
#' Note that if TRUE, typos in the race parameter will uncheck those races. If TRUE, a blank string ("") for race will uncheck all races.
#' 
#' @return None
#' @export
nbs_investigation_race<-function(race,uncheck_others=F){
  race_options<-c('American Indian or Alaska Native'='pageClientVO.americanIndianAlskanRace'
                  ,'Asian'='pageClientVO.asianRace'
                  ,'Black or African American'='pageClientVO.africanAmericanRace'
                  ,'Native Hawaiian or Other Pacific Islander'='pageClientVO.hawaiianRace'
                  ,'White'='pageClientVO.whiteRace'
                  ,'Other'='pageClientVO.otherRace'
                  ,'Refused to answer'='pageClientVO.refusedToAnswer'
                  ,'Not Asked'='pageClientVO.notAsked'
                  ,'Unknown'='pageClientVO.unKnownRace'
  )
  
  for (ro in names(race_options)) {
    if(grepl(ro,race, ignore.case = T)){
      
      if(is.null(unlist(remDr$findElement('name',race_options[ro])$getElementAttribute('checked')))){
        remDr$findElement('name',race_options[ro])$clickElement()
      }
      
    }else if (uncheck_others){
      if(!is.null(unlist(remDr$findElement('name',race_options[ro])$getElementAttribute('checked')))){
        remDr$findElement('name',race_options[ro])$clickElement()
      }
    }
  }
}




#' Transfer an investigation
#'
#' Transfer an investigation to a different jurisdiction from the investigation view page.
#' 
#' @param jurisdiction String with new jurisdiction to switch to. Must match jurisdiction dropdown option exactly!
#' 
#' @return None
#' @export
nbs_investigation_transfer<-function(jurisdiction){
  # Open transfer window
  remDr$executeScript('return transferPamOwnership();')
  # Switch to new window
  window_switch()
  # Set jurisdiction
  remDr$findElement('name','INV107_button')$clickElement()
  Sys.sleep(.1)
  edit_text_field('INV107',jurisdiction)
  Sys.sleep(.1)
  # Run adapted submit which doesn't close the window
  remDr$executeScript('var opener = getDialogArgument();
		var newJur = getElementByIdOrByName("INV107").value;
		var exportFacility= getElementByIdOrByName("exportFacility").value;
		var theDocumentType = getElementByIdOrByName("documentType").value;
		var comment = getElementByIdOrByName("NTF137").value;
		if(validateTransferInputFields()){
			return false;
		}else{
			opener.pageTOwnership(newJur,exportFacility,comment,theDocumentType);
			var invest = getElementByIdOrByNameNode("pageview", opener.document)
			invest.style.display = "none";
                      }')
  # Switch windows and close old one
  window_switch(close_old = T)
  # Remove back button error message
  remDr$executeScript('hideBackButtonMessage()')
}

#' Switch to a tab on a legacy page
#' 
#' @param tab Number of tab to switch to
#' 
#' @return None
#' @export
nbs_legacy_tab<-function(tab){
  tabs<-remDr$findElement('xpath','/html/body/table/tbody/tr/td/table/tbody/tr[3]/td/table/tbody/tr[5]/td/table/tbody/tr[1]/td/table')$findChildElements('tag name','td')
  tabs[[tab*3]]$clickElement()
}

#' Create an investigation from a patient page
#' 
#' @param condition Condition to create. Must match dropdown value exactly.
#' @param pre_submit_fields Only for HIV/STI. Named list with exact values for referralBasis_textbox and reviewReason_textbox.
#' @param initial_data Optional. Named list with data elements and their desired values.
#' 
#' @return Status of investigation creation
#' @export
nbs_investigation_from_patient<-function(condition,pre_submit_fields=list(),initial_data=list()){
  
  if(remDr$getTitle()!='View Patient File') stop('This function can only be called from the patient page. To create an investigation from a lab, use nbs_investigation_from_lab')
  remDr$executeScript("getWorkUpPage('/nbs/ViewFile1.do?ContextAction=AddInvestigation');")
  nbs_back_button_error_dismiss()
  
  edit_text_field('ccd_textbox',condition,'name','')
  if(!is.null(names(pre_submit_fields))){
    if(!'referralBasis_textbox' %in% names(pre_submit_fields) | !'reviewReason_textbox' %in% names(pre_submit_fields)) stop('pre_submit_fields must contain reviewReason_textbox and referralBasis_textbox with correct dropdown selections. This field is only for STI and HIV conditions. Use NA or list() otherwise.')
    edit_text_field('referralBasis_textbox', pre_submit_fields[['referralBasis_textbox']],'name','')
    
    nbs_investigation_submit(legacy=T)
    window_switch()
    edit_text_field('reviewReason_textbox', pre_submit_fields[['reviewReason_textbox']],'name','')
    remDr$executeScript('var opener = getDialogArgument();
        if(checkRequired()){
		    return false;
        }
        
	        var reason = getElementByIdOrByName("reviewReason").value;
	        getElementByIdOrByNameNode("ProcessingDecision", opener.document).value=reason;
	        getElementByIdOrByNameNode("investigationType", opener.document).value="New";
	        //opener.submitForm();
	        //opener.document.forms[0].submit();
	        if(opener.submitDialog==undefined)//eICR > Choose Condition XSP page > select processing decision
				opener.document.forms[0].submit();
			else
				opener.submitDialog("submitFromProcessingDecision");
	    

        var invest = getElementByIdOrByNameNode("blockparent", opener.document);
        invest.style.display = "none";
        window.returnValue ="true";')
    window_switch(close_old = T)
  }else{
    nbs_investigation_submit(legacy=T)
  }
  
  for(d in names(initial_data)){
    nbs_field_set(d,initial_data[[d]], metadata = condition, check_tab = T)
  }
  
  return(nbs_investigation_submit())
  
  
}



