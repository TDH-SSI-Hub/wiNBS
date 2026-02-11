#' Pull out a 5 digit zip code from a string
#'
#'
#' @param string Single string to evaluate
#' @param comma_priority TRUE will restrict the search to lines which have a comma,
#' if a comma is present. FALSE will find the first instance of 5 digits.
#' @param map_to_state_df Leaving as NA will return numeric zip code.
#' Optional data frame to map the zip codes to some other value.
#' A data frame with at least 2 columns; first column must be numeric.
#' @return 5 digit zip code or a mapped value
#' @export
extract_zipcode<-function(string, comma_priority=T, map_to_state_df=NA){
  if(!is.data.frame(map_to_state_df)|class(map_to_state_df[[1]]) %in% c('numeric','integer')){
    if(grepl(' |\\d',string)){
      lines<-unlist(str_split(string,'\\n'))

      if(comma_priority & grepl(',',string)) lines<-lines[grepl(',',lines)]

      string1<-paste(lines, collapse = ' ')
      output<-as.numeric(str_extract(string1,'\\d\\d\\d\\d\\d'))

      if(is.data.frame(map_to_state_df)){
        state<-map_to_state_df[map_to_state_df[,1]==output,2]
        if (length(state)==1){
          output<-state
        }else{
          output<-NA
        }
      }
      return(output)
    }else{
      return(NA)
    }
  }else{
    message('map_to_state_df is formatted incorrectly. Column 1 must be numeric. Column 2 must be character.')
  }
}



#' Log OOS processing to a SQL table
#'
#'
#' @param df Output data frame from one of the OOS scripts
#' @param odbc_name Name of the ODBC connection
#' @param table_name Name of the table to log results in
#'
#' @return Nothing
#' @export
log.output<-function(df, odbc_name, table_name){
  if(nrow(df)>0){
    df$Date=as.character(Sys.Date())

    important<-df[!is.na(df$local.id)&df$local.id!='',c('local.id',"person.id","program", 'address.state','error','folder','Date','Type' )]
    if (nrow(important)>1) important<-dplyr::distinct(important)

    important[is.na(important)]<-''

    sandbox<-RODBC::odbcConnect(odbc_name)
    sqlSave(sandbox,important, table_name, rownames = F, append = T)
    print(paste('Logged to ',table_name))
  }else{
    print('Nothing to log')
  }
}



#' Find an ancestor by attribute
#'
#' @param x html to search
#' @param attr Attribute to extract
#' @param val Pattern to search for
#'
#' @return xpath of attribute
#' @export
find_ancestor<-function(x,attr,val){
  xml2::xml_parents(x[[1]]) %>% 
    xml_attr(attr) %>% 
    str_extract(val)  %>% 
    .[!is.na(.)]
}


#' Find an ancestor by attribute
#'
#' @param x html to search
#' @param attr Attribute to extract
#' @param val Pattern to search for
#'
#' @return xpath of attribute
#' @export
find_ancestor_xpath<-function(x,attr,val){
  parents<-xml2::xml_parents(x[[1]]) 
  parent_bool<-parents %>% xml_attr(attr)  %>% str_detect(val)
  parent_bool[is.na(parent_bool)]<-F
  parents[parent_bool][1] %>% xml_path()
}

#' Convert a vector into a string list for SQL queries
#' @param vector Vector of values
#' @param quote T/F Should the values be surrounded by quotes?
#' @param unique T/F Should duplicate values be removed from the list?
#' @export
create_sql_list<-function(vector, quote=T, unique=T){
  sep1<-ifelse(quote,"'","")
  sep2<-ifelse(quote,"','",",")
  if(unique) vector<-unique(vector)
  return(paste0(sep1,paste0(vector, collapse = sep2),sep1))
}



#' Log event to sandbox
#' @param action Action taken by user
#' @param target Local ID for target of action
#' @param time_limit Amount of time that can pass before another SQL call is made
check_in<-function(action='',target='', time_limit=60){
  .bot_nbs_events<<-c(.bot_nbs_events,action)
  .bot_nbs_targets<<-c(.bot_nbs_targets,target)
  if(as.numeric(difftime(Sys.time(), .bot_nbs_check_time, units='secs')) > time_limit ){
    if(length(.bot_nbs_events)>0){
    sqlSave(.bot_odbc_sandbox_conn
            ,data.frame(log_time=as.character(Sys.time())
                        ,user=.bot_nbs_username
                        , environment=.bot_nbs_environment
                        , event=.bot_nbs_events
                        , target=.bot_nbs_targets
                        , process=.bot_nbs_process)
            ,'NBS_Bot_Activity'
            ,rownames = F
            ,append = T
    )
    }
    .bot_nbs_check_time<<-Sys.time()
    .bot_nbs_events<<-c()
    .bot_nbs_targets<<-c()
  }else{
    
  }
  
  
}


#' Convert a local ID into a different ID
#'
#' @param local_id Patient, lab, case report, or invesigation local ID
#' @param output_type What ID should be returned? "default" will autodetect the type.
#' For labs, investigations, and case reports, the default type is observation_uid,
#' public_health_case_uid, and nbs_document_uid, respectively. For patient local ID,
#' the default return value is the search ID. You can get the patient_uid by specifying that in the output_type.
#' Lab, case report, and person uids are only available if you specify an odbc connection 
#' that has ODS permissions.
#' @param odbc_name Name of a previously created odbc connection.
#' @param ods T/F. Does the connection have access to the ODS? Set to F unless you need it.
#'
#' @return vector of IDs
#' @export
nbs_id_convert<-function(local_id, output_type='default', odbc_name='NBS_Prod', ods=F){
  if(output_type == 'default'){
    if(grepl('obs',local_id[1],ignore.case = T)){
      output_type<-'observation_uid'
      table<-ifelse(!ods,NA,'nbs_odse..observation')
    }else if (grepl('cas',local_id[1],ignore.case = T)){
      output_type<-'public_health_case_uid'
      table<-ifelse(!ods,'rdb..PHCDemographic','nbs_odse..public_health_case')
    }else if (grepl('doc',local_id[1],ignore.case = T)){
      output_type<-'nbs_document_uid'
      table<-ifelse(!ods,NA,'nbs_odse..nbs_document')
    }else if (grepl('psn',local_id[1],ignore.case = T)){
      output_type<-'patient ID'
      table<-'yo mama'
      psn_n<-stringr::str_extract(tolower(local_id),'\\d+')
      message('Returning patient search ID')
      return(sapply(psn_n, function(x) as.numeric(x)-10^(nchar(x)-1), USE.NAMES = F))
    }else{
      output_type<-NA
    }
  }else if(output_type=='person_uid'){
    table<-ifelse(!ods,NA,'nbs_odse..person')
  }
  if(is.na(output_type)){
    message('Could not determine default output type')
    return(NA)
  }else if(is.na(table)){
    message('ID conversion for this ID type needs ODS access. Try ods=T if you have access.')
    return(NA)
  }else if(output_type=='patient ID'){
    
  }else if(output_type %in% c('observation_uid','public_health_case_uid','nbs_document_uid','nbs_document_uid')){
    ocon<-RODBC::odbcConnect(odbc_name)
    output_df<-RODBC::sqlQuery(ocon,paste0("select distinct ",output_type,", local_id from ",table," where local_id in (",create_sql_list(local_id),")"))
    mdf<-merge(data.frame(local_id=local_id),output_df, all.x=T, sort=F)
    message(paste0('Returning ',output_type))
    mdf[,2][order(match(mdf[,1],local_id))] 
  }else{
    message('Output type not recognized')
    return(NA)
  }
}

#' @import xml2
#' @import magrittr
#' @import RSelenium
#' @import stringr
#' @import jsonlite
#' @import binman
#' @import parallelly
#' @import RODBC
#' @import rvest
#' @import dplyr
#' @import keyring
NULL

nbs_back_button_error_dismiss<-function(){
  remDr$executeScript('hideBackButtonMessage()')
}




template_name<-"nbs_bot_mar_template"
load_template_script <- function(template_name) {
  template_path <- system.file(paste0("inst/",template_name, ".R"), package = 'wiNBS')
  if (file.exists(template_path)) {
    readLines(template_path)
  } else {
    stop("Template not found.")
  }
}

template='MAR'
filename='test'
nbs_template_create <- function(template, filename){
  if(!grepl('\\.R$|\\.r$',filename)) filename<-paste0(filename,'.R')
  filename_base<-gsub('\\.R$|\\.r$','',basename(filename))
  if(template=='MAR'){
    temp_lines <- load_template_script('nbs_bot_mar_template')
    temp_lines <- gsub('!!!',filename_base,temp_lines)
    writeLines(temp_lines,filename)
  }else{
    stop('template must be one of MAR')
  }
}





#' Flag labs, morbs, or case reports to be marked as reviewed
#'
#' @param ids Local IDs for labs, morb reports, or case reports
#' @param environment Either 'NBS Production' or 'NBS Staging'
#' @param program Program area that is responsible for these events or who is running the code.
#' @param requested_by Program area that is responsible for these events or who is running the code.
#' @param comments Brief description of why these are being marked as reviewed.
#'
#' @return Nothing
#' @export
flag_for_mark_as_reviewed<-function(ids,environment,program,requested_by,comments){
  
  if(!environment %in% c('NBS Production','NBS Staging')) stop("ERROR: environment must be 'NBS Production' or 'NBS Staging'")
  
  upload_df<-data.frame(Local_ID=as.character(ids),
                        Environment=as.character(environment),
                        Program=as.character(program),
                        Requested_by=as.character(requested_by),
                        Comments=as.character(comments)
  )
  
  upload_df$Requested_date<-as.character(Sys.time())
  upload_df$Class<-'Lab'
  upload_df$Class[grepl('morb',upload_df$Class, ignore.case = T)]<-'Morb'
  upload_df$Class[grepl('DOC',upload_df$Local_ID, ignore.case = T)]<-'Case Report'
  upload_df$Status<-'Pending'
  upload_df$Status_date<-as.character(Sys.time())
  
  upload_df<-upload_df[,c('Local_ID','Environment','Program','Requested_by','Requested_date','Comments','Class','Status','Status_date')]
  
  sand<-odbcConnect('Sandbox')
  sqlSave(sand,upload_df,'NBS_Mark_As_Reviewed', rownames = F, append = T)
  
  runtimes<-c(6,10,12,16,20)
  
  next_runs<-runtimes[runtimes > lubridate::hour(as.POSIXct(Sys.time(),'America/Chicago'))]
  
  if(length(next_runs)==0){
    next_time<-runtimes[1]
  }else{
    next_time<-next_runs[1]
  }
  
  num_stuff<-table(upload_df$Class)
  
  message(paste0(paste0(num_stuff,' ', names(num_stuff), collapse=', '),' scheduled for MAR at ',next_time,':00 CT'))
  message('See the NBS_Mark_As_Reviewed table in the Sandbox to check the status')
}

#' Function for testing wiNBS
#' @param Test String of what the test is attempting
#' @param Passed T/F. Did the test pass?
test_append<-function(Test, Passed){
  if(!exists('test_df')){
    test_df<<-data.frame(Test=c(), Passed=c())
  }
  test_df<-test_df[test_df$Test!=Test,]
  test_df<<-rbind(test_df,
                  data.frame( Test=Test
                              , Passed = Passed
                  ))
  if(Passed){
    message(paste0('Test passed: ',Test))
  }else{
    message(paste0('Test FAILED: ',Test))
  }
}
