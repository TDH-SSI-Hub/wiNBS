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

    sandbox<-RODBC::odbcConnect('odbc_name')
    sqlSave(sandbox,important, table_name, rownames = F, append = T)
    print(paste('Logged to ',table_name))
  }else{
    print('Nothing to log')
  }
}


#' Draft and possibly send an Outlook email.
#'
#'
#' @param to Main recipients
#' @param subject Email subject
#' @param body Email body. Searches for html line break to decide if body is html
#' @param from Alternate email to send on behalf of
#' @param attach Character vector of files to attach
#' @param cc Email cc
#' @param bcc Email bcc
#' @param visible Open the email in the viewer
#' @param check_ooo Check for and remove recipients who are out of office the entire day
#' @param send Send the email
#' @param signature Attach signature to end of email
#'
#' @return T/F
#' @export
email_draft <- function(to, subject, body, from = NA, attach = c(), cc = c(), bcc = c(), visible = T, check_ooo = F, send = F, signature=T) {
  if(require('RDCOMClient')){
    
    check_error<-tryCatch(getCOMInstance('Outlook.Application', force=F), error=function(e){
      return(e)
    })
    
    if(!"COMIDispatch" %in% unlist(class(check_error))){
      message('No Outlook instance open; signature and OOO check disabled')
      check_ooo <- F
      signature <- F
    }
    OutApp <- RDCOMClient::COMCreate("Outlook.Application", existing = F)
    email <- OutApp$CreateItem(0)
    
    # Send the message from an alternate account
    if (!is.na(from)) {
      email[["sentonbehalfofname"]] <- from
    }
    

    if(signature){
      inspector<-email$GetInspector()
      signaturetext <- email[["HTMLBody"]]
      inspector$Close(1)
    }
    
    if (visible) email$Display()
    
    for (r in c("to", "cc", "bcc")) {
      if (length(get(r)) > 0) {
        email[[r]] <- paste0(get(r), collapse = ";")
      }
    }
    
    if (check_ooo) {
      remove_list <- c()
      for (i in seq_len(email[["recipients"]]$Count())) {
        tryCatch({
        time_string <- email[["recipients"]]$item(i)[["AddressEntry"]]$GetFreeBusy(as.character(Sys.Date()), 60, T) %>% substr(1, 24)
        if (time_string == paste0(rep(3, 24), collapse = "")) {
          message(paste0(email[["recipients"]]$item(i)[["Name"]], " is OOO"))
          remove_list <- c(remove_list, i)
        } else {
          #message(email[["recipients"]]$item(i)[["Name"]])
        }
        }, error=function(e) message(paste0('Error checking OOO status for ',email[["recipients"]]$item(i)[["Name"]])))
      }
      
      for (i in remove_list) {
        email[["recipients"]]$Remove(i)
      }
    }
    

    
    email[["subject"]] <- subject
    
    
    if (grepl("<br>",body)) {
      email[["HTMLBody"]] <- body
    } else {
      email[["body"]] <- body
    }
    

    if(signature){
      email[["HTMLBody"]] <- paste0(email[["HTMLBody"]],signaturetext)
    }

    
    if (length(attach) > 0) {
      for (i in attach) {
        if (!grepl(getwd(), i)) i <- paste0(getwd(), "/", i)
        tryCatch(
          {
            email[["attachments"]]$Add(i)
          },
          error = function(e) message(paste0("Failed to attach ", i))
        )
      }
    }
    
    
    if (send) {
      
      if (email[["to"]] == "") {
        message("Email has no main recipient; email discarded")
        email$Close(1)
        return(F)
      }
      
      return(email$Send())
    }else{
      return(NA)
    }
  }else{
    message('You need to install RDCOMClient to send emails')
    return(F)
  }
}


#' Send an Outlook email. Wrapper around email_draft()
#'
#'
#' @param ... options for email_draft()
#' @param visible Open the email in the viewer
#' @param send Send the email
#'
#' @return T/F
#' @export
email_send <- function(..., visible = F, send = T) {
  email_draft(..., visible = visible, send = send)
}

