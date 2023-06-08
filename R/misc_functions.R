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



