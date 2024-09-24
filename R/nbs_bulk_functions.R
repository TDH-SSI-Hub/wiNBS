
#' Create bulk update algorithm.
#' 
#' This function creates a data.frame that is a template for nbs_bulk_updates() algorithms.
#' 
#' @param outfile NA/Character vector. File name for output csv. If NA, no file is created, but a data.frame is returned.
#'  
#' @return null/data.frame
#' @export
nbs_bulk_template<-function(outfile=NA){
  df<-data.frame(matrix(character(0),ncol=7))
  colnames(df)<-c("Group","Field","Tab","ID","Overwrite","Value_type", "Value")
  if(is.na(outfile)){
    return(df)
  }else{
    write.csv(df, outfile, row.names = F)
  }
}


#' Perform updates to investigations using a data file and template of changes
#' 
#' This function brings together many elements of scripts used to update investigations.
#' A processing algorithm csv/dataframe is used to make changes to investigations found in each row of the data csv.
#' This function handles pre and post checks of data, error handling and re-login, time monitoring, summary printing, and process logging.
#' You can also choose to not submit open investigations, and not overwrite existing data.
#' 
#' @param data String/data.frame. The name a csv or the data.frame itself. Must contain an investigation local ID column.
#' @param algorithm String/data.frame. The name a csv or the data.frame itself which conforms to the format returned by nbs_bulk_template().
#' @param metadata NA/String/data.frame. A string acceptable to nbs_page_metadata_get() or a metadata data.frame. If NA, the metadata will be pulled for each investigation upon page loading (slower, but allows for changes to multiple condition types).
#' @param username String. The username to be used to log  into NBS initially and if an error is encountered.
#' @param envirnoment String. The NBS environment to log into. Should be "NBS Production" or "NBS Staging" typically.
#' @param cancel_open T/F. Should edits not be submitted if the case is open?
#' @param id_col String/NA. The name of the column to use for the investigation local ID. Will attempt to autodetect if NA.
#' @param uid_col String/NA. (Optional) The name of the column to use for the investigation uid. If present, nbs_investigation_go_to() will behave slightly differently which may help searches complete on some machines.
#' @param log_file String/NA. Name for the output csv. If NA, no csv will be generated.
#' @param log_every Numeric. Number of rows after which a log file is generated and a summary is printed.
#' @param message_vars Character vector. Columns to print after a row is processed. Should be columns from data, or columns created during processing (see output for examples).
#'  
#' @return data.frame
#' @export
nbs_bulk_updates<-function(data, algorithm=NA, metadata=NA, username=NA, environment='NBS Production', cancel_open=F, id_col=NA, uid_col=NA, log_file=NA, log_every=100, message_vars=c('status','mismatches','time')){
  
  nbs_load(username, environment)
  
  # Load algorithm
  if('logical' %in% class(algorithm)){
    params<-nbs_bulk_template()
  } else if('character' %in% class(algorithm)){
    params<-read.csv(algorithm)
  }else if('data.frame' %in% class(algorithm)){
    params<-algorithm
  }
  
  # load data
  if('character' %in% class(data)){
    data<-read.csv(data)
  }else if('data.frame' %in% class(data)){
    data<-data
  }
  
  # Find local id column
  if(is.na(id_col)){
    id_cols<-grep('local_id',colnames(data), ignore.case = T, value = T)
    if(length(id_cols)==0){
      stop('Could not autodetect investigation local ID column. Specify with id_col parameter.')
    }else if(length(id_cols)==1){
      id_col<-id_cols
    }else{
      id_cols<-grep('inv',id_cols, ignore.case = T, value = T)
      if(length(id_cols)!=1){
        stop('Could not autodetect investigation local ID column. Specify with id_col parameter.')
      }else{
        id_col<-id_cols
      }
    }
    
  }
  
  # Create field cols
  for(fc in params$ID){
    data[,paste0(c('old_'),fc)]<-NA
    data[,paste0(c('new_'),fc)]<-NA
    data[,paste0(c('check_'),fc)]<-NA
    data[,paste0(c('correct_'),fc)]<-NA
  }
  
  # Create log variables
  data[,c('status','mismatches','time')]<-NA
  
  # Create other helper columns
  if(nrow(params)>0){
    utabs<-unique(params$Tab)
    params$check_tab<-F
    for(t in utabs){
      params$check_tab[min((1:nrow(params))[params$Tab==t])]<-T
    }
  }
  params$check_tab[is.na(params$Tab)]<-T
  params$quick_code<-params$Value_type=='Quick Code'
  
  # Define common bot names
  qlist<-list('STI WDS Created'='STI','nbs bot, ESQ Tennessee'='nbs-bot')
  
  refresh_metadata<-F
  if('character' %in% class(metadata)){
    metadata<-nbs_page_metadata_get(metadata)
  }else if('data.frame' %in% class(metadata)){
    
  }else{
    refresh_metadata<-T
    message('Metadata not specified. Will determine on each page (slower)')
  }
  
  message(paste0('If an error occurs, will attempt to log back into ',environment))
  
  i<-1
  for(i in seq_len(nrow(data))){
    
    start_time<-Sys.time()
    tryCatch({
      # Go to investigation
      if(is.na(uid_col)){
        nbs_investigation_go_to(data[i,id_col])
      }else{
        nbs_investigation_go_to(data[i,id_col],data[i,uid_col])
      }
      
      
      
      # Record existing fields and input unique values
      ps<-remDr$getPageSource() %>% unlist() %>% read_html()
      j<-1
      for(j in seq_len(nrow(params))){
        old_col<-paste0('old_',params$ID[j])
        new_col<-paste0('new_',params$ID[j])
        data[i,old_col] <- nbs_field_get(params$ID[j],ps)
        
        if(params$quick_code[j]) data[i,old_col]<- unlist(nbs_quick_code_get(unlist(data[i,old_col])))
        if(params$Value_type[j]=='R Code') data[i,new_col] <- eval(parse(text = params$Value[j]))
        if(params$Value_type[j]=='String') data[i,new_col] <- params$Value[j]
        
        
        keep_old<-(!params$Overwrite[j] & !is.na(data[i,old_col]) ) | is.na(data[i,new_col])
        if(keep_old) data[i,new_col] <- data[i,old_col]
      }
      
      # Make edits
      nbs_investigation_edit()
      
      if(refresh_metadata) metadata<-nbs_page_metadata_get()
      
      for(j in seq_len(nrow(params))){
        if(params$quick_code[j]){
          if(params$Overwrite[j]){
            
            new_col<-paste0('new_',params$ID[j])
            
            remDr$findElement('id',paste0('clear',params$ID[j]))$clickElement()
          }else if(unlist(remDr$findElement('id',params$ID[j])$getElementText())!='') next
        }
        nbs_field_set(params$ID[j],data[i,new_col],metadata,params$check_tab[j])
      }
      
      data$is_closed[i]<-nbs_field_get('INV109')=='Closed'
      
      # Submit if not open
      if(data$is_closed[i] | !cancel_open){
        
        data$status[i] <- nbs_investigation_submit()
        # Record updated values
        ps<-remDr$getPageSource() %>% unlist() %>% read_html()
        for(j in seq_len(nrow(params))){
          check_col<-paste0('check_',params$ID[j])
          new_col<-paste0('new_',params$ID[j])
          cor_col<-paste0('correct_',params$ID[j])
          
          if(params$quick_code[j]) {
            data[i,check_col]<- nbs_quick_code_get(unlist(nbs_field_get(params$ID[j],ps)))
          }else{
            data[i,check_col] <- unlist(nbs_field_get(params$ID[j],ps))
          }
          data[i,cor_col] <- data[i,check_col]==data[i,new_col]
        }
        
        # Check for mismatches
        
        data$mismatches[i] <- sum(!data[i,paste0('correct_',params$ID)])
        if(data$mismatches[i]>0) message(paste0('Mismatch on: ',paste0(params$Field[!data[i,paste0('correct_',params$ID)]], collapse = ', ')))
        
      }else{
        if(!data$is_closed[i]) data$status[i]<-'Not submitted'
      }
      # Save field info to df
      
    },error=function(e){
      message('Error')
      if(!is.na(username)){
        nbs_load(username, environment)
      }else{
        message('Specify a username to skip over errors')
      }
    })
    
    data$time<-round(Sys.time()-start_time,2)
    
    print(paste(c(i,data[,message_vars]), collapse = ' - '))
    
    # Save data every 100 rows
    if(i %% log_every == 0 | i == nrow(data) ){
      
      message('Field comparison summary')
      data[1:i,] %>%
        select(starts_with('correct_')) %>%
        pivot_longer(starts_with('correct_'),names_to='Field', names_prefix='correct_') %>%
        group_by(Field) %>%
        summarise(Correct=sum(value, na.rm=T), Wrong=sum(!value, na.rm=T),'NA'=sum(is.na(value)), Percent=paste0(round(Correct/n()*100,2),'%')) %>%
        as.data.frame() %>% print()
      
      message('Open/Closed/Submitted summary')
      data[1:i,] %>% summarise(Attempted=n()
                               ,Open=sum(!is_closed, na.rm = T)
                               ,Closed=sum(is_closed, na.rm = T)
                               ,'NA'=sum(is.na(is_closed))
                               ,Submitted=sum(status=='Investigation has been successfully saved in the system.', na.rm=T)
                               ,Not_Submitted=Attempted-Submitted
      ) %>% print()
      
      if (!is.na(log_file)) write.csv(data,log_file, row.names = F)
      
      
      
      
    }
    
  }
  
  return(data)
  
}