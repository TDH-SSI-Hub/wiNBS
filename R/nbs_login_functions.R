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
  
  if(url=='http://cdcnbsdemo.com/nbs/login'){
    user_id<-'UserName'
    user_password<-'Password'
    demosite<-T
  }else{
    user_id<-'usr_name'
    user_password<-'usr_password'
    demosite<-F
  }
  
  .bot_nbs_username<<-u
  .bot_nbs_environment<<-environment
  .bot_nbs_events<<-c('Logged in to NBS')
  .bot_nbs_targets<<-c('')
  .bot_nbs_check_time<<-Sys.time()
  .bot_odbc_sandbox_conn <<- odbcConnect('Sandbox')
  
  remDr$navigate(url)
  

  remDr$findElement(using = "name", value = user_id)$clearElement()
  remDr$findElement(using = "name", value = user_id)$sendKeysToElement(list(u))
  remDr$findElement(using = "name", value = user_password)$clearElement()
  remDr$findElement(using = "name", value = user_password)$sendKeysToElement(list(password))
  if(demosite){
    remDr$findElement("id", "id_Submit_top_ToolbarButtonGraphic")$clickElement()
  }else{
  remDr$findElements("tag name", "button")[[1]]$clickElement()
    Sys.sleep(1)
    if (remDr$getTitle() == "Welcome!") {
      remDr$findElements("class", "btn")[[1]]$clickElement()
    }
    if(!is.na(environment)){
      Sys.sleep(.5)
      #remDr$findElements("link text", environment)[[1]]$clickElement()
      remDr$navigate(paste0('https://',stringr::str_replace_all(stringr::str_to_lower(environment),' ',''),'.tn.gov:443/login/index.asp'))
    }
  }
  
  nbs_url<<-nbs_url_get()

}

