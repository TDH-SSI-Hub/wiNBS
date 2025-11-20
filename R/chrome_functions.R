


#' Delete Chrome licenses
#'
#' The licenses interfere with opening browsers.
#'
#' @return Nothing
#' @export
chrome_license_clear<-function(){
  file.remove(list.files(binman::app_dir('chromedriver'), full.names = T, pattern = 'LICENSE',recursive = T))
}

#' Determine latest Chrome version
#'
#' Examines the Chrome application to identify which Chrome driver version to use.
#'
#' @param chrome_loc File location for Chrome.
#' @return The latest compatible Chromedriver version.
#' @export
chrome_version<-function(chrome_loc="C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"){
  if(!file.exists(chrome_loc)) chrome_loc<-gsub(' \\(x86\\)','',chrome_loc)
  chrome_loc<-gsub('\\\\','\\\\\\\\',chrome_loc)
  l1<-system2(command = "wmic", args = paste0('datafile where name="',chrome_loc,'" get Version /value'), stdout = TRUE, stderr = TRUE)
  l2<-stringr::str_extract(l1,pattern = "(?<=Version=)(\\d+\\.){3}")
  l3<-magrittr::extract(l2,!is.na(l2))
  l4<-stringr::str_replace_all(l3,pattern = "\\.", replacement = "\\\\.")
  l5<-paste0("^",  l4)
  l6<-stringr::str_subset(l5,string = dplyr::last(binman::list_versions(appname = "chromedriver")))
  as.character(max(as.numeric_version(l6)))
}

#' Find best Chrome Driver for use
#'
#' Compares Chromedriver versions to Chrome version. 
#' Orders driver versions by closeness to chrome version.
#'
#' @param limit How many versions should be returned?
#' @return The closest compatible chromedriver versions
#' @export
chrome_driver_versions<-function(limit=5, wmic=F){
  version_section<-function(v,s){
    as.numeric(sapply(str_split(v,'\\.'), function(x) x[[s]]))
  }
  
  if(wmic){
    path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\")
    chrome_loc<-path[["(Default)"]]
    chrome_loc<-gsub('\\\\','\\\\\\\\',chrome_loc)
    l1<-system2(command = "wmic", args = paste0('datafile where name="',chrome_loc,'" get Version /value'), stdout = TRUE, stderr = TRUE)
    l2<-stringr::str_extract(l1,pattern = "(?<=Version=)(\\d+\\.){3}\\d+$")
  }else{
    l1<-list.dirs('C:\\Program Files\\Google\\Chrome\\Application', recursive = F, full.names = F)
    l2<-stringr::str_extract(l1,pattern = "(\\d+\\.){3}\\d+$")
  }

  ref<-magrittr::extract(l2,!is.na(l2))
  options<-binman::list_versions("chromedriver")[['win64']]
  
  ref_sections<-matrix(sapply(1:4, function(x) version_section(ref,x), simplify=T), nrow=1)
  
  vmatrix<-abs(sapply(1:4, function(x) version_section(options,x), simplify=T)-matrix(ref_sections,nrow=length(options), ncol=4, byrow = T))
  
  for(e in 1:4) vmatrix[,e]<-vmatrix[,e]*1000^(4-e)
  
  output<-options[order(apply(vmatrix,1,sum))]
  return(output[1:min(limit,length(output))])
}


#' Open a Chrome browser
#'
#' Opens a Chrome browser ('remDr') on the designated port. When multiple sessions are
#' running concurrently, each session must have a different port, and kill_java
#' must be FALSE.
#'
#' @param kill_java Close all java instances (can affect external processes)
#' @param port Port to use for browser.
#' @param chromever Version of Chrome to use. Leave as NA to attempt autodetection.
#' @param print_to Folder location for downloads (only needed when printing pdfs or downloading files).
#' @return Chrome browser object
#' @export
chrome_open_browser<-function(kill_java=T, port=NA, chrome_ver=NA, print_to=getwd(), wmic=T){
  
  # Download chromedrivers
  temp<-wdman::chrome(verbose = F)
  
  if(exists('rD')) rm('rD',pos = 1)
  if(exists('remDr')) rm('remDr',pos = 1)
  chrome_license_clear()
  
  if(!is.na(print_to)){
    print_to<-gsub('/','\\\\',print_to)
    if(!grepl('\\\\$',print_to)) print_to<-paste0(print_to,'\\')
    message(paste0('Downloads routed to ',print_to))
    eCaps <- list(chromeOptions = list(args = list('--kiosk-printing'),prefs = list("savefile.default_directory"=print_to,"download.default_directory" = print_to, "printing.print_preview_sticky_settings.appState"= jsonlite::toJSON(list(recentDestinations=list(id='Save as PDF',origin='local',account=''),selectedDestinationId='Save as PDF', version=2),auto_unbox=TRUE))))
  } else{
    eCaps<-list()
  }
  if(is.na(port)){
    port<-parallelly::freePort()
  }
  
  #cver<-as.numeric_version(binman::list_versions("chromedriver")[[1]])
  #cver<-cver[order(cver, decreasing = T)]
  if(is.na(chrome_ver)){
  cver<-chrome_driver_versions(wmic=wmic)
  }else{
    cver<- chrome_ver
  }
  vtry<-0
  while(!exists('rD')){
    vtry<-vtry+1
    tryCatch({
      if(kill_java) system("taskkill /im java.exe /f", ignore.stderr=T,show.output.on.console = F)
  rD <<-RSelenium::rsDriver(browser="chrome", port=port, verbose=F,phantomver = NULL, chromever = as.character(cver[vtry]),extraCapabilities=eCaps)
  remDr <<- rD[["client"]]
    }, message=function(e){
      rm('rD',pos=1)
      rm('remDr',pos=1)
    })
  }
  

  remDr$setTimeout(type = "implicit", milliseconds = 4000)
  message("Browser 'remDr' created")
}


#' Close a Chrome browser
#'
#' Close the 'remDr' browser. If multiple sessions are running concurrently,
#' kill_java must be FALSE.
#'
#' @param kill_java Close all java instances (can affect external processes)
#' @return Nothing
#' @export
chrome_close_browser<-function(kill_java=T){
  check_in('Closed Browser','',-1)
  remDr$close()
  rD$server$stop()
  if(kill_java)  system("taskkill /im java.exe /f", ignore.stderr=T,show.output.on.console = F)
  message("Browser 'remDr' closed")
}


#' Switch to a new tab
#'
#' @param windowId Window ID for destination tab.
#' @return Nothing
#' @export
chrome_window_switch <- function( windowId) {
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

#' Switch to a new tab
#'
#' @param windowId Window ID for destination tab.
#' @param close_old T/F. Should the old window be closed?
#' @param verbose T/F. Should the new window title be printed?
#' @return T/F
#' @export
window_switch<-function( windowId=NA, close_old=F, verbose=F) {
  if(is.na(windowId)){
    home.window <- remDr$getCurrentWindowHandle()[[1]]
    all.window <- remDr$getWindowHandles()
    windowId <- all.window[!all.window %in% home.window]
  }
  if(length(windowId)==1){
    if(close_old) remDr$closeWindow()
    qpath <- sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
    remDr$queryRD(qpath, "POST", qdata = list(handle = windowId[[1]]))
    if(verbose) print(unlist(remDr$getTitle()))
    return(T)
  }else if(length(windowId)==0){
    message('No window to switch to.')
    return(F)
  }else if(length(windowId)>1){
    message('Too many windows open to auto detect windowId. Please specify windowId parameter.')
    return(F)
  }
  
}


#' Open a Firefox browser
#'
#' Opens a Firefox browser ('remDr') on the designated port. When multiple sessions are
#' running concurrently, each session must have a different port, and kill_java
#' must be FALSE.
#'
#' @param kill_java Close all java instances prior to running (can affect external processes).
#' @param port Port to use for browser. If NA, uses freeport().
#' @return Firefox browser object
#' @export
firefox_open_browser<-function(kill_java=T, port=NA){
  if (exists("rD")) 
    rm("rD", pos = 1)
  if (exists("remDr")) 
    rm("remDr", pos = 1)
  
  if (is.na(port)) {
    port <- parallelly::freePort()
  }else{
    port<-as.integer(port)
  }
  while (!exists("rD")) {
    tryCatch({
      if (kill_java) 
        system("taskkill /im java.exe /f", ignore.stderr = T, 
               show.output.on.console = F)
      rD <<- RSelenium::rsDriver(browser = "firefox"
                                 , port = port
                                 , phantomver = NULL
                                 , verbose = F
                                 , chromever = NULL
      )
      remDr <<- rD[["client"]]
    }, message = function(e) {
      rm("rD", pos = 1)
      rm("remDr", pos = 1)
    })
  }
  remDr$setTimeout(type = "implicit", milliseconds = 4000)
  message("Browser 'remDr' created")
}

#' Open a web browser
#'
#' Opens a Chrome or Firefox browser ('remDr') on the designated port. When multiple sessions are
#' running concurrently, each session must have a different port, and kill_java
#' must be FALSE.
#'
#' @param kill_java Close all java instances prior to running (can affect external processes).
#' @param port Port to use for browser. If NA, uses freeport().
#' @param ... Additonal arguments passed on to chrome_open_browser().
#' @return Nothing, but browser remDr is created in global environment.
#' @export
browser_open<-function(browser_type='Chrome',kill_java=T, port=NA, ...){
  if(tolower(browser_type)=='chrome'){
    ignore<-wdman::chrome(verbose=F)
    chrome_open_browser(kill_java,port,...)
  }else if(tolower(browser_type)=='firefox'){
    firefox_open_browser(kill_java,port)
  }else{
    message("browser_type must be one of 'Chrome' or 'Firefox'")
  }
}


#' Close a browser
#'
#' Close the 'remDr' browser. If multiple sessions are running concurrently,
#' kill_java must be FALSE or else all other sessions will be interrupted as well.
#'
#' @param kill_java Close all java instances (can affect external processes)
#' @return Nothing
#' @export
browser_close<-function(kill_java=T){
  check_in('Closed Browser','',-1)
  remDr$close()
  rD$server$stop()
  rm('remDr', envir = globalenv())
  rm('rD', envir = globalenv())
  if(kill_java)  system("taskkill /im java.exe /f", ignore.stderr=T,show.output.on.console = F)
  message("Browser 'remDr' closed")
}


