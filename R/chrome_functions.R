


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
chrome_open_browser<-function(kill_java=T, port=4636L, chrome_ver=NA, print_to=NA){
  if(!is.na(print_to)){
    eCaps <- list(chromeOptions = list(args = list('--kiosk-printing'),prefs = list("savefile.default_directory"=print_to,"download.default_directory" = print_to, "printing.print_preview_sticky_settings.appState"= jsonlite::toJSON(list(recentDestinations=list(id='Save as PDF',origin='local',account=''),selectedDestinationId='Save as PDF', version=2),auto_unbox=TRUE))))
  } else{
    eCaps<-list()
  }
  cver<-as.numeric_version(binman::list_versions("chromedriver")[[1]])
  cver<-cver[order(cver, decreasing = T)]
  vtry<-0
  while(!exists('rD')){
    vtry<-vtry+1
    tryCatch({
      if(kill_java) system("taskkill /im java.exe /f", ignore.stderr=T,show.output.on.console = F)
  rD <<-RSelenium::rsDriver(browser="chrome", port=port, verbose=F, chromever = as.character(cver[vtry]),extraCapabilities=eCaps)
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
#' @param browser Browser object created by chrome_open_browser()
#' @param kill_java Close all java instances (can affect external processes)
#' @return Nothing
#' @export
chrome_close_browser<-function(kill_java=T){
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

