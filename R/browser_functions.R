
library(wiNBS)
firefox_open_browser<-function(kill_java=T, port=NA){
  if (exists("rD")) 
    rm("rD", pos = 1)
  if (exists("remDr")) 
    rm("remDr", pos = 1)

  if (is.na(port)) {
    port <- parallelly::freePort()
  }
  while (!exists("rD")) {
    tryCatch({
      if (kill_java) 
        system("taskkill /im java.exe /f", ignore.stderr = T, 
               show.output.on.console = F)
      rD <<- RSelenium::rsDriver(browser = "firefox", port = port
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


browser_open<-function(browser_type='Chrome',kill_java=T, port=NA, ...){
  if(tolower(browser_type)=='chrome'){
    chrome_open_browser(kill_java,port,...)
  }else if(tolower(browser_type)=='firefox'){
    firefox_open_browser(kill_java,port)
  }
}
