
# !!!

library(wiNBS)

# Put your dc number here
username <- ''
environment <- 'NBS Staging'

# Read in your data somehow
inputfile<-data.frame()

# Create column for logging success/failure
inputfile$MAR <- NA

browser_open()


# Loop through inputfile
for (i in seq_len(nrow(inputfile))){

  # If you aren't at the NBS home page, log into NBS
  if(remDr$getTitle()[[1]] != "NBS Dashboard") nbs_load(username, environment, process =  'NBS MAR Template')

  # Use try catch to keep working through loop in case of errors in one row
  tryCatch({

  # Go to the lab
  nbs_go_to(inputfile$lab_rpt_local_id[i])

  # Mark lab as reviewed, store output in column
  inputfile$MAR[i] <- nbs_lab_mark_as_reviewed()

  # Go back to the home page
  nbs_home_page()

  # Print out progress
  message(paste0(i,' - ',inputfile$lab_rpt_local_id[i],' - ',inputfile$MAR[i]))

  },error=function(e) {message(e)}
  )

}

write.csv(inputfile,"!!!_output.csv")

