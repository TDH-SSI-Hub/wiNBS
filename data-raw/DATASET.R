library(usethis)
library(RODBC)

# county to region
prod<-odbcConnect('NBS_Prod')
page_metadata<-sqlQuery(prod,"select * from [nbs_odse].[dbo].[NBS_ui_metadata]")
use_data(page_metadata, overwrite = T)

condition_metadata<-sqlQuery(prod,"select * from [nbs_srte].[dbo].[Condition_code]")
use_data(condition_metadata, overwrite = T)
