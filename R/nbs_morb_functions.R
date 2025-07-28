#' Go to a morb report from anywhere
#' 
#' @param ID A morb report ID. Required if this function is called from outside the patient page.
#' @param uid The morb report uid. Providing a uid may be slightly faster.
#' @param patient_page Boolean. Is the function called from a patient page? By default, if an ID is supplied, the home page search is used. Setting patient_page=T speeds up code where the function is called from the patient page.
#' @param verbose T/F. Should some extra messages be printed?
#'
#' @return Nothing
#' @export
nbs_morb_go_to<-function(ID=NA,uid=NA,patient_page=F, verbose=T){
  
  go_to_event('Morbidity Report ID' , ID, uid, patient_page, verbose)
  
}