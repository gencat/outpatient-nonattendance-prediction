########################################################
# NO-SHOWS: Ad-hoc functions to preprocess input data. 
########################################################

# Target variable depending on SAP state.
codeTarget <- function(column)
{
  return(factor(column == "70", levels = c(T,F), labels = c("Yes","No"))) # TRUE -> show, FALSE -> no-show
}  

# Time intervals of the visit
codeTime <- function(column, 
                     timeBreaks = c(00,07,09,11,13,15,17,19,23),
                     timeLabels = c("-08","08-10","10-11","12-14","14-16","16-18","18-20","20-"))
{
  rtime = as.numeric(substring(str_pad(column,6,pad="0"),1,2))
  return(cut(x = rtime, breaks=timeBreaks, labels=timeLabels))
}  

# Number of visit
codeNumVisit <- function(column)
{
  return(cut(column,breaks=c(0,1,2,1000),
             labels=c("PRI","SEG","SUC")))
}  

# Age interval
codeAge <- function(column,
                    ageBreaks = c(0,14,18,25,35,50,65,80,1000),
                    ageLabels = c("H14","H18","H25","H35","H50","H65","H80","M80"))
{
  nAge = as.numeric((Sys.Date()-as.Date(as.character(column), "%Y%m%d"))/365.25)
  return(cut(nAge,breaks = ageBreaks, 
             labels=ageLabels))
}  

# Nacionality
codeCountry <- function(column)
{
  nCountry = as.character(column)
  nCountry[column=="ES"] = "ESP"
  nCountry[column==" "]="NoInformada"
  nCountry[column!="ES" & column!=" "]="OTRAS"
  return(as.factor(nCountry))
}  

# Reason of outpatient visit
codeReason <- function(column)
{
  return(factor(column, levels = c("CO","DR","OR","PR"), 
                labels = c("Control","Diag. Rapid","Ordinaria","Preferent")))
}  

# Year of outpatient visit
codeYear <- function(column)
{
  return(as.factor(substring(column,1,4)))
}  

# Month of outpatient visit
codeMonth <- function(column)
{
  return(as.factor(substring(column,5,6)))
}  

# Day of outpatient visit
codeDay <- function(column, dateFormat = "%Y%m%d")
{
  return(as.factor(weekdays(as.Date(as.character(column),dateFormat))))
}  

# Waiting days
codeWaitingDays <- function(ini_date,end_date, dateFormat = "%Y%m%d")
{
  numDays=as.Date(as.character(end_date),dateFormat) - as.Date(as.character(ini_date),dateFormat)
  return(as.numeric(numDays))
}  

# Treatment category
codeTreatment <- function(column,speciality){
  t = factor(column, levels = addNA(c("V12", "V120", "V13", "V18", "V15", "P92", "V19", "P153")), 
             labels = c("First visit", "First visit referral", "Second visit", "Second visit (special)",
                        "Ambulatory surgery", "Ambulatory care", "Consultation", "Polysomnography"))
  
  tNA <- addNA(t)
  levels(tNA) <- c(levels(t), "Other")
  
  if(speciality == "DERCEHMB"){
    tNA = factor(as.character(tNA))
  }
  
  return(tNA)
}
