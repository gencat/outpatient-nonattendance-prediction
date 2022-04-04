########################################################
# NO-SHOWS: Ad-hoc functions for the pilot study
########################################################


# Function to stratify a dataset depending on the features included in "group". Specify percentage in "size" between 0 and 1
stratified <- function(pDF, group, size=0.5){
  interact <- interaction(pDF[group], drop = TRUE)
  contingency <- table(interact)
  splitTable <- split(pDF, interact)

  n <- round(contingency * size, digits = 0)

  temp <- lapply(
    names(splitTable),
    function(x) splitTable[[x]][sample(contingency[x],
                                     n[x], replace = FALSE), ])
  strata1 <- do.call("rbind", temp)
  return(strata1)
}


# Function to validate the stratification 
validateStrata <- function(pDF){
  tableEstrata = data.frame(Sample = c("NoShows", "Control","Intervention"),
                            N = c(nrow(pDF), as.numeric(table(pDF$Grup))),
                            Sex.M=0, Sex.F=0, E14=0, E18=0, E25=0, E35=0, E50=0, E65=0, E80=0, EM80=0)
  
  tableEstrata[1,c("Sex.M","Sex.F")] = round(100*table(pDF$sexe) / nrow(pDF), 2)
  tableEstrata[2,c("Sex.M","Sex.F")] = round(100*table(pDF[pDF$Grup=="Control","sexe"]) / nrow(pDF[pDF$Grup=="Control",]), 2)
  tableEstrata[3,c("Sex.M","Sex.F")] = round(100*table(pDF[pDF$Grup=="Intervention","sexe"]) / nrow(pDF[pDF$Grup=="Intervention",]), 2)
  tableEstrata[1,5:12] = round(100*table(pDF$Age) / nrow(pDF), 2)
  tableEstrata[2,5:12] = round(100*table(pDF[pDF$Grup=="Control","Age"]) / nrow(pDF[pDF$Grup=="Control",]), 2)
  tableEstrata[3,5:12] = round(100*table(pDF[pDF$Grup=="Intervention","Age"]) / nrow(pDF[pDF$Grup=="Intervention",]), 2)
  return(tableEstrata)
}