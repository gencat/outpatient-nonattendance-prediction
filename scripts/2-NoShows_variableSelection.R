########################################################
# NO-SHOWS: Data Exploration - baseline characteristics, dependence tests and correlations
########################################################

# 0. Set up ###########
  
  #Clean space
  rm(list=ls())
  
  #Libraries
  suppressWarnings(suppressMessages(library(ggcorrplot)))

  # Global parameters
  workingDirectory = "C:/Projectes/000_Incompareixences/GitRepo/outputs/"
  scriptDirectory = "C:/Projectes/000_Incompareixences/GitRepo/NoShows_GitHub/"
  fileName = "DatosProcesados"
  medicalSpecility = "DERCEHMB" 
  processedDataset = paste0(workingDirectory, "DatosProcesados", medicalSpecility, ".RData")

  
# 1. Load data and functions ###########
  
  load(file = processedDataset)
  source(file = paste0(scriptDirectory, "NoShows_variableSelectionFunctions.R"))
  source(file = paste0(scriptDirectory, "NoShows_modellingFunctions.R"))

  
# 2. Variables to explore ###########
  
  # Categorical Variables
  varsCat = c("Age",
              "Sex",
              "TimeInterval",
              "Nation",
              "DistanceHMB",
              "CivilStatus",
              "Month",
              "Day",
              "Doctor",
              "CMA",
              "IdCategoriaTractament",
              "Reason",
              "VisitNumber",
              "AttendedLastVisit")
  if(medicalSpecility == "PNECEHMB") varsCat = varsCat[!varsCat=="CMA"]
  
  # Numeric variables 
  varsNum <- c("Reprogramacions", 
               "waitingDays",
               "DiesUltimaVisita",
               "NumVisitesAnteriors",
               "PercCompaAnteriors")
  
# 3. Data exploration ###########
  
  # Baseline characteristics shows VS no-shows for categorical [ N(%) ] and numerical [ mean(sd) ]
  groupBaseCaracteristics(dat = datos, categorical_vars = varsCat, numeric_vars = varsNum)
  
  # Execute Chi-squared test for every categorical variable
  dependenceTestCategorical(dat = datos, categorical_vars = varsCat)
  
  # Execute Analysis Of Variance for every numeric variable
  dependenceTestNumeric(dat = datos, numeric_vars = varsNum)
  
  
# 4. Correlation between predictors ###########
  
  # Correlation between categorical variables
  corrCat <- categoricCorrelation(dat = datos, categorical_vars = varsCat)
  ggcorrplot(corrCat,  
             type = "lower", 
             lab = TRUE, 
             lab_size = 4, 
             method="circle", tl.cex = 10,
             ggtheme=theme_bw)
  
  
  # Correlation between continuous variables
  corrNum <- numericCorrelation(dat = datos, numeric_vars = varsNum)
  ggcorrplot(corrNum,  
             type = "lower", 
             lab = TRUE, 
             lab_size = 4, 
             method="circle", tl.cex = 10,
             ggtheme=theme_bw)
  
