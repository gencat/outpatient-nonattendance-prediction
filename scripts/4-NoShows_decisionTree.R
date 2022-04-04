########################################################
# NO-SHOWS: Decision tree modelling
########################################################

# 0. Set up ###########
  
  #Clean space
  rm(list=ls())
  
  #Libraries
  suppressWarnings(suppressMessages(library(sampling)))
  suppressWarnings(suppressMessages(library(splitstackshape)))
  suppressWarnings(suppressMessages(library(rpart)))
  suppressWarnings(suppressMessages(library(rpart.plot)))
  suppressWarnings(suppressMessages(library(caret)))
  suppressWarnings(suppressMessages(library(data.table)))
  
  # Global parameters
  workingDirectory = "./outputs/"
  functionDirectory = "./functions/"
  fileName = "DatosProcesados"
  medicalSpecility = "DERCEHMB" 
  processedDataset = paste0(workingDirectory, "DatosProcesados", medicalSpecility, ".RData")
  modelPath =  paste0(workingDirectory, medicalSpecility,".model")
  
  
# 1. Load data and functions ###########
  
  load(file = processedDataset)
  source(file = paste0(functionDirectory, "NoShows_modellingFunctions.R"))


# 2. Settings ###########
  
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
              "Treatment",
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
  
  # Setup
  varsModel <- c(varsNum, varsCat)
  dfModelo = datos[,c(varsModel,"Target")]
  
  
# 3. Hyperparameter optimization #######
  
  # Create grid of hyperparameters to test
  cpList = c(0.00001,0.0001,0.001,0.01,0.1)
  maxDepthList = c(3,5,10,20,30)
  
  # Execute the function to train DT models for each cp and md
  dtResults <- doDecisionTreeSearch(pDF = dfModelo, pVarsNoDummy = varsModel, pVarsDummy = NULL, 
                                    pCPList = cpList, pMaxDepth = maxDepthList, kFoldCV = 5, percTrain=0.75)
  dtResults
  
  # Select best model based on accuracy and specificity
  (dtBest = dtResults[which.max(dtResults$Acc + dtResults$Specificity),])
  
  bestCP = dtBest$CP
  bestMD = dtBest$MD
  
  
# 4. Decision Tree modelling ########
  
  # LossMatrix to force optimize negative prediction
  modelo = doDecisionTree(pDF = dfModelo, pVarsNoDummy = varsModel, pVarsDummy = NULL,
                          pCP = bestCP, pMD = bestMD, percTrain=0.75, lossMatrix = matrix(c(0,1,1.2,0),2))
  
  # Confusion Matrix and Statistics
  modelo$cm
  
  # Variable Importance
  modelo$modelo$variable.importance
  
  # Tree plot
  rpart.plot(modelo$modelo, cex=0.7, extra=7, clip.right.labs = FALSE, roundint = FALSE, compress = F, box.palette ="RdBu")
  
  # Save the model
  save(modelo,file=modelPath)
  
