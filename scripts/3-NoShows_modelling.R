########################################################
# NO-SHOWS: Data processing
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
  suppressWarnings(suppressMessages(library(philentropy)))
  suppressWarnings(suppressMessages(library(e1071)))
  suppressWarnings(suppressMessages(library(nnet)))
  suppressWarnings(suppressMessages(library(xgboost)))
  suppressWarnings(suppressMessages(library(reshape2)))
  
  # Global parameters
  workingDirectory = "C:/Projectes/000_Incompareixences/GitRepo/outputs/"
  scriptDirectory = "C:/Projectes/000_Incompareixences/GitRepo/NoShows_GitHub/"
  fileName = "DatosProcesados"
  medicalSpecility = "DERCEHMB" 
  processedDataset = paste0(workingDirectory, "DatosProcesados", medicalSpecility, ".RData")

  
# 1. Load data and functions ###########
  
  load(file = processedDataset)
  source(file = paste0(scriptDirectory, "NoShows_modellingFunctions.R"))


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
  
  # Setup A: without dummy variables
  varsModelo <- c(varsNum, varsCat)
    
  varsDummy = c()
  dfModelo = datos[,c(varsModelo,varsDummy,"Target")]
  
  dummyObj = dummify(dfModelo,varsDummy,varsModelo,"Target")
  dummyVars = dummyObj$dummies
  dummyDF = dummyObj$df
  
  
  # Setup B: categorical as dummy
  varsTrabajoB <- c(varsNum)
  
  varsDummyB = c(varsCat)
  dfModeloB = datos[,c(varsTrabajoB,varsDummyB,"Target")]
  
  dummyObjB = dummify(dfModeloB,varsDummyB,varsTrabajoB,"Target")
  dummyVarsB = dummyObjB$dummies
  dummyDFB = dummyObjB$df

  
# 3. Support Vector Machines (SVM) with Radial kernel: grid search with 5-Fold Cross-Validation #######

  # Grid search: 20 SVM models depending on Gamma (5) and Cost (4). k-Fold CV for each of them
  GAMMA = c(0.0001, 0.001, 0.01, 0.1, 1)
  COST = 2^(c(2,4,6,9))
  
  # Execute the function to train SVM models for each cp and md
  svmResults = doSVMgridSearch(pDF = dummyDF, pVarsNoDummy = varsModelo, pVarsDummy = dummyVars, kernel = "radial", 
                               pGammaList = GAMMA, pCostList = COST, kFoldCV = 5, percTrain=0.75)
  svmResults
  
  # Select best model based on accuracy and specificity
  svmBest = svmResults[which.max(svmResults$Acc + svmResults$Specificity),]
  
  
# 4. Decision trees: Grid search with 5-Fold Cross-Validation ########
  
  # Create grid of hyperparameters to test
  cpList = c(0.00001,0.0001,0.001,0.01,0.1)
  maxDepthList = c(5,10,20)
  
  # Execute the function to train DT models for each cp and md
  dtResults <- doDecisionTreeSearch(pDF = dummyDF, pVarsNoDummy = varsModelo,pVarsDummy = dummyVars, 
                                    pCPList = cpList, pMaxDepth = maxDepthList, kFoldCV = 5, percTrain=0.75)
  dtResults
  
  # Select best model based on accuracy and specificity
  dtBest = dtResults[which.max(dtResults$Acc + dtResults$Specificity),]
  
  
# 5. XGBoost: grid search with 5-Fold Cross-Validation ########
  
  # Train / Test split
  splits = splitTrainTest(dummyDFB,pTargetName="Target",pSplitType="balancedAll", percTrain = .75)
  trainSet = splits$train
  testSet =splits$test
  
  # Create grid of hyperparameters to test
  NTREES=c(50,100,200,300)
  ROW_SAMP=c(0.5)
  COL_SAMP=c(0.4) 
  TREE_DEPT=c(3,5,10)
  MIN_SPLIT_GAIN=0
  ETA= 0.1
  MIN_CHILD_WEIGHT=c(3)
  GAMMA=c( 0.0  )
  
  paramGrid=expand.grid( ROW_SAMP ,COL_SAMP ,TREE_DEPT, ETA , MIN_CHILD_WEIGHT , GAMMA, NTREES )
  

  xgResults = crossValXGB(pDF = trainSet[,c(varsTrabajoB,dummyVarsB)], label = trainSet$Target, paramGrid = paramGrid)
  xgResults
  
  # Select best model based on accuracy and specificity
  xgBest <- xgResults[which.max(xgResults$Acc + xgResults$Specificity),]
  

# 6. k-Nearest Neighbours  ########
  
  # Train / Test split
  splits = splitTrainTest(dummyDF,pTargetName="Target",pSplitType="balancedAll", percTrain = .75)
  trainSet = splits$train
  testSet =splits$test
  
  # Calculate the similarity matrix according to cosine metric
  trainSet = trainSet[complete.cases(trainSet),]
  simMatrix = distance(trainSet[,c(dummyVars,varsNum)],"cosine")
  diag(simMatrix) = 0
  
  kNNResults = doKNN(pDF = trainSet,pSimMatrix = simMatrix,pTarget = "Target",pkList = c(1,2,5,10))
  kNNResults
  
  # Select best model based on accuracy and specificity
  kNNBest = kNNResults[which.max(kNNResults$Acc+kNNResults$Specificity),]
  
  
# 7. Model selection ######
  
  modelSelection = data.frame(Algorithm = c("Decision Tree", "SVM", "XGBoost", "kNN"),
                              Accuracy = c(dtBest$Acc, svmBest$Acc, xgBest$Acc, kNNBest$Acc),
                              Sensitivity = c(dtBest$Sensitivity, svmBest$Sensitivity, xgBest$Sensitivity, kNNBest$Sensitivity),
                              specificity = c(dtBest$Specificity, svmBest$Specificity, xgBest$Specificity, kNNBest$Specificity),
                              F1 = c(dtBest$F1, svmBest$F1, xgBest$F1, kNNBest$F1))
  modelSelection
  
  modelSelection_l = reshape2::melt(modelSelection, id.vars=c("Algorithm"))
  ggplot(data = modelSelection_l, aes(x = Algorithm, y = value, fill=variable))+
    geom_bar(stat = "identity", position = "dodge")
  
# Selection: DECISION TREES