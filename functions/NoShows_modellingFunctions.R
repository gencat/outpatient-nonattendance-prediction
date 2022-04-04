########################################################
# NO-SHOWS: Ad-hoc functions to create prediction models
########################################################

# 1. Helper functions: split train/test and dummyfy dataset #####

  
  # Function to split the dataset into train and test. pSplitType="balancedAll" apply a stratified random sampling
  splitTrainTest <- function(pDF,pTargetName="Target",pSplitType="balancedAll",percTrain)
  {
    if(!is.factor(pDF[,pTargetName])){
      pDF[,pTargetName]=as.factor(pDF[,pTargetName])
    }
    
    numLevels = length(levels(pDF[,pTargetName]))
    freqLevels =  sort(as.numeric(table(pDF[,pTargetName])), decreasing = F)
    minLevel = floor(min(table(pDF[,pTargetName])))
    n = nrow(pDF)
  
    # Class balancing (50/50)
    if(pSplitType == "balancedAll"){
      sizes = rep(minLevel,numLevels)
    }else{
      sizes = freqLevels
    }
  
    sampleVec=strata(pDF,pTargetName,sizes,method="srswor")
    balancedDF=pDF[sampleVec[,2],]
    
    # Train / Test
    sizes=floor(sizes*percTrain)
    sampleVec=strata(balancedDF,pTargetName,sizes,method="srswor")
    train=balancedDF[sampleVec[,2],]
    test=balancedDF[-sampleVec[,2],]
  
    return (list("train" = train, "test" = test))
  }  
  
  # Function to process categorical variables as dummy columns (separates each level into a dummy 0/1 column)
  dummify <- function(pDF,pDum,pTra,pTarget)
  {
    rDataFrame = pDF
    dummyVars = c()
    for (dummy in pDum)
    { 
      rDataFrame[,dummy] = gsub("-","_",rDataFrame[,dummy])
      rDataFrame=cSplit_e(rDataFrame, dummy, ",", type = "character", fill = 0, drop = F)
      tmpNames=names(rDataFrame)
      dummyVars=c(dummyVars,tmpNames[tmpNames %like% paste(dummy,"_",sep="")])
    }
    rDataFrame = rDataFrame[,c(pDum,pTra,pTarget,dummyVars)]
    return (list("df" = rDataFrame, "dummies" = dummyVars))
  }  

  
# 2. Model testing with cross-validation and grid search: DT, XGBoost, RF, kNN, SVM #####
  
  # Decision tree hyperparamter search grid and k-fold CV
  doDecisionTreeSearch <- function(pDF, pVarsNoDummy, pVarsDummy, pCPList, pMaxDepth, percTrain=0.75,kFoldCV=5)
  {
    splits = splitTrainTest(pDF,pTargetName="Target",pSplitType="balancedAll",percTrain = percTrain)
    trainSet = splits$train
    testSet =splits$test
    
    # Summary table to store the mean results of each 5-fold cross validation, executed for every combination of grid search
    resultColumns = c("Index","CP","MD","Acc","F1","Kappa","Sensitivity","Specificity","PosPredVal",
                      "NegPredVal","Precision","Recall","BalancedAcc")
    
    tableResults = as.data.frame(matrix(nrow=(length(cpList)*length(maxDepthList)),
                                        ncol=length(resultColumns)))
    names(tableResults) = resultColumns
    j=1
    
    for (CP in pCPList)
    {  
      for (MD in pMaxDepth)
      {  
        dtModel = rpart(Target ~ .,data=trainSet[,c(pVarsNoDummy,pVarsDummy,"Target")],
                        parms=list(split='gini'),
                        control=rpart.control(cp=CP,xval=kFoldCV,maxdepth=MD))
        
        (alfaB <- dtModel$cptable[which.min(dtModel$cptable[,4]),1])
        dtModelPruned <- prune(dtModel,cp=alfaB)
        
        preds = predict(dtModelPruned, testSet[,c(pVarsNoDummy,pVarsDummy,"Target")],type="class")
        cm=confusionMatrix(preds,testSet$Target)
        
        tableResults[j,"Index"] <- j
        tableResults[j,"CP"] <- CP
        tableResults[j,"MD"] <- MD
        tableResults[j,"Acc"] <- cm$overall["Accuracy"]
        tableResults[j,"Kappa"] <- cm$overall["Kappa"]
        tableResults[j,"Sensitivity"] <- cm$byClass["Sensitivity"]
        tableResults[j,"Specificity"] <- cm$byClass["Specificity"]
        tableResults[j,"PosPredVal"] <- cm$byClass["Pos Pred Value"]
        tableResults[j,"NegPredVal"] <- cm$byClass["Neg Pred Value"]
        tableResults[j,"Precision"] <- cm$byClass["Precision"]
        tableResults[j,"Recall"] <- cm$byClass["Recall"]
        tableResults[j,"F1"] <- cm$byClass["F1"]
        tableResults[j,"BalancedAcc"] <- cm$byClass["Balanced Accuracy"]
        j <- j +  1
      }
    }
    return(tableResults)
  }
  
  # Support Vector Machines (SVM) search grid and k-fold CV
  doSVMgridSearch <- function(pDF ,pVarsNoDummy, pVarsDummy, kernel, pGammaList, pCostList, percTrain=0.75, kFoldCV=5){
    splits = splitTrainTest(pDF,pTargetName="Target",pSplitType="balancedAll",percTrain)
    trainSet = splits$train
    testSet =splits$test
    trainSet=trainSet[complete.cases(trainSet[,c(pVarsNoDummy,pVarsDummy,"Target")]),c(pVarsNoDummy,pVarsDummy,"Target")]
    testSet=testSet[complete.cases(testSet[,c(pVarsNoDummy,pVarsDummy,"Target")]),c(pVarsNoDummy,pVarsDummy,"Target")]
    
    ranges = list(gamma = pGammaList, cost = pCostList)
    
    # instantiate data structure for reporting results
    resultColumns = c("Index","Gamma","Cost","Acc","F1","Kappa","Sensitivity","Specificity","PosPredVal",
                      "NegPredVal","Precision","Recall","BalancedAcc")
    tableResults = as.data.frame(matrix(nrow=(length(GAMMA)*length(COST)),
                                        ncol=length(resultColumns)))
    names(tableResults) = resultColumns
    
    
    # Execute for every gamma and cost value: Grid Search
    j=1
    for(g in ranges$gamma){
      for(c in ranges$cost){
        
        model_svm <- svm(formula = Target~ ., data = trainSet, kernel = kernel, gamma = g, cost = c, cross = 5)
        preds = predict(model_svm,testSet,type="class")
        cm = confusionMatrix(preds,testSet$Target)
        
        tableResults[j,"Index"] <- j
        tableResults[j,"Gamma"] <- g
        tableResults[j,"Cost"] <- c
        tableResults[j,"Acc"] <- cm$overall["Accuracy"]
        tableResults[j,"Kappa"] <- cm$overall["Kappa"]
        tableResults[j,"Sensitivity"] <- cm$byClass["Sensitivity"]
        tableResults[j,"Specificity"] <- cm$byClass["Specificity"]
        tableResults[j,"PosPredVal"] <- cm$byClass["Pos Pred Value"]
        tableResults[j,"NegPredVal"] <- cm$byClass["Neg Pred Value"]
        tableResults[j,"Precision"] <- cm$byClass["Precision"]
        tableResults[j,"Recall"] <- cm$byClass["Recall"]
        tableResults[j,"F1"] <- cm$byClass["F1"]
        tableResults[j,"BalancedAcc"] <- cm$byClass["Balanced Accuracy"]
        j <- j +  1
        
        cat(paste("Testing model ",j," out of ",nrow(tableResults),"\n",sep=""))
      }
    }
    return(tableResults)
  }
  
  # Gradient Boosting Trees (XGBoost) search grid and k-fold CV
  doXGBoostSearch <- function(pDF,label,paramGrid)
  {  
    ### Preparacion del muestreo
    numLevels=length(levels(label))
    n=nrow(pDF)
    
    ### Transformación de la label y el datast según XGBoost
    labelXGB=as.numeric(label)-1
    trainXGB = data.matrix(pDF)
    
    ### Parámetros de ajuste mediante Cross Validation
    n_folds=5
    folds = createFolds(labelXGB , k=n_folds , list=FALSE)
    
    
    # Summary table to store the mean results of each 5-fold cross validation, executed for every combination of grid search
    resultColumns = c("Index","RowSample","ColSample","TreeDepth","Eta","MinChildWeight","Gamma","NumTrees",
                      "Acc","F1","Kappa","Sensitivity","Specificity","PosPredVal","NegPredVal",
                      "Precision","Recall","BalancedAcc")
    
    tableResults = as.data.frame(matrix(nrow=nrow(paramGrid), ncol=length(resultColumns)))
    names(tableResults) = resultColumns
    j=1
    
    for (paramCase in 1:nrow(paramGrid) ) {
      resultColumns = c("Acc","F1","Kappa",
                        "Sensitivity","Specificity","PosPredVal","NegPredVal","Precision",
                        "Recall")
      resCVXGB = as.data.frame(matrix(nrow=n_folds,
                                      ncol=length(resultColumns)))
      names(resCVXGB) = resultColumns
      
      
      for (i in 1:n_folds) {
        modXGB = xgboost(data=trainXGB[folds!=i,] , label=labelXGB[folds!=i] , nrounds=paramGrid[paramCase,7] , 
                         verbose= FALSE, 
                         objective= "binary:logistic", 
                         eval_metric = "logloss",
                         params=list(
                           subsample = paramGrid[paramCase,1] , 
                           colsample_bytree = paramGrid[paramCase,2],
                           max_depth = paramGrid[paramCase,3],
                           eta = paramGrid[paramCase,4],
                           min_child_weight=paramGrid[paramCase,5],
                           gamma=paramGrid[paramCase,6]
                         )
        )
        
        predsProb =predict(modXGB, trainXGB[folds==i,])
        preds = as.numeric(predsProb > 0.5)
        
        cm=confusionMatrix(as.factor(preds), as.factor(labelXGB[folds==i]))
        
        
        resCVXGB[i,"Acc"] <- cm$overall["Accuracy"]
        resCVXGB[i,"Kappa"] <- cm$overall["Kappa"]
        resCVXGB[i,"Sensitivity"] <- cm$byClass["Sensitivity"]
        resCVXGB[i,"Specificity"] <- cm$byClass["Specificity"]
        resCVXGB[i,"PosPredVal"] <- cm$byClass["Pos Pred Value"]
        resCVXGB[i,"NegPredVal"] <- cm$byClass["Neg Pred Value"]
        resCVXGB[i,"Precision"] <- cm$byClass["Precision"]
        resCVXGB[i,"Recall"] <- cm$byClass["Recall"]
        resCVXGB[i,"F1"] <- cm$byClass["F1"]
        resCVXGB[i,"BalancedAcc"] <- cm$byClass["Balanced Accuracy"]
      }
      tableResults[j,"Index"] <- j
      tableResults[j,"RowSample"] <- paramGrid[paramCase,1]
      tableResults[j,"ColSample"] <- paramGrid[paramCase,2]
      tableResults[j,"TreeDepth"] <- paramGrid[paramCase,3]
      tableResults[j,"Eta"] <- paramGrid[paramCase,4]
      tableResults[j,"MinChildWeight"] <- paramGrid[paramCase,5]
      tableResults[j,"Gamma"] <- paramGrid[paramCase,6]
      tableResults[j,"NumTrees"] <- paramGrid[paramCase,7]
      tableResults[j,"Acc"] <-  mean(resCVXGB[,"Acc"])
      tableResults[j,"Kappa"] <- mean(resCVXGB[,"Kappa"])
      tableResults[j,"Sensitivity"] <- mean(resCVXGB[,"Sensitivity"])
      tableResults[j,"Specificity"] <- mean(resCVXGB[,"Specificity"])
      tableResults[j,"PosPredVal"] <- mean(resCVXGB[,"PosPredVal"])
      tableResults[j,"NegPredVal"] <- mean(resCVXGB[,"NegPredVal"])
      tableResults[j,"Precision"] <- mean(resCVXGB[,"Precision"])
      tableResults[j,"Recall"] <- mean(resCVXGB[,"Recall"])
      tableResults[j,"F1"] <- mean(resCVXGB[,"F1"])
      tableResults[j,"BalancedAcc"] <- mean(resCVXGB[,"BalancedAcc"])
      j <- j +  1
      
      cat(paste("Testing model ",paramCase," out of ",nrow(paramGrid),"\n",sep=""))
    }
    return(tableResults)
  }
  
  # K-Nearest Neighbours model(without cross-validation). Look for most similiar K individuals
  doKNN <- function(pDF,pSimMatrix,pTarget="Target",pkList=c(1))
  {
    nSamples=nrow(pDF)
    resultColumns = c("Index","K","Acc","F1","Kappa",
                      "Sensitivity","Specificity","PosPredVal","NegPredVal","Precision",
                      "Recall", "BalancedAcc")
    
    tableResults = as.data.frame(matrix(nrow=length(pkList),
                                        ncol=length(resultColumns)))
    names(tableResults) = resultColumns
    
    j=1
    for (k in pkList)
    {
      pred = vector("character",nSamples)
      truth = vector("character",nSamples)
      
      for (i in 1:nSamples)
      {
        simInd = order(pSimMatrix[i,],decreasing = TRUE)[1:k]
        
        # Predicción 
        neigLabels = pDF[simInd,pTarget]
        maxLabel = names(which.max(sort(table(neigLabels),decreasing=TRUE)))
        pred[i] = maxLabel
        truth[i] = as.character(pDF[i,pTarget]) 
      }  
      levels=sort(union(pred,truth))
      pred=factor(pred,levels)
      truth=factor(truth,levels)
      
      cm=confusionMatrix(pred,truth)
      tableResults[j,"Index"] = j
      tableResults[j,"K"] = k
      tableResults[j,"Acc"] = cm$overall["Accuracy"]
      tableResults[j,"Kappa"] = cm$overall["Kappa"]
      tableResults[j,"Sensitivity"] = cm$byClass["Sensitivity"]
      tableResults[j,"Specificity"] = cm$byClass["Specificity"]
      tableResults[j,"PosPredVal"] = cm$byClass["Pos Pred Value"]
      tableResults[j,"NegPredVal"] = cm$byClass["Neg Pred Value"]
      tableResults[j,"Precision"] = cm$byClass["Precision"]
      tableResults[j,"Recall"] = cm$byClass["Recall"]
      tableResults[j,"F1"] = cm$byClass["F1"]
      tableResults[j,"BalancedAcc"] = cm$byClass["Balanced Accuracy"]
      j = j +  1
    }
    return(tableResults)
  }  
  
  # Random Forest
  doRandomForest <- function(pDF,pVarsNoDummy,pVarsDummy)
  {
    splits = splitTrainTest(pDF,pTargetName="Target",pSplitType="balancedAll")
    trainSet = splits$train
    testSet =splits$test
    trainSet=trainSet[complete.cases(trainSet[,c(pVarsNoDummy,pVarsDummy,"Target")]),c(pVarsNoDummy,pVarsDummy,"Target")]
    testSet=testSet[complete.cases(testSet[,c(pVarsNoDummy,pVarsDummy,"Target")]),c(pVarsNoDummy,pVarsDummy,"Target")]
    
    rfModel = randomForest(Target ~ .,data=trainSet,
                           mtry=5,importance=FALSE,ntree=500)
    preds = predict(rfModel,testSet,type="class")
    cm=confusionMatrix(preds,testSet$Target)
    
    return (list("model" = rfModel, "cm" = cm))
  }  
  
  
# 3. Decision Tree functions to tune the model ######
  
  # Main function that models and prunes the decision tree, and creates the confusion matrix with the train/test data
  doDecisionTree <- function(pDF,pVarsNoDummy,pVarsDummy,pCP,pMD,percTrain, 
                             lossMatrix = matrix(c(0,1,1,0),2), predType = "class")
  {
    # Split train and test
    splits = splitTrainTest(pDF,pTargetName="Target",pSplitType="balancedAll",percTrain)
    trainSet = splits$train
    testSet =splits$test
    
    # Decision tree model
    dtModel = rpart(Target ~ .,data=trainSet[,c(pVarsNoDummy,pVarsDummy,"Target")],
                    parms=list(split='gini', loss = lossMatrix),
                    control=rpart.control(cp=pCP,xval=20,maxdepth=pMD))
    
    # Prune the tree
    alfaB <- dtModel$cptable[which.min(dtModel$cptable[,4]),1]
    dtModelPruned <- prune(dtModel,cp=alfaB)
    
    # Make predictions and build confusion matrix (CM)
    preds = predict(dtModelPruned,testSet[,c(pVarsNoDummy,pVarsDummy,"Target")],type=predType)
    cm=confusionMatrix(preds,testSet$Target)
    
    listToReturn = list(modelo = dtModelPruned, cm = cm)
    return(listToReturn)
  }

