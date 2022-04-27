########################################################
# NO-SHOWS: Ad-hoc functions to explore data and select variables that explain more variance
########################################################

# 1. Tests, visualizations and exploration outputs ###########
  explorationTestCategorical <- function(dat, column, figure = F)
  {
    cat(paste0("Standard contingency table: Target vs " ,column, "(%)\n"))
    cat("-------------------------------------------------------------------------------\n")
    print(table(dat[,"Target"],dat[,column])[2,] / table(dat[,column]) *100)
    print(chisq.test(dat[,"Target"],dat[,column]))
    
    if(figure) plot(table(dat[,column])/nrow(dat)*100,xlab="Freq (%)")
  } 

  explorationTestNumerical <- function(dat, column, figure = F)
  {
    print(summary(aov(data = dat, formula = as.formula(paste("Target~", column)))))
    
    if(figure) ggplot(data = dat, aes_string(x="Target", y=column))+ geom_boxplot()
  } 

# 2. Baseline characteristics  ###############
  
  # Compute N(%) for each level of given categorical variable
  baseCaracteristics_cat <- function(dat, x){
    auxt = data.frame(Variable = character(), Level = character(), N_T = character(), N_F = character())
    l = levels(factor(as.character(dat[,x])))
    for(level in l){
      n_T = length(which(dat[dat$Target=="Yes", x]==level))
      n_F = length(which(dat[dat$Target=="No", x]==level))
      auxt = rbind(auxt,
                   data.frame(Variable = x, 
                              Level = level,
                              N_T = paste0(n_T, " (", round(100*n_T/nrow(dat[dat$Target=="Yes",]),2),")"),
                              N_F = paste0(n_F, " (", round(100*n_F/nrow(dat[dat$Target=="No",]),2),")"))
      )
    }
    # Report NAs
    na_T = length(which(is.na(dat[dat$Target=="Yes",x])))
    na_F = length(which(is.na(dat[dat$Target=="No",x])))
    auxt = rbind(auxt,
                 data.frame(Variable = x, 
                            Level = "Not Available",
                            N_T = paste0(na_T, " (", round(100*na_T/nrow(dat[dat$Target=="Yes",]),2),")"),
                            N_F = paste0(na_F, " (", round(100*na_F/nrow(dat[dat$Target=="No",]),2),")")) 
                 )
    return(auxt)
  }
  
  # Compute mean(SD) for each level of given numeri variable
  baseCaracteristics_num <- function(dat, x){    
    mean_T = round(mean(dat[dat$Target=="Yes",x], na.rm=T),2)
    sd_T = round(sd(dat[dat$Target=="Yes",x], na.rm=T),2)
    mean_F = round(mean(dat[dat$Target=="No",x], na.rm=T),2)
    sd_F = round(sd(dat[dat$Target=="No",x], na.rm=T),2)
    auxt = data.frame(Variable = x,
                      AVG_T = paste0(mean_T, " (", sd_T, ")"),
                      AVG_F = paste0(mean_F, " (", sd_F, ")"))
    return(auxt)
  }
  
  # Main function to obtain 2 tables containing baseline characteristics shows VS no-shows for categorical and numerical variables
  groupBaseCaracteristics <- function(dat, categorical_vars, numeric_vars){
    t1 = data.frame(Variable = character(), Level = character(), N_T = character(), N_F = character())
    for(catVar in categorical_vars){
      t1 = rbind(t1, baseCaracteristics_cat(dat, catVar))
    }
    cat("\n","\n","\n")
    t2 = data.frame(Variable = character(), AVG_T = character(), AVG_F = character())
    for(numVar in numeric_vars){
      t2 = rbind(t2, baseCaracteristics_num(dat, numVar))
    }
    return(list(Categorical = t1, Numerical = t2))
  }


# 3. Significance (P-values) of variables against target ######
  
  # Categorical variables
  dependenceTestCategorical <- function(dat, categorical_vars){
    t = data.frame(Variable = character(),P = character())
    for(catVar in categorical_vars){
      chisqTest = chisq.test(x = dat[,"Target"],y = dat[,catVar], simulate.p.value = TRUE)
      pval = round(chisqTest$p.value,5)
      if(pval < 0.005) pval = "< 0.005"
      t = rbind(t, data.frame(Variable = catVar, P = pval))
    }
    return(t)
  }
  
  # Numeric variable
  dependenceTestNumeric <- function(dat, numeric_vars){
    dat$Target <- dat$Target == "Yes"
    t = data.frame(Variable = character(),P = character())
    for(numVar in numeric_vars){
      aovResult = unlist(summary(aov(data = dat, formula = as.formula(paste("Target~", numVar)))))
      pval = round(aovResult["Pr(>F)1"],5)
      if(pval < 0.005) pval = "< 0.005"
      t = rbind(t, data.frame(Variable = numVar, P = pval))
    }
    return(t)
  }
  
# 4. Correlations #######
  
  # Categorical
  getCategoricCorr <- function(x,y){
    d = table(x,y) 
    d = d[as.logical(rowSums(d != 0)), ]
    d = d[,as.logical(colSums(d != 0))]
    chi2 = chisq.test(d, simulate.p.value = T)
    vcramer = sqrt((chi2$statistic / sum(d)) / ( min(ncol(d)-1, nrow(d)-1) ) )
    return(round(vcramer,2))
  }
  
  categoricCorrelation <- function(dat, categorical_vars){
    corrCat = matrix(nrow=length(categorical_vars), ncol=length(categorical_vars))
    colnames(corrCat) = categorical_vars
    rownames(corrCat) = categorical_vars
    for(i in 1:length(categorical_vars)){
      for(j in 1:length(categorical_vars)){
        corrCat[i,j] = getCategoricCorr(x = dat[,categorical_vars[i]], y=dat[,categorical_vars[j]])
      }
    }
    return(corrCat)
  }
  
  # Numeric
  numericCorrelation <- function(dat, numeric_vars){
    d = dat[,numeric_vars]
    corrNum = cor(d, method= 'pearson', use =  "complete.obs")
    return(corrNum)
  }
  
