########################################################
# NO-SHOWS: Weekly predictions. RScript inside Pentaho Data Integration automated Pipeline (ETL)
########################################################

# 0. Set up ###########
  
  #Clean space
  rm(list=ls())
  
  #Libraries
  suppressWarnings(suppressMessages(library(rpart)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(geosphere)))
  
  # Arguments gathered from PDI ETL previous steps
  args = commandArgs(trailingOnly=TRUE)
  medicalSpecility = args[1]
  workingDirectory = args[2]
  fileName = args[3]
  
  # Global parameters
  scriptDirectory = "C:/Projectes/000_Incompareixences/GitRepo/NoShows_GitHub/"
  sourceDataset = paste0(workingDirectory, fileName)
  modelPath = paste0(workingDirectory, medicalSpecility,".model")
  outputPathControl = paste0(fileName,"_Control.csv")
  outputPathIntervencio= paste0(fileName,"_Intervention.csv")

  
# 1. Load data and functions ###########
  
  datos = read.table(file = sourceDataset,header=T,sep=";",dec=",", na.strings = "")
  source(file = paste0(scriptDirectory, "NoShows_distanceFunctions.R"))
  source(file = paste0(scriptDirectory, "NoShows_processingFunctions.R"))
  source(file = paste0(scriptDirectory, "NoShows_modellingFunctions.R"))
  load(modelPath)

# 2. Data processing / Feature engineering ###########
  
  # Target
  datos$Target = codeTarget(column = datos$Estat_1)
  
  # Age intervals
  datos$Age = codeAge(column = datos$naixement,
                      ageBreaks = c(0,14,18,25,35,50,65,80,1000),
                      ageLabels = c("H14","H18","H25","H35","H50","H65","H80","M80"))
  
  # Civil status
  datos$CivilStatus = as.factor(datos$estatCivil)
  
  # Factor the visit order number into 1st visit, 2nd visit, successive
  datos$VisitNumber = codeNumVisit(column = datos$NumOrdre)
  
  # Sex
  datos$Sex = as.factor(datos$sexe)
  
  # Time intervals. 
  datos$TimeInterval = codeTime(column = datos$HoraIniciMoviment,
                                timeBreaks = c(00,07,09,11,13,15,17,19,23),
                                timeLabels = c("-08","08-10","10-11","12-14","14-16","16-18","18-20","20-"))
  
  # Factor country
  datos$Nation = codeCountry(datos$nacionalitat)
  
  # Reason of appointment
  datos$Reason = codeReason(datos$idMotiuMoviment1)
  
  # Waiting days (registration date - visit date). 
  datos$waitingDays = codeWaitingDays(datos$DataCreacio,datos$DataIniciMoviment)
  
  # Visit year
  datos$Year = codeYear(datos$DataIniciMoviment)
  
  # Visit month
  datos$Month = codeMonth(datos$DataIniciMoviment)
  
  # Visit week day
  datos$Day = codeDay(column = datos$DataIniciMoviment, dateFormat = "%Y%m%d")
  
  # CMA (MOS: Major outpatient surgery) 1 if it occurred, 0 otherwise (NA by default)
  datos$CMA[is.na(datos$CMA)]=0
  
  # Procesamos comparece ultima visita
  datos$AttendedLastVisit = factor(datos[,"CompareixUltimaVisita"] == "70", levels = c(T,F), labels = c("Yes","No"))
  
  # Distance from the patient's postal code to hospital
  datos$DistanceHMB = codeDistanceHMB(column = datos$cp, workingDir = workingDirectory)
  
  # Rescheduling
  datos[is.na(datos$Reprogramacions),"Reprogramacions"] = 0
  
  # Doctor
  datos$Doctor = as.factor(datos$idMetge)
  
  # Treatment Category
  datos$Treatment = codeTreatment(column = datos$IdCategoriaTractament, speciality = medicalSpecility)
  
# 3. Data selection ###########
  
  # Only face-to-face visits. Delete outpatients with treatment category = remote
  datos = subset(datos,  
                 IdCategoriaTractament != "NOPREA" &
                   IdCategoriaTractament != "NOPRES" &
                   IdCategoriaTractament != "TDERMA")
  
  # Delete visits on weekend
  datos = subset(datos,
                 Day!="domingo" & Day!="sábado")
  
  # Delete negative waiting days (post-recorded visits)
  datos = subset(datos, waitingDays > 0)
  
  # Delete unusual  time intervals ("-08","18-20","20-"), outside hospital typical schedulling
  datos = subset(datos, 
                 TimeInterval != "-08" &
                   TimeInterval != "18-20" &
                   TimeInterval != "20-")
  
  # Delete sex != male / female (only 1 case out of >160k)
  datos = subset(datos, 
                 Sex == 1 | Sex == 2)
  
  
  
# 4. Prediction ###########
  
  # Prepare input data
  varsModel = unlist(strsplit(x = as.character(modelo$modelo$terms[[3]]), split = " + ", fixed = T))[-1]
  dfModelo = datos[,c(varsModel,"Target")]
  
  # Probabilistic prediction: Define threshold to decide
  threshold = 0.5
  preds = predict(modelo$modelo,dfModelo[,varsModel],type="prob")[,1]
  attendance = preds>threshold
  
# 5. Stratification and output ##########
  
  # Prepare output data structure
  tableOutput = data.frame(datos[,c("nhc","Age","sexe","NumEpisodi","NumOrdre","DataIniciMoviment")],
                           preds, attendance)
  
  tableOutput$Id = row.names(tableOutput)
  noShows = tableOutput[tableOutput$attendance==FALSE,c("Id","NumEpisodi","NumOrdre","Age","sexe")]
  
  # Separate ONLY THE NOSHOWS in Intervention and Control (50/50)
  Intervention = stratified(pDF = noShows, group = c("Age","sexe"), size = 0.5)
  noShows$Grup = "Control"
  noShows[noShows$Id %in% Intervention$Id,"Grup"] = "Intervention"
  
  # Check if the stratification by age and sex is correct
  validateStrata(pDF = noShows)
  
  # Final output data
  tableOutput$Pilot = "Control"
  tableOutput[tableOutput$attendance==TRUE,"Pilot"] = NA
  tableOutput[tableOutput$Id %in% Intervention$Id, "Pilot"] = "Intervention"
  
  # Intervention list
  tableOutputIntervencio = tableOutput[tableOutput$Id %in% Intervention$Id,]
  

# 6. Output ######

  write.table(x = tableOutputIntervencio, file = outputPathIntervencio,row.names = FALSE,sep=";", dec = ",", quote = F)
  write.table(x = tableOutput, file = outputPath,row.names = FALSE,sep=";", dec = ",", quote = F)
  
  