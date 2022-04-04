########################################################
# NO-SHOWS: Data processing
########################################################

# 0. Set up ###########
  
  #Clean space
  rm(list=ls())
  
  #Libraries
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(geosphere)))

  # Global parameters
  workingDirectory = "./outputs/"
  functionDirectory = "./functions/"
  fileName = "DatosModeloIncomparecencia"
  medicalSpecility = "DERCEHMB" # DERCEHMB and PNECEHMB
  sourceDataset = paste0(workingDirectory, fileName, medicalSpecility, ".txt")

  
# 1. Load data and functions ###########
  
  datos = read.table(file = sourceDataset,header=T,sep=";",dec=",", na.strings = "")
  source(file = paste0(functionDirectory, "NoShows_distanceFunctions.R"))
  source(file = paste0(functionDirectory, "NoShows_processingFunctions.R"))


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
  
  # Define time interval
  datos = subset(datos,DataIniciMoviment >= "20150101")
  
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
  
  
# 4. Save data into an .RData object ############
  save(datos, file = paste0(workingDirectory, "DatosProcesados",medicalSpecility,".Rdata"))
  