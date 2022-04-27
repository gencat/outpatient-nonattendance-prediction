########################################################
# NO-SHOWS: Download lat/lon zip codes from GeoNames http://www.geonames.org/ ####
# licensed under a Creative Commons Attribution 4.0 License
########################################################

# Functions to calculate the Haversine distance from a postal codes to the hospital.  


  # Reads the Postal Codes Dataset from GeoNames. If not found, download it. 
  readPostalCodes <- function(fileName, workingDir)
  {
    if(!file.exists(paste0(workingDir,fileName))){
      temp <- tempfile()
      download.file(url = "https://download.geonames.org/export/zip/allCountries.zip", destfile = temp)
      postalCodes <- read.table(unz(temp, fileName), 
                                sep="\t", 
                                quote="", 
                                fill=TRUE, 
                                encoding = "UTF-8",
                                stringsAsFactors = F,
                                col.names = c("Pais", "CP", "Ciutat", "A1","A2","A3","B1","B2", "B3", "Latitud", "Longitud", "Exactitud"))
      unlink(temp)
      write.table(x = postalCodes, 
                  fileEncoding = "UTF-8",
                  file = paste0(workingDir,fileName), 
                  row.names = F)
    }else{
      postalCodes = read.table(paste0(workingDir,fileName),  
                               encoding = "UTF-8",
                               header = T,
                               stringsAsFactors = F)
    }
    return(postalCodes)
  }
  
  # Calculates the distance to a given coordinates depending on postal code
  distHMB <- function(cp, coordsHMB, postalCodes)
  {
    # Manually code wrong lat/lon values in GeoName
    manualLatLonCP = data.frame( 
      CP  = c("08911", "08912", "08913", "08914", "08915", "08916", "08917", "08918", "08390", "08391", "08930", "08921", "08320", "08922", "08923", "08328","08928",
              "08105", "08020", "08329","08397","08924", "08015","08330", "08110", "08931","25552","30500","25300","08860", "08191", "08830","17430","07001","08219"),
      latlon = c("41.4517949, 2.2506723", "41.4419642, 2.2407752", "41.4386742, 2.217501", "41.4436098, 2.224852", "41.4668974, 2.2506640", "41.4756256, 2.2289224",
                 "41.45461, 2.22866999", "41.4322894, 2.2359959", "41.4708648, 2.2802337", "41.4838889, 2.267934", "41.4226883, 2.221402", "41.45403, 2.21013749",
                 "41.4806525, 2.3147172", "41.4488667, 2.210262", "41.4439159, 2.21254", "41.4921165, 2.294174", "41.51878, 2.23387", "41.5126, 2.2342",
                 "41.4226404, 2.201676", "41.5030704, 2.317164", "41.6370728, 2.672298","41.4644767, 2.199510","41.3786759, 2.1521155","41.4946275, 2.35903",
                 "41.4916022, 2.19458", "40.463667, -3.74922", "0.463667, -3.74922","38.0591761,-1.2276177","41.6485477,1.1321236", "41.277919,1.9540305",
                 "41.5019205,1.9825163", "41.323667,2.0040565","42.4268911,2.1257354", "39.5674685,2.6490027", "54.7068,25.2859"), 
      stringsAsFactors = F
    )
    
    if(cp %in% manualLatLonCP$CP){
      latlon = manualLatLonCP[manualLatLonCP$CP == cp, "latlon"]
      latlon = as.numeric(unlist(strsplit(latlon, ",")))
    }else{
      latlon = postalCodes[postalCodes$CP == cp & postalCodes$Pais == "ES", c("Latitud", "Longitud")][1,]
      latlon = as.numeric(latlon)
      if(is.na(latlon[1])){
        latlon = postalCodes[postalCodes$CP == cp, c("Latitud", "Longitud")][1,]
        latlon = as.numeric(latlon)
      }
    }
    
    d = distHaversine(coordsHMB[c(2,1)], latlon[c(2,1)]) #Distance in meters. Function expects lon/lat, not lat/lon
    return(round(d))
  }
  
  # Main function that returns different levels depending on distance to central point.
  codeDistanceHMB <- function(column, workingDir,
                              coordsHMB = c(41.4508,2.2456), 
                              levelBreaks = c(0,1200,3500,20000, 500000,Inf),
                              nameBreaks = c("Centre", "Periferia", "BCN", "Cat","Mon"))
  {
    # column expects  postal code. coordsHMB are the latitude/longitude of the central point to calculate the distance. 
    # levelBreaks is used to create the different levels 

    postalCodes = readPostalCodes(fileName = "allCountries.txt", workingDir)

    # Lookup dataset
    if(is.numeric(column)){
      column = as.character(column)
    }
    column = str_pad(column, 5, pad = "0")
    
    cps = data.frame(CP = unique(column))
    
    # Apply the function for each postal code
    cps$DifCPsHMB = sapply(X = cps$CP, FUN = distHMB, coordsHMB, postalCodes=postalCodes)
    
    # Create different levels based on distance distribution
    cps$distLevel = cut(cps$DifCPsHMB, breaks = levelBreaks, labels = nameBreaks)
    
    # Cross data to obtain levels
    newColumn = cps$distLevel[match(column, cps$CP)]
    
    return(newColumn)
  }
  
