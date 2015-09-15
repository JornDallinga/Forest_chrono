
############################################################
Buffer_Point <- function(BufferDistance, select_chrono) {
  
  
  NeedUTMOutput <- F #Set to True if used to convert it to UTM.

  ## Selecting unique chronosequences
  Buffer_union <- NULL
  
  t <- 1
  for (i in 1:nrow(select_chrono)){
    CountryShape <- getData('GADM', country = as.character(select_chrono[t,1]), level=1)
    coordsys <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    spTransform(CountryShape, coordsys)
    Spat <- SpatialPoints(data.frame(x = select_chrono$x[t], y = select_chrono$y[t]), proj4string = CRS(proj4string(CountryShape)))
    
    # Pinpoint UTM location in UTM grid.
    X = Spat@coords[1,1]
    Y = Spat@coords[1,2]
    UTMLocation <- utm_zone(x = X, y = Y)
    
    # Make location ready for projection change.
    Zone <- substr(UTMLocation, 1, 2)
    Hemisphere <- substr(UTMLocation, 3,3)
    
    # Hemisphere <- "N"
    # Hemisphere <- "S"
    
    # Assign String values for CRS input.
    if(Hemisphere == "N") {
      HemiText <- "+north"
    } else if (Hemisphere == "S") {
      HemiText <- "+south"
    } else stop("Not a correct Hemisphere given")
    ZoneText = paste("+zone=", Zone, sep = "")
    
    # Combine prepared strings for final input string.
    CRSText <- paste("+proj=utm", ZoneText, HemiText, sep = " ")
    
    # Transform WGS to UTM.
    PointsUTM <- spTransform(Spat, CRS(CRSText))
    
    # Buffers the point
    BufferUTM <- gBuffer(PointsUTM, width=BufferDistance)
    # Convert to WGS
    BufferWGS <- spTransform(BufferUTM, CRS("+proj=longlat +datum=WGS84")) 
    
    if (t == 1){
      Buffer_union <- union(BufferWGS)
    } else {
      Buffer_union <- gUnion(BufferWGS, Buffer_union)
    }
    t <- t + 1 
  }
  
  
  if (NeedUTMOutput == F) {
    BufferWGS <- spTransform(BufferUTM, CRS("+proj=longlat +datum=WGS84"))
    saveRDS(Buffer_union, file = "Data/BufferWGS.rds", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  } else if (NeedUTMOutput == T) {
    saveRDS(BufferUTM, file = "Data/BufferUTM.rds", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  }
  
  
  #as = T
  #return(as)

  
}


#plot(CountryShape)
#plot(BufferWGS, add=TRUE, col = "red")
#Spat
#CountryShape
