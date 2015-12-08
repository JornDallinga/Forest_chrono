# Team DDJ
# Geo-Scripting Project Week 

# Global Forest Cover Analysis with Hansen data

# Modification date: 29-01-2015
# E-mail: daniel.scheerooren@wur.nl

#-------------------------------------- Function -----------------------------------

Hansen <- function(Threshold, year = Year){
  ## Create variable Area Of Interest (aio)
  aoi <- readRDS(file = 'data/BufferWGS.rds', refhook = NULL)
    
  ## Calculate tiles needed to cover the AOI
  tiles <- calc_gfc_tiles(aoi)
  print(length(tiles))
   
  ## Download GFC data
  download_tiles(tiles, 'data')
  
  ## Extract data from tiles
  gfc_extract <- extract_gfc(aoi, "data", filename="data/extract_hansen/GFC_extract.tif", overwrite=TRUE, data_year = 2015)
  
  
  ## Apply threshold to extracted data 
  gfc_thresholded <- threshold_gfc(gfc_extract, Threshold=Threshold, 
                                   filename="data/extract_hansen/GFC_extract_thresholded.tif", overwrite=TRUE)
  
  ## Masking for water perc calculations
  mask_water <- mask(gfc_thresholded, aoi)
  
  
  ## Masking gfc data to aoi
  mask_gfc <- mask(gfc_thresholded, aoi)
  
  ## anual stack of years
  annual_Hansen <- annual_stack(mask_gfc, data_year = 2015)
  
  
  # retrieve water
  Water <- freq(mask_water$datamask, digits= 0, value = 2, useNA = no)
  listvalues <- values(mask_water$datamask)
  countcells <- count(listvalues)
  countcells <- countcells[!is.na(countcells$x),]
  total_cells <- sum(countcells$freq)
  ## percentage water
  Water_perc <- (Water / total_cells) * 100

  # Select a year from the annual Hansen dataset
  select_y <- sprintf("y%s",year)
  subset_Year <- subset(annual_Hansen, subset = select_y)
  
  # Create binary forest cover map and figure output

  if (year == 2000){
    ## create forest cover mask year 2000
    Figure_output <- mask_gfc
    
    Figure_output$forest2000[Figure_output$forest2000 == 1] <- 2 # Forest
    Figure_output$forest2000[Figure_output$forest2000 == 0] <- 1 # Non-Forest
    Figure_output$datamask[Figure_output$datamask == 2] <- 3 # water
    Figure_output$datamask[Figure_output$datamask == 0] <- 4 # No data
    suppressWarnings(Figure_output$datamask[Figure_output$datamask < 3] <- NA) # Nodata for merging
    Figure_output <- merge(Figure_output$datamask, Figure_output$forest2000, overlap = T)
    names(Figure_output) <- "Hansen"
    
    mask_gfc <- mask_gfc$forest2000
    
  } else if (year >= 2001 & year <= 2014){
    ## create forest cover mask of years 2001 till 2014
    Figure_output <- subset_Year
    
    Figure_output[Figure_output == 2] <- 8 # Non-forest
    Figure_output[Figure_output == 1] <- 2 # Forest
    Figure_output[Figure_output == 8] <- 1 # Non-forest
    
    Figure_output[Figure_output == 3] <- 1 # Non-forest
    Figure_output[Figure_output == 4 ] <- 2 # Forest gain to Forest
    Figure_output[Figure_output == 5 ] <- 2 # Forest loss and gain to Forest
    Figure_output[Figure_output == 6] <- 3 # Water
    names(Figure_output) <- "Hansen"
    
    subset_Year[subset_Year == 2 ] <- 0
    subset_Year[subset_Year == 3 ] <- 0
    subset_Year[subset_Year == 4 ] <- 1
    subset_Year[subset_Year == 5 ] <- 1
    subset_Year[subset_Year == 6 ] <- 0
    mask_gfc <- subset_Year
  } else {
    warning("invalid year")
  }
  
  names(mask_gfc) <- "Hansen"
  
  return_list <- list(mask_gfc, Water_perc, Figure_output)
  
  return (return_list)
}
