Forest_Analysis <- function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Countrycode = Countrycode, Chronosequence = Chronosequence){
  
  caption <- c("Country","Chronosequence", "Year", "Buffer", "Threshold", "x_coordinates", "y_coordinates", "Forest_cover", "Water_cover", "Cloud_cover")

  
  if (Year == 1990){
    ## writing metadata to matrix
    mat3 <- Write_metadata(mat = mat3 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = 30 )
    
    ## calculating forest cover raster
    K_1990 <- Kim_fun(Year = 19902000)
    
    if (length(K_1990) < 2){
      
    } else {
      
      ## applying SDM function to forest cover raster
      SDMK_1990 <- SDM_function(K_1990[[1]])
      
      ## reading number of columns from SDM function output
      SDMK_col <- ncol(SDMK_1990) + 10
      
      ## writing results to matrix
      mat3[i, 11:SDMK_col] <- SDMK_1990
      
      ## Forest cover calc
      FC_K_1990 <- Forest_cover(K_1990[[1]])
      mat3[i, 8] <- FC_K_1990
      ## Water
      mat3[i, 9] <- K_1990[[2]]
      ## Cloud
      mat3[i, 10] <- K_1990[[3]]
      
      
      
      ## assigning col names
      if (j < 1) {
        colnames(SDMK_1990) -> K_1990_colnames
        names(mat3) <- c(caption, K_1990_colnames)
      } else{
        
      } 
      

      
      # plotting the figures and writing to file
      Figure_output <- list(K_1990[[4]])
      plot_Figures(Figure_output,j)
      
    }
    ## create global variable of matrix
    mat3 <<- mat3
    
    matrix_list <- list(kim = mat3)

    
  } else if (Year == 2000){
    ## writing metadata to matrix
    mat <- Write_metadata(mat = mat ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold)
    
    Figure_output <- list()
    ## calculating forest cover raster
    S <- Sexton(Year, Threshold)
    
    if (length(S) < 2){
      
    } else {
      ## applying SDM function to forest cover raster
      SDMS_2000 <- SDM_function(S[[1]])
      
      ## reading number of columns from SDM function output
      SDMS_col <- ncol(SDMS_2000) + 10
      
      ## writing results to matrix
      mat[i, 11:SDMS_col] <- SDMS_2000
      
      ## Forest cover, water, cloud calc
      FC_S_2000 <- Forest_cover(S[[1]])
      mat[i, 8] <- FC_S_2000
      ## Water
      mat[i, 9] <- S[[2]]
      ## Cloud
      mat[i, 10] <- S[[3]]
      
      Figure_output[length(Figure_output)+1] <- S[[4]]
    }
    

    ## writing metadata to matrix
    mat1 <- Write_metadata(mat = mat1,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    # Resample Hansen dataset for similar dimensions
    if (length(S) < 2){
      
    } else {
      H[[3]] <- resample(H[[3]], S[[4]], method = "ngb")
      Confusion_Matrix(Sexton = S[[4]], Hansen = H[[3]])
    }
    
    Figure_output[length(Figure_output)+1] <- H[[3]]
    
    ## applying SDM function to forest cover raster
    SDMH <- SDM_function(H[[1]])
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH) + 10
    
    ## writing results to matrix
    mat1[i, 11:SDMH_col] <- SDMH
    
    ## Forest cover calc
    FC_H_2000 <- Forest_cover(H[[1]])
    mat1[i, 8] <- FC_H_2000
    ## Water
    mat1[i, 9] <- H[[2]]
    ## Cloud
    mat1[i, 10] <- 0
    
    
    ## writing metadata to matrix
    mat2 <- Write_metadata(mat = mat2 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = 30 )

    
    ## calculating forest cover raster
    K_2000 <- Kim_fun(Year = 20002005, Data = 2000)
    
    if (length(K_2000) < 2){
      
    } else {
      ## applying SDM function to forest cover raster
      SDMK_2000 <- SDM_function(K_2000[[1]])
      
      ## reading number of columns from SDM function output
      SDMK_col <- ncol(SDMK_2000) + 10
      
      ## writing results to matrix
      mat2[i, 11:SDMK_col] <- SDMK_2000
      
      ## Forest cover calc
      FC_K_2000 <- Forest_cover(K_2000[[1]])
      mat2[i, 8] <- FC_K_2000
      ## Water
      mat2[i, 9] <- K_2000[[2]]
      ## Cloud
      mat2[i, 10] <- K_2000[[3]]
      
      Figure_output[length(Figure_output)+1] <- K_2000[[4]]
    }
    
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMS_2000) -> SDMS_2000_colnames
      names(mat) <- c(caption, SDMS_2000_colnames)
      colnames(SDMK_2000) -> K_2000_colnames
      names(mat2) <- c(caption, K_2000_colnames)
      colnames(SDMH) -> SDMH_colnames
      names(mat1) <- c(caption, SDMH_colnames)
    } else{
      
    } 
    
    ## create global variable of matrix
    mat <<- mat
    mat1 <<- mat1
    mat2 <<- mat2
    
    matrix_list <- list(Sexton = mat, Hansen = mat1, Kim = mat2)
    
    
    ## listing raster files and creating forest cover figure
    #Plot_Raster <- list(S[[1]], H[[1]], K_2000[[1]])
    #Plot_function(Year = Year, BufferDistance = BufferDistance, Threshold = Threshold, Plot_Raster)

    # Create figure output 
    plot_Figures(Figure_output,j)
    
    
  } else if (Year == 2005){
    ## writing metadata to matrix
    mat4 <- Write_metadata(mat = mat4 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    Figure_output <- list()
    ## calculating forest cover raster
    S <- Sexton(Year, Threshold)
    
    if (length(S) < 2){
      
    } else {
      ## applying SDM function to forest cover raster
      SDMS_2005 <- SDM_function(S[[1]])
      
      ## reading number of columns from SDM function output
      SDMS_col <- ncol(SDMS_2005) + 10
      
      ## writing results to matrix
      mat4[i, 11:SDMS_col] <- SDMS_2005
      
      ## Forest cover calc
      FC_S_2005 <- Forest_cover(S[[1]])
      mat4[i, 8] <- FC_S_2005
      ## Water
      mat4[i, 9] <- S[[2]]
      ## Cloud
      mat4[i, 10] <- S[[3]]
      
      Figure_output[length(Figure_output)+1] <- S[[4]]
      
    }
    
    
    mat1 <- Write_metadata(mat = mat4 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold)
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    # Resample Hansen dataset for similar dimensions
    if (length(S) < 2){
      
    } else {
      H[[3]] <- resample(H[[3]], S[[4]], method = "ngb")
      Confusion_Matrix(Sexton = S[[4]], Hansen = H[[3]])
    }
    
    Figure_output[length(Figure_output)+1] <- H[[3]]
    
    ## applying SDM function to forest cover raster
    SDMH <- SDM_function(H[[1]])
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH) + 10
    
    ## writing results to matrix
    mat1[i, 11:SDMH_col] <- SDMH
    
    ## Forest cover calc
    FC_H_2000 <- Forest_cover(H[[1]])
    mat1[i, 8] <- FC_H_2000
    ## Water
    mat1[i, 9] <- H[[2]]
    ## Cloud
    mat1[i, 10] <- 0
    
    
    ## writing metadata to matrix
    mat5 <- Write_metadata(mat = mat5 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = 30 )
    
    ## calculating forest cover raster
    K_2005 <- Kim_fun(Year = 20002005, Data = 2005)
    
    if (length(K_2005) < 2){
      
    } else {
      ## applying SDM function to forest cover raster
      SDMK_2005 <- SDM_function(K_2005[[1]])
      
      ## reading number of columns from SDM function output
      SDMK_col <- ncol(SDMK_2005) + 10
      
      ## writing results to matrix
      mat5[i, 11:SDMK_col] <- SDMK_2005
      
      ## Forest cover calc
      FC_K_2005 <- Forest_cover(K_2005[[1]])
      mat5[i, 8] <- FC_K_2005
      ## Water
      mat5[i, 9] <- K_2005[[2]]
      ## Cloud
      mat5[i, 10] <- K_2005[[3]]
      
      Figure_output[length(Figure_output)+1] <- K_2005[[4]]
    }

    
    
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMS_2005) -> SDMS_2005_colnames
      names(mat4) <- c(caption, SDMS_2005_colnames)
      colnames(SDMK_2005) -> K_2005_colnames
      names(mat5) <- c(caption, K_2005_colnames)
      colnames(SDMH) -> H_2005_colnames
      names(mat1) <- c(caption, H_2005_colnames)
    } else {
      
    } 
    
    ## create global variable of matrix
    mat4 <<- mat4
    mat5 <<- mat5
    mat1 <<- mat1
    
    matrix_list <- list(Sexton = mat4, Kim = mat5, Hansen = mat1)
    
    if (length(Figure_output) == 0) {
      
    } else {
      # plotting the figures and writing to file
      plot_Figures(Figure_output,j)
    }

    
  } else if (Year >= 2001 & Year <= 2014 & Year != 2005) {
    
    ## writing metadata to matrix
    mat6 <- Write_metadata(mat = mat6 ,Countrycode = Countrycode, Chronosequence = Chronosequence, Year = Year, BufferDistance = BufferDistance, Threshold = Threshold )
    
    ## calculating forest cover raster
    H <- Hansen(Threshold, Year)
    
    ## applying SDM function to forest cover raster
    SDMH_2012 <- SDM_function(H[[1]])
    
    ## reading number of columns from SDM function output
    SDMH_col <- ncol(SDMH_2012) + 10
    
    ## writing results to matrix
    mat6[i, 11:SDMH_col] <- SDMH_2012
    
    ## Forest cover calc
    FC_H_2012 <- Forest_cover(H[[1]])
    mat6[i, 8] <- FC_H_2012
    ## Water
    mat6[i, 9] <- H[[2]]
    ## Cloud
    mat6[i, 10] <- NA
    
    ## assigning col names
    if (j < 1) {
      colnames(SDMH_2012) -> SDMH_2012_colnames
      names(mat6) <- c(caption, SDMH_2012_colnames)
      
    } else {
      
    } 
    
    ## create global variable of matrix
    mat6 <<- mat6
    matrix_list <- list(Hansen = mat6)
    
    # plotting the figures and writing to file
    Figure_output <- list(H[[3]])
    plot_Figures(Figure_output,j)
    
  } else {
    warning("No valid year")
  }
  
  return(matrix_list)


}