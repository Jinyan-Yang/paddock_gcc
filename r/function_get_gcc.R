source('r/functions.R')
# modifed from Remko's function

# fn <- 'pic/ng/9/WSCT1262.jpg'

processImage.new <- function(fn, ROI=NULL){
  
  phot <- read_and_crop(fn,NULL)
  if(is.null(phot)){
    return(data.frame(filename=NA, GCC=NA, RCC=NA, BCC=NA, RGBtot=NA))
  }
  
  if(!is.null(ROI)){
    xmin <- ceiling(max(phot$x) * ROI[1])
    xmax <- ceiling((max(phot$x) * ROI[2]))
    
    ymin <- ceiling(max(phot$y) * ROI[3])
    ymax <- ceiling(max(phot$y) * ROI[4])
    
    phot <- phot[phot$x >= xmin & phot$x <= xmax &
                   phot$y >= ymin & phot$y <= ymax,]
  }
  library(raster)
  
  RDN <- mean(phot$R)
  GDN <- mean(phot$G)
  BDN <- mean(phot$B)
  bn <- RDN+GDN+BDN
  
  RI <- RDN/bn
  GI <- GDN/bn
  BI <- BDN/bn
  
  #GEI <- 2*GDN - (RDN + BDN)
  
  # Convention
  rgbtot <- bn * 255
  
  return(data.frame(filename=fn, GCC=GI, RCC=RI, BCC=BI, RGBtot=rgbtot))
}
# 
get_gcc_func <- function(fn, ROI=NULL){
  # path.vec='pic/ng/5/'

  # reread picture names
  # fn <- list.files(path.vec,pattern = '.JPG',full.names = T)
  # fn = c('download/irrigated/WSCT7365.JPG','download/irrigated/WSCT7366.JPG')
  
  # get the date
  date.vec <- read_exif(fn)$CreateDate

  # read and calculate gcc
  temp.ls <- list()
  for(i in seq_along(fn)){
    
    df <-  try(processImage.new(fn[i], ROI=ROI))
    if(class(df) != 'try-error'){
      temp.ls[[i]] <- df
    }
    
  }
  if(length(temp.ls)>0){
    # put gcc into a data frame
    gcc.day.df <- do.call(rbind,temp.ls)
    
    # gcc.day.df$DateTime <- as.Date(as.character(date.vec),'%Y%m%d')
    gcc.day.df$DateTime <- strptime(as.character(date.vec),'%Y:%m:%d %H:%M:%S')
    gcc.day.df$Date <-  as.Date(gcc.day.df$DateTime,'%Y:%m:%d')
    
    return(gcc.day.df)
  }

  
}

# 
get.smooth.gcc.func = function(Date.vec,gcc.vec){
  library(mgcv)
  library(lubridate)
  gam.frdm = round(length(Date.vec)/3)
  
  gam.in.df = data.frame(x = as.numeric(Date.vec),
                         y = gcc.vec)
  fit.gam <- gam(y~s(x,k = gam.frdm),data = gam.in.df)
  
  out.df = predict(fit.gam,gam.in.df)
  return(out.df)
}

# 
cal.gcc.site.func <- function(site.nm,ROI){
  site.folders <- list.files(file.path('download',site.nm),full.names = T)
  
  # define the nm of output file
  out.nm <- sprintf('cache/gcc_%s.rds',site.nm)
  
  # check if the output file exists
  if(!file.exists(out.nm)){
    gcc.old.df <- data.frame(filename = NULL,
                             GCC=NULL,
                             RCC = NULL,
                             BCC = NULL,
                             RGBtot = NULL,
                             DateTime = NULL,
                             Date = NULL)
    saveRDS(gcc.old.df,out.nm)
  }
  gcc.old.df <- readRDS(out.nm)
  # loop through plots

    # take only those are not prcessed yet
    unprocessed.vec <- setdiff(site.folders, gcc.old.df$filename)

    if(length(unprocessed.vec)>0){
      
      # loop throught the photos to get gcc
      for(j in seq_along(unprocessed.vec)){
        
        # get old gcc
        # gcc.old.df <- readRDS(out.nm)
        
        # calculated GCC
        gcc.new.df <-try( get_gcc_func(unprocessed.vec[j],ROI = ROI))
        
        if(class(gcc.new.df) != 'try-error'){
          # put old and new gcc together
          gcc.out.df <- rbind(gcc.old.df,gcc.new.df)
          
          saveRDS(gcc.out.df,out.nm)
          
          # do a print to check prograss 
          print(unprocessed.vec[j])
        }
        
       
      }
      
    }
    
  }
  


