# base.path <- 'download/Paddock_Con/'


file.move.func <- function(base.path){
  folders.vec <- list.dirs(base.path,full.names = TRUE)
  
  for (folder.nm in seq_along(folders.vec)) {
    
    if(folders.vec[folder.nm] != base.path){
      photo.vec <- list.files(folders.vec[folder.nm],full.names = F,include.dirs = FALSE,pattern ='.JPG')
      
      from.nm <- file.path(folders.vec[folder.nm],photo.vec)
      to.nm <- paste0(base.path,photo.vec)
      
      if(length(from.nm)>0){
        file.rename(from = from.nm,
                    to   = to.nm)
      }
      
      
    }
    
  }
}


file.move.func('download/Paddock_Irr/')
file.move.func('download/Paddock_Con/')