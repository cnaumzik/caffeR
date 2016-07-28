#'@export
generateNewImages <- function(imagedir = "~/main",
                              image_ids = NULL,
                              suffix = NULL,
                              image_mean = NULL){
  
  if(is.null(image_ids)){
    stop("Image IDs have to be supplied")
  }
  if(is.null(image_mean)){
    stop("The image mean file has not been supplied. Please ensure to run computeMeanHdf5 first.")
  }
  image <- EBImage::Image(image_mean,colormode = "color")
  image_list <-
    list.files(imagedir, pattern = paste0(suffix, ".jpg"))
  n <- length(image_ids)
  
  for(k in 1:n){
    file <- paste0(image_ids[k],suffix,".jpg")
    
    if(length(grep(file  ,image_list)) == 0){
        EBImage::writeImage(image,paste0(imagedir,"/",file))
    }
    
  }
  
}