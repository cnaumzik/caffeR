#' @export
padImage <- function(image, dim, resize_width, resize_height , image_mean) {
  
  image_output <- array(0, dim = c(resize_width, resize_height, 3))
  
  other_dim <- ifelse(dim == 1, 2, 1)
  
  if (dim == 1) {
      image <- image %>%
        EBImage::resize(w = resize_width) 
      
      lower <- max(ceiling((resize_height - dim(image)[other_dim]) / 2),1)
      upper <- lower + dim(image)[other_dim] -1
      
      image_output[,lower:upper , ] <- image
  } else {
    image <- image %>%
      EBImage::resize(h = resize_height)
    
    lower <- max(ceiling((resize_width- dim(image)[other_dim]) / 2),1)
    upper <- lower + dim(image)[other_dim] -1
    
    image_output[lower:upper, , ] <- image
  }
  
  image_output <- EBImage::Image(image_output, colormode = "color") - image_mean %>%
    EBImage::equalize(range = c(0, 1), levels = 256)
  return(image_output)
}

#'@export
preprocessImagesHdf5 <- function(input_path = NULL, padding = FALSE,
                             resize_height = 227, resize_width = 300 , image_mean = NULL) {
  
  if (is.null(input_path) || !file.exists(input_path)) {
    stop("Argument 'input_path' is NULL but must specify a folder destination.")
  }
  if(is.null(image_mean)){
    stop("Image mean must be supplied.")
  }
  
  image_read <- EBImage::readImage(input_path, type = "jpg", all = FALSE) # WxHxC
  
  
    if (padding) {
      image_output <- padImage(image_read, which.max(dim(image_read)[1:2]) , resize_width , resize_height , image_mean )
    } else {
      image_output <- image_read %>% #WxHxC
        EBImage::resize (w = resize_width , h = resize_height) - image_mean %>%
        EBImage::equalize(range = c(0, 1), levels = 256)
    }
  image_mat <- array(image_output,dim = c(resize_width, resize_height, 3)) #WxHxC
  image_mat <- image_mat[,,c(3,2,1)]#Changing from RGB to BGR due to underlying opencv implementation
  return(image_mat)
}
