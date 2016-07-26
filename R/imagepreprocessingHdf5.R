#' @export
padImage <- function(image, dim, resize_width, resize_height) {
  image_output <- EBImage::Image(array(0, dim = c(resize_width, resize_height, 3)), colormode = "color")
  
  image <- image %>%
    EBImage::equalize(range = c(0, 1), levels = 256)
  
  other_dim <- ifelse(dim == 1, 2, 1)
  
  if (dim == 1) {
    image <- image %>%
      EBImage::resize(w = resize_width)
    
    lower <- ceiling((resize_height - dim(image)[other_dim]) / 2)
    upper <- lower + dim(image)[other_dim] - 1
    image_output[ , lower:upper, ] <- image
  } else {
    image <- image %>%
      EBImage::resize(h = resize_height)
    
    lower <- ceiling((resize_width - dim(image)[other_dim]) / 2)
    upper <- lower + dim(image)[other_dim] - 1
    image_output[ , lower:upper, ] <- image
  }
  
  return(image_output)
}

#'@export
preprocessImagesHdf5 <- function(input_path = NULL, padding = FALSE,
                             resize_height = 227, resize_width = 227) {
  if (is.null(input_path) || !file.exists(input_path)) {
    stop("Argument 'input_path' is NULL but must specify a folder destination.")
  }
  #image_mat <- array(0, dim = c(resize_width, resize_height, 3))
  
  image_read <- EBImage::readImage(input_path, type = "jpg", all = FALSE)
  
  
    if (padding) {
      image_output <- padImage(image_read, which.max(dim(image_read)[1:2]) , resize_width , resize_height )
    } else {
      image_output <- image_read %>%
        EBImage::equalize(range = c(0, 1), levels = 256) %>%
        EBImage::resize (w = resize_width , h = resize_height)
    }
  image_mat <- array(image_output,dim = c(resize_width, resize_height, 3))
  image_mat <- image_mat[,,c(3,2,1)]#Changing from RGB to BGR due to underlying opencv implementation
  image_mat <- aperm(image_mat,c(3,1,2))
  return(image_mat)

}
