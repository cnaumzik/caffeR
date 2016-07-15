padImage <- function(image, dim, resize_width, resize_height) {
  image_output <- Image(array(0, dim = c(resize_width, resize_height, 3)), colormode = "color")

  image <- image %>%
    equalize(range = c(0, 1), levels = 256)

  other_dim <- ifelse(dim == 1, 2, 1)

  if (dim == 1) {
    image <- image %>%
      resize(h = resize_height)

    lower <- ceiling((resize_height - dim(image)[other_dim]) / 2)
    upper <- lower + dim(image)[other_dim] - 1
    image_output[ , lower:upper, ] <- image
  } else {
    image <- image %>%
      resize(w = resize_width)

    lower <- ceiling((resize_width - dim(image)[other_dim]) / 2)
    upper <- lower + dim(image)[other_dim] - 1
    image_output[ , lower:upper, ] <- image
  }

  return(image_output)
}

#'@export
preprocessImages <- function(input_path = NULL, output_path = NULL,
                             caffe_preprocessing = FALSE,
                             padding = FALSE,
                             resize_height = 227, resize_width = 227) {
  if (is.null(input_path) || !dir.exists(input_path)) {
    stop("Argument 'input_path' is NULL but must specify a folder destination.")
  }
  if (is.null(output_path) || !dir.exists(output_path)) {
    stop("Argument 'output_path' is NULL but must specify a folder destination.")
  }
  if (input_path == output_path) {
    stop("Arguments of 'input_path' and 'output_path' must specify different locations.")
  }

  image_read <- readImage(input_path, type = "jpg", all = FALSE)

  if (!caffe_preprocessing) {
    if (padding) {
      image_output <- padImage(image_read, which.max(dim(image_read)[1:2]))
    } else {
      image_output <- image_read %>%
        equalize(range = c(0, 1), levels = 256) %>%
        resize (w = resize_width , h = resize_height)
    }
  } else {
    image_output <- image_read

  }

  writeImage(image_output , output_path)
}
