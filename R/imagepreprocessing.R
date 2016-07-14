#'@export
imagepreprocessing<-function(
  file_path ,
  target_path ,
  caffe_preprocessing = FALSE ,
  padding = FALSE ,
  Resize_height = 227,
  Resize_width = 227
) {
  Image_output <-
    Image(array(0, dim = c(Resize_width, Resize_height, 3)), colormode = "color")

  if (file.exists(file_path)) {

    Image_read <- readImage(file_path, type = "jpg", all = FALSE)

    if (!caffe_preprocessing) {
      if (padding) {
        dims <- dim(Image_read)[1:2]
        if (which.max(dims) == 1) {
          #Width ist größer
          Image_read <- Image_read %>% equalize(range = c(0, 1), levels = 256) %>% resize (w = Resize_width)
          lower <- ceiling((Resize_height - dim(Image_read)[2]) / 2)
          upper <- lower + dim(Image_read)[2]-1
          Image_output[ , lower:upper, ] <- Image_read

        } else {
          Image_read <- Image_read %>% equalize(range = c(0, 1), levels = 256) %>% resize (h = Resize_height)
          lower <- ceiling((Resize_width - dim(Image_read)[1]) / 2)
          upper <- lower + dim(Image_read)[1] - 1
          Image_output[lower:upper , ,] <- Image_read

        }
      } else {
        Image_output <-
          Image_read %>% equalize(range = c(0, 1), levels = 256) %>% resize (w =
                                                                               Resize_width , h = Resize_height)
      }
    } else {
      Image_output <- Image_read

    }
  }
  writeImage(Image_output , target_path)
}
