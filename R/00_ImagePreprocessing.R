ImagePreprocessing<-function(
  file_path ,
  target_path ,
  preprocessing = FALSE ,
  padding = FALSE ,
  Resize_height = 227,
  Resize_width = 227
) {
  Image_output <-
    Image(array(0, dim = c(Resize_width, Resize_height, 3)), colormode = "color")

  if (file.exists(file_path)) {
    Image_read <- readImage(file_path, type = "jpg", all = FALSE)

    if (preprocessing) {
      if (padding) {
        dims <- dim(Image_read)[1:2]
        if (which.max(dims) == 1) {
          #Width ist größer
          Image_read <-
            Image_read %>% equalize(range = c(0, 1), levels = 256) %>% resize (w =
                                                                                 Resize_width)
          range <-
            c(ceiling((
              Resize_height - dim(Image_read)[2]
            ) / 2), ceiling((
              Resize_height + dim(Image_read)[2]
            ) / 2) - 1)
          Image_output[, range[1]:range[2], ] <- Image_read

        } else {
          Image_read <-
            Image_read %>% equalize(range = c(0, 1), levels = 256) %>% resize (h =
                                                                                 Resize_height)
          range <-
            c(ceiling((
              Resize_width - dim(Image_read)[1]
            ) / 2), ceiling((
              Resize_width + dim(Image_read)[1]
            ) / 2) - 1)
          Image_output[range[1]:range[2] , ,] <- Image_read

        }

      } else {
        Image_output <-
          Image_read %>% equalize(range = c(0, 1), levels = 256) %>% resize (w =
                                                                               Resize_width , h = Resize_height)
      }
    }
  }
  writeImage(Image_output , target_path)
}
