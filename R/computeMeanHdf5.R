#'@export
computeMeanHdf5 <-
  function(caffedir = "~/Documents/caffe",
           name = "My_Model",
           imagedir = "~/main",
           suffix = NULL,
           method = "pixel",
           resize_width = 227,
           resize_height = 227) {
    images <- list.files(imagedir, paste0(suffix,".jpg"))
    if (length(images) == 0) {
      stop(
        paste0(
          "There are no images in ",
          imagedir,
          ". Make sure the directory is not empty/you have provided the correct directory."
        )
      )
    }
    
    
    if (method == "channel") {
      image_mean <- c(0, 0, 0)
    } else{
      image_mean <- array(0, dim = c(resize_width, resize_height,3))
    }
    file_name <- paste0(caffedir,"/data/",name,"/image_mean.h5")
    
    rhdf5::h5createFile(file_name)
    
    
    N <- length(images)
    
    if (method == "channel") {
      for (k in 1:N) {
        if(k%%1000 = 0){
          print(paste0("Processed ",k," images. Only ",N-k," to go."))
        }
        current_image <-
          EBImage::readImage(paste0(imagedir, "/", images[k]),
                             type = "jpg",
                             all = FALSE)
        
        image_mean <- image_mean + apply(current_image , 3 , sum)
        print(k)
      }
    } else {
      for(k in 1:N) {
        if(k%%1000 = 0){
          print(paste0("Processed ",k," images. Only ",N-k," to go."))
        }
        current_image <-
          EBImage::readImage(paste0(imagedir, "/", images[k]),
                             type = "jpg",
                             all = FALSE) %>%
          EBImage::resize (w = resize_width , h = resize_height)
      
        image_mean <- image_mean + current_image
      }
      
    }
    if (method == "channel") {
      image_mean <- image_mean / (N * resize_height * resize_width)
      
    } else{
      image_mean <- image_mean / N
      
    }
    rhdf5::h5write(image_mean,file=file_name,"mean")
    rhdf5::H5Fclose(file_name)
  }
