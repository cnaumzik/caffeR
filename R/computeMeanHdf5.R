#'@export
computeMeanHdf5 <-
  function(caffedir = "~/Documents/caffe",
           name = "My_Model",
           imagedir = "~/main" ,
           method = "pixel",
           resize_width = 227,
           resize_height = 227) {
    imagedir<-"~/Promotion/Research/Code/Data/Catsvsdogs/main"
    images <- list.files(imagedir, ".jpg")
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
    #file_name <- paste0(caffedir,"/data/",name,"image_mean.h5")
    file_name <- "~/Promotion/mean.h5"
    rhdf5::h5createFile(file_name)
    
    
    N <- length(images)
    
    if (method == "channel") {
      for (k in 1:N) {
        current_image <-
          EBImage::readImage(paste0(imagedir, "/", images[k]),
                             type = "jpg",
                             all = FALSE)
        
        image_mean <- image_mean + apply(current_image , 3 , sum)
        print(k)
      }
    } else {
      for(k in 1:N) {
        current_image <-
          EBImage::readImage(paste0(imagedir, "/", images[k]),
                             type = "jpg",
                             all = FALSE) %>%
          EBImage::resize (w = resize_width , h = resize_height)
      
        image_mean <- image_mean + current_image
        print(paste0(k," : ",max(current_image), ":" , max(image_mean)))
      }
      
    }
    if (method == "channel") {
      image_mean <- image_mean / (N * resize_height * resize_width)
      
    } else{
      image_mean <- image_mean / N
      
    }
    rhdf5::h5write(image_mean,file=file_name,"mean")
    rhdf5::H5close()
  }
