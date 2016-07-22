#' @export
prepareImages <-
  function(
    caffedir = "~/Documents/caffe" ,
    name = "MyModel" ,
    imagedir = "~/Documents/main" ,
    labels = NULL ,
    image_ids = NULL,
    suffix = NULL,
    caffe_preprocessing = FALSE ,
    padding = FALSE,
    share_val = 0.1 ,
    seed = 0,
    Resize_height = 227,
    Resize_width =227
  ) {

    if(is.null(image_ids)){
      stop ("The image ids are required.")
    }
    if(is.null(labels)){
      stop ("The labels are required.")
    }
    set.seed(seed)

    on.exit(closeAllConnections())

    image_list <- list.files(imagedir , pattern = ".jpg")

    n <- length(image_ids)

    if (length(image_list) == 0) {
      stop(
        "There are no \".jpg\" files in the directory. Please check if you provided the correct path."
      )
    }

    train_file <- paste0(caffedir, "/data/", name, "/train.txt")
    val_file <- paste0(caffedir, "/data/", name, "/val.txt")

    m <- round(share_val * n)

    validation_images <- image_ids[sample(seq(1:n), m, replace = FALSE)]

    sapply(seq(1:n) , function(k){
      file_path <- paste0(imagedir, "/", image_ids[k], suffix, ".jpg")

      if (length(which(image_ids[k] %in% validation_images)) > 0) {
        target_path <-
          paste0(caffedir, "/data/", name, "/val/", image_ids[k], ".jpg")
        target_file <- val_file


      } else {
        target_path <-
          paste0(caffedir , "/data/", name , "/train/" , image_ids[k] , ".jpg")
        target_file <- train_file
      }
      preprocessImages(file_path , target_path , caffe_preprocessing , padding , Resize_height , Resize_width)
      write(paste0(image_ids[k], ".jpg ", labels[k]) , target_file , append = TRUE)
      print(k)
    })
}


