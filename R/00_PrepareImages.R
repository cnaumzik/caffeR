PrepareImages <-
  funtion(
    caffedir = "~/Documents/caffe" ,
    name = "MyModel" ,
    imagedir = "~/Documents/data" ,
    labels ,
    image_ids,
    suffix = NULL,
    preprocessing = FALSE ,
    padding = TRUE,
    share_val = 0.1 ,
    seed_no = 12345678
  ) {
    set.seed(seed_no)

    on.exit(closeAllConnections())

    image_list <- file.list(imagedir , pattern = ".jpg")

    n <- length(image_ids)

    if (length(image_list) == 0) {
      stop(
        "There are no \".jpg\" files in the directory. Please check if you provided the correct path."
      )
    }

    train_file <- paste0(caffedir, "/data", name, "/train.txt")
    val_file <- paste0(caffedir, "/data", name, "/val.txt")

    m <- round(share_val * n)

    validation_images <- image_ids[sample(seq(1:n), m, replace = FALSE)]

    for (k in 1:n) {
      file_path <- paste0(imagedir, "/", image_ids[k], suffix, ".jpg")

      if (which(names[k] %in% validation_images)) {
        target_path <-
          paste0(caffedir, "/data", name, "/val", image_ids[k], ".jpg")
        target_file <- val_file


      } else {
        target_path <-
          paste0(caffedir , "/data", name , "/train" , image_ids[k] , ".jpg")
        target_file <- train_file
      }

      ImagePreprocessing(file_path , target_path , preprocessing , padding , Resize_height,Resize_width)
      write(paste0(image_ids[k], ".jpg ", labels[k]) , target_file , append = TRUE)
      print(k)
    }



  }

