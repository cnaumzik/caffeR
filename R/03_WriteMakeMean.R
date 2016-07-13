#' @export
WriteMakeMean <-
  function(caffedir = "~/Documents/caffe" , name = "MyModel") {
    #This function assumes that you have downloaded the imagent example from TODO and that the function
    #make_imagenet_mean.sh is stored in /path/to/caffe/examples/imagenet
    #Do not set resize parameter if you use TODO to preprocess the images

    on.exit(closeAllConnections())

    source_path <-
      paste0(caffedir, "/examples/imagenet/make_imagenet_mean.sh")

    target_path <- paste0(caffedir, "/models/", name, "/create.sh")

    new_MakeMean <- readLines(source_path)

    new_MakeMean[5] <- paste0("EXAMPLE=", caffedir, "/examples/", name)

    new_MakeMean[6] <- paste0("DATA=", caffedir, "/data/", name)

    new_MakeMean[7] <- paste0("Tools=", caffedir, "/build/tools")

    new_MakeMean[9] <-
      paste0("$TOOLS/compute_image_mean $EXAMPLE/",
             name,
             "_train_lmdb \\")

    new_MakeMean[10] <- paste0("$DATA/", name, "_train_mean.binaryproto")

    new_MakeMean[12] <-
      paste0("$TOOLS/compute_image_mean $EXAMPLE/", name, "_val_lmdb \\")

    new_MakeMean[13] <- paste0("$DATA/", name, "_val_mean.binaryproto")


    write(new_MakeMean, target_path, append = FALSE)
    write("", target_path, append = TRUE)
    write("echo \"Done.\" ", target_path, append = TRUE)
  }
