#' @export
writeMakeMean <- function(name = "MyModel", caffedir = "~/Documents/caffe") {
    #This function assumes that you have downloaded the imagent example from TODO and that the function
    #make_imagenet_mean.sh is stored in /path/to/caffe/examples/imagenet
    #Do not set resize parameter if you use TODO to preprocess the images

    on.exit(closeAllConnections())

    source_path <-
      paste0(caffedir, "/examples/imagenet/make_imagenet_mean.sh")

    target_path <- paste0(caffedir, "/models/", name, "/make_mean.sh")

    fileMakeMean <- readLines(source_path)

    fileMakeMean[5] <- paste0("EXAMPLE=", caffedir, "/examples/", name)

    fileMakeMean[6] <- paste0("DATA=", caffedir, "/data/", name)

    fileMakeMean[7] <- paste0("TOOLS=", caffedir, "/build/tools")

    fileMakeMean[9] <-
      paste0("$TOOLS/compute_image_mean -backend=lmdb $EXAMPLE/",
             name,
             "_train_lmdb \\")

    fileMakeMean[10] <- paste0("$DATA/", name, "_train_mean.binaryproto")

    fileMakeMean[12] <-
      paste0("$TOOLS/compute_image_mean -backend=lmdb $EXAMPLE/", name, "_val_lmdb \\")

    fileMakeMean[13] <- paste0("$DATA/", name, "_val_mean.binaryproto")


    write(fileMakeMean, target_path, append = FALSE)
    write("", target_path, append = TRUE)
    write("echo \"Done.\" ", target_path, append = TRUE)
  }
