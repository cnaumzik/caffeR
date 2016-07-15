#' @export
writecreatelmdb <-
  function(caffedir = "~/Documents/caffe" ,
           name = "MyModel" ,
           caffe_preprocessing = FALSE ,
           Resize_height = 227 ,
           Resize_width = 227) {
    #This function assumes that you have downloaded the imagent example from TODO and that the function
    #create_imagenet.sh is stored in /path/to/caffe/examples/imagenet
    #Do not set resize parameter if you use TODO to preprocess the images

    on.exit(closeAllConnections())

    source_path <-
      paste0(caffedir, "/examples/imagenet/create_imagenet.sh")
    target_path <- paste0(caffedir, "/models/", name, "/create.sh")


    new_createLmdb[5] <- paste0("EXAMPLE=", caffedir, "/examples/", name)

    new_createLmdb[6] <- paste0("DATA=", caffedir, "/data/", name)

    new_createLmdb[7] <- paste0("TOOLS=", caffedir, "/build/tools")

    new_createLmdb[9] <-
      paste0("TRAIN_DATA_ROOT=", caffedir, "/data/", name, "/train/")

    new_createLmdb[10] <-
      paste0("VAL_DATA_ROOT=", caffedir, "/data/", name, "/val/")

    if (!caffe_preprocessing) {
      new_createLmdb[14] <- "RESIZE=false"


    } else {
      new_createLmdb[14] <- "RESIZE=true"

      new_createLmdb[16] <- paste0("  RESIZE_HEIGHT=", Resize_height)
      new_createLmdb[17] <- paste0("  RESIZE_WIDTH=", Resize_width)
    }


    new_createLmdb[44] <- "$DATA/train.txt \\"
    new_createLmdb[45] <- paste0("$EXAMPLE/", name, "_train_lmdb")

    new_createLmdb[54] <- "$DATA/val.txt \\"
    new_createLmdb[55] <- paste0("$EXAMPLE/", name, "_val_lmdb")

    write(new_createLmdb, target_path, append = FALSE)
  }
