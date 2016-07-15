#' @export
writeCreateLmdb <- function(name = "MyModel", caffedir = "~/Documents/caffe",
                            resize_height = NULL, resize_width = NULL) {
    #This function assumes that you have downloaded the imagent example from TODO and that the function
    #create_imagenet.sh is stored in /path/to/caffe/examples/imagenet
    #Do not set resize parameter if you use TODO to preprocess the images

    on.exit(closeAllConnections())

    source_path <- paste0(caffedir, "/examples/imagenet/create_imagenet.sh")
    output_path <- paste0(caffedir, "/models/", name, "/create.sh")

    lmdbFile <- readLines(source_path)

    lmdbFile[5] <- paste0("EXAMPLE=", caffedir, "/examples/", name)

    lmdbFile[6] <- paste0("DATA=", caffedir, "/data/", name)

    lmdbFile[7] <- paste0("TOOLS=", caffedir, "/build/tools")

    lmdbFile[9] <-
      paste0("TRAIN_DATA_ROOT=", caffedir, "/data/", name, "/train/")

    lmdbFile[10] <-
      paste0("VAL_DATA_ROOT=", caffedir, "/data/", name, "/val/")

    if (is.null(resize_height) || is.null(resize_width)) {
      lmdbFile[14] <- "RESIZE=false"


    } else {
      lmdbFile[14] <- "RESIZE=true"

      lmdbFile[16] <- paste0("  RESIZE_HEIGHT=", resize_height)
      lmdbFile[17] <- paste0("  RESIZE_WIDTH=", resize_width)
    }


    lmdbFile[44] <- "$DATA/train.txt \\"
    lmdbFile[45] <- paste0("$EXAMPLE/", name, "_train_lmdb")

    lmdbFile[54] <- "$DATA/val.txt \\"
    lmdbFile[55] <- paste0("$EXAMPLE/", name, "_val_lmdb")

    write(lmdbFile, output_path, append = FALSE)
  }
