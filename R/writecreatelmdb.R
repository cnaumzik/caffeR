#' @export
writecreatelmdb <- function(caffedir = "~/Documents/caffe", name = "MyModel",
                            caffe_preprocessing = FALSE,
                            resize_height = 227, resize_width = 227) {
    #This function assumes that you have downloaded the imagent example from TODO and that the function
    #create_imagenet.sh is stored in /path/to/caffe/examples/imagenet
    #Do not set resize parameter if you use TODO to preprocess the images

    on.exit(closeAllConnections())

    script <- generateCreateLmdb(caffedir, resize_width, resize_height, caffe_preprocessing)

    output_path <- paste0(caffedir, "/models/", name, "/create.sh")

    write(new_createLmdb, output_path, append = FALSE)
}

generateCreateLmdb <- function(caffedir, resize_width, resize_height, caffe_preprocessing) {
  script <- system.file("extdata", "create_imagenet.sh", package="caffeR")

  script <- gsub("__CAFFEDIR__", caffedir, script)
  script <- gsub("__NAME__", caffedir, script)
  script <- gsub("__RESIZE_HEIGHT__", resize_width, script)
  script <- gsub("__RESIZE_WIDTH__", resize_height, script)
  script <- gsub("__RESIZE_FLAG__", caffe_preprocessing, script)

  return(script)
}
