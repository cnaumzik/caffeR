#' @export
writeCreateLmdb <- function(caffedir = "~/Documents/caffe", name = "MyModel",
                            caffe_preprocessing = FALSE,
                            resize_height = 227, resize_width = 227) {
    #This function assumes that you have downloaded the imagent example from TODO and that the function
    #create_imagenet.sh is stored in /path/to/caffe/examples/imagenet
    #Do not set resize parameter if you use TODO to preprocess the images

    on.exit(closeAllConnections())

    script <- generateCreateLmdb(caffedir, name, caffe_preprocessing, resize_height, resize_width)

    output_path <- paste0(caffedir, "/models/", name, "/create.sh")

    writeLines(script, con = output_path)
}

generateCreateLmdb <- function(caffedir, name, caffe_preprocessing, resize_height, resize_width) {
  script <- readLines(system.file("extdata", "create_imagenet.sh", package = "caffeR"))

  script <- gsub("__CAFFEDIR__", caffedir, script)
  script <- gsub("__NAME__", name, script)
  script <- gsub("__RESIZE_HEIGHT__", resize_width, script)
  script <- gsub("__RESIZE_WIDTH__", resize_height, script)
  script <- gsub("__RESIZE_FLAG__", caffe_preprocessing, script)

  return(script)
}
