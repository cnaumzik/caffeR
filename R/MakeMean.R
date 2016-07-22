#' @export
writeMakeMean <- function(caffedir = "~/Documents/caffe" , name = "MyModel"){
  #This function assumes that you have downloaded the imagent example from TODO and that the function
  #create_imagenet.sh is stored in /path/to/caffe/examples/imagenet
  #Do not set resize parameter if you use TODO to preprocess the images

  on.exit(closeAllConnections())

  script <- generateMakeMean(caffedir, name)

  output_path <- paste0(caffedir, "/models/", name, "/make_mean.sh")

  writeLines(script, con = output_path)
}

generateMakeMean <- function(caffedir, name) {

  script <- readLines(system.file("extdata", "make_imagenet_mean.sh", package = "caffeR"))
  script <- gsub("__CAFFEDIR__", caffedir, script)
  script <- gsub("__NAME__", name, script)
  return(script)
}
