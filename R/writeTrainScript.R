#'@export
writeTrainScript <- function (caffedir = "~/Documents/caffe" , name = "My_Model" , network_name = "bvlc_reference_caffenet"){

  on.exit(closeAllConnections())

  script <- generateTrainScript(caffedir, name , network_name)

  output_path <- paste0(caffedir, "/models/", name, "/train_network.sh")

  writeLines(script, con = output_path)

}

generateTrainScript(caffedir = "~/Documents/caffe" , name = "My_Model" , network_name = "bvlc_reference_caffenet"){
  script <- readLines(system.file("extdata", "train_network.sh", package = "caffeR"))
  script <- gsub("__CAFFEDIR__", caffedir, script)
  script <- gsub("__NAME__", name, script)
  script <- gsub("__NETWORK__", network_name, script)
  return(script)

  }
