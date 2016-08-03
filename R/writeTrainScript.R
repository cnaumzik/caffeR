#'@export
writeTrainScript <-
  function (caffedir = "~/Documents/caffe" ,
            name = "My_Model" ,
            network_name = NULL ,
            finetune = FALSE) {
    on.exit(closeAllConnections())
    if (!finetune &&
        !file.exists(paste0(
          caffedir,
          "/models/",
          network_name,
          "/",
          network_name,
          ".caffemodel"
        ))) {
      stop(
        paste0(
          "There does not exist a .caffemodel file for the given network: ",
          network_name,
          ". Please ensure that you have provided the correct name and dowload the .caffemodel file"
        )
      )
    }
    script <-
      generateTrainScript(caffedir, name , network_name, finetune)
    
    output_path <-
      paste0(caffedir, "/models/", name, "/train_network.sh")
    
    writeLines(script, con = output_path)
    
  }

generateTrainScript <-
  function(caffedir = "~/Documents/caffe" ,
           name = "My_Model" ,
           network_name = "bvlc_reference_caffenet" ,
           finetune = TRUE) {
    script <-
      readLines(system.file("extdata", "train_network.sh", package = "caffeR"))
    script <- gsub("__CAFFEDIR__", caffedir, script)
    script <- gsub("__NAME__", name, script)
    if (finetune) {
      script <- gsub("__PH__", "", script)
      script <- gsub("__NETWORK__", network_name, script)
    } else {
      script <- gsub("__PH__", "#", script)
    }
    return(script)
    
  }
