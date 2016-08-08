#'@export
predictValue <-
  function(caffedir = "~/caffe",
           name = "My_Model",
           image = NULL,
           Net_name = NULL) {
    if(is.null(image) || !file.exists(image)) {
      stop("No image supplied")
    }
    deploy_file <- paste0(caffedir, "/models/", name, "/deploy.prototxt")
    if (!file.exists(deploy_file)) {
      stop(
        "The deploy.prototxt file cannot be found. Please ensure that you have created this file in your model's folder and try again"
      )
    }
    model_file <-
      paste0(caffedir, "/models/", name, "/", Net_name, "caffemodel")
    if (!file.exists(model_file)) {
      stop(
        paste0(
          "No .caffemodel file named ",
          Net_name,
          " can be found in your model's folder. Please ensure the file is in the correct location and try again."
        )
      )
    }
    label_file <-
      system.file("extdata", "dummy_label.txt", package = "caffeR")
    function_file <-
      system.file("extdata", "regress.bin", package = "caffeR")
    
    system(paste0(
      "./",
      function_file,
      deploy_file ,
      model_file ,
      label_file ,
      image
    ))
    system(paste0("sudo rm -r -f ", image_file))
    result <-
      readResults(paste0(caffedir, "/models/", name, "/results.txt"), mode =
                    "last")
    print(paste0("The predicted value for ", result[1], " is:", result[2]))
    
    
  }


readResults <-
  function(result_file = "~/results.txt", mode = "all") {
    results_txt <- readLines(result_file)
    N <- length(results_txt)
    output <- array(0, dim = c(N, 2))
    
    if (mode == "all") {
      output <- array(0, dim = c(N, 2))
      for (k in 1:N) {
        temp <- results_txt[k]
        output[k, 1] <- gsub("\\s-\\s[0-9]{1,}.[0-9]{1,4}", "", temp)
        output[k, 2] <-
          as.numeric(gsub("\\s-\\s", "", gsub(output[k, 1], "", temp)))
      }
    } else {
      output <- array(0, dim = c(1, 2))
      temp <- results_txt[N]
      output[1, 1] <- gsub("\\s-\\s[0-9]{1,}.[0-9]{1,4}", "", temp)
      output[1, 2] <-
        as.numeric(gsub("\\s-\\s", "", gsub(output[1, 1], "", temp)))
    }
    
    
    return(output)
    
  }