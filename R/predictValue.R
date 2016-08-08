#'@export
predictValue <-
  function(caffedir = "~/caffe",
           name = "My_Model",
           image_file = "~/caffe/examples/images/cat.jpg",
           Net_name = NULL,
           padding = NULL,
           resize_height = NULL,
           resize_width = NULL,
           image_mean = NULL) {
    if(sum(apply(c(padding,resize_height,resize_width,image_mean),1,is.null()))>0) {
      stop("Missing arguments to function predictValue.")
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
    
    
    image <-
      preprocessImagesHdf5(image_file, padding, resize_height, resize_width, image_mean)
    image_file <- paste0(caffedir, "/data/", name, "/temp.jpg")
    EBImage::writeImage(image, image_file)
    
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
      image_file
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