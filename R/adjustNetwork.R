#' @export
adjustNetwork <-
  function(caffedir = "~/Documents/caffe" ,
           name = "My_model",
           network_name = "bvlc_reference_caffenet" ,
           no_new_layers = 1 ,
           freeze_all = TRUE ,
           lr = NULL,
           num_output = 1,
           loss = "EuclideanLoss",
           delete_accuracy = FALSE,
           backend = "HDF5",
           batch_size_train = 128,
           batch_size_val = 64) {
    source_path <-
      paste0(caffedir, "/models/", network_name, "/train_val.prototxt")
    
    if (!file.exists(source_path)) {
      stop(
        paste0(
          "No model named ",
          network_name,
          " exists. Please make sure that you supplied the correct name and that you downloaded the network from the model zoo."
        )
      )
    }
    
    target_path <-
      paste0(caffedir, "/models/", name, "/train_val.prototxt")
    
    
    file <- readLines(source_path)
    
    layer_indx <- c(grep("layer", file) , length(file) + 1)
    
    if (length(layer_indx) - 1 < no_new_layers) {
      stop(
        paste0(
          "This network only has ",
          length(layer_indx) - 1,
          " layers. Choose a different network or give a lower number of layers to be changed."
        )
      )
      
    }
    
    changed_layers <- identifyNewLayers(file , no_new_layers)
    
    new_layer <- changed_layers[1,]
    new_bottom <- changed_layers[2,]
    
    vision_lr <- rep(0.1, length(layer_indx) - 1)
    
    if (!is.null(lr)) {
      vision_lr <- lr
    } else {
      if (freeze_all) {
        vision_lr[!new_layer] <- 0
        
      } else {
        vision_lr[new_layer] <- 100
      }
      
    }
    
    for (k in 1:(length(layer_indx) - 1)) {
      current_layer <- unlist(file[layer_indx[k]:(layer_indx[k + 1] - 1)])
      current_layer <- adjustLayer(
        current_layer,
        caffedir ,
        name ,
        vision_lr[k] ,
        new_layer[k] ,
        new_bottom[k],
        loss,
        delete_accuracy,
        backend,
        batch_size_train,
        batch_size_val
      )
      file[layer_indx[k]:(layer_indx[k + 1] - 1)] <-
        unlist(current_layer)
    }
    
    # Adjusting # output in final layer (manual workaround until additional functions are included)
    k <- max(which(new_layer))
    
    current_layer <- file[layer_indx[k]:(layer_indx[k + 1] - 1)]
    output_indx <- grep("num_output", current_layer)
    current_layer[output_indx] <-
      changeFieldValue(current_layer[output_indx], num_output, "integer", FALSE)
    
    file[layer_indx[k]:(layer_indx[k + 1] - 1)] <- current_layer
    
    writeLines(file, target_path)
    
  }
#========================================================================================================================================================================

identifyNewLayers <- function (file = NULL ,
                               no_new_layers = 1) {
  if (is.null(file)) {
    stop("No .prototxt file supplied")
  }
  
  layer_indx <- c(grep("layer", file) , length(file) + 1)
  
  n <- length(layer_indx)
  
  changed_layers <- matrix(rep(FALSE, 2 * (n - 1)) , nrow = 2)
  
  i <- no_new_layers
  k <- n
  while (i > 0) {
    layer <- file[layer_indx[k - 1]:layer_indx[k] - 1]
    indx <- grep("type", layer)
    layer_types <-
      c("Data",
        "Loss",
        "Accuracy",
        "Convolution",
        "Pooling",
        "LRN",
        "InnerProduct")
    current_type <-
      sapply(layer_types, function(x) {
        if (length(grep(x, layer[indx], ignore.case = TRUE)) > 0) {
          return(TRUE)
        } else{
          return(FALSE)
        }
      })
    
    if (sum(current_type[-(1:3)])) {
      changed_layers[1, k - 1] <- TRUE
      i <- i - 1
    }
    k <- k - 1
    
  }
  if (no_new_layers > 0) {
    k <- min(which(changed_layers[1,]))
    while (k <= (n - 1)) {
      layer <- file[layer_indx[k]:layer_indx[k + 1] - 1]
      
      bottom <-
        gsub("\"", "", gsub("[A-z]*:\\s" , "" , trimws(layer[grep("bottom" , layer)])))
      top <-
        gsub("\"", "", gsub("[A-z]*:\\s" , "" , trimws(layer[grep("top" , layer)])))
      
      if (bottom == top) {
        k <- k + 1
      } else {
        pos <- k + 1
        k <- n
      }
      
    }
    changed_layers[2, pos:(n - 1)] <- TRUE
    
  }
  return(changed_layers)
}

#========================================================================================================================================================================
#This function simply determines of which type the current layer is and calls the respective function

adjustLayer <-
  function(layer,
           caffedir = "~/Documents/caffe",
           name = "My_model",
           vision_lr = NULL ,
           new_layer = FALSE,
           new_bottom = FALSE,
           loss = "EuclideanLoss",
           delete_accuracy = TRUE,
           backend = "HDF5",
           batch_size_train = 64,
           batch_size_val = 32) {
    
    if (is.null(layer)) {
      stop("No layer provided")
    }
    
    indx <- grep("type", layer)
    layer_types <- c("Data","Loss","Accuracy","Convolution","Pooling","LRN","InnerProduct")
    current_type <- sapply(layer_types,function(x){if(length(grep(x,layer[indx],ignore.case=TRUE))>0){return(TRUE)}else{return(FALSE)}})

    if(current_type[1]){
      if(backend == "lmdb" || backend == "Lmdb"){
        layer <- adjustDataLayer(layer, caffedir , name , batch_size_train , batch_size_val)
      } else {
        layer <- adjustDataLayerToHDF5(layer, caffedir , name , batch_size_train , batch_size_val)
      }
    } else if (current_type[2]) {
      layer <- adjustLossLayer(layer, name , new_bottom , loss)
    } else if (current_type[3]) {
      layer <- adjustAccuracyLayer(layer , name, new_bottom , loss , delete_accuracy)
    } else if (sum(current_type[4:7])) {
      layer <- adjustVisionLayer(layer , name , vision_lr , new_layer , new_bottom)
    } else {
      layer <- adjustActivationLayer(layer , name , new_layer , new_bottom)
    }
    return(layer)
  }
#=====================================================================================================================================================================
#Function sets path to mean.binaryproto and lmdb files as well as batch_size for both training and test data
adjustDataLayer <-
  function(layer ,
           caffedir = "~/Documents/caffe" ,
           name = "My_model" ,
           batch_size_train = 64 ,
           batch_size_val = 32) {
    if (is.null(layer)) {
      stop("No data layer provided")
    }
    indx <-
      unlist(sapply(c("mean_file", "source", "batch_size"), function(x) {
        grep(x , layer)
      }, USE.NAMES = FALSE))
    field_type <- c("string", "string", "integer")
    phase <- "train"
    batch_size <- batch_size_train
    
    if (length(grep("TEST", layer)) > 0) {
      phase <- "test"
      batch_size <- batch_size_val
    }
    new <-
      c(
        paste0(caffedir , "/data/" , name , "/" , phase, "_mean.binaryproto"),
        paste0(caffedir , "/examples/", name, "/", phase, "_lmdb"),
        batch_size
      )
    layer[indx] <- sapply(seq(1:3), function(x) {
      changeFieldValue(layer[indx[x]] , new[x] , field_type[x] , FALSE)
    })
    
    return(layer)
    
  }
#==================================================================================================================================================================
#Function adjust data layer to HDF5 format
adjustDataLayerToHDF5 <- function(layer,
                                  caffedir = "~/Documents/caffe",
                                  name = "My_model",
                                  batch_size_train = 64,
                                  batch_size_val = 32) {
  if (is.null(layer)) {
    stop("No data layer provided")
  }
  #Removing all unnecessary fields from Lmdb data layers
  delete <-
    unlist(sapply(c(
      "#",
      "transform_param",
      "crop_size",
      "mirror",
      "mean_file",
      "backend"
    ), function(x) {
      grep(x , layer)
    }, USE.NAMES = FALSE))
  layer[delete] <- ""
  
  #Removing superflous }
  i <- grep("}", layer)
  layer[i] <-
    unlist(sapply(1:length(i), function(x) {
      if (layer[i[x] - 1] == "" &&
          layer [i[x] + 1] == "") {
        return("")
      } else{
        return(layer[i[x]])
      }
    }))
  
  #Identifying fields that need to change
  indx <-
    c(unlist(sapply(c("type", "source", "batch_size"), function(x) {
      grep(x , layer)
    }, USE.NAMES = FALSE)))
  
  #Differentiate between training and test data layers
  phase <- "train"
  batch_size <- batch_size_train
  if (length(grep("TEST", layer)) > 0) {
    phase <- "test"
    batch_size <- batch_size_val
  }
  #New input for fields
  new <- c("HDF5Data",
           paste0(caffedir , "/data/", name, "/", phase, ".txt"),
           batch_size)
  field_type <- c("string" , "string" , "integer")
  layer[indx] <- sapply(seq(1:3), function(x) {
    changeFieldValue(layer[indx[x]] , new[x] , field_type[x] , FALSE)
  })
  return(layer)
}

#==================================================================================================================================================================
#Function adjust accuracy layer to compute accuracy by applying the same loss function as loss or by deleting the layer completely
adjustAccuracyLayer <- function(layer,
                                name = "My_model",
                                new_bottom = TRUE,
                                loss = "EuclideanLoss",
                                delete = TRUE) {
  if (is.null(layer)) {
    stop("No accuracy layer provided")
  }
  if (delete) {
    layer <- ""
  } else {
    indx <-
      unlist(sapply(c("type", "bottom"), function(x) {
        grep(x , layer)
      }, USE.NAMES = FALSE))
    
    layer[indx[1]] <-
      changeFieldValue(layer[indx[1]] , loss , "string" , FALSE)
    
    if (new_bottom) {
      change_indx <- grep("label" , layer[indx[-1]])
      
      layer[indx[change_indx]] <-
        changeFieldValue(layer[indx[change_indx]] , name , "string" , TRUE)
      
    }
  }
  return(layer)
}
#==================================================================================================================================================================
#Function changes type of loss layer as well as name of bottom layer if the respective layer has been newly initialized
adjustLossLayer <-
  function(layer ,
           name = "My_model" ,
           new_bottom = TRUE ,
           loss = "EuclideanLoss") {
    if (is.null(layer)) {
      stop("No loss layer provided")
    }
    indx <-
      unlist(sapply(c("type", "bottom"), function(x) {
        grep(x , layer)
      }, USE.NAMES = FALSE))
    
    layer[indx[1]] <-
      changeFieldValue(layer[indx[1]] , loss , "string" , FALSE)
    
    if (new_bottom) {
      change_indx <- grep("label" , layer[indx[-1]])
      
      layer[indx[change_indx]] <-
        changeFieldValue(layer[indx[change_indx]] , name , "string" , TRUE)
      
    }
    
    return(layer)
  }
#=========================================================================================================================================================
#Function adjusts lr (same assumed for both Bias and Weights) as well as the layer's name (in two fields: name and top)
#and the name of the bottom layer (if that layer has been changed as well)

adjustVisionLayer <-
  function(layer ,
           name = "My_model" ,
           vision_lr = NULL ,
           new_layer = FALSE ,
           new_bottom = FALSE) {
    if (is.null(layer)) {
      stop("No vision layer provided")
    }
    indx <-
      unlist(sapply(c("name", "bottom", "top", "lr_mult"), function(x) {
        grep(x , layer)
      }, USE.NAMES = FALSE))
    
    if (new_layer) {
      #Changing name and top from "xyz" to "xyz-My_model"
      layer[indx[1]] <-
        changeFieldValue(layer[indx[1]] , name , "string" , TRUE)
      layer[indx[3]] <-
        changeFieldValue(layer[indx[3]] , name , "string" , TRUE)
      
      if (new_bottom) {
        #Changing bottom from "xyz" to "xyz-My_model"
        layer[indx[2]] <-
          changeFieldValue(layer[indx[2]] , name , "string" , TRUE)
      }
    }
    #Setting all learning rate (for bias and weight) to vision_lr
    if (!is.null(vision_lr)) {
      layer[indx[-seq(1:3)]] <-
        sapply(indx[-seq(1:3)], function(x) {
          changeFieldValue(layer[x] , vision_lr , "integer" , FALSE)
        })
    }
    
    return(layer)
  }
#========================================================================================================================================================
adjustActivationLayer <-
  function(layer ,
           name = "My_model" ,
           new_layer = FALSE ,
           new_bottom = FALSE) {
    if (is.null(layer)) {
      stop("No activation layer provided")
    }
    indx <-
      sapply(c("name", "bottom", "top"), function(x) {
        grep(x , layer)
      }, USE.NAMES = FALSE)
    if (new_layer) {
      layer[indx[1]] <-
        changeFieldValue(layer[indx[1]] , name , "string" , TRUE)
    } 
      if (new_bottom) {
        layer[indx[2]] <-
          changeFieldValue(layer[indx[2]] , name , "string" , TRUE)
        layer[indx[3]] <-
          changeFieldValue(layer[indx[3]] , name , "string" , TRUE)
        
      }
      
    return(layer)
  }
#========================================================================================================================================================
#Function changes the value in Field. The value in Field can be a string, i.e. marked with " ", or a number.
#'@export
changeFieldValue <- function(Field , newvalue = NULL , value_type = "string" , add = FALSE) {
  if(is.null(newvalue)){
    stop(paste0("No new value for field ",Field," supplied"))
  }
  if(value_type == "string"){
  extracted_value <-
    gsub("\"", "", gsub("[A-z]*:\\s" , "" , trimws(Field)))
  } else {
    extracted_value <-
      gsub("[A-z]*:\\s" , "" , trimws(Field))
  }
  if(add){
    newField <-
      gsub(extracted_value , paste0(extracted_value,"-",newvalue) , Field)
  } else {
  newField <-
    gsub(extracted_value , newvalue , Field)
  }
  return(newField)
}
