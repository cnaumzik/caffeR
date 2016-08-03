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
           backend = "HDF5") {
    source_path <- paste0(caffedir,"/models/",network_name,"/train_val.prototxt")
    if(!file.exists(source_path)){
      stop(paste0("No model named ",network_name," exists. Please make sure that you supplied the correct name and that you downloaded the network from the model zoo."))
    }
    
    target_path <- paste0(caffedir,"/models/",name,"/train_val.prototxt")

    
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
    new_layer <- changed_layers[1, ]
    new_bottom <- changed_layers[2, ]


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
      file[layer_indx[k]:(layer_indx[k + 1] - 1)] <-
        adjustLayer(file[layer_indx[k]:(layer_indx[k + 1] - 1)],
                    caffedir ,
                    name ,
                    vision_lr[k] ,
                    new_layer[k] ,
                    new_bottom[k],
                    loss,
                    backend)
    }
    # Adjusting # output in final layer (manual workaround until additional functions are included)
    k <- max(which(new_layer))
    current_layer <- file[layer_indx[k]:(layer_indx[k + 1] - 1)]
    current_layer[grep("num_output", current_layer)] <-
      paste0("num_output: ", num_output)
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

    if (length(grep("Data" , layer[indx])) > 0 ||
        length(grep("loss" , layer[indx] , ignore.case = TRUE)) > 0 ||
        length(grep("accuracy" , layer[indx], ignore.case = TRUE)) > 0) {

    } else {
      changed_layers[1, k - 1] <- TRUE
      i <- i - 1
    }
    k <- k - 1

  }
  if (no_new_layers > 0) {
    k <- min(which(changed_layers[1, ]))
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
           vision_lr = 1 ,
           new_layer = FALSE,
           new_bottom = FALSE,
           loss = "EuclideanLoss",
           backend = "HDF5") {
    
    if (is.null(layer)) {
      stop("No layer provided")
    }
    
    indx <- grep("type", layer)
    
    if (length(grep("Data" , layer[indx])) > 0) {
      if(backend == "lmdb" || backend == "Lmdb"){
        layer <- adjustDataLayer(layer, caffedir , name)
      } else {
        #TODO adjust data layer to HDF5 data layer
      }
    } else if (length(grep("loss" , layer[indx] , ignore.case = TRUE)) > 0) {
      layer <- adjustLossLayer(layer, name , new_bottom , loss)
    } else if (length(grep("accuracy" , layer[indx] , ignore.case = TRUE)) > 0){
      #TODO adjust Accuracylayer
    } else if (length(grep("Convolution" , layer[indx])) > 0 ||
               length(grep("Pooling" , layer[indx])) > 0 ||
               length(grep("LRN" , layer[indx])) > 0 ||
               length(grep("InnerProduct" , layer[indx])) > 0) {
      layer <-
        adjustVisionLayer(layer , name , vision_lr , new_layer , new_bottom)

    } else {
      layer <-
        adjustActivationLayer(layer , name , new_layer , new_bottom)

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


    if (length(grep("TRAIN", layer)) > 0) {
      layer[grep("mean_file", layer)] <-
        paste0("mean_file: \"" ,
               caffedir ,
               "/data/" ,
               name ,
               "/" ,
               name ,
               "_train_mean.binaryproto\"")

      layer[grep("source", layer)] <-
        paste0("source: \"" ,
               caffedir ,
               "/examples/",
               name ,
               "/" ,
               name ,
               "_train_lmdb\"")

      layer[grep("batch_size", layer)] <-
        paste0("batch_size: ", batch_size_train)

    } else {
      layer[grep("mean_file", layer)] <-
        paste0("mean_file: \"" ,
               caffedir ,
               "/data/" ,
               name ,
               "/" ,
               name ,
               "_val_mean.binaryproto\"")

      layer[grep("source", layer)] <-
        paste0("source: \"" ,
               caffedir ,
               "/examples/",
               name ,
               "/" ,
               name ,
               "_val_lmdb\"")

      layer[grep("batch_size", layer)] <-
        paste0("batch_size: ", batch_size_val)

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
      stop("No loss/accuracy layer provided")
    }
    if(loss == "EuclideanLoss") {

      layer[grep("type", layer)] <- paste0("type: \"", loss, "\"")
      #layer[grep("name", layer)] <- paste0("name: \"loss\"")
      layer[grep("top", layer)] <- paste0("top: \"loss\"")

    } else {

      if (length(grep("loss", layer)) > 0) {

        layer[grep("type", layer)] <- paste0("type: \"", loss, "\"")
      }

      if (new_bottom) {

        bottom_indx <- grep("bottom" , layer)

        change_indx <- switch(grep("label" , layer[bottom_indx]), 2, 1)

        layer[bottom_indx[change_indx]] <-
          changeLayerName(layer[bottom_indx[change_indx]])

    }
    }
    return(layer)
  }
#=========================================================================================================================================================
#Function adjusts lr (same assumed for both Bias and Weights) as well as the layer's name (in two fields: name and top)
#and the name of the bottom layer (if that layer has been changed as well)

adjustVisionLayer <-
  function(layer ,
           name = "My_model" ,
           vision_lr = 1 ,
           new_layer = FALSE ,
           new_bottom = FALSE) {
    if (is.null(layer)) {
      stop("No vision layer provided")
    }

    if (new_layer) {
      layer[grep("name" , layer)] <-
        changeLayerName(layer[grep("name" , layer)] , name)
      layer[grep("top" , layer)] <-
        changeLayerName(layer[grep("top" , layer)] , name)

      if (new_bottom) {
        layer[grep("bottom" , layer)] <-
          changeLayerName(layer[grep("bottom" , layer)] , name)

      }


    }

    layer[grep("lr_mult" , layer)] <-
      gsub("[0-9]*$", vision_lr, layer[grep("lr_mult" , layer)])

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
    if (new_layer) {
      layer[grep("name" , layer)] <-
        changeLayerName(layer[grep("name" , layer)] , name)

      if (new_bottom) {
        layer[grep("bottom" , layer)] <-
          changeLayerName(layer[grep("bottom" , layer)] , name)
        layer[grep("top" , layer)] <-
          changeLayerName(layer[grep("top" , layer)] , name)

      }

    }
    return(layer)
  }
#========================================================================================================================================================
#Function changes the value in Field. The value in Field can be a string, i.e. marked with " ", or a number.
#'@export
changeFieldValue <- function(Field , newvalue = NULL , value_type = "string") {
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
  newField <-
    gsub(extracted_value , newvalue , Field)
  return(newField)
}
