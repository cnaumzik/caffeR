#'@export
prepareHdf5 <- function(caffedir = "~/Documents/caffe" ,
                        name = "MyModel" ,
                        imagedir = "~/main" ,
                        phase = "train",
                        values = NULL ,
                        image_ids = NULL,
                        suffix = NULL,
                        padding = FALSE,
                        resize_height = 227,
                        resize_width =227,
                        batch_size = 64){
  
  if(is.null(image_ids)){
    stop ("The image ids are required.")
  }
  if(is.null(labels)){
    stop ("The labels are required.")
  }
  file_name <- paste0(caffedir,"/data/",name,"/",phase,".h5")
  on.exit(closeAllConnections())
  
  
  rhdf5::h5createFile(file_name)
  n <- length(values)
  
  if(n != length(image_ids)){
    stop("Number of labels and Images do not match")
  }
  data_batch <- array(0, dim = c(batch_size,3,resize_width,resize_height))
  label_batch <- array(0, dim = c(batch_size,1))
  rhdf5::h5createDataset(file_name , "data", c(n,3,resize_width,resize_height), storage.mode = "double", showWarnings = FALSE,level=9 ,chunk = c(batch_size,3,resize_width,resize_height))
  rhdf5::h5createDataset(file_name , "label", c(n,1), storage.mode = "double", showWarnings = FALSE,level = 9, chunk =c(batch_size,1))
  i<-1
  chunks <-0
  for(k in 1:n){
    print(k)
    image_path <- paste0(imagedir,"/",image_ids[k],".jpg")
    print(image_path)
    data_batch[i,,,]<- preprocessImagesHdf5(image_path, padding , resize_height , resize_width)
    label_batch[i,] <- values[k]
    i <- i+1
 
    if(i==batch_size+1){
      i <-1
      lower<-chunks*batch_size+1
      upper<-lower+batch_size-1
      
      #h5write(data_batch, file = file_name, name ="data", index = list(lower:upper,1:3,1:resize_width,1:resize_height))
      rhdf5::h5write(data_batch, file = file_name, name ="data", start = c(lower,1,1,1),count=c(batch_size,3,resize_width,resize_height))
      #h5write(label_batch, file = file_name, name ="label", index = list(lower:upper,1))
      rhdf5::h5write(label_batch, file = file_name, name ="label", start = c(lower,1),count=c(batch_size,1))
      chunks <- chunks +1
    }
  }
  
  rhdf5::H5close()
  
}