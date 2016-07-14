MakeDir <- function(caffedir = "~/Documents/caffe" , name = "MyModel") {
  #Remove all folders which might be existing from previous operations with the same name

  remove <- "sudo rm -r -f "
  create <- "sudo mkdir "
  examples_dir <- paste0(caffedir, "/examples/", name)
  data_dir <- paste0(caffedir, "/data/", name)
  data_train_dir <-paste0(data_dir,"/train")
  data_val_dir <-paste0(data_dir,"/val")
  model_dir <- paste0(caffedir, "/models/", name)

  mapply(function(x) {
    system(paste0(remove , x))
  } , c(examples_dir , data_dir , model_dir))

  mapply(function(x) {
    system(paste0(create , x))
  } , c(examples_dir , data_dir , data_train_dir , data_val_dir , model_dir))



}
