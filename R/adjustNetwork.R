adjustNetwork(caffedir = "~/Documents/caffe" , name = "My_model", network_name = "bvlc_reference_caffenet" , no_new_layers = 1 , freeze_all = TRUE , lr = NULL ){

source_path <- paste0(caffedir,"/models/",network_name,"/train_val.prototxt")

target_path <- paste0(caffedir,"/models/",name,"/train_val.prototxt")

system(paste0("sudo cp " , source_path , target_path))















}
