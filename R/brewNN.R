#' @export
#'
prepareNN <-
  function(caffedir = "~/Documents/caffe" ,
           name = "Catsvsdogs" ,
           caffe_preprocessing = FALSE,
           Resize_height = 227,
           Resize_width = 227,
           padding = FALSE,
           image_dir = "~/main",
           labels,
           image_ids,
           suffix = NULL,
           share_val = 0.1,
           architecture = "bvlc_reference_caffenet" ,
           parameters = FALSE ,
           seed = 0) {


#Construct required folders in caffedir/data, caffedir/examples and caffedir/models
makedir(caffedir , name)

#Construct create.sh responsible for creating lmdb for images
writecreatelmdb(caffedir ,
                name ,
                caffe_preprocessing ,
                Resize_height ,
                Resize_width)

#Construct make_mean.sh responsible for creating the mean of the images in the data set
writemakemean(caffedir , name)

#Split image set into training and validation set and prepares
#corresponding .txt files for creation of lmdb files via caffe routines
prepareimages(
  caffedir ,
  name ,
  imagedir ,
  labels ,
  image_ids ,
  suffix ,
  caffe_preprocessing ,
  padding ,
  share_val,
  seed,
  Resize_height,
  Resize_width)

#Execute shell commands to create lmdb and mean.binaryproto files

system(paste0("sudo ", caffedir , "/models/", name , "/create.sh"))
system(paste0("sudo ", caffedir , "/models/", name , "/make_mean.sh"))

#Create solver.prototxt - If parameters == FALSE default setting is chosen

if(!parameters){
  setsolver(caffedir , name)
} else {
  setsolver(caffedir , name , parameters)
}



}
