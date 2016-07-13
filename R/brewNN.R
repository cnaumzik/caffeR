#' @export
#'
brewNN <- function(name = "Craigslist" , architecture = "bvlc_reference_caffenet" , caffedir = "~/Documents/caffe" , parameters = FALSE){



  if (architecture == "own") {
    #TODO write constructrNN.R
  } else {

    folders <- c(paste0(caffedir,"/models/",name), paste0(caffedir,"/data/",name,"/train") ,paste0(caffedir,"/data/",name ,"/val"))
    system2("mkdir folder[1]")

    #TODO
    #Bilder gruppieren und .txt file erzeugen
    #create_imagenet.sh schreiben
    #create_imagenet.mean schreiben
    #Solver.prototxt
    #net_val.prototxt
    #train
    #...

  }













}
