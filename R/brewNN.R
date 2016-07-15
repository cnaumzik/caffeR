#' @export
#'
brewNN <- function(name = "Craigslist" , architecture = "bvlc_reference_caffenet" ,
                   caffedir = "~/Documents/caffe" , parameters = FALSE) {



  if (is.null(architecture)) {
    #TODO write constructrNN.R
  } else {

    folders <- c(paste0(caffedir, "/models/", name),
                 paste0(caffedir, "/data/", name, "/train"),
                 paste0(caffedir, "/data/", name, "/val"))
    # TODO: previously, it was only folders[1] but I think all folders are create somewhere else
    lapply(folders, dir.create)

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
