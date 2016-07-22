#' @export
setsolver <-
  function(caffedir = "~/Documents/caffe" ,
           name = "My_model" ,
           net = "train_val.prototxt", #proto filename for the training net
           test_iter = 1000, # Number of iterations for each test net
           test_interval = 1000, # The number of iterations between two testing phases
           base_lr = 0.01, # Base learning rate
           display = 20, # Number of iterations between displaying info. 0 for mute
           max_iter = 450000,#Maximum number of iterations
           lr_policy = "step",#Learning rate policy
           gamma = 0.1,
           power = 0,
           momentum = 0.9,
           weight_decay = 0.0005,
           stepsize = 100000,
           snapshot = 10000,
           snapshot_prefix = "My_model_train",
           solver_mode = "CPU",
           solver_type = "SGD",
           clip_gradients = -1) {

    RProtoBuf::readProtoFiles(paste0(caffedir,"/src/caffe/proto/caffe.proto"))
    output_path <- paste0(caffedir,"/models/",name,"/solver.prototxt")
    solver_message <- RProtoBuf::new(caffe.SolverParameter)

    solver_message[[1]] <- paste0(caffedir, "/models/", name, "/", net)
    solver_message[[3]] <- test_iter
    solver_message[[4]] <- test_interval
    solver_message[[5]] <- base_lr
    solver_message[[6]] <- display
    solver_message[[7]] <- max_iter
    solver_message[[8]] <- lr_policy
    solver_message[[9]] <- gamma
    solver_message[[10]] <- power
    solver_message[[11]] <- momentum
    solver_message[[12]] <- weight_decay
    solver_message[[13]] <- stepsize
    solver_message[[14]] <- snapshot
    solver_message[[15]] <- snapshot_prefix
    solver_message[[35]] <- clip_gradients
    solver_message[[40]] <- solver_type
    if(solver_mode == "GPU"){
      solver_message[[17]] <- 1
    } else {
      solver_message[[17]] <- 0
    }

    writeLines(as.character(solver_message) , output_path)
}
