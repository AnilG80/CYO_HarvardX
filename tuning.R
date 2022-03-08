


params <- list(
  learning_rate = 0.1 # multiplication performed on each boosting iteration.
  , num_iterations = 1000 # how many total iterations if not early stopped 
  , objective = "multiclass" # classification more than 2 
  , metric = "multi_error" # metric of choice for evaluation on the test set 
  , num_class = 3 # How many levels to classify 
  , early_stopping_rounds = 50 # Number of maximum iterations without improvements.
  , min_sum_hessian_in_leaf = 0.1 # Prune by minimum hessian requirement 
  , num_threads = 4 # computing 
  , max_depth = -1 # Maximum depth of each trained tree, -1 is unlimited 
  , boosting = "gbdt" # boosting method gradient boosting decision tree 
  , max_bin = 16320 #Number of maximum unique values per feature.
  , num_leaves = 127 # Maximum leaves for each trained tree
)

# running the model 

fit_gb <- lgb.train(
  params
  , data=dtrainqk
  , valids=valids
  , verbose = 0
)
tinytex::install_tinytex()


