## Creates a list of functions which work as getters and setters to 
## the matrix and its inverse variables 
## which are stored in the global env.
## The setters also change the value of the global variable 
##and does not make a definition scope variable, and instead assigns the value to the global variable

## Notes:  (1) even though the special assignment operator used here, 
## uses global variables, which might be messy, we could have also 
## done with a locally maintained copy of the variabls in the list 
## we return and have the functions in the list accessing the variables there. 
## that would have made it a true "object" in some sense, having the data 
## and the accessor functions together without owing it to the environment.
## (2) The cache only stores the last operated matrix and its inverse, but not more than that.

makeCacheMatrix <- function(cached_matrix = matrix()) {
  cached_inv_matrix=NULL
  set=function(matrix_var){
    if(is.matrix(matrix_var)){
      cached_matrix <<- matrix_var
      cached_inv_matrix <<- NULL
    }
  }
  setInverse=function(inv_matrix_var){
    if(is.matrix(inv_matrix_var))
      cached_inv_matrix <<- inv_matrix_var
  }
  get=function()cached_matrix
  getInverse=function()cached_inv_matrix
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## Takes the list of functions which are returned by the makeCachedMatrix method, 
## and treats the first of the variable argument list's object to be the matrix 
## in question and after few sanity checks, checks if the matrix in question's 
## inverse was precomputed, and if so, returns it from the global cache instead 
## of recomputing it. the function stores the matrix in question and its computed 
## inverse if at all it has to compute afresh to use as cache for its next computation. 

cacheSolve <- function(list_fn, ...) {
  matrix_in_question=list(...)[[1]]
  if(is.list(list_fn) && class(list_fn$get) == "function" && class(list_fn$set) == "function" 
     && class(list_fn$getInverse) == "function" && class(list_fn$setInverse) == "function" 
     && is.matrix(matrix_in_question)){
    if(! identical(matrix_in_question,list_fn$get())){
      list_fn$set(matrix_in_question)
      list_fn$setInverse(solve(matrix_in_question))
      list_fn$getInverse()
    }else{
      list_fn$getInverse()
    }
  } 
  
}