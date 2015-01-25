##################################################################################################################
#
#  The below two functions take advantage of R's Scoping environment to preserve the state inside
#  of an R object. 
#  The functions allow us to cache the inverse of a matrix (assuming the matrix supplied is already invertible), 
#  so that when the inverse is needed again, it can be looked up in the cache rather than being computed 
#  again.
#  The caching saves potentially time consuming computation for very large matrices.
#
##################################################################################################################
#
#  makeCacheMatrix: 
#     *  The function creates a special object, a list of functions defined how to access 
#        matrix 'x', but the list does not contain the matrix 'x'. 
#     *  It creates a unique environment (basically a namespace for variables) and this environment persists 
#        and stays in memory.
#        E.g. funclist <- makeCacheMatrix(x)
#     *  But if you were to call makeCacheMatrix again, it would get a new/different environment/namespace.
#
#################################################################################################################
makeCacheMatrix <- function(x = matrix()) {   
        # mean is set to NULL, essentially resetting the process.
        m <- NULL                                 
        # [f1] set - used in future cases to avoid having to reassign the function list to another variable.        
        # assigns y to symbol x in an environment different to current environment, i.e outside the function set.        
        # assigns m to NULL in an environment different to current environment.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # [f2] get - simply returns the entry matrix. Note can also be written as 'get <- function() {x}'
        get <- function() x
        # [f3] setinverse - assigns an argument to m and makes this available outside the function.
        # The function takes an argument which is the (later on) the calculated inverse.
        setinverse<- function(solve) m <<- solve
        # [f4] getinverse - Returns the value for m
        getinverse <- function() m
        # outputs of the function makeCacheMatrix returns a named list which point to functions 1-4) as values. 
        # Thus the names (characters before the equal signs) are arbitrary and are set to the same as the function 
        # names just to avoid any confusion.
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##################################################################################################################
#
#  cachesolve:
#    * operates on the special function list you pass to it from makeCacheMatrix. 
#    * It is applied to the function list to execute specific functions within it to get the inverse.
#    * If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#      retrieves the inverse from the cache.
#    * If you have a new matrix called x, it does not know that you have assigned some other matrix to x 
#      in the global environment. So by typing funclist$set(x) followed by
#      cacheSolve(funclist) allows you to cache the inverse of the new matrix x.
#
##################################################################################################################
cachesolve <- function(x, ...){  
        # m is set to the existing value of [f4], and getinverse function simply returns the value of m
        m<-x$getinverse()
        # if m (as in x$getinverse) is NOT null 
        # i.e. if there is a value currently stored in m then return it and print "getting cached data".
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # set data to calculate a new inverse. [f2] returns the value of the matrix 
        data <- x$get()
        # calculate a new inverse based on data
        m <- solve(data, ...)
        # Sets this new value for the inverse to m [f3]
        x$setinverse(m)
        # #output the value of m, i.e the inverse.
        m
}
##################################################################################################################
# Example of Usage
##################################################################################################################
# x<-matrix(1:4,2,2)
# solve(x) # to check what value should be
# funclist<-makeCacheMatrix(x)
# cachesolve(funclist)    # get the inverse
# cachesolve(funclist)    # got the cached value this time

# x<-matrix(c(4,1,3,1),2,2) #New matrix to cache
# solve(x) # to check what value should be
# cachesolve(funclist) # inverse of old matrix still in memory
# funclist$set(x)   # set makeCacheMatrix to new matrix
# cachesolve(funclist) # gets inverse of new matrix