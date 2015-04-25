## Put comments here that give an overall description of what your
makeCacheMatrix <- function(pb = matrix()) {
        ##             Will use the Set and get fn’s for  the matrix inversed
        inv = NULL
        set = function(y) {
                pb <<- m
                inv <<- NULL
        }
        get = function() pb
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
cacheSolve <- function(pb, ...) {
                ## Inverse of the matrix will be returned
        inv = pb$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        # In the above method, it calculates the inverse, if not here it calculates the inverse 
        mat.data = pb$get()
        inv = solve(mat.data, ...)
        pb$setinv(inv)
        
        return(inv)
}
test = function(mat){
        ## mat variable helps in the invertible matrix
        temp = makeCacheMatrix(mat)
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}

