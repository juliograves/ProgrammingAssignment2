makeCacheMatrix <- function(x=numeric(),lines,cols){
    m <- matrix(x,lines,cols)
    inverse <-NULL
    set <- function(x,l,c) {
        m <<- matrix(x,l,c)
        inverse <<- NULL
    }
    
    get <- function() {
        m
    }
    
    setinverse <- function(inver){
        inverse <<- inver
    }
    
    getinverse <- function(){
        inverse
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


cacheSolve <- function(objMakeCache,...){
    matinv <- objMakeCache$getinverse()
    if(!is.null(matinv)){
        message("getting cached inverse")
        return(matinv)
    }
    objMakeCache$setinverse(solve(objMakeCache$get()))
    objMakeCache$getinverse()
}
    
    
    
## YOU CAN TEST IT BY USING
    
    
a <- makeCacheMatrix(sample(1:10),2,5)
a$get()    
a$set(sample(1:25),5,5) 
a$get()    

cacheSolve(a)
a$getinverse()    
