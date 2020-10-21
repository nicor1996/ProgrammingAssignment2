## Con esta funcion se crea un objeto "matriz" especial que pueda almacenar en cache 
##el inverso de la matrix.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Esta funcion calcula la inversa de la matriz dijitada espcial devuelta por la funcion 
## makeCacheMatrix y si se habia calculado la inversa, entonces se recupera la inversa por cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
