# The below functions are handy if you have to repeat calculations a lot. 
# Repeated calculations will not be calculcated over and over again, but are rather
# stored in a different environment and called when nessecary. If you have a big 
# dataset, it saves a lot of time.


a<-matrix(c(-1, -2, 1, 1), 2, 2)


#makeCacheMatrix sets a matrix, get the values of the matrix, 
#then sets the value of the inversed matrix in a different environment and finally 
#gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#This sets the function in an object called 'mydata'
mydata<-makeCacheMatrix(a)


# cacheSolve returns an inversed matrix. If the value is already calculated before, 
# it returns the value of the stored matrix (and displayse "getting cached data"), 
# if it is a new matrix, it calculates solve(matrix)

cacheSolve<- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

#This prints your inversed matrix
cacheSolve(mydata)
