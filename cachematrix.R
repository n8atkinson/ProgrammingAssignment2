## This a is secondary function call that returns already calculated matrix inverse or that it needs to be done

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # Still a little unclear about this
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # 
        get <- function() x
        
        # 
        set_inverse <- function(matrix_inverse) inverse <<- matrix_inverse
        
        #
        get_inverse <- function() inverse
        
        # Returns a list of functions allowing them to exist in parent environment
        list(set = set, get = get, get_inverse = get_inverse, set_inverse = set_inverse)

}


## This is the primary function call of the original matrix that will have its inverse calculated

cacheSolve <- function(x, ...) {
        # Pull in either NULL or already calculated inverse
        inverse <- x$get_inverse
        
        # If inverse<>NULL then return already calcuated inverse
        # Question: where is the check if inverse changes instead of being NULL?
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # This is like an else_if,  if previous "return" is called then cacheSolve closes at that line
        
        # The rest of cacheSolve runs if inverse=NULL, meaning inverse needs to be calculated
        data <- x$get_inverse()
        
        # 
        inverse <- solve(data, ...)
        
        #
        x$set_inverse(inverse)
        
        # Return new calculated inverse
        inverse
}


# Ref: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
