## Unit tests for Programming Assignment 1

test.createandget <- function()
{
  m <- matrix(c(1,2,3,4), 2,2)
  c <- makeCacheMatrix(m)
  checkEquals(c$get(), m)
}

test.inverse <- function()
{
  c <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
  ident <- rbind(c(1,0),c(0,1))
  checkEquals(c$get() %*% cacheSolve(c), ident)
}

test.set <- function()
{
  ident <- rbind(c(1,0),c(0,1))
  c <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
  checkEquals(c$get() %*% cacheSolve(c), ident)
  c$set(matrix(c(1,2,3,4), 2,2))
  checkEquals(c$get() %*% cacheSolve(c), ident)
}
