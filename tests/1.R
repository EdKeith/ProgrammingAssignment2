## Unit tests for Programming Assignment 1

test.examples <- function()
{
  c = rbind(c(1, -1/4), c(-1/4, 1))
  ident = rbind(c(1,0),c(0,1))
  checkEquals(c %*% solve(c), ident)
}
