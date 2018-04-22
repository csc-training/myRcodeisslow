# simple example on the use of system.time
# and what difference a small change makes

N <- 1e6
x <- runif(N)

# squaring the elements

# the R way: whole object at a time

system.time(y <- x^2)

# for loop, with initialized object

y <- x # same length, values don't matter
system.time(
  for(i in seq_along(x)) y[i] <- x[i]^2
)

# for loop with copying
y <- numeric(0) # 0-length vector, no content at all
system.time(
  for(i in seq_along(x)) y <- c(y,x[i]^2)
)