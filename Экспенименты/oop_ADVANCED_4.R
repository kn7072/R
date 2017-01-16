# Functional programming
set.seed(1014)
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df) <- letters[1:6]

fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}



summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}
x <- 1:10
summary(x)
#####################

function(x) 3 ()
(function(x) 3)()
(function(x) x + 3)(10)


i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}
new_counter2()
new_counter2()

new_counter3 <- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}
new_counter3()()
new_counter3()()

########################################################## 190
# 10.4 Lists of functions
compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x) / length(x),
  manual = function(x) {
    total <- 0
    n <- length(x)
    for (i in seq_along(x)) {
      total <- total + x[i] / n
    }
    total
  }
)

x <- runif(1e5)
lapply(compute_mean, function(f) f(x))



















