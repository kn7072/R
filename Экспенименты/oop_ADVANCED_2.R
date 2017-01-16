# 6.5.2 Replacement functions
#x <- 1:10
`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:10
second(x) <- 5L
x
#> [1] 1 5 3 4 5 6 7 8 9 10


library(pryr)
x <- 1:10
address(x)
#> [1] "0x7fb3024fad48"
second(x) <- 6L
address(x)
#> [1] "0x7fb3059d9888"


# 92 страница
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10
x
#> [1] 10 6 3 4 5 6 7 8 9 10
x <- `modify<-`(x, 1, 15)

rm(list=ls())
###################################################################
f <- function(x) {
  x$a <- 2
  x
}
x <- list(a = 1)
f(x)
#> $a
#> [1] 2
x$a
#> [1] 1

###################################################################
# 6.6.1 On exit
in_dir <- function(dir, code) {
  old <- setwd(dir)
  print(old)
  on.exit(setwd(old))
  force(code)
}
getwd()
#> [1] "/Users/hadley/Documents/adv-r/adv-r"
in_dir("~", getwd())
#> [1] "/Users/hadley"


capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE)
  sink(temp)
  on.exit(sink(), add = TRUE)
  force(code)
  readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))
#> [1] "a" "b" "c"

###################################################################
library(pryr)
ftype(t.data.frame)
ftype(t.test)
methods(class = "ts")
###################################################################
iclass <- function(x) {
  if (is.object(x)) {
    stop("x is not a primitive type", call. = FALSE)
  }
  c(
    if (is.matrix(x)) "matrix",
    if (is.array(x) && !is.matrix(x)) "array",
    if (is.double(x)) "double",
    if (is.integer(x)) "integer",
    mode(x)
  )
}
iclass(matrix(1:5))
#> [1] "matrix" "integer" "numeric"
iclass(array(1.5))
#> [1] "array" "double" "numeric"


y <- 1
g <- function(x) {
  y <- 2
  UseMethod("g")
}
g.numeric <- function(x) y
g(10)

h <- function(x) {
  x <- 10
  UseMethod("h")
}
h.character <- function(x) paste("char", x)
h.numeric <- function(x) paste("num", x)
h("a")
###################################################################
# 7.3 S4
getGenerics()
getClasses()


setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),
         contains = "Person")
alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)
alice@age
slot(john, "boss")

###################################################################
# 8.1 Environment basics

e <- new.env()
e$a <- FALSE
e$b <- "a"
e$c <- 2.3
e$d <- 1:3
ls.str(e)

x <- 10
exists("x", envir = e)
###################################################################
# 8.3.3 Execution environments
g <- function(x) {
  if (!exists("a", inherits = FALSE)) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g(10)
g(10)
##########

h <- function(x) {
  a <- 2
  x + a
}
y <- h(1)
##########

rm(list = ls())
x <- 0
y <- 10

f <- function() {
  x <- 1
  y <- 5 
  g()
}

g <- function() {
  x <- 2
  h()
}

h <- function() {
  x <- 3
  x + y
}

f()
#> [1] 13

##########
modify <- function(x) {
  x$a <- 2
  invisible()
}

x_l <- list()
x_l$a <- 1
modify(x_l)
x_l$a


###################################################################
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)

