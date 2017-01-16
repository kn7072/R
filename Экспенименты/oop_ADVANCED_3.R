library(pryr)
rm(list=ls())
##########
modify <- function(x) {
  (address(x))
  ls.str(parent.frame())
  x$a <- 2
  #invisible()
  x
}

x_l <- list()
x_l$a <- 1
(address(x_l))
modify(x_l)
x_l$a
(address(x_l))
##########################################################

# 9.2.1 Determining the sequence of calls

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
#f(10)
#####################

# browseOnce <- function() {
#   old <- getOption("error")
#   function() {
#     options(error = old)
#     browser()
#   }
# }
# 
# options(error = browseOnce())
# f <- function() stop("!")
# # Enters browser
# f()
# # Runs normally
# f()
#####################


success <- try(1 + 2)
failure <- try("a" + "b")
class(success)
#> [1] "numeric"
class(failure)
#> [1] "try-error"
#####################

show_condition <- function(code) {
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
  )
}
show_condition(stop("!"))
#> [1] "error"
show_condition(warning("?!"))
#> [1] "warning"
show_condition(message("?"))
#> [1] "message"

#####################◙
f <- function() g()
g <- function() h()
h <- function() stop("!")
tryCatch(f(), error = function(e) print(sys.calls()))
withCallingHandlers(f(), error = function(e) print(sys.calls()))

