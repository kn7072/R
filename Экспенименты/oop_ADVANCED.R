y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
attr(y, "my_attribute")
str(attributes(y))

############################################################
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)

bad <- data.frame(cbind(a = 1:2, b = c("a", "b")))
str(bad)

good <- data.frame(a = 1:2, b = c("a", "b"),  stringsAsFactors = FALSE)
str(good)

x <- c(2.1, 4.2, 3.3, 5.4)
x[c(3, 1)]



(vals <- outer(1:5, 1:5, FUN = "paste", sep = ","))


a <- list(a = 1, b = 2)
b <- list(a = list(b = list(c = list(d = 1))))
b[[c("a", "b", "c", "d")]]
b[["a"]][["b"]][["c"]][["d"]]


y <- list(a = 1, b = 2)
str(y[1])
str(y[[1]])

###########################################################
# $
# where x$y is equivalent to xy[["y"
xy <- c(1, 3, 4)
var <- "cyl"
attr(xy, "ff") <- 7
xy$ff <- 100
xy[["ff"]]
`[[`(xy, 1)
`+`(1, `*`(2, 3))

###########################################################
subset(mtcars, gear == 5 & cyl == 4)


##########################################################
y <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()


#mean(, TRUE, x = c(1:10, NA)) # 70


f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))

##########################################################
# 6.2.2 Functions vs. variables
l <- function(x) x + 1
m <- function(){
  l <- function(x) x * 2
  l(10)
}
m()
#> [1] 20
rm(l, m)

n <- function(x) x / 2
o <- function() {
  n <- 10
  n(n)
}
o()
#> [1] 5
rm(n, o)


# 6.2.4 Dynamic lookup
f <- function() x
x <- 15
f()
#> [1] 15
x <- 20
f()
#> [1] 20


f <- function() x + 3
codetools::findGlobals(f)



x <- 10; y <- 5
x + y
#> [1] 15
`+`(x, y)
#> [1] 15


for (i in 1:2) print(i)
#> [1] 1
#> [1] 2
`for`(i, 1:2, print(i))
#> [1] 1
#> [1] 2


{ print(1); print(2); print(3) }
`{`(print(1), print(2), print(3))


x <- list(1:3, 4:9, 10:12)
sapply(x, "[", 2)
# equivalent to
sapply(x, function(x) x[2])
#> [1] 2 5 11
#mean(, TRUE, x = c(1:10, NA))

##########################################################
i <- function(a, b) {
  c(missing(a), missing(b))
}
i()
#> [1] TRUE TRUE
i(a = 1)
#> [1] FALSE TRUE
i(b = 2)
#> [1] TRUE FALSE
i(1, 2)
#> [1] FALSE FALSE


f <- function(y) function() y
lf <- vector("list", 5)
for (i in seq_along(lf)) lf[[i]] <- f(i)
lf[[1]]()  # returns 5

g <- function(y) { force(y); function() y }
lg <- vector("list", 5)
for (i in seq_along(lg)) lg[[i]] <- g(i)
lg[[1]]()  # returns 1

## This is identical to
g <- function(y) { y; function() y }

##########################################################

f <- function(x = ls()) {
  a <- 1
  x
}
# ls() evaluated inside f:
f()
#> [1] "a" "x"
# ls() evaluated in global environment:
f(ls())
#> [1] "add" "adders" "adders2" "args" "f"
#> [6] "funs" "g" "h" "i" "metadata"
#> [11] "objs" "x" "y"


rm(list=ls())
f <- function(x) {
  a <- 1
  x
}
f({z <- 4; 10})
##########################################################
x <- NULL
if (!is.null(x) && x > 0) {
}

`&&` <- function(x, y) {
  if (!x) return(FALSE)
  if (!y) return(FALSE)
  TRUE
}
a <- NULL
!is.null(a) && a > 0


f1 <- function(x = {y <- 1; z <- 3; 2}, y = 0) {
  x + y + z
}
f1()

rm(list=ls())
f2 <- function(x = z) {
  z <- 100
  x
}
f2()
##########################################################
# 6.5.1 Inﬁx functions
`%+%` <- function(a, b) paste(a, b, sep = "")
"new" %+% " string"
#> [1] "new string"

`% %` <- function(a, b) paste(a, b)
`%'%` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)
"a" % % "b"
#> [1] "a b"
"a" %'% "b"
#> [1] "a b"
"a" %/\% "b"
#> [1] "a b"


# 6.5.2 Replacement functions
`second<-` <- function(K, value) {
  K[2] <- value
  k
}
K <- 1:10
second(K) <- 5L
K
#> [1] 1 5 3 4 5 6 7 8 9 10













