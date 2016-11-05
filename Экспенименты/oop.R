# https://www.ibm.com/developerworks/ru/library/l-r3/  ООП

x = c(1, 2)
x.y = c(3, 4)

args(lm)
predict
methods("predict")

v = 1:1000
typeof(v)
attributes(v)
dim(v) = c(10, 10, 10)  # Установка размерности
attributes(v)

v2 = matrix(1:1000, nrow=100, ncol=10)
typeof(v2)
attributes(v2)
attr(v2,'dim') = c(10,10,10)  # Изменение размерности
attributes(v2)

# Обобщенные функции
# В качестве простого примера создадим обобщенную функцию whoami() и несколько помеченных методов для диспетчеризации:
#------------- Создание обобщенного  метода
whoami <- function(x, ...) UseMethod("whoami")
whoami.foo <- function(x) print("I am a foo")
whoami.bar <- function(x) print("I am a bar")
whoami.default <- function(x) print("I don't know who I am")

# Ключевая идея состоит в том, что каждый объект в R может принадлежать нулю, одному или большему числу классов. 
# MRO любого заданного объекта (относительно конкретного метода) — это просто вектор именованных классов 
# (если они есть) в атрибуте его class. Например:
a = 1:10
b = 2:20
whoami(a)  # Нет соответствующего класса
attributes(a)
attr(a, 'class') <- 'foo'
attributes(a)

attr(b, 'class') <- c('baz', 'bam', 'bar')
whoami(a)
whoami(b)  # поиск MRO для описываемого вектора
attr(a, 'class') <- 'bar'  # изменение класса 'a'
whoami(a)

# Разрешение методов
meth1 <- function(x) UseMethod("meth1")
meth1.Mom <- function(x) print("Mom's meth1")
meth1.Dad <- function(x) print("Dad's meth1")

meth2 <- function(x) UseMethod("meth2")
meth2.Dad <- function(x) print("Dad's meth2")
attr(a, 'class') <- c('Mom', 'Dad')

meth1(a)  # несмотря на существование meth1.Dad, Mom используется первым для a
meth2(a)

# Включение предков
char0 = character(0)
makeMRO <- function(classes=char0, parents=char0) {
  # Создание MRO из опционального явного списка
  # и опционального списка предков
  mro <- c(classes)
  for (name in parents) {
    mro <- c(mro, name)
    ancestors <- attr(get(name), 'class')
    mro <- c(mro, ancestors[ancestors != name])
  }
  return(mro)
}

NewInstance <- function(value=0, classes=char0, parents=char0) {
  # Создание нового объекта, основываясь на первоначальном значении,
  # явных классах и предках (все опционально)
  obj <- value
  attr(obj, 'class') <- makeMRO(classes, parents)
  return(obj)
}

MaternalGrandma <- NewInstance()
PaternalGrandma <- NewInstance()
Mom <- NewInstance(classes='Mom', parents='MaternalGrandma')
Dad <- NewInstance(0, classes=c('Dad', 'Uncle'), 'PaternalGrandma')
Me <- NewInstance(value='Hello World', 'Me', c('Mom', 'Dad'))

print(Me)



# Снова о бесконечном векторе
# Теперь, имея некоторые механизмы ООП, можно намного удобнее работать с бесконечным вектором, который был описан ранее. 
# Наше первое решение вполне работоспособно, но лучше было бы иметь еще более органичный и прозрачный бесконечный вектор.
# Операторы в R – это просто сокращенный способ вызова функций; вы можете свободно дифференцировать поведение операторов на основе классов, 
# так же, как и для вызовов других функций. Попутно исправим еще несколько недостатков первой системы:
#   Мы хотим иметь возможность создавать столько отдельных бесконечных векторов, сколько необходимо.
#   Мы хотим иметь возможность настраивать применяемое распределение вероятностей.
#   Мы хотим иметь возможность инициализировать бесконечный случайный вектор значениями из другого вектора.

"[.infinite_random" <- function(v, index) {
  name <- attr(v, 'name')
  rfunc <- attr(v, 'rfunc')
  extend_by = max(index-length(v), 0)
  extras = rfunc(extend_by)
  new <- c(v, extras)
  makeInfiniteRandomVector(name, v=new, rfunc)
  return(new[index])
}

unitnorm <- function(n) return(rnorm(n, 0, 1))
empty <- vector('numeric', 0)

makeInfiniteRandomVector <- function(name, v=empty, rfunc=unitnorm) {
  # Создание бесконечного вектора
  # можно расширить существующий вектор, настраиваемая функция rand
  attr(v, 'class') <- 'infinite_random'
  attr(v, 'name') <- name
  attr(v, 'rfunc') <- rfunc
  assign(name, v, env=.GlobalEnv)
}

makeInfiniteRandomVector('v')
# makeInfiniteRandomVector('inf_poisson', rfunc=my_poisson)
# Использование: v[1]; v[10]; v[9:12]; и так далее.

# Индексирование уже определено в R как обобщенная функция, так что для его настройки не нужно вызывать метод UseMethod(); 
# можно просто определить столько новых специализаций, сколько нужно. Аналогично, встроенная функция print() тоже является обобщенной. 
# Ею можно воспользоваться так:
print.infinite_random <- function(v) {
  a_few = 5
  len = length(v)
  end_range = (len-a_few)+1
  cat('* Infinite Random Vector *\n')
  cat('[1] ', v[1:a_few], '...\n')
  cat('[')
  cat(end_range)
  cat('] ', v[end_range:len], '...\n')
}

v[1000]
print(v)
###############################################
methods(class="data.frame") 







z = list(j=c(2, 3), k=c(9, 0))
z$p = c(3, 4)



A = matrix(1:9, nrow = 3);A
# Впрочем, можно осуществлять смешанные операции, когда один из опeрандов - матрица, а другой - вектор (в частности, скаляр).
# В этом случае матрица рассматривается как вектор, составленный из ее элементов, записанных по столбцам,
# и действуют те же правила, что и для арифметических операций над векторами:
  
A + 3
x = c(1, 2, 3)
y = c(2, 4, 6)
z = x * y

A = matrix(1:9, nrow = 3);A
(1:3)*A

A = matrix(1:9, nrow = 3);A
A*(1:3)


A = matrix(1:9, nrow = 3);A
(1:9)+A
