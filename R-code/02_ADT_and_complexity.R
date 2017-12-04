## ------------------------------------------------------------------------------
insert <- function(set, elem) UseMethod("insert")
member <- function(set, elem) UseMethod("member")

## ------------------------------------------------------------------------------
empty_list_set <- function() {
  structure(c(), class = "list_set")
}

insert.list_set <- function(set, elem) {
  structure(c(elem, set), class = "list_set")
}

member.list_set <- function(set, elem) {
  elem %in% set
}

## ------------------------------------------------------------------------------
s <- empty_list_set()
member(s, 1)
s <- insert(s, 1)
member(s, 1)

## ------------------------------------------------------------------------------
linked_list_cons <- function(head, tail) {
  structure(list(head = head, tail = tail), 
            class = "linked_list_set")
}

## ------------------------------------------------------------------------------
linked_list_nil <- linked_list_cons(NA, NULL)
empty_linked_list_set <- function() linked_list_nil
is_empty.linked_list_set <- function(x) 
  identical(x, linked_list_nil)

## ------------------------------------------------------------------------------
empty_list_set <- function() {
  structure(c(), class = "list_set")
}

insert.list_set <- function(set, elem) {
  structure(c(elem, set), class = "list_set")
}

member.list_set <- function(set, elem) {
  elem %in% set
}

## ------------------------------------------------------------------------------
insert.linked_list_set <- function(set, elem) {
  linked_list_cons(elem, set)
}

member.linked_list_set <- function(set, elem) {
  while (!is_empty(set)) {
    if (set$head == elem) return(TRUE)
    set <- set$tail
  }
  return(FALSE)
}

## ------------------------------------------------------------------------------
library(tibble)
library(microbenchmark)

get_performance_n <- function(
  algo
  , n
  , setup
  , evaluate
  , times
  , ...) {

  config <- setup(n)
  benchmarks <- microbenchmark(evaluate(n, config),
                               times = times)
  tibble(algo = algo, n = n,
         time = benchmarks$time / 1e9) # time in sec
}

get_performance <- function(
  algo
  , ns
  , setup
  , evaluate
  , times = 10
  , ...) {
  f <- function(n)
    get_performance_n(algo, n, setup, evaluate,
                      times = times, ...)
  results <- Map(f, ns)
  do.call('rbind', results)
}

## ------------------------------------------------------------------------------
setup <- function(empty) function(n) empty
evaluate <- function(n, empty) {
  set <- empty
  elements <- sample(1:n)
  for (elm in elements) {
    set <- insert(set, elm)
  }
}

ns <- seq(1000, 5000, by = 500)
performance <- rbind(
  get_performance("list()", ns,
                  setup(empty_list_set()), evaluate),
  get_performance("linked list", ns,
                  setup(empty_linked_list_set()), evaluate)
)

## ------------------------------------------------------------------------------
library(ggplot2)
ggplot(performance, aes(x = n, y = time, colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess",
              span = 2, se = FALSE) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab("Time (sec)") + theme_minimal()

## ------------------------------------------------------------------------------
ggplot(performance, aes(x = n, y = time / n, colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess",
              span = 2, se = FALSE) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab("Time / n") + theme_minimal()

ggplot(performance, aes(x = n, y = time / n**2, colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess",
              span = 2, se = FALSE) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab(expression(Time / n**2)) + theme_minimal()

## ------------------------------------------------------------------------------
setup <- function(empty) function(n) {
  set <- empty
  elements <- sample(1:n)
  for (elm in elements) {
    set <- insert(set, elm)
  }
  set
}
evaluate <- function(n, set) {
  member(set, sample(n, size = 1))
}

ns <- seq(10000, 50000, by = 10000)
performance <- rbind(
  get_performance("linked list", ns,
                  setup(empty_linked_list_set()), evaluate),
  get_performance("list()", ns,
                  setup(empty_list_set()), evaluate))

## ------------------------------------------------------------------------------
ggplot(performance, aes(x = n, y = time / n, colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess",
              span = 2, se = FALSE) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab("Time / n") + theme_minimal()

