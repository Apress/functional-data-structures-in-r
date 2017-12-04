## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
is_empty <- function(x) UseMethod("is_empty")
insert <- function(x, elm) UseMethod("insert")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge <- function(x, y) UseMethod("merge")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
is_empty <- function(x) UseMethod("is_empty")
push <- function(x, elm) UseMethod("push")
pop <- function(x) UseMethod("pop")
top <- function(x) UseMethod("top")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
is_empty <- function(x) UseMethod("is_empty")
enqueue <- function(x, elm) UseMethod("enqueue")
front <- function(x) UseMethod("front")
dequeue <- function(x) UseMethod("dequeu")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
bag_cons <- function(elem, lst)
  structure(list(item = elem, tail = lst),
            class = c("list_bag", "linked_list"))

bag_nil <- bag_cons(NA, NULL)
is_empty.list_bag <- function(x) identical(x, bag_nil)
empty_list_bag <- function() bag_nil

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
insert.list_bag <- function(x, elm) bag_cons(elm, x)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge.list_bag <- function(x, y) {
  result <- list_concatenate(x, y)
  class(result) <- c("list_bag", "linked_list")
  result
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
bag_node <- function(elem, left, right)
  structure(list(item = elem, left = left, right = right),
            class = "tree_bag")

tree_bag_nil <- bag_node(NA, NULL, NULL)
is_empty.tree_bag <- function(x) identical(x, tree_bag_nil)
empty_tree_bag <- function() tree_bag_nil

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
insert.tree_bag <- function(x, elm) {
  element_leaf <- bag_node(elm, empty_tree_bag(), empty_tree_bag())
  if (is_empty(x)) element_leaf
  else bag_node(NA, element_leaf, x)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
x <- insert(insert(insert(empty_tree_bag(), 7, 5, 1)))

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
insert(x, 4)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge.tree_bag <- function(x, y) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)
  bag_node(NA, x, y)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
is_leaf <- function(x) {
  is_empty(x$left) && is_empty(x$right)
}
bag_to_list <- function(x, acc = empty_list()) {
  if (is_leaf(x)) list_cons(x$item, acc)
  else bag_to_list(x$right, bag_to_list(x$left, acc))
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
stack_cons <- function(elem, lst)
  structure(list(item = elem, tail = lst),
            class = c("stack", "linked_list"))

stack_nil <- stack_cons(NA, NULL)
is_empty.stack <- function(x) identical(x, stack_nil)
empty_stack <- function() stack_nil

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
push.stack <- function(x, elm) stack_cons(elm, x)
pop.stack <- function(x) list_tail(x)
top.stack <- function(x) list_head(x)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
queue_environment <- function(front, back) {
  e <- new.env(parent = emptyenv())
  e$front <- front
  e$back <- back
  class(e) <- c("env_queue", "environment")
  e
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
empty_env_queue <- function()
  queue_environment(empty_list(), empty_list())

is_empty.env_queue <- function(x)
  is_empty(x$front) && is_empty(x$back)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
enqueue.env_queue <- function(x, elm) {
  x$back <- list_cons(elm, x$back)
  x
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
front.env_queue <- function(x) {
  if (is_empty(x$front)) {
    x$front <- list_reverse(x$back)
    x$back <- empty_list()
  }
  list_head(x$front)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
dequeue.env_queue <- function(x) {
  if (is_empty(x$front)) {
    x$front <- list_reverse(x$back)
    x$back <- empty_list()
  }
  x$front <- list_tail(x$front)
  x
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
queue <- function(front, back)
  list(front = front, back = back)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
queue_closure <- function() {
  q <- queue(empty_list(), empty_list())

  queue_is_empty <- function()
    is_empty(q$front) && is_empty(q$back)

  enqueue <- function(elm) {
    q <<- queue(q$front, list_cons(elm, q$back))
  }

  front <- function() {
    if (is_empty(q$front)) {
      q <<- queue(list_reverse(q$back), empty_list())
    }
    list_head(q$front)
  }

  dequeue <- function() {
    if (is_empty(q$front)) {
      q <<- queue(list_tail(list_reverse(q$back)), empty_list())
    } else {
      q <<- queue(list_tail(q$front), q$back)
  }

  structure(list(is_empty = queue_is_empty,
                 enqueue = enqueue,
                 front = front,
                 dequeue = dequeue),
            class = "closure_queue")
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
empty_queue <- function() queue_closure()

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
is_empty.closure_queue <- function(x) x$queue_is_empty()
enqueue.closure_queue <- function(x, elm) {
  x$enqueue(elm)
  x
}
front.closure_queue <- function(x) x$front()
dequeue.closure_queue <- function(x) {
  x$dequeue()
  x
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
queue_extended <- function(x, front, back)
  structure(list(x = x, front = front, back = back),
            class = "extended_queue")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
empty_extended_queue <- function()
  queue_extended(NA, empty_list(), empty_list())

is_empty.extended_queue <- function(x)
  is_empty(x$front) && is_empty(x$back)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
enqueue.extended_queue <- function(x, elm)
  queue_extended(ifelse(is_empty(x$back), elm, x$x),
                 x$front, list_cons(elm, x$back))

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
front.extended_queue <- function(x) {
  if (is_empty(x$front)) x$x
  else list_head(x$front)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
dequeue.extended_queue <- function(x) {
  if (is_empty(x$front))
    x <- queue_extended(NA, list_reverse(x$back), empty_list())
  queue_extended(x$x, list_tail(x$front), x$back)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
enqueue_front <- function(x, elm) UseMethod("enqueue_front")
enqueue_back <- function(x, elm) UseMethod("enqueue_back")

front <- function(x) UseMethod("front")
back <- function(x) UseMethod("back")

dequeue_front <- function(x) UseMethod("dequeue_front")
dequeue_back <- function(x) UseMethod("dequeue_back")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
list_get_n_reversed <- function(lst, n) {
  l <- empty_list()
  while (n > 0) {
    l <- list_cons(list_head(lst), l)
    lst <- list_tail(lst)
    n <- n - 1
  }
  l
}

list_drop_n <- function(lst, n) {
  l <- lst
  while (n > 0) {
    l <- list_tail(l)
    n <- n - 1
  }
  l
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
list_length <- function(lst) {
  n <- 0
  while (!is_empty(lst)) {
    lst <- lst$tail
    n <- n + 1
  }
  n
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
deque_environment <- function(front, back) {
  e <- new.env(parent = emptyenv())
  e$front <- front
  e$back <- back
  class(e) <- c("env_deque", "environment")
  e
}

empty_env_deque <- function()
  deque_environment(empty_list(), empty_list())

is_empty.env_deque <- function(x)
  is_empty(x$front) && is_empty(x$back)

enqueue_back.env_deque <- function(x, elm) {
  x$back <- list_cons(elm, x$back)
  x
}
enqueue_front.env_deque <- function(x, elm) {
  x$front <- list_cons(elm, x$front)
  x
}

front.env_deque <- function(x) {
  if (is_empty(x$front)) {
    n <- list_length(x$back)
    x$front <- list_get_n_reversed(x$back, ceiling(n))
    x$back <- list_drop_n(x$back, ceiling(n))
  }
  list_head(x$front)
}
back.env_deque <- function(x) {
  if (is_empty(x$back)) {
    n <- list_length(x$front)
    x$back <- list_get_n_reversed(x$front, ceiling(n))
    x$front <- list_drop_n(x$front, ceiling(n))
  }
  list_head(x$back)
}

dequeue_front.env_deque <- function(x) {
  if (is_empty(x$front)) {
    n <- list_length(x$back)
    x$front <- list_get_n_reversed(x$back, ceiling(n))
    x$back <- list_drop_n(x$back, ceiling(n))
  }
  x$front <- list_tail(x$front)
  x
}
dequeue_back.env_deque <- function(x) {
  if (is_empty(x$back)) {
    n <- list_length(x$front)
    x$back <- list_get_n_reversed(x$front, ceiling(n))
    x$front <- list_drop_n(x$front, ceiling(n))
  }
  x$back <- list_tail(x$back)
  x
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
deque_environment <- function(front, back,
                              front_length, back_length) {
  e <- new.env(parent = emptyenv())
  e$front <- front
  e$back <- back
  e$front_length <- front_length
  e$back_length <- back_length
  class(e) <- c("env_deque", "environment")
  e
}

empty_env_deque <- function()
  deque_environment(empty_list(), empty_list(), 0, 0)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
enqueue_back.env_deque <- function(x, elm) {
  x$back <- list_cons(elm, x$back)
  x$back_length <- x$back_length + 1
  x
}
enqueue_front.env_deque <- function(x, elm) {
  x$front <- list_cons(elm, x$front)
  x$front_length <- x$front_length + 1
  x
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
move_front_to_back <- function(x) {
  n <- list_length(x$front)
  m <- ceiling(n)
  x$back <- list_get_n_reversed(x$front, m)
  x$front <- list_drop_n(x$front, m)
  x$back_length <- m
  x$front_length <- n - m
}

move_back_to_front <- function(x) {
  n <- list_length(x$back)
  m <- ceiling(n)
  x$front <- list_get_n_reversed(x$back, m)
  x$back <- list_drop_n(x$back, m)
  x$front_length <- m
  x$back_length <- n - m
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
front.env_deque <- function(x) {
  if (is_empty(x$front)) move_back_to_front(x)
  list_head(x$front)
}
back.env_deque <- function(x) {
  if (is_empty(x$back)) move_front_to_back(x)
  list_head(x$back)
}

dequeue_front.env_deque <- function(x) {
  if (is_empty(x$front)) move_back_to_front(x)
  x$front <- list_tail(x$front)
  x
}
dequeue_back.env_deque <- function(x) {
  if (is_empty(x$back)) move_front_to_back(x)
  x$back <- list_tail(x$back)
  x
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
1:10000

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
f <- function(x, y) x

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
f(5, 1:10000)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
lazy_thunk <- function(expr) function() expr

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
library(microbenchmark)
microbenchmark(lazy_vector <- lazy_thunk(1:100000), times = 1)
microbenchmark(lazy_vector()[1], times = 1)
microbenchmark(lazy_vector()[1], times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
nil <- function() NULL
cons <- function(car, cdr) {
  force(car)
  force(cdr)
  function() list(car = car, cdr = cdr)
}

is_nil <- function(lst) is.null(lst())
car <- function(lst) lst()$car
cdr <- function(lst) lst()$cdr

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
concat <- function(l1, l2) {
  rev_l1 <- nil
  while (!is_nil(l1)) {
    rev_l1 <- cons(car(l1), rev_l1)
    l1 <- cdr(l1)
  }
  result <- l2
  while (!is_nil(rev_l1)) {
    result <- cons(car(rev_l1), result)
    rev_l1 <- cdr(rev_l1)
  }
  result
}

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
vector_to_list <- function(v) {
  lst <- nil
  for (x in rev(v)) lst <- cons(x, lst)
  lst
}

l1 <- vector_to_list(1:100000)
l2 <- vector_to_list(1:100000)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(lst <- concat(l1, l2), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(car(lst), times = 1)
microbenchmark(car(lst), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
concat <- function(l1, l2) {
  do_cat <- function(l1, l2) {
    rev_l1 <- nil
    while (!is_nil(l1)) {
      rev_l1 <- cons(car(l1), rev_l1)
      l1 <- cdr(l1)
    }
    result <- l2
    while (!is_nil(rev_l1)) {
      result <- cons(car(rev_l1), result)
      rev_l1 <- cdr(rev_l1)
    }
    result
  }
  force(l1)
  force(l2)
  lazy_thunk <- function(lst) function() lst()
  lazy_thunk(do_cat(l1, l2))
}

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(lst <- concat(l1, l2), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(car(lst), times = 1)
microbenchmark(car(lst), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
concat <- function(l1, l2) {
  force(l1)
  force(l2)
  if (is_nil(l1)) l2
  else {
    lazy_thunk <- function(lst) function() lst()
    lazy_thunk(cons(car(l1), concat(cdr(l1), l2)))
  }
}

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(lst <- concat(l1, l2), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(car(lst), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
microbenchmark(car(lst), times = 1)
microbenchmark(car(lst), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
reverse <- function(lst) {
  r <- function(l, t) {
    force(l)
    force(t)
    if (is_nil(l)) t
    else {
      lazy_thunk <- function(lst) function() lst()
      lazy_thunk(r(cdr(l), cons(car(l), t)))
    }
  }
  r(lst, nil)
}

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
l <- vector_to_list(1:500)
microbenchmark(lst <- reverse(l), times = 1)
microbenchmark(car(lst), times = 1)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
reverse <- function(lst) {
  do_reverse <- function(lst) {
    result <- nil
    while (!is_nil(lst)) {
      result <- cons(car(lst), result)
      lst <- cdr(lst)
    }
    result
  }
  force(lst)
  lazy_thunk <- function(lst) {
    function() lst()
  }
  lazy_thunk(do_reverse(lst))
}

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
l <- vector_to_list(1:10000)
microbenchmark(lst <- reverse(l), times = 1)
microbenchmark(car(lst), times = 1)
microbenchmark(car(lst), times = 1)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
lazy_queue <- function(front, back, front_length, back_length) {
  structure(list(front = front, back = back,
                 front_length = front_length,
                 back_length = back_length),
            class = "lazy_queue")
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
empty_lazy_queue <- function() lazy_queue(nil, nil, 0, 0)
is_empty.lazy_queue <- function(x)
  is_nil(x$front) && is_nil(x$back)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
rot <- function(front, back, a) {
  force(front)
  force(back)
  force(a)
  if (is_nil(front)) cons(car(back), a)
  else {
    lazy_thunk <- function(lst) function() lst()
    lazy_thunk(cons(car(front),
                    rot(cdr(front), cdr(back), cons(car(back), a))))
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
make_q <- function(front, back, front_length, back_length) {
  if (back_length <= front_length)
    lazy_queue(front, back, front_length, back_length)
  else
    lazy_queue(rot(front, back, nil), nil,
               front_length + back_length, 0)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
enqueue.lazy_queue <- function(x, elm)
  make_q(x$front, cons(elm, x$back),
         x$front_length, x$back_length + 1)

front.lazy_queue <- function(x) car(x$front)

dequeue.lazy_queue <- function(x)
  make_q(cdr(x$front), x$back,
         x$front_length - 1, x$back_length)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
lazy_thunk(cons(car(front),
                rot(cdr(front), cdr(back), cons(car(back), a))))

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
rot <- function(front, back, a) {
  force(front)
  force(back)
  force(a)
  if (is_nil(front)) cons(car(back), a)
  else {
    lazy_thunk <- function(lst) function() lst()
    lazy_thunk(cons(car(front),
                    rot(cdr(front), cdr(back), cons(car(back), a))))
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
rot <- function(front, back, a) {
  if (is_nil(front)) cons(car(back), a)
  else {
    lazy_thunk <- function(lst) function() lst()
    lazy_thunk(cons(car(front),
                    rot(cdr(front), cdr(back), cons(car(back), a))))
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
q <- empty_lazy_queue()
for (x in 1:10000) {
  q <- enqueue(q, x)
}
for (i in 1:10000) {
  q <- dequeue(q)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
rot <- function(front, back, a) {
  if (is_nil(front)) cons(car(back), a)
  else {
    lazy_thunk <- function(lst) function() lst()
    tail <- cons(car(back), a)
    lazy_thunk(cons(car(front), rot(cdr(front), cdr(back), tail)))
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
lazy_queue <- function(front, back, helper) {
  structure(list(front = front, back = back, helper = helper),
            class = "lazy_queue")
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
make_q <- function(front, back, helper) {
  if (is_nil(helper)) {
    helper <- rot(front, back, nil)
    lazy_queue(helper, nil, helper)
  } else {
    lazy_queue(front, back, cdr(helper))
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
empty_lazy_queue <- function()
  lazy_queue(nil, nil, nil)
is_empty.lazy_queue <- function(x)
  is_nil(x$front) && is_nil(x$back)

enqueue.lazy_queue <- function(x, elm)
  make_q(x$front, cons(elm, x$back), x$helper)
front.lazy_queue <- function(x) car(x$front)
dequeue.lazy_queue <- function(x)
  make_q(cdr(x$front), x$back, x$helper)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
IDLE <- 0
REVERSING <- 1
APPENDING <- 2
DONE <- 3

idle_state <- function() list(state = IDLE)
reversing_state <- function(
  keep
  , front
  , reverse_front
  , back
  , reverse_back) {
  list(state = REVERSING,
       keep = keep,
       front = front,
       reverse_front = reverse_front,
       back = back,
       reverse_back = reverse_back)
}
appending_state <- function(
  keep
  , front
  , back) {
  list(state = APPENDING,
       keep = keep,
       front = front,
       back = back)
}
done_state <- function(result) {
  list(state = DONE, result = result)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
rebuild_queue_node <- function(
  state
  , front_size
  , front
  , back_size
  , back) {
  structure(list(state = state,
                 front_size = front_size,
                 front = front,
                 back_size = back_size,
                 back = back),
            class = "rebuild_queue")
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
empty_rebuild_queue <- function() {
  rebuild_queue_node(state = idle_state(),
                     front_size = 0,
                     front = empty_list(),
                     back_size = 0,
                     back = empty_list())
}

is_empty.rebuild_queue <- function(x) is_empty(x$front)
front.rebuild_queue <- function(x) {
  list_head(x$front)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
exec <- function(state) {
  if (state$state == REVERSING) {
    if (is_empty(state$front)) {
      appending_state(keep = state$keep,
                      front = state$reverse_front,
                      back = list_cons(
                        list_head(state$back),
                        state$reverse_back)
                      )
    } else {
      reversing_state(keep = state$keep + 1,
                      front = list_tail(state$front),
                      reverse_front = list_cons(
                        list_head(state$front),
                        state$reverse_front),
                      back = list_tail(state$back),
                      reverse_back = list_cons(
                        list_head(state$back),
                        state$reverse_back))
    }
  } else if (state$state == APPENDING) {
    if (state$keep == 0) {
      done_state(result = state$back)
    } else {
      appending_state(keep = state$keep - 1,
                      front = list_tail(state$front),
                      back = list_cons(
                        list_head(state$front),
                        state$back))
    }
  } else {
    state
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
exec2 <- function(x) {
  new_state <- exec(exec(x$state))
  if (new_state$state == DONE)
    rebuild_queue_node(state = idle_state(),
                       front_size = x$front_size,
                       front = new_state$result,
                       back_size = x$back_size,
                       back = x$back)
  else
    rebuild_queue_node(state = new_state,
                       front_size = x$front_size,
                       front = x$front,
                       back_size = x$back_size,
                       back = x$back)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
check <- function(x) {
  if (x$back_size <= x$front_size) {
    exec2(x)
  } else {
    # when back gets longer than front, we start reversing
    new_state <- reversing_state(keep = 0,
                                 front = x$front,
                                 reverse_front = empty_list(),
                                 back = x$back,
                                 reverse_back = empty_list())
    new_queue <- rebuild_queue_node(state = new_state,
                                    front_size = x$front_size + x$back_size,
                                    front = x$front,
                                    back_size = 0,
                                    back = empty_list())
    exec2(new_queue)
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
enqueue.rebuild_queue <- function(x, elm) {
  check(rebuild_queue_node(state = x$state,
                           front_size = x$front_size,
                           front = x$front,
                           back_size = x$back_size + 1,
                           back = list_cons(elm, x$back)))
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
invalidate <- function(state) {
  if (state$state == REVERSING) {
    reversing_state(keep = state$keep - 1,
                    front = state$front,
                    reverse_front = state$reverse_front,
                    back = state$back,
                    reverse_back = state$reverse_back)
  } else if (state$state == APPENDING) {
    if (state$keep == 0) {
      done_state(result = list_tail(state$back))
    } else {
      appending_state(keep = state$keep - 1,
                      front = state$front,
                      back = state$back)
    }
  } else {
      state
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
dequeue.rebuild_queue <- function(x) {
  new_queue <- rebuild_queue_node(state = invalidate(x$state),
                                  front_size = x$front_size - 1,
                                  front = list_tail(x$front),
                                  back_size = x$back_size,
                                  back = x$back)
  check(new_queue)
}

