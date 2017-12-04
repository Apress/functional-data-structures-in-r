is_empty <- function(x) UseMethod("is_empty")
insert <- function(x, elm) UseMethod("insert")

find_minimal <- function(heap) UseMethod("find_minimal")
delete_minimal <- function(heap) UseMethod("delete_minimal")

merge <- function(x, y) UseMethod("merge")

merge.default <- function(x, y) {
  while (!is_empty(y)) {
    x <- insert(x, find_minimal(y))
    y <- delete_minimal(y)
  }
  x
}

merge.heap <- function(x, y) {
  while (!is_empty(y)) {
    x <- insert(x, find_minimal(y))
    y <- delete_minimal(y)
  }
  x
}

heap_to_list <- function(x) {
  l <- empty_list()
  while (!is_empty(x)) {
    l <- list_cons(find_minimal(x), l)
    x <- delete_minimal(x)
  }
  l
}

vector_to_heap <- function(empty_heap, vec) {
  heap <- empty_heap
  for (e in vec)
    heap <- insert(heap, e)
  heap
}

heap_sort <- function(vec, empty_heap) {
  heap <- vector_to_heap(empty_heap, vec)
  lst <- heap_to_list(heap)
  list_reverse(lst)
}

singleton_heap <- function(empty_heap, e) insert(empty_heap, e)
vector_to_heap <- function(vec, empty_heap, empty_queue) {
  q <- empty_queue
  for (e in vec)
    q <- enqueue(q, singleton_heap(empty_heap, e))
  repeat {
    first <- front(q) ; q <- dequeue(q)
    if (is_empty(q)) break
    second <- front(q) ; q <- dequeue(q)
    new_heap <- merge(first, second)
    q <- enqueue(q, new_heap)
  }
  first
}

heap_sort <- function(vec, empty_heap, empty_queue) {
  heap <- vector_to_heap(vec, empty_heap, empty_queue)
  lst <- heap_to_list(heap)
  list_reverse(lst)
}

leftist_heap_node <- function(
  value
  , left = empty_leftist_heap()
  , right = empty_leftist_heap()
  , rank = 0
  ) {
  structure(list(left = left,
                 value = value,
                 right = right,
                 rank = rank),
            class = c("leftist_heap", "heap"))
}

empty_leftist_heap_node <- leftist_heap_node(NA, NULL, NULL)
empty_leftist_heap <- function() empty_leftist_heap_node
is_empty.leftist_heap <- function(x)
  is.null(x$left) && is.null(x$right)

find_minimal.leftist_heap <- function(heap) {
  heap$value
}

delete_minimal.leftist_heap <- function(heap) {
  merge(heap$left, heap$right)
}

insert.leftist_heap <- function(x, elm) {
  merge(x, leftist_heap_node(elm))
}

build_leftist_heap <- function(value, a, b) {
  if (a$rank >= b$rank)
    leftist_heap_node(value = value,
                      left = a,
                      right = b,
                      rank = b$rank + 1)
  else
    leftist_heap_node(value = value,
                      left = b,
                      right = a,
                      rank = a$rank + 1)
}

merge.leftist_heap <- function(x, y) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)
  if (x$value <= y$value)
	  build_leftist_heap(x$value, x$left, merge(x$right, y))
  else
	  build_leftist_heap(y$value, y$left, merge(x, y$right))
}

binomial_tree_node <- function(value, trees) {
  list(value = value, trees = trees)
}

link_binomial_trees <- function(t1, t2) {
  if (t1$value < t2$value) {
    binomial_tree_node(t1$value, list_cons(t2, t1$trees))
  } else {
    binomial_tree_node(t2$value, list_cons(t1, t2$trees))
  }
}

binomial_heap_node <- function(rank, tree) {
  list(rank = rank, tree = tree)
}

binomial_heap <- function(min_value, heap_nodes = empty_list()) {
  structure(list(min_value = min_value, heap_nodes = heap_nodes),
            class = c("binomial_heap", "heap"))
}

empty_binomial_heap <- function() binomial_heap(NA)
is_empty.binomial_heap <- function(x) is_empty(x$heap_nodes)

find_minimal.binomial_heap <- function(heap) {
  heap$min_value
}

singleton_binomial_heap_node <- function(value) {
  tree <- binomial_tree_node(value, empty_list())
  binomial_heap_node(0, tree)
}

insert_binomial_node <- function(new_node, heap_nodes) {
  if (is_empty(heap_nodes)) {
    return(list_cons(new_node, empty_list()))
  }

  first_node <- list_head(heap_nodes)
  if (new_node$rank < first_node$rank) {
    list_cons(new_node, heap_nodes)
  } else {
    new_tree <- link_binomial_trees(new_node$tree, first_node$tree)
    new_node <- binomial_heap_node(new_node$rank + 1, new_tree)
    insert_binomial_node(new_node, list_tail(heap_nodes))
  }
}

insert.binomial_heap <- function(x, elm, ...) {
  new_min_value <- min(x$min_value, elm, na.rm = TRUE)
  new_node <- singleton_binomial_heap_node(elm)
  new_nodes <- insert_binomial_node(new_node, x$heap_nodes)
  binomial_heap(new_min_value, new_nodes)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge_heap_nodes <- function(x, y) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)

  first_x <- list_head(x)
  first_y <- list_head(y)
  if (first_x$rank < first_y$rank) {
    list_cons(first_x, merge_heap_nodes(list_tail(x), y))
  } else if (first_y$rank < first_x$rank) {
    list_cons(first_y, merge_heap_nodes(list_tail(y), x))
  } else {
    new_tree <- link_binomial_trees(first_x$tree, first_y$tree)
    new_node <- binomial_heap_node(first_x$rank + 1, new_tree)
    rest <- merge_heap_nodes(list_tail(x), list_tail(y))
    insert_binomial_node(new_node, rest)
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge.binomial_heap <- function(x, y, ...) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)
  new_min_value <- min(x$min_value, y$min_value)
  new_nodes <- merge_heap_nodes(x$heap_nodes, y$heap_nodes)
  binomial_heap(new_min_value, new_nodes)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
insert_binomial_node <- function(new_node, heap_nodes) {
  merge_heap_nodes(list_cons(new_node, empty_list()), heap_nodes)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
get_minimal_node <- function(min_value, heap_nodes) {
  first_node <- list_head(heap_nodes)
  if (first_node$tree$value == min_value) first_node
  else get_minimal_node(min_value, list_tail(heap_nodes))
}

delete_minimal_node <- function(min_value, heap_nodes) {
  first_node <- list_head(heap_nodes)
  if (first_node$tree$value == min_value) {
    list_tail(heap_nodes)
  } else {
    rest <- delete_minimal_node(min_value, list_tail(heap_nodes))
    list_cons(first_node, rest)
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
binomial_trees_to_nodes <- function(rank, trees) {
  if (is_empty(trees)) {
    empty_list()
  } else {
    list_cons(binomial_heap_node(rank, list_head(trees)),
              binomial_trees_to_nodes(rank - 1, list_tail(trees)))
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
binomial_nodes_min_value <- function(heap_nodes, cur_min = NA) {
  if (is_empty(heap_nodes)) {
    cur_min
  } else {
    front_value <- list_head(heap_nodes)$tree$value
    new_cur_min <- min(cur_min, front_value, na.rm = TRUE)
    binomial_nodes_min_value(list_tail(heap_nodes), new_cur_min)
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
delete_minimal.binomial_heap <- function(heap) {
  min_node <-
    get_minimal_node(heap$min_value, heap$heap_nodes)
  other_nodes <-
    delete_minimal_node(heap$min_value, heap$heap_nodes)
  min_node_nodes <-
    binomial_trees_to_nodes(min_node$rank - 1,
                            min_node$tree$trees)
  new_nodes <-
    merge_heap_nodes(other_nodes, list_reverse(min_node_nodes))
  new_min_value <- binomial_nodes_min_value(new_nodes)
  binomial_heap(new_min_value, new_nodes)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
splay_tree_node <- function(value, left = NULL, right = NULL) {
  list(left = left, value = value, right = right)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
splay_heap <- function(min_value, splay_tree) {
  structure(list(min_value = min_value, tree = splay_tree),
            class = c("splay_heap", "heap"))
}

empty_splay_heap <- function() splay_heap(NA, NULL)
is_empty.splay_heap <- function(x) is.null(x$tree)

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
find_minimal.splay_heap <- function(heap) {
  heap$min_value
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
splay_find_minimal_value <- function(tree) {
  if (is.null(tree)) NA
  else if (is.null(tree$left)) tree$value
  else splay_find_minimal_value(tree$left)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
splay_delete_minimal_value <- function(tree) {
  if (is.null(tree$left)) {
    tree$right

  } else {
    a <- tree$left$left
    x <- tree$left$value
    b <- tree$left$right
    y <- tree$value
    c <- tree$right

    if (is.null(a))
      splay_tree_node(left = b, value = y, right = c)
    else
      splay_tree_node(
        left = splay_delete_minimal_value(a),
        value = x,
        right = splay_tree_node(left = b, value = y, right = c)
      )
  }
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
delete_minimal.splay_heap <- function(heap) {
  if (is_empty(heap))
    stop("Can't delete the minimal value in an empty heap")
  new_tree <- splay_delete_minimal_value(heap$tree)
  new_min_value <- splay_find_minimal_value(new_tree)
  splay_heap(min_value = new_min_value, splay_tree = new_tree)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
partition <- function(pivot, tree) {
  if (is.null(tree)) {
    smaller <- NULL
    larger <- NULL

  } else {
    a <- tree$left
    x <- tree$value
    b <- tree$right
    if (x <= pivot) {
      if (is.null(b)) {
        smaller <- tree
        larger <- NULL
      } else {
        b1 <- b$left
        y <- b$value
        b2 <- b$right
        if (y <= pivot) {
          part <- partition(pivot, b2)
          smaller <- splay_tree_node(
            left = splay_tree_node(
              left = a,
              value = x,
              right = b1
            ),
            value = y,
            right = part$smaller
          )
          larger <- part$larger
        } else {
          part <- partition(pivot, b1)
          smaller <- splay_tree_node(
            left = a,
            value = x,
            right = part$smaller
          )
          larger <- splay_tree_node(
            left = part$larger,
            value = y,
            right = b2
          )
        }
      }
    } else {
      if (is.null(a)) {
        smaller <- NULL
        larger <- tree
      } else {
        a1 <- a$left
        y <- a$value
        a2 <- a$right
        if (y <= pivot) {
          part <- partition(pivot, a2)
          smaller <- splay_tree_node(
            left = a1,
            value = y,
            right = part$smaller
          )
          larger <- splay_tree_node(
            left = part$larger,
            value = x,
            right = b
          )
        } else {
          part <- partition(pivot, a1)
          smaller <- part$smaller
          larger <- splay_tree_node(
            left = part$larger,
            value = y,
            right = splay_tree_node(
              left = a2,
              value = x,
              right = b
            )
          )
        }
      }
    }
  }
  list(smaller = smaller, larger = larger)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
is_case_1 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  x <= pivot && is.null(b)
}

transform_case_1 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  list(smaller = tree, larger = NULL)
}

is_case_2 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right
  x <= pivot && y <= pivot
}

transform_case_2 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right

  part <- partition(pivot, b2)
  smaller <- splay_tree_node(
    left = splay_tree_node(
      left = a,
      value = x,
      right = b1
    ),
    value = y,
    right = part$smaller
  )
  larger <- part$larger

  list(smaller = smaller, larger = larger)
}

is_case_3 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right
  x <= pivot && y > pivot
}

transform_case_3 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right

  part <- partition(pivot, b1)
  smaller <- splay_tree_node(
    left = a,
    value = x,
    right = part$smaller
  )
  larger <- splay_tree_node(
    left = part$larger,
    value = y,
    right = b2
  )

  list(smaller = smaller, larger = larger)
}

is_case_4 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  x > pivot && is.null(a)
}

transform_case_4 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  list(smaller = NULL, larger = tree)
}

is_case_5 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right
  x > pivot && y <= pivot
}

transform_case_5 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right

  part <- partition(pivot, a2)
  smaller <- splay_tree_node(
    left = a1,
    value = y,
    right = part$smaller
  )
  larger <- splay_tree_node(
    left = part$larger,
    value = x,
    right = b
  )

  list(smaller = smaller, larger = larger)
}

is_case_6 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right
  x > pivot && y > pivot
}

transform_case_6 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right

  part <- partition(pivot, a1)
  smaller <- part$smaller
  larger <- splay_tree_node(
    left = part$larger,
    value = y,
    right = splay_tree_node(
      left = a2,
      value = x,
      right = b
    )
  )

  list(smaller = smaller, larger = larger)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
partition <- function(pivot, tree) {
  if (is.null(tree))
    list(smaller = NULL, larger = NULL)
  else if (is_case_1(pivot, tree))
    transform_case_1(pivot, tree)
  else if (is_case_2(pivot, tree))
    transform_case_2(pivot, tree)
  else if (is_case_3(pivot, tree))
    transform_case_3(pivot, tree)
  else if (is_case_4(pivot, tree))
    transform_case_4(pivot, tree)
  else if (is_case_5(pivot, tree))
    transform_case_5(pivot, tree)
  else if (is_case_6(pivot, tree))
    transform_case_6(pivot, tree)
  else stop("Unknown case")
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
insert.splay_heap <- function(x, elm) {
  part <- partition(elm, x$tree)
  new_tree <- splay_tree_node(
    value = elm,
    left = part$smaller,
    right = part$larger
  )
  new_min_value <- min(x$min_value, elm, na.rm = TRUE)
  splay_heap(min_value = new_min_value, splay_tree = new_tree)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge_splay_trees <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.null(y)) return(x)

  a <- x$left
  val <- x$value
  b <- x$right

  part <- partition(val, y)
  splay_tree_node(left = merge_splay_trees(part$smaller, a),
                  value = val,
                  right = merge_splay_trees(part$larger, b))
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
merge.splay_heap <- function(x, y, ...) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)

  new_tree <- merge_splay_trees(x$tree, y$tree)
  new_min_value <- min(x$min_value, y$min_value, na.rm = TRUE)
  splay_heap(min_value = new_min_value, splay_tree = new_tree)
}

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
if(!require(devtools)) install.packages("devtools")
devtools::install_github("mailund/ralgo")

## ## ----------## ----------## ----------## ----------## ----------## ----------## ------------
heap <- empty_leftist_heap()
for (i in 1:10)
  heap <- insert(heap, i)
plot(heap)

heap <- delete_minimal(heap)
plot(heap)

heap <- delete_minimal(heap)
plot(heap)

heap <- delete_minimal(heap)
plot(heap)

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
heap_sort <- function(x, empty_heap) {
  n <- length(x)
  
  # Build the heap
  heap <- vector_to_heap(x, empty_heap, empty_env_queue())
  
  # extract elements in increasing order
  result <- vector(typeof(x), length = n)
  for (i in seq_along(result)) {
    result[i] <- find_minimal(heap)
    heap <- delete_minimal(heap)
  }
  
  result
}

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
(x <- sample(10))
heap_sort(x, empty_leftist_heap())
heap_sort(x, empty_binomial_heap())
heap_sort(x, empty_splay_heap())

## ----------## ----------## ----------## ----------## ----------## ----------## ------------
splay_heap_sort <- function(x) {
  n <- length(x)
  
  heap <- empty_splay_heap()
  for (element in x)
    heap <- insert(heap, element)
  
  result <- vector(typeof(x), length = n)
  i <- 1
  recurse <- function(tree) {
    if (!is_empty(tree)) {
      recurse(tree$left)
      result[i] <<- tree$value
      i <<- i + 1
      recurse(tree$right)
    }
  }
  recurse(heap$tree)
  result
}

splay_heap_sort(x)

