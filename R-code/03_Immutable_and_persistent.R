## ------------------------------------------------------------------------------
x[i] <- v

## ------------------------------------------------------------------------------
is_empty <- function(x) UseMethod("is_empty")

list_cons <- function(elem, lst)
  structure(list(item = elem, tail = lst), class = "linked_list")

list_nil <- list_cons(NA, NULL)
is_empty.linked_list <- function(x) identical(x, list_nil)
empty_list <- function() list_nil

list_head <- function(lst) lst$item
list_tail <- function(lst) lst$tail

## ------------------------------------------------------------------------------
x <- list_cons(2, list_cond(1, empty_list()))
y <- list_cons(3, x)
z <- list_cons(4, empty_list())

## ------------------------------------------------------------------------------
list_reverse_helper <- function(lst, acc) {
  if (is_empty(lst)) acc
  else list_reverse_helper(list_tail(lst),
                           list_cons(list_head(lst), acc))
}
list_reverse_rec <- function(lst)
  list_reverse_helper(lst, empty_list())

## ------------------------------------------------------------------------------
list_reverse_loop <- function(lst) {
  acc <- empty_list()
  while (!is_empty(lst)) {
    acc <- list_cons(list_head(lst), acc)
    lst <- list_tail(lst)
  }
  acc
}

## ------------------------------------------------------------------------------
setup <- function(n) {
  lst <- empty_list()
  elements <- sample(1:n)
  for (elm in elements) {
    lst <- list_cons(elm, lst)
  }
  lst
}
evaluate_rec <- function(n, lst) {
  list_reverse_rec(lst)
}
evaluate_loop <- function(n, lst) {
  list_reverse_loop(lst)
}

## ------------------------------------------------------------------------------
ns <- seq(100, 500, by = 50)
performance <- rbind(
  get_performance("recursive", ns, setup, evaluate_rec, times = 25),
  get_performance("loop", ns, setup, evaluate_loop, times = 25)
)

## ------------------------------------------------------------------------------
library(ggplot2)
ggplot(performance, aes(x = n, y = time / n, colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess", se = FALSE, span = 2) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab("Time / n") + theme_minimal()

## ------------------------------------------------------------------------------
list_concatenate <- function(l1, l2) {
  if (is_empty(l1)) l2
  else list_cons(list_head(l1),
                 list_concatenate(list_tail(l1), l2))
}

## ------------------------------------------------------------------------------
list_concatenate_loop <- function(l1, l2) {
  rev_l1 <- empty_list()
  while (!is_empty(l1)) {
    rev_l1 <- list_cons(list_head(l1), rev_l1)
    l1 <- list_tail(l1)
  }
  result <- l2
  while (!is_empty(rev_l1)) {
    result <- list_cons(list_head(rev_l1), result)
    rev_l1 <- list_tail(rev_l1)
  }
  result
}

## ------------------------------------------------------------------------------
evaluate_rec <- function(n, lst) {
  list_concatenate(lst, lst)
}
evaluate_loop <- function(n, lst) {
  list_concatenate_loop(lst, lst)
}

## ------------------------------------------------------------------------------
list_remove <- function(lst, elm) {
  if (is_empty(lst)) lst
  else if (list_head(lst) == elm) list_tail(lst)
  else list_cons(list_head(lst), list_remove(list_tail(lst), elm))
}

## ------------------------------------------------------------------------------
search_tree_node <- function(
  value
  , left = empty_search_tree()
  , right = empty_search_tree()
) {
  structure(list(left = left, value = value, right = right),
            class = c("unbalanced_search_tree"))
}

empty_search_tree <- function()
  search_tree_node(NA, NULL, NULL)
is_empty.unbalanced_search_tree <- function(x)
  is.null(x$left) && is.null(x$right)

## ------------------------------------------------------------------------------
insert <- function(x, elm) UseMethod("insert")
remove <- function(x, elm) UseMethod("remove")
member <- function(x, elm) UseMethod("member")

## ------------------------------------------------------------------------------
member.unbalanced_search_tree <- function(x, elm) {
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)
  if (elm < x$value) member(x$left, elm)
  else member(x$right, elm)
}

## ------------------------------------------------------------------------------
st_member <- function(x, elm) {
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)
  if (elm < x$value) st_member(x$left, elm)
  else st_member(x$right, elm)
}
member.unbalanced_search_tree <- function(x, elm) {
  st_member(x, elm)
}

## ------------------------------------------------------------------------------
st_member <- function(x, elm, candidate = NA) {
  if (is_empty(x)) return(!is.na(candidate) && elm == candidate)
  if (elm < x$value) st_member(x$left, elm, candidate)
  else st_member(x$right, elm, x$value)
}
member.unbalanced_search_tree <- function(x, elm) {
  st_member(x, elm)
}

## ------------------------------------------------------------------------------
member.unbalanced_search_tree <- function(x, elm) {
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)
  if (elm < x$value) member(x$left, elm)
  else member(x$right, elm)
}

st_member_slow <- function(x, elm) {
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)
  if (elm < x$value) st_member_slow(x$left, elm)
  else st_member_slow(x$right, elm)
}

st_member_fast <- function(x, elm, candidate = NA) {
  if (is_empty(x)) return(!is.na(candidate) && elm == candidate)
  if (elm < x$value) st_member_fast(x$left, elm, candidate)
  else st_member_fast(x$right, elm, x$value)
}


setup_for_member <- function(n) {
  tree <- empty_search_tree()
  elements <- sample(1:n)
  for (elm in elements) {
    tree <- insert(tree, elm)
  }
  tree
}
evaluate_member <- function(member_func) function(n, tree) {
  elements <- sample(1:n, size = 100, replace = TRUE)
  for (elm in elements) {
    member_func(tree, elm)
  }
}

## ------------------------------------------------------------------------------
insert.unbalanced_search_tree <- function(x, elm) {
  if (is_empty(x)) return(search_tree_node(elm))
  if (elm < x$value)
    search_tree_node(x$value, insert(x$left, elm), x$right)
  else if (elm > x$value)
    search_tree_node(x$value, x$left, insert(x$right, elm))
  else
    x # the value is already in the tree
}

## ------------------------------------------------------------------------------
st_insert <- function(tree, elm) {
  if (is_empty(tree))
    return(search_tree_node(elm))
  if (elm < tree$value)
    search_tree_node(tree$value,
                     st_insert(tree$left, elm),
                     tree$right)
  else if (elm > tree$value)
    search_tree_node(tree$value,
                     tree$left,
                     st_insert(tree$right, elm))
  else
    tree
}
insert.unbalanced_search_tree <- function(x, elm, ...) {
  st_insert(x, elm)
}

## ------------------------------------------------------------------------------
setup_for_construction <- function(n) n
evaluate_construction <- function(order, ins) function(n, x) {
  tree <- empty_search_tree()
  elements <- order(1:n)
  for (elm in elements) {
    tree <- ins(tree, elm)
  }
  tree
}

## ------------------------------------------------------------------------------
ns <- seq(100, 400, by = 50)
construction_performance <- rbind(
  get_performance("Generic & ordered", ns,
                  setup_for_construction,
                  evaluate_construction(identity, insert)),
  get_performance("Direct recursion & ordered", ns,
                  setup_for_construction,
                  evaluate_construction(identity, st_insert)),
  get_performance("Generic & random", ns,
                  setup_for_construction,
                  evaluate_construction(sample, insert)),
  get_performance("Direct recursion & random", ns,
                  setup_for_construction,
                  evaluate_construction(sample, st_insert))
)

## ------------------------------------------------------------------------------
library(ggforce)
construction_performance %>%
  mutate(is_random = grepl(".*random", algo)) %>%
  ggplot(aes(x = n, y = time / n^2, colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess", se = FALSE, span = 1) +
  scale_colour_grey("Data structure & order", end = 0.8) +
  xlab(quote(n)) + ylab(expression(Time / n**2)) +
  facet_zoom(y = is_random, zoom.size = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ------------------------------------------------------------------------------
st_leftmost <- function(x) {
  while (!is_empty(x)) {
    value <- x$value
    tree <- x$left
  }
  value
}

st_remove <- function(tree, elm) {
  # if we reach an empty tree, there is nothing to do
  if (is_empty(tree)) return(tree)

  if (tree$value == elm) {
    a <- tree$left
    b <- tree$right
    if (is_empty(a)) return(b)
    if (is_empty(b)) return(a)

    s <- st_leftmost(tree$right)
    return(search_tree_node(s, a, st_remove(b, s)))
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    search_tree_node(tree$value, st_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    search_tree_node(tree$value, tree$left, st_remove(tree$right, elm))
}

remove.unbalanced_search_tree <- function(x, elm, ...) {
  st_remove(x, elm)
}

## ------------------------------------------------------------------------------
library(tidygraph)
library(ggraph)
library(tibble)

## ------------------------------------------------------------------------------
node_number_annotate_tree <- function(tree, i = 1) {
  if (is_empty(tree)) {
    tree$dfn <- i
  } else {
    left <- node_number_annotate_tree(tree$left, i)
    right <- node_number_annotate_tree(tree$right, left$dfn + 1)
    tree$dfn <- right$dfn + 1
    tree$left <- left
    tree$right <- right
  }
  tree
}

## ------------------------------------------------------------------------------
extract_graph <- function(tree) {
  n <- tree$dfn
  values <- vector("numeric", length = n)
  from <- vector("integer", length = n - 1)
  to <- vector("integer", length = n - 1)
  edge_idx <- 1

  extract <- function(tree) {
    # we change the index so the root is number 1
    # that is easier
    i <- n - tree$dfn + 1
    values[i] <<- ifelse(is.na(tree$value), "", tree$value)

    if (!is_empty(tree)) {
      j <- n - tree$left$dfn + 1
      from[edge_idx] <<- i
      to[edge_idx] <<- j
      edge_idx <<- edge_idx + 1

      k <- n - tree$right$dfn + 1
      from[edge_idx] <<- i
      to[edge_idx] <<- k
      edge_idx <<- edge_idx + 1

      extract(tree$left)
      extract(tree$right)
    }
  }

  extract(tree)
  nodes <- tibble(1:n, value = values)
  edges <- tibble(from = from, to = to)
  list(nodes = nodes, edges = edges)
}

## ------------------------------------------------------------------------------
plot.unbalanced_search_tree <- function(x, ...) {
  x %>% node_number_annotate_tree %>%
    extract_graph %$% tbl_graph(nodes, edges) %>%
    mutate(leaf = node_is_leaf()) %>%
    ggraph(layout = "tree") +
    scale_x_reverse() +
    geom_edge_link() +
    geom_node_point(aes(filter = leaf),
                    size = 2, shape = 21, fill = "black") +
    geom_node_point(aes(filter = !leaf),
                    size = 10, shape = 21, fill = "white") +
    geom_node_text(aes(label = value), vjust = 0.4) +
    theme_graph()
}

## ------------------------------------------------------------------------------
tree <- empty_search_tree()
for (x in 1:7)
  tree <- insert(tree, x)
plot(tree) + ggtitle("Ordered")

tree <- empty_search_tree()
for (x in rev(1:7))
  tree <- insert(tree, x)
plot(tree) + ggtitle("Reverse")

tree <- empty_search_tree()
for (x in sample(1:7))
  tree <- insert(tree, x)
plot(tree) + ggtitle("Random")

## ------------------------------------------------------------------------------
ral_binary_tree <- function(value, left, right) {
  list(value = value, left = left, right = right)
}

## ------------------------------------------------------------------------------
ral_is_leaf <- function(tree)
  !is.null(tree) && is.null(tree$left) && is.null(tree$right)

ral_tree_lookup <- function(tree, tree_size, idx) {
  if (idx == 1) return(tree$value)

  if (ral_is_leaf(tree)) # a leaf but idx is not one!
    if (idx > 1) stop("Index error in lookup")

  child_size <- (tree_size - 1) / 2
  if (idx <= child_size + 1)
    ral_tree_lookup(tree$left, child_size, idx - 1)
  else
    ral_tree_lookup(tree$right, child_size, idx - 1 - child_size)
}

## ------------------------------------------------------------------------------
ral_tree_update <- function(tree, tree_size, idx, value) {
  if (ral_is_leaf(tree)) {
    if (idx == 1)
      return(ral_binary_tree(value, NULL, NULL))
    # a leaf but idx is not one!
    stop("Index error")
  }

  if (idx == 1) {
    ral_binary_tree(value, tree$left, tree$right)
  } else {
    child_size <- (tree_size - 1) / 2
    if (idx <= child_size + 1) {
      ral_binary_tree(tree$value,
                      ral_tree_update(tree$left, child_size,
                      idx - 1, value),
                      tree$right)
    } else {
      ral_binary_tree(tree$value,
                      tree$left,
                      ral_tree_update(tree$right, child_size,
                                      idx - 1 - child_size, value))
    }
  }
}

## ------------------------------------------------------------------------------
ral_node <- function(tree, tree_size, siblings) {
  list(tree = tree, tree_size = tree_size, siblings = siblings)
}

## ------------------------------------------------------------------------------
ral_is_empty <- function(ral) is.null(ral)

## ------------------------------------------------------------------------------
ral_head <- function(ral) {
  ral$tree$value
}

## ------------------------------------------------------------------------------
ral_singleton_node <- function(value, siblings = NULL) {
  singleton_tree <- ral_binary_tree(value, NULL, NULL)
  ral_node(singleton_tree, 1, siblings)
}
ral_cons <- function(elem, ral) {
  if (ral_is_empty(ral) || ral_is_empty(ral$siblings))
      return(ral_singleton_node(elem, ral))

  first <- ral$tree
  first_size <- ral$tree_size
  second <- ral$siblings$tree
  second_size <- ral$siblings$tree_size
  rest <- ral$siblings$siblings

  if (first_size < second_size)
    ral_singleton_node(elem, ral)
  else
    ral_node(ral_binary_tree(elem, first, second),
             first_size + second_size + 1, rest)
}

## ------------------------------------------------------------------------------
ral_is_singleton <- function(ral) {
  ral$tree_size == 1
}

ral_tail <- function(ral) {
  if (ral_is_singleton(ral))
    ral$siblings
  else {
    left <- ral$tree$left
    right <- ral$tree$right
    size <- (ral$tree_size - 1) / 2
    ral_node(left, size, ral_node(right, size, ral$siblings))
  }
}

## ------------------------------------------------------------------------------
ral_lookup <- function(ral, idx) {
  while (!is.null(ral)) {
    if (idx <= ral$tree_size)
      return(ral_tree_lookup(ral$tree, ral$tree_size, idx))
    idx <- idx - ral$tree_size
    ral <- ral$siblings
  }
  stop("Index out of bounds")
}

## ------------------------------------------------------------------------------
ral_update <- function(ral, idx, value) {
  if (idx < 1) stop("Index out of bounds")
  if (idx <= ral$tree_size)
    ral_node(ral_tree_update(ral$tree, ral$tree_size, idx, value),
             ral$tree_size, ral$siblings)
  else
    ral_node(ral$tree, ral$tree_size,
             ral_update(ral$siblings, idx - ral$tree_size, value))

}

