insert <- function(x, elm) UseMethod("insert")
remove <- function(x, elm) UseMethod("remove")
member <- function(x, elm) UseMethod("member")

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

member.unbalanced_search_tree <- function(x, elm) {
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)
  if (elm < x$value) member(x$left, elm)
  else member(x$right, elm)
}

st_member <- function(x, elm, candidate = NA) {
  if (is_empty(x)) return(!is.na(candidate) && elm == candidate)
  if (elm < x$value) st_member(x$left, elm, candidate)
  else st_member(x$right, elm, x$value)
}
member.unbalanced_search_tree <- function(x, elm) {
	st_member(x, elm)
}

insert.unbalanced_search_tree <- function(x, elm) {
  if (is_empty(x)) return(search_tree_node(elm))
  if (elm < x$value)
    search_tree_node(x$value, insert(x$left, elm), x$right)
  else if (elm > x$value)
    search_tree_node(x$value, x$left, insert(x$right, elm))
  else
    x # the value is already in the tree
}

st_leftmost <- function(x) {
  while (!is_empty(x)) {
    value <- x$value
    tree <- x$left
  }
  value
}

remove.unbalanced_search_tree <- function(x, elm) {
  # if we reach an empty tree, there is nothing to do
  if (is_empty(x)) return(x)

  if (x$value == elm) {
    a <- x$left
    b <- x$right
    if (is_empty(a)) return(b)
    if (is_empty(b)) return(a)

    s <- st_leftmost(x$right)
    return(search_tree_node(s, a, remove(b, s)))
  }
  # we need to search further down to remove the element
  if (elm < x$value)
    search_tree_node(x$value, remove(x$left, elm), x$right)
  else # (elm > x$value)
    search_tree_node(x$value, x$left, remove(x$right, elm))
}

# colours
RED <- 1
BLACK <- 2

# helper function
red_black_tree_node <- function(
  colour
  , value
  , left = empty_red_black_tree()
  , right = empty_red_black_tree()
  ) {
  structure(list(colour = colour,
                 left = left,
                 value = value,
                 right = right),
            class = "red_black_tree")
}

empty_red_black_tree <- function()
	red_black_tree_node(BLACK, NA, NULL, NULL)
	
is_empty.red_black_tree <- function(x)
	is.null(x$left) && is.null(x$right)

member.search_tree <- function(x, elm) {
  st_member(x, elm)
}

search_tree_node <- function(
  value
  , left = empty_search_tree()
  , right = empty_search_tree()
) {
  structure(list(left = left, value = value, right = right),
            class = c("unbalanced_search_tree", "search_tree"))
}

red_black_tree_node <- function(
  colour
  , value
  , left = empty_red_black_tree()
  , right = empty_red_black_tree()
  ) {
  structure(list(colour = colour,
                 left = left,
                 value = value,
                 right = right),
            class = c("red_black_tree", "search_tree"))
}

rbt_insert <- function(tree, elm) {
  if (is_empty(tree)) return(red_black_tree_node(RED, elm))
  if (elm < tree$value)
    rbt_balance(tree$colour,
                tree$value,
                rbt_insert(tree$left, elm),
                tree$right)
  else if (elm > tree$value)
    rbt_balance(tree$colour,
                tree$value,
                tree$left,
                rbt_insert(tree$right, elm))
  else
    tree # the value is already in the tree, at this level,
         # so just return
}

insert.red_black_tree <- function(x, elm, ...) {
  # insert the value in the tree and set the root to be black
  new_tree <- rbt_insert(x, elm)
  new_tree$colour <- BLACK
  new_tree
}

pattern_match <- function(...) {
  bindings <- eval(substitute(alist(...)))
  scope <- parent.frame()

  var_names <- names(bindings)
  for (i in seq_along(bindings)) {
    name <- var_names[i]
    val <- eval(bindings[[i]], scope)

    if (is.null(val)) return(FALSE)

    # for expressions that are not assignments,we consider them
    # conditions that must be true for the pattern to match.
    # Return FALSE if they are not.
    if (nchar(name) == 0 && (is.na(val) || !val)) return(FALSE)
    else if (nchar(name) > 0) assign(name, val, envir = scope)

  }
  return(TRUE)
}

rbt_balance <- function(colour, value, left, right) {
  # Setting these to avoid warnings
  a <- b <- c <- d <- x <- y <- z <- NULL
  if (pattern_match(a = left$left, b = left$right$left,
                    c = left$right$right, d = right,
                    x = left$value, y = left$right$value,
                    z = value,
                    colour == BLACK, left$colour == RED,
                    left$right$colour == RED)

      || pattern_match(a = left$left$left, b = left$left$right,
                       c = left$right, d = right,
                       x = left$left$value, y = left$value,
                       z = value,
                       colour == BLACK, left$colour == RED,
                       left$left$colour == RED)

      || pattern_match(a = left, b = right$left,
                       c = right$right$left, d = right$right$right,
                       x = value, y = right$value,
                       z = right$right$value,
                       colour == BLACK, right$colour == RED,
                       right$right$colour == RED)

      || pattern_match(a = left, b = right$left$left,
                       c = right$left$right, d = right$right,
                       x = value, y = right$left$value,
                       z = right$value,
                       colour == BLACK, right$colour == RED,
                       right$left$colour == RED)
  ) {

    left <- red_black_tree_node(colour = BLACK, value = x,
                                left = a, right = b)
    right <- red_black_tree_node(colour = BLACK, value = z,
                                 left = c, right = d)
    red_black_tree_node(colour = RED, value = y, left, right)

  } else {
    red_black_tree_node(colour, value, left, right)
  }
}

DOUBLE_BLACK <- 3

remove.red_black_tree <- function(x, elm, ...) {
  new_tree <- rbt_remove(x, elm)
  new_tree$colour <- BLACK
  new_tree
}

rbt_remove <- function(tree, elm) {
  if (is_empty(tree)) { # we didn't find the value...
    return(tree)
  }

  if (tree$value == elm) { # found the value to delete
    a <- tree$left
    b <- tree$right
    if (is_empty(a) && is_empty(b)) { # leaf
      if (tree$colour == BLACK)
        return(red_black_tree_node(DOUBLE_BLACK, NA, NULL, NULL))
      else
        return(red_black_tree_node(BLACK, NA, NULL, NULL))

    } else if (is_empty(a) || is_empty(b)) { # one empty child
      non_empty <- if (is_empty(a)) b else a
      non_empty$colour <- BLACK
      return(non_empty)

    } else { # inner node
      s <- st_leftmost(tree$right)
      return(rbt_rotate(tree$colour, s, a, rbt_remove(b, s)))
    }
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    rbt_rotate(tree$colour, tree$value,
               rbt_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    rbt_rotate(tree$colour, tree$value,
               tree$left, rbt_remove(tree$right, elm))
}

rbt_rotate <- function(colour, value, left, right) {
  # first case
  if (pattern_match(a_x_b = left, c = right$left, d = right$right,
                    y = value, z = right$value,
                    a_x_b$colour == DOUBLE_BLACK,
                    colour == RED,
                    right$colour == BLACK)) {

    a_x_b$colour <- BLACK
    rbt_balance(BLACK, z,
                red_black_tree_node(RED, y, a_x_b, c),
                d)

  } else if (pattern_match(a = left$left, b = left$right,
                           c_z_d = right,
                           y = value, x = left$value,
                           left$colour == BLACK,
                           colour == RED,
                           c_z_d$colour == DOUBLE_BLACK)) {

    c_z_d$colour <- BLACK
    rbt_balance(BLACK, x,
                a,
                red_black_tree_node(RED, y, b, c_z_d))


  # second case
  } else if (pattern_match(a_x_b = left, c = right$left, d = right$right,
                           y = value, z = right$value,
                           colour == BLACK,
                           a_x_b$colour == DOUBLE_BLACK,
                           right$colour == BLACK)) {

    a_x_b$colour <- BLACK
    new_left <- red_black_tree_node(RED, y, a_x_b, c)
    rbt_balance(DOUBLE_BLACK, z, new_left, d)

  } else if (pattern_match(a = left$left, b = left$right,
                           y = value, c_z_d = right,
                           left$colour == BLACK,
                           colour == BLACK,
                           c_z_d$colour == DOUBLE_BLACK)) {

    c_z_d$colour <- BLACK
    new_right <- red_black_tree_node(RED, y, b, c_z_d)
    rbt_balance(DOUBLE_BLACK, x, a, new_right)

  # third case
  } else if (pattern_match(a_w_b = left,
                           c = right$left$left,
                           d = right$left$right,
                           e = right$right,
                           x = value, z = right$value,
                           y = right$left$value,
                           a_w_b$colour == DOUBLE_BLACK,
                           colour == BLACK,
                           right$colour == RED)) {

    a_w_b$colour <- BLACK
    new_left_left <- red_black_tree_node(RED, x, a_w_b, c)
    new_left <- rbt_balance(BLACK, y, new_left_left, d)
    red_black_tree_node(BLACK, z, new_left, e)

  } else if (pattern_match(a = left$left,
                           b = left$right$left,
                           c = left$right$right,
                           d_w_e = right,
                           z = left$right$value,
                           x = left$value,
                           y = value,
                           left$colour == RED,
                           colour == BLACK,
                           d_w_e$colour == DOUBLE_BLACK)) {

    d_w_e$colour <- BLACK
    new_right_right <- red_black_tree_node(RED, y, c, d_w_e)
    new_right <- rbt_balance(BLACK, z, b, new_right_right)
    red_black_tree_node(BLACK, x, a, new_right)

  } else {
    red_black_tree_node(colour, value, left, right)
  }
}

rbt_balance <- function(colour, value, left, right) {
  if (pattern_match(a = left$left,
                    b = left$right$left,
                    c = left$right$right,
                    d = right,
                    x = left$value,
                    y = left$right$value,
                    z = value,
                    colour == BLACK,
                    left$colour == RED,
                    left$right$colour == RED)

      || pattern_match(a = left$left$left,
                       b = left$left$right,
                       c = left$right,
                       d = right,
                       x = left$left$value,
                       y = left$value,
                       z = value,
                       colour == BLACK,
                       left$colour == RED,
                       left$left$colour == RED)

      || pattern_match(a = left,
                       b = right$left,
                       c = right$right$left,
                       d = right$right$right,
                       x = value,
                       y = right$value,
                       z = right$right$value,
                       colour == BLACK,
                       right$colour == RED,
                       right$right$colour == RED)

      || pattern_match(a = left,
                       b = right$left$left,
                       c = right$left$right,
                       d = right$right,
                       x = value,
                       y = right$left$value,
                       z = right$value,
                       colour == BLACK,
                       right$colour == RED,
                       right$left$colour == RED)
  ) {

    left <- red_black_tree_node(BLACK, x, a, b)
    right <- red_black_tree_node(BLACK, z, c, d)
    red_black_tree_node(colour = RED, value = y, left, right)

  } else if (pattern_match(a = left$left,
                           b = left$right$left,
                           c = left$right$right,
                           d = right,
                           z = value,
                           x = left$value,
                           y = left$right$value,
                           colour == DOUBLE_BLACK,
                           left$colour == RED,
                           left$right$colour == RED)

             || pattern_match(a = left,
                              b = right$left$left,
                              c = right$left$right,
                              d = right$right,
                              x = value,
                              z = right$value,
                              y = right$left$value,
                              colour == DOUBLE_BLACK,
                              right$colour == RED,
                              right$left$colour == RED)) {

    left <- red_black_tree_node(BLACK, x, a, b)
    right <- red_black_tree_node(BLACK, z, c, d)
    red_black_tree_node(BLACK, y, left, right)

  } else {
    red_black_tree_node(colour, value, left, right)
  }
}

min_max_depth <- function(tree) {
  if (is_empty(tree))
    return(c(0, 0))
  left_min_max <- min_max_depth(tree$left)
  right_min_max <- min_max_depth(tree$right)
  min_depth <- min(left_min_max[1], right_min_max[1]) + 1
  max_depth <- max(left_min_max[2], right_min_max[2]) + 1
  c(min = min_depth, max = max_depth)
}

library(tibble)
measure_depth <- function(process, empty_tree, ns) {
  measure_depth <- function(n) {
    tree <- empty_tree
    for (x in process(1:n))
      tree <- insert(tree, x)
    c(n = n, min_max_depth(tree))
  }
  measure_depth <- Vectorize(measure_depth)
  measurements <- measure_depth(ns)
  as_tibble(t(measurements))
}

ns <- seq(1, 50, by = 5)
ns_rep <- rep(ns, times = 10)
depth <- rbind(
  cbind(data_structure = "Unbalanced",
        data = "Ordered",
        measure_depth(identity, empty_search_tree(), ns)),
  cbind(data_structure = "Red-black",
        data = "Ordered",
        measure_depth(identity, empty_red_black_tree(), ns)),
  cbind(data_structure = "Unbalanced",
        data = "Random",
        measure_depth(sample, empty_search_tree(), ns_rep)),
  cbind(data_structure = "Red-black",
        data = "Random",
        measure_depth(sample, empty_red_black_tree(), ns_rep))
)

library(ggplot2)
ggplot(depth, aes(x = n, colour = data_structure)) +
  geom_jitter(aes(y = min)) +
  geom_smooth(aes(y = min), method = "loess",
              span = 2, se = FALSE) +
  geom_jitter(aes(y = max)) +
  geom_smooth(aes(y = max), method = "loess",
              span = 2, se = FALSE) +
  facet_grid(. ~ data) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab("Tree depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

setup_for_construction <- function(n) n
evaluate_construction <- function(empty, process) function(n, x) {
  tree <- empty
  elements <- process(1:n)
  for (elm in elements) {
    tree <- insert(tree, elm)
  }
  tree
}

get_performance_n <- function(
  algo
  , data
  , n
  , setup
  , evaluate
  , times
  , ...) {

  config <- setup(n)
  benchmarks <- microbenchmark(evaluate(n, config), times = times)
  tibble(algo = algo, data = data,
         n = n, time = benchmarks$time / 1e9)
}

get_performance <- function(
  algo
  , data
  , ns
  , setup
  , evaluate
  , times = 10
  , ...) {
  f <- function(n)
    get_performance_n(algo, data, n,
                      setup, evaluate,
                      times = times, ...)
  results <- Map(f, ns)
  do.call('rbind', results)
}

ns <- seq(100, 450, by = 50)
performance_small <- rbind(
  get_performance("Unbalanced", "Increasing order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_search_tree(), identity)),
  get_performance("Unbalanced", "Random order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_search_tree(), sample)),
  get_performance("Red-black", "Increasing order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_red_black_tree(), identity)),
  get_performance("Red-black", "Random order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_red_black_tree(), sample))
)

ns <- seq(500, 1000, by = 50)
performance_medium <- rbind(
  get_performance("Unbalanced", "Random order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_search_tree(), sample)),
  get_performance("Red-black", "Increasing order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_red_black_tree(), identity)),
  get_performance("Red-black", "Random order", ns,
                  setup_for_construction,
                  evaluate_construction(empty_red_black_tree(), sample))
)

performance <- rbind(performance_small, performance_medium)
ggplot(performance,
       aes(x = n, y = time / (n*log(n)), colour = algo)) +
  geom_jitter() +
  geom_smooth(method = "loess",
              span = 2, se = FALSE) +
  facet_grid(. ~ data) +
  scale_colour_grey("Data structure", end = 0.5) +
  xlab(quote(n)) + ylab(expression(Time / n*log(n))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

setup_for_member <- function(empty, process) function(n) {
  tree <- empty
  elements <- process(1:n)
  for (elm in elements) {
    tree <- insert(tree, elm)
  }
  tree
}
evaluate_member <- function(n, tree) {
  elements <- sample(1:n, size = 100, replace = TRUE)
  for (elm in elements) {
    member(tree, elm)
  }
}

setup_for_remove <- function(empty, process) function(n) {
  tree <- empty
  elements <- process(1:n)
  for (elm in elements) {
    tree <- insert(tree, elm)
  }
  tree
}
evaluate_remove <- function(n, tree) {
  elements <- sample(1:n, size = 100, replace = TRUE)
  for (elm in elements) {
    tree <- remove(tree, elm)
  }
}

# helper function for traversing trees
depth_first_visit_binary_tree <- function(tree, visitor) {
  if (!is_empty(tree)) {
    depth_first_visit_binary_tree(tree$left, visitor)
    depth_first_visit_binary_tree(tree$right, visitor)
  }
  visitor(tree)
}

# function for extracting a graph from a tree
extract_graph <- function(tree) UseMethod("extract_graph")

extract_graph.search_tree <- function(tree) {
  n <- tree$dfn
  values <- vector("numeric", length = n)
  from <- vector("integer", length = n - 1)
  to <- vector("integer", length = n - 1)
  edge_idx <- 1

  extract <- function(tree) {
    # we change the index so the root is number 1
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
    }
  }

  depth_first_visit_binary_tree(tree, extract)
  nodes <- tibble(value = values)
  edges <- tibble(from = from, to = to)
  list(nodes = nodes, edges = edges)
}

plot.search_tree <- function(x, ...) {
  x %>% node_number_annotate_tree %>%
    extract_graph %$% tbl_graph(nodes, edges) %>%
    mutate(leaf = node_is_leaf()) %>%
    ggraph(layout = "tree") +
    scale_x_reverse() +
    geom_edge_link() +
    geom_node_point(aes(filter = leaf),
                    size = 2, shape = 21,
                    fill = "black") +
    geom_node_point(aes(filter = !leaf),
                    size = 10, shape = 21,
                    fill = "white") +
    geom_node_text(aes(label = value),
                   vjust = 0.4) +
    theme_graph()
}

extract_graph.red_black_tree <- function(tree) {
  n <- tree$dfn
  colours <- vector("numeric", length = n)
  extract <- function(tree) {
    # we change the index so the root is number 1
    i <- n - tree$dfn + 1
    colours[i] <<- tree$colour
  }
  depth_first_visit_binary_tree(tree, extract)

  graph <- NextMethod()
  RB <- c("Red", "Black", "Double black")
  nodes <- graph$nodes %>% add_column(colour = RB[colours])
  edges <- graph$edges
  list(nodes = nodes, edges = edges)
}

plot.red_black_tree <- function(x, ...) {
  NextMethod() +
    scale_fill_manual("Colour",
                      values = c("Red" = "white",
                                 "Black" = "black",
                                 "Double black" = "lightgray")) +
    geom_node_point(aes(filter = leaf, fill = colour),
                    size = 2, shape = 21) +
    geom_node_point(aes(filter = !leaf, fill = colour),
                    size = 10, shape = 21) +
    geom_node_text(aes(filter = colour == "Black",
                       label = value),
                   colour = 'white', vjust = 0.4) +
    geom_node_text(aes(filter = colour == "Double black",
                       label = value),
                   colour = 'black', vjust = 0.4) +
    geom_node_text(aes(filter = colour == "Red",
                       label = value),
                   colour = 'black', vjust = 0.4)
}

rb_tree <- empty_red_black_tree()
for (i in 1:10)
  rb_tree <- insert(rb_tree, i)
plot(rb_tree)

empty_splay_tree <- function() {
  ref <- new.env(parent = emptyenv())
  ref$tree <- empty_splay_node()
  structure(ref, class = c("splay_tree", "search_tree"))
}

is_empty.splay_tree <- function(x)
  is_empty(x$tree)

insert.splay_tree <- function(x, elm) {
  if (member(x, elm))
    return(x) # don't insert if we already have the element

  part <- partition(elm, x$tree)
  x$tree <- splay_tree_node(
    value = elm,
    left = part$smaller,
    right = part$larger
  )
  x
}

splay_remove <- function(tree, elm) {
  # if we reach an empty tree, there is nothing to do
  if (is_empty(tree)) return(tree)

  if (tree$value == elm) {
    a <- tree$left
    b <- tree$right
    if (is_empty(a)) return(b)
    if (is_empty(b)) return(a)

    s <- st_leftmost(tree$right)
    return(splay_tree_node(s, a, splay_remove(b, s)))
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    splay_tree_node(tree$value, splay_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    splay_tree_node(tree$value, tree$left, splay_remove(tree$right, elm))
}

remove.splay_tree <- function(x, elm) {
  x$tree <- splay_remove(x$tree, elm)
  x
}

member.splay_tree <- function(x, elm) {
  x$tree <- splay(x$tree, elm)
  # if elm is in the tree it is now at the root
  !is.na(x$tree$value) && x$tree$value == elm
}

splay <- function(tree, v) {
  if (is_empty(tree) || tree$value == v) {
    tree # if v is already the root, we are done splaying
         # we are also done if we reach an empty tree;
         # then v is not in the tree

  # -- Zig-zig -------------------------------------
  } else if (pattern_match(z = tree$value,
                           y = tree$left$value,
                           z > v && y > v,

                           s = splay(tree$left$left, v),
                           x = s$value,

                           a = s$left,
                           b = s$right,
                           c = tree$left$right,
                           d = tree$right)) {
    splay_tree_node(
      value = x,
      left = a,
      right = splay_tree_node(
        value = y,
        left = b,
        right = splay_tree_node(
          value = z,
          left = c,
          right = d)))

  # -- Zag-zag -------------------------------------
  } else if (pattern_match(z = tree$value,
                           y = tree$right$value,
                           z < v && y < v,

                           s = splay(tree$right$right, v),
                           x = s$value,

                           a = s$left,
                           b = s$right,
                           c = tree$right$left,
                           d = tree$left)) {
    splay_tree_node(
      value = x,
      left = splay_tree_node(
        value = y,
        left = splay_tree_node(
          value = z,
          left = d,
          right = c),
        right = a),
      right = b)

  # -- Zig-zag & zag-zig ------------------------
  } else if (pattern_match(z = tree$value,
                           y = tree$left$value,
                           v < z && v > y,

                           s = splay(tree$left$right, v),
                           x = s$value,

                           a = tree$left$left,
                           b = s$left,
                           c = s$right,
                           d = tree$right)
             ||
             pattern_match(y = tree$value,
                           z = tree$right$value,
                           y < v && z > v,

                           s = splay(tree$right$left, v),
                           x = s$value,

                           a = tree$left,
                           b = s$left,
                           c = s$right,
                           d = tree$right$right)) {
  splay_tree_node(
      value = x,
      left = splay_tree_node(
        value = y,
        left = a,
        right = b),
      right = splay_tree_node(
        value = z,
        left = c,
        right = d))

  # -- Zig --------------------------------------
  } else if (pattern_match(y = tree$value,
                           y > v,

                           s = splay(tree$left, v),
                           x = s$value,

                           a = s$left,
                           b = s$right,
                           c = tree$right)) {
    splay_tree_node(
      value = x,
      left = a,
      right = splay_tree_node(
        value = y,
        left = b,
        right = c))

  # -- Zag --------------------------------------
  } else if (pattern_match(y = tree$value,
                           y < v,

                           s = splay(tree$right, v),
                           x = s$value,

                           a = tree$left,
                           b = s$left,
                           c = s$right)) {
    splay_tree_node(
      value = x,
      left = splay_tree_node(
        value = y,
        left = a,
        right = b),
      right = c)

  } else {
    # if the recursive splay operation returns an empty tree,
    # which can happen if v is not in the tree, we reach this point
    # and here we just give up and return the tree.
    tree

  }
}

tree <- empty_splay_tree()
for (x in 1:10)
  tree <- insert(tree, x)
plot(tree)

member(tree, 4)
plot(tree)

member(tree, 8)
plot(tree)

member(tree, 6)
plot(tree)

