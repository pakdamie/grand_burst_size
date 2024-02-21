rename_labels <- function(tree, vec) {
  tree$tip.label <- vec
  return(tree)
}
