# This is the main script for creating the full phylogeny for the
# mammal (MAM), avian (AVE), and reptile (REP) hosts.

REP_Phylo <- read.nexus(here("Data", "Reptile", "reptile_phylo_1000.nex"))
AVE_Phylo <- read.nexus(here("Data", "Avian", "avian_phylo_1000.nex"))
MAM_Phylo <- read.nexus(here("Data", "Mammal", "mammal_phylo_1000.nex"))

# There are a 1000 trees so I am creating a consensus tree 
# (these have no branch lengths). The p = 0.5 means that it's 
# majority rule consensus

consensus_AVE_Tree <- consensus(AVE_Phylo, p = 0.5, check.labels = TRUE, rooted = TRUE)
consensus_REP_Tree <- consensus(REP_Phylo, p = 0.5, check.labels = TRUE, rooted = TRUE)
consensus_MAM_Tree <- consensus(MAM_Phylo, p = 0.5, check.labels = TRUE, rooted = TRUE)

### We can use `multi2di` to randomly resolve phylogeny
consensus_AVE_Tree <- multi2di(consensus_AVE_Tree, random = TRUE)
consensus_REP_Tree <- multi2di(consensus_REP_Tree, random = TRUE)
consensus_MAM_Tree <- multi2di(consensus_MAM_Tree, random = TRUE)

# To merge these trees together, we create a super tree
#that has both reptile, mammal, and avian hosts.

tip.labels <- c("mam", "birds", "squam")

edge <- matrix(
  c(
    4, 1,
    4, 5,
    5, 2,
    5, 3
  ),
  byrow = TRUE, ncol = 2
)

edge.length <- c(1, 1, 1, 1)

Nnode <- 2

ordertree <- list(edge = edge, Nnode = Nnode, tip.label = tip.labels)
class(ordertree) <- "phylo"

### We then graft the individual clades into the supertree, one by one
tree_list <- list(
  squam = consensus_REP_Tree,
  birds = consensus_AVE_Tree,
  mam = consensus_MAM_Tree
)

class(tree_list) <- "multiPhylo"


Order_1_Reptile <- bind.tree(
  x = ordertree,
  y = consensus_REP_Tree,
  where = 3, interactive = FALSE
)

Order_2_Reptile_Avian <- bind.tree(
  x = Order_1_Reptile,
  y = consensus_AVE_Tree,
  where = 2,
  interactive = FALSE
)

### This is the full super-tree that will be used for all analyses.
Full_SuperTree_Host <- bind.tree(
  x = Order_2_Reptile_Avian,
  y = consensus_MAM_Tree,
  where = 1,
  interactive = FALSE
)

### USE Full_SuperTree_Host.
