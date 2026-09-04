

# Packages
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(igraph)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readxl)
  library(purrr)
  
  
  
})



plot_hs_igraph <- function(df, n = NULL,edge_cols = c("hs2002_code_upstream", "hs2002_code_downstream")) {
  stopifnot(all(edge_cols %in% names(df)))
  if (!is.null(n)) df <- utils::head(df, n)
  
  # Prepare edges
  df <- df[, edge_cols]
  df[[edge_cols[1]]] <- as.character(df[[edge_cols[1]]])
  df[[edge_cols[2]]] <- as.character(df[[edge_cols[2]]])
  df <- dplyr::distinct(df, .data[[edge_cols[1]]], .data[[edge_cols[2]]])
  
  # Build graph
  verts <- tibble::tibble(name = unique(c(df[[edge_cols[1]]], df[[edge_cols[2]]])))
  g <- igraph::graph_from_data_frame(d = df, directed = TRUE, vertices = verts)
  
  # Labels
  V(g)$label <- V(g)$name
  
  invisible(g)
}


#

product4d <- "8703"
#product4d <- "8470"
Lvl<-2

 

graphh <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx") # Arnaud
hsnames <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS22")
BEC_database <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BEC database.xlsx")

Capital_good<-BEC_database$HS6[BEC_database$BEC5EndUse=="CAP"] #Adapter BEC
# -----------------------------
# 1) Data cleaning helpers
# -----------------------------
clean_edges <- function(graphh) {
  # ensure character
  graphh <- graphh %>%
    mutate(
      hs2002_code_upstream   = as.character(hs2002_code_upstream),
      hs2002_code_downstream = as.character(hs2002_code_downstream)
    )
  # pad 3-digit codes with a leading 0
  pad3 <- function(x) ifelse(nchar(x) == 3, paste0("0", x), x)
  graphh %>%
    mutate(
      hs2002_code_upstream   = pad3(hs2002_code_upstream),
      hs2002_code_downstream = pad3(hs2002_code_downstream)
    )
}


#Take out exclude_capital_goods machinery
graphh_clean <- clean_edges(graphh)

graphh_clean<-graphh_clean[!graphh_clean$hs2002_code_upstream %in% unique(substr(Capital_good,1,4)) ,]
graphh_clean<-graphh_clean[!graphh_clean$hs2002_code_downstream %in%unique(substr(Capital_good,1,4)) ,]


#edge_df<-graphh_clean

# -----------------------------
# 2) Build hidden graph
#    Direction: input (upstream) -> product (downstream)
#Select only for the input
# -----------------------------
build_hidden_graph <- function(ed, product) {
  # deduplicate edges
  ed <- ed[ed$hs2002_code_downstream == product, ]
}
Hidden_network<-build_hidden_graph(graphh,product4d)#list of upstream product
potential_p1<-Hidden_network$hs2002_code_upstream

# -----------------------------
# 2) Identify at the second stage which of the input of P0 are input in the other product
# -----------------------------


# Stage 1 inputs of root (nodes pointing to root) #identify linkages between nodes that are input of p0
#t1<-Hidden_network[Hidden_network$hs2002_code_downstream%in%potential_p1,] #Total input list of the product 0 inputs

#t1<-Hidden_network[Hidden_network$hs2002_code_downstream%in%potential_p1,] #Total input list of the product 0 inputs


tpp<-graphh_clean[graphh_clean$hs2002_code_downstream%in%potential_p1,] #Total input list of the product 0 inputs
t1<-tpp[tpp$hs2002_code_upstream%in%potential_p1,] #Total input list of the product 0 inputs

rm(tpp)




distr <- as.matrix(table(t1$hs2002_code_downstream))



library(ggplot2)

x <- as.vector(distr)
df <- data.frame(x)

# histogram
ggplot(df, aes(x)) +
  geom_histogram(binwidth = 10, fill = "lightblue", colour = "black") +
  labs(title = "Histogram of distr", x = "Values", y = "Count")

# cumulative distribution
ggplot(df, aes(x)) +
  stat_ecdf(geom = "step", colour = "blue") +
  labs(title = "Cumulative distribution of distr", x = "Values", y = "Cumulative probability")

prod_list <- as.data.frame(distr[,1])


Highconnectedprod <- prod_list %>%
  arrange(desc(distr[, 1])) %>%
  slice(1:ceiling(0.05 * n()))

#On this input list select only the product that are input of p0, that also input of another
# t1<-t1[t1$hs2002_code_upstream%in%potential_p1,] 

# Identifier les noeuds qui ne sont pas attache aux autre et les rattacher a p0
# Hidden_network_stage1<-Hidden_network[!t1$hs2002_code_downstream%in%Hidden_network$hs2002_code_upstream,]

# Identifying the most down stream nodes and reattaching them to p0. If the node doesn't send inputs to any othere product attach it to p0

t2<-cbind.data.frame(hs2002_code_upstream =row.names(Highconnectedprod),hs2002_code_downstream =product4d)

# Hidden_network_stage1<-rbind.data.frame(Hidden_network_stage1, t2)

Hidden_network_stage12<-rbind.data.frame(t1,t2)





















g<-graph_from_data_frame(Hidden_network_stage12)
g_rev <- igraph::reverse_edges(g)


# Keep edges where a path exists of length 1, 2, and 3 between source and target
keep_multi_distance_edges <- function(g) {
  edges_to_keep <- integer(0)
  vids <- V(g)
  n <- vcount(g)
  
  # Precompute all-pairs shortest paths (this is fine for moderate graphs)
  D <- distances(g, mode = "out")
  
  for (e in E(g)) {
    ends_e <- ends(g, e, names = FALSE)
    v <- ends_e[1]
    u <- ends_e[2]
    
    d <- D[v, u]
    # If no path exists (Inf), skip
    if (is.infinite(d)) next
    
    # Check existence of alternative paths of length 2 and 3
    has2 <- FALSE
    has3 <- FALSE
    
    # Find nodes reachable in 1 and 2 steps
    n1 <- neighbors(g, v, mode = "out")
    n2 <- unique(unlist(lapply(n1, function(x) neighbors(g, x, mode = "out"))))
    n3 <- unique(unlist(lapply(n2, function(x) neighbors(g, x, mode = "out"))))
    
    if (u %in% n2) has2 <- TRUE
    if (u %in% n3) has3 <- TRUE
    
    # Keep edge only if there are 1, 2, and 3-step paths from v to u
    if (d == 1 && has2 && has3) {
      edges_to_keep <- c(edges_to_keep, e)
    }
  }
  
  subgraph.edges(g, edges_to_keep, delete.vertices = FALSE)
}

gplus <- keep_multi_distance_edges(g_rev)



plot(gplus)


#################################################################################
#################################################################################
###################################################################################
library(igraph)

# Internal: 0->"a", 1->"b", ..., 26->"aa", etc.
.stage_code <- function(s) {
  stopifnot(s >= 0)
  alph <- letters
  q <- s
  out <- character()
  repeat {
    out <- c(alph[(q %% 26) + 1], out)
    q <- q %/% 26 - 1
    if (q < 0) break
  }
  paste0(out, collapse = "")
}

# Build an unfolded walk tree with branch multiplication.
# Each tree node reproduces all out-edges (or in-edges, or both) of its underlying node.
# Labels: a<root>, b<child>a<root>, c<child>b<parent>a<root>, ...
build_unfolded_tree <- function(g, root, max_depth = 4, mode = c("out", "in", "all")) {
  stopifnot(inherits(g, "igraph"), max_depth >= 0)
  mode <- match.arg(mode)
  
  # Ensure vertex names exist and are character
  has_names <- "name" %in% vertex_attr_names(g)
  if (!has_names) {
    V(g)$name <- as.character(seq_len(vcount(g)) - 1L)
    has_names <- TRUE
  }
  V(g)$name <- as.character(V(g)$name)
  vnames <- V(g)$name
  
  # Resolve root by name
  root <- as.character(root)
  v_root <- which(vnames == root)
  if (length(v_root) != 1) stop("Root not found or not unique")
  
  # Helper to get names from vertex ids
  to_name <- function(idx) vnames[as.integer(idx)]
  
  # Stage 0 label
  a_root <- paste0(.stage_code(0), root)
  
  # Map: tree label -> original node name
  label_orig <- setNames(root, a_root)
  
  # Layers: list of labels per stage
  layers <- vector("list", max_depth + 1L)
  names(layers) <- sapply(0:max_depth, .stage_code)
  layers[[1]] <- a_root
  
  edges_out <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)
  
  # Expand per stage, replicating all neighbors of the underlying node
  if (max_depth >= 1) {
    for (s in 1:max_depth) {
      parents <- layers[[s]]
      if (length(parents) == 0) { layers[[s + 1]] <- character(0); next }
      
      stage_tag <- .stage_code(s)
      child_labels <- character(0)
      
      for (L in parents) {
        u_name <- label_orig[[L]]
        u_idx  <- which(vnames == u_name)
        
        ns <- neighbors(g, v = u_idx, mode = mode)
        if (length(ns) == 0) next
        
        tgt_names <- unique(to_name(ns))
        labs <- paste0(stage_tag, tgt_names, L)  # e.g., c2b1a0
        
        edges_out <- rbind(edges_out,
                           data.frame(Source = L, Target = labs, stringsAsFactors = FALSE))
        child_labels <- c(child_labels, labs)
        label_orig <- c(label_orig, setNames(tgt_names, labs))
      }
      
      layers[[s + 1]] <- unique(child_labels)
    }
  }
  
  # Build tree graph
  verts <- unique(unlist(layers, use.names = FALSE))
  g_tree <- graph_from_data_frame(edges_out, directed = TRUE,
                                  vertices = data.frame(name = verts, stringsAsFactors = FALSE))
  
  list(
    graph  = g_tree,
    edges  = edges_out,
    layers = layers,
    label_to_original = label_orig
  )
}





# g is your igraph
# root is the original vertex name, for example "0" or "8703"
tree <- build_unfolded_tree(gplus, root = "8703", max_depth = 2, mode = "out")



# Plot. Root label in the tree is paste0("a", root)
gT <- tree$graph
root_label <- paste0("a", "0")
plot(
  gT,
  layout = layout_as_tree(gT, root = which(V(gT)$name == root_label)),
  vertex.size = 22,
  vertex.label.cex = 0.8,
  edge.arrow.size = 0.45
)



