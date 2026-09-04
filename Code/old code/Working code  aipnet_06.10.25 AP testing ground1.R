

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


plot_hs_igraph <- function(df, n = NULL,edge_cols = c("hs2022_code_upstream", "hs2022_code_downstream")) {
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

#Identify nodes that are in distance 2 from v
two_hop_walk <- function(g, v, mode = c("all","out","in")) {
  mode <- match.arg(mode)
  v_id <- if (is.character(v)) V(g)[v] else V(g)[v]
  n1 <- neighbors(g, v_id, mode = mode)
  n2 <- unique(unlist(lapply(n1, function(u) neighbors(g, u, mode = mode))))
  n2 <- setdiff(n2, v_id)  # drop the source itself
  # Return names if present, else numeric ids
  if (!is.null(V(g)$name)) V(g)[n2]$name else as.integer(n2)
}
# remove direct edges from v to nodes that also appear in its two-hop list
remove_twohop_neighbors <- function(g, v, mode = "out") {
  # map name to index if needed
  v_idx <- if (is.character(v)) which(V(g)$name == v) else as.integer(v)
  if (length(v_idx) != 1L || is.na(v_idx)) return(g)  # skip if v not found
  
  # 1-hop and 2-hop as indices
  n1 <- as.integer(neighbors(g, v_idx, mode = mode))
  n2 <- as.integer(V(g)[ two_hop_walk(g, v, mode = mode) ])
  
  redundant <- intersect(n1, n2)
  if (length(redundant) == 0) return(g)
  
  # delete only v -> redundant
  eids <- get.edge.ids(g, c(rbind(v_idx, redundant)), directed = TRUE)
  eids <- eids[eids != 0]
  if (length(eids) == 0) return(g)
  delete_edges(g, eids)
}

product4d <- "8703"
#product4d <- "8470"

graphh <- read.csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/edge_list_hs2022_4digit.csv") # Arnaud
hsnames <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS22")
BEC_database <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BEC database.xlsx")

Capital_good<-BEC_database$HS6[BEC_database$BEC5EndUse=="CAP"]
# -----------------------------
# 1) Data cleaning helpers
# -----------------------------
clean_edges <- function(graphh) {
  # ensure character
  graphh <- graphh %>%
    mutate(
      hs2022_code_upstream   = as.character(hs2022_code_upstream),
      hs2022_code_downstream = as.character(hs2022_code_downstream)
    )
  # pad 3-digit codes with a leading 0
  pad3 <- function(x) ifelse(nchar(x) == 3, paste0("0", x), x)
  graphh %>%
    mutate(
      hs2022_code_upstream   = pad3(hs2022_code_upstream),
      hs2022_code_downstream = pad3(hs2022_code_downstream)
    )
}


#Take out exclude_capital_goods machinery
graphh_clean <- clean_edges(graphh)

graphh_clean<-graphh_clean[!graphh_clean$hs2022_code_upstream %in% unique(substr(Capital_good,1,4)) ,]
graphh_clean<-graphh_clean[!graphh_clean$hs2022_code_downstream %in%unique(substr(Capital_good,1,4)) ,]


#edge_df<-graphh_clean

# -----------------------------

# 2) Build hidden graph
# -----------------------------
build_hidden_graph <- function(ed, product) {
  # deduplicate edges
  ed <- ed[ed$hs2022_code_downstream == product, ]
}
Hidden_network<-build_hidden_graph(graphh,product4d)#list of upstream product
potential_p1<-Hidden_network$hs2022_code_upstream

# -----------------------------
# 2) Identify at the second stage which of the input of P0 are input in the other product
# -----------------------------

tpp<-graphh_clean[graphh_clean$hs2022_code_downstream%in%potential_p1,] #Total input list of the product 0 inputs
t1<-tpp[tpp$hs2022_code_upstream%in%potential_p1,] #Total input list of the product 0 inputs

rm(tpp)

distr <- as.matrix(table(t1$hs2022_code_downstream))

library(ggplot2)

x <- as.vector(distr)
df <- data.frame(x)


prod_list <- as.data.frame(distr[,1])


Highconnectedprod <- prod_list %>%
  arrange(desc(distr[, 1])) %>%
  slice(1:ceiling(0.05 * n()))

#On this input list select only the product that are input of p0, that also input of another
# t1<-t1[t1$hs2022_code_upstream%in%potential_p1,] 

# Identifier les noeuds qui ne sont pas attache aux autre et les rattacher a p0
# Hidden_network_stage1<-Hidden_network[!t1$hs2022_code_downstream%in%Hidden_network$hs2022_code_upstream,]
# Identifying the most down stream nodes and reattaching them to p0. If the node doesn't send inputs to any othere product attach it to p0
t2<-cbind.data.frame(hs2022_code_upstream =row.names(Highconnectedprod),hs2022_code_downstream =product4d)
Hidden_network_stage12<-rbind.data.frame(t1,t2)

#Turn hidden dataframe to network and reverse it to have tree shape
g_rev<-igraph::reverse_edges(graph_from_data_frame(Hidden_network_stage12))

############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
#Create the tree with multiplication of branches


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

############################################################################################################################################
############################################################################################################################################
#Deleting 1path if 2pth exist for step 1 and 2


Step1_productlist<-neighbors(g_rev, v = product4d, mode = "out")$name

g_iter <- g_rev
for (v in Step1_productlist) {
  # sequential, only on step 2 nodes
  g_iter <- remove_twohop_neighbors(g_iter, v, mode = "out")
}


Step2_productlist<-c()
for (it in neighbors(g_iter, v = product4d, mode = "out")$name) {
  Step2_productlist<-c(Step2_productlist,neighbors(g_iter, v = it, mode = "out")$name)
}




Step2_productlist<-unique(Step2_productlist)

g_itri <- g_iter
for (v in Step2_productlist) {
  # sequential, only on step 2 nodes
  g_itri <- remove_twohop_neighbors(g_itri, v, mode = "out")
}









############################################################################################################################################
############################################################################################################################################
#Model the tree




# g is your igraph
# root is the original vertex name, for example "0" or "8703"
tree <- build_unfolded_tree(g_itri, root = "8703", max_depth = 4, mode = "out")



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


library(visNetwork)

# Assume: tree is the object returned by build_unfolded_tree()
gT <- tree$graph
root <- "8703"  # or product4d if that’s your root variable
root_label <- paste0("a", root)

# Convert igraph object to visNetwork format
nodes <- data.frame(
  id = V(gT)$name,
  label = V(gT)$name,
  color = ifelse(V(gT)$name == root_label, "#ff6666", "#b3cde3"),
  shape = "dot",
  size = ifelse(V(gT)$name == root_label, 25, 15)
)

edges <- data.frame(
  from = as.character(ends(gT, es = E(gT))[, 1]),
  to   = as.character(ends(gT, es = E(gT))[, 2]),
  arrows = "to"
)

# Interactive network
visNetwork(nodes, edges, height = "800px", width = "100%") %>%
  visHierarchicalLayout(direction = "UD", sortMethod = "directed") %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
  visPhysics(stabilization = TRUE)




























