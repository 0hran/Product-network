

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
  library(visNetwork)
  library(htmltools)
  
})

rm = list = ls()

Number_data<-list()
Numb_data<-list()


ttt<-c(1:30)
sss<-c(1:100)



for (Flow_size_threshold in ttt) {
  
  
Number_data[[Flow_size_threshold]]<-list()
Numb_data[[Flow_size_threshold]]<-list()


#Input_per_nodes<-10# - k: number of inputs to keep per node, at each level
#Flow_size_threshold<-5 #Threshold for the weight of the flow
Max_stage <-10# - depth: number of upstream stages (root is level 0)
product4d <- "8703" # car"8712" # bycicle




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






# Arnaud

graphh <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx") # Arnaud
hsnames <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS02")
BEC_database <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BEC database.xlsx") # TO update

# Adria

#  graphh <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx")
#  hsnames <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS02")
#  BEC_database <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Data/BEC database.xlsx") # TO update

Capital_good<-BEC_database$HS6[BEC_database$BEC5EndUse=="CAP"] #Adapter BEC


hsnames<-hsnames[hsnames$Level==4,]



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

######################################1 step

# -----------------------------
# 2) Build hidden graph
#    Direction: input (upstream) -> product (downstream)
#Select only for the input
# -----------------------------
build_hidden_graph <- function(ed, product) {
  # deduplicate edges
  ed <- ed[ed$hs2002_code_downstream == product, ]
}
Hidden_network<-build_hidden_graph(graphh_clean,product4d)#list of upstream product

potential_p1<-Hidden_network$hs2002_code_upstream



Hidden_net<-graphh_clean[graphh_clean$hs2002_code_upstream%in%potential_p1,]

Hidden_net<-Hidden_net[Hidden_net$hs2002_code_downstream%in%potential_p1,]

length(unique(Hidden_net$hs2002_code_upstream))
length(unique(Hidden_net$hs2002_code_downstream))



# Install and load required packages
library(visNetwork)
library(htmlwidgets)


edges<-Hidden_net

colnames(edges)<-c("from","to")


# Create unique nodes from the edges
nodes <- data.frame(
  id = unique(c(as.character(edges$from), as.character(edges$to))),
  label = unique(c(as.character(edges$from), as.character(edges$to)))
)

visNetwork(nodes, edges) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = TRUE
  ) %>%
  visPhysics(
    enabled = TRUE,
    repulsion = list(
      nodeDistance = 800,  # Increase this value to push nodes further apart
      centralGravity = 0.1,
      springLength = 100,
      springConstant = 0.05,
      damping = 0.09
    )
  )




#2 digit


# --- 1. Création du Réseau igraph ---
# Le réseau "toy" (non-dirigé)
set.seed(42)
g<-graph_from_edgelist(as.matrix(edges),directed = TRUE)


E(g)$weight <- 1
A <- as_adjacency_matrix(g, sparse = FALSE)
A2 <- A %*% A
edges_df <- igraph::as_data_frame(g, what = "edges") 
updated_edges_list <- list()


# Calcul des nouveaux poids (boucle corrigée)
for (i in 1:nrow(edges_df)) {
  node1_name <- edges_df[i, "from"]
  node2_name <- edges_df[i, "to"]
  path_count_2_degree <- A2[node1_name, node2_name] 
  new_weight <- edges_df[i, "weight"] + path_count_2_degree
  updated_edges_list[[i]] <- data.frame(
    from = node1_name,
    to = node2_name,
    weight = new_weight
  )
}

weighted_edges_df <- bind_rows(updated_edges_list)
# Création du graphe igraph pondéré final
g_weighted <- graph_from_data_frame(weighted_edges_df, directed = TRUE, vertices = V(g)$name)

# --- 2. Création de la Matrice d'Adjacence Pondérée ---

# Utilisation de as_adjacency_matrix avec l'argument 'attr'
weighted_adj_matrix <- as_adjacency_matrix(
  graph = g_weighted, 
  type = "both",        # Inclut tous les nœuds
  names = TRUE,         # Utilise les noms des nœuds pour les lignes/colonnes
  sparse = FALSE,       # Pour obtenir une matrice dense (standard R matrix)
  attr = "weight"       # L'attribut à utiliser comme valeur dans la matrice
)

# Affichage de la matrice
print("Matrice d'Adjacence Pondérée (Basée sur les Chemins de Longueur 2) :")
print(weighted_adj_matrix)

indegree_weighted <- strength(
  graph = g_weighted,
  mode = "in",        # "in" = incoming edges
  weights = E(g_weighted)$weight
)

V(g_weighted)$indegree_weighted <- indegree_weighted

#
Name_desc<-V(g_weighted)$name


Name_desc<-as.data.frame(as.matrix(c(Name_desc,product4d)))

colnames(Name_desc)<-"desc"

hsnames$IsBasicLevel<-NULL
hsnames$Level<-NULL
hsnames$`Parent Code`<-NULL
hsnames$Classification<-NULL

AI<-left_join(Name_desc, hsnames, by = c("desc" = "Code"))
colnames(AI)<-c("id","desc")

V(g_weighted)$Desc<-AI$desc[1:c(length(AI$id)-1)]


# V(g_weighted)$Desc names of the product




#write.csv(
#  AI,
#  file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/edge_name.csv",
#  row.names = FALSE # Ensure the node names (A, B, C, D, E) are included as the first column
#)


# 1. Convert the matrix to a CSV file
#write.csv(
#  weighted_adj_matrix,
#  file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/weighted_adjacency_matrix.csv",
#  row.names = TRUE # Ensure the node names (A, B, C, D, E) are included as the first column
#)



g_weighted <- delete_edges(g_weighted, E(g_weighted)[weight <= Flow_size_threshold])


Decomp_g_weighted<-decompose.graph(g_weighted)



#Select the 90% highest indegree


from<-c()

for( it in c(1:length(Decomp_g_weighted)) ){
  #  
  from1<-  V(Decomp_g_weighted[[it]])$name[V(Decomp_g_weighted[[it]])$indegree_weighted>quantile( V(Decomp_g_weighted[[it]])$indegree_weighted,  probs = c(0.9))]
  from<-c(from,from1)
}





# 5 key products

# dataframe of links
links <- data.frame(
  from,
  to=product4d,
  weight = Flow_size_threshold+1,
  stringsAsFactors = FALSE
)



g <- g_weighted

# ensure vertex names are character
V(g)$name <- as.character(V(g)$name)
links$from <- as.character(links$from)
links$to   <- as.character(links$to)

# add any missing vertices
missing <- setdiff(unique(c(links$from, links$to)), V(g)$name)
if (length(missing) > 0) {
  g <- add_vertices(g, nv = length(missing), name = missing)
}

# add edges
edge_vec <- as.vector(t(links[, c("from","to")]))  # interleave from,to
g <- add_edges(g, edge_vec)

# assign weights to the newly added edges
new_eids <- seq(ecount(g) - nrow(links) + 1, ecount(g))
E(g)$weight[new_eids] <- links$weight


indegree_weighted <- strength(
  graph = g,
  mode = "in",        # "in" = incoming edges
  weights = E(g)$weight
)
V(g)$indegree_weighted <- indegree_weighted



outdegree_weighted <- strength(
  graph = g,
  mode = "out",        # "in" = incoming edges
  weights = E(g)$weight
)
V(g)$outdegree_weighted <- outdegree_weighted



# Build a k-input upstream subgraph recursively, level by level.
# - g: directed igraph
# - root: vertex name or id (e.g., "8712")
# - k: number of inputs to keep per node, at each level
# - depth: number of upstream stages (root is level 0)
# - rank_by: "edge" (incoming edge weight) or "outdegree" (source node out-strength)
# - group_parallel: if TRUE, for multiple edges from the same source to the same target,
#                   keep only the single strongest edge when selecting inputs
build_k_input_subgraph <- function(
    g, root, k = 5, depth = 5,
    rank_by = c("edge","outdegree"),
    group_parallel = TRUE
) {
  rank_by <- match.arg(rank_by)
  if (!is_directed(g)) stop("Graph must be directed.")
  if (is.null(E(g)$weight)) E(g)$weight <- 1
  
  # map root -> vertex id
  root_vid <- if (is.character(root)) {
    if (!(root %in% V(g)$name)) stop("Root not found in V(g)$name.")
    V(g)[name == root]
  } else if (is.numeric(root)) {
    if (!(root %in% V(g))) stop("Root id not found.")
    V(g)[root]
  } else stop("root must be a name or numeric id.")
  
  # precompute source OUT-strength if needed
  if (rank_by == "outdegree") {
    out_strength <- strength(g, mode = "out", weights = E(g)$weight)
  }
  
  kept_eids <- integer(0)
  frontier  <- as.integer(root_vid)  # nodes whose inputs we will select next
  visited_nodes <- as.integer(root_vid)  # track to avoid infinite loops in cyclic graphs
  
  for (lvl in seq_len(depth)) {
    if (length(frontier) == 0) break
    
    next_frontier <- integer(0)
    
    for (v in frontier) {
      inc_e <- incident(g, v, mode = "in")
      if (length(inc_e) == 0) next
      
      em <- ends(g, inc_e, names = FALSE)
      src <- em[, 1]
      
      # Optionally collapse parallels per SOURCE for this selection step: keep only the strongest edge per source->v
      if (group_parallel && any(duplicated(src))) {
        w <- E(g)$weight[inc_e]
        # pick the index of the maximum-weight edge for each source
        pick_idx <- tapply(seq_along(src), src, function(ix) ix[which.max(w[ix])])
        pick_idx <- as.integer(unlist(pick_idx))
        inc_e <- inc_e[pick_idx]
        src   <- src[pick_idx]
      }
      
      # Ranking score
      scores <- if (rank_by == "edge") {
        E(g)$weight[inc_e]  # edge weights into v
      } else {
        out_strength[src]   # source node out-strength
      }
      
      ord <- order(scores, decreasing = TRUE, na.last = TRUE)
      keep_n <- min(k, length(ord))
      sel_idx <- ord[seq_len(keep_n)]
      sel_e   <- inc_e[sel_idx]
      sel_src <- src[sel_idx]
      
      kept_eids    <- c(kept_eids, sel_e)
      next_frontier <- c(next_frontier, sel_src)
    }
    
    # prepare next layer; allow revisiting if a node reappears later is unnecessary, so deduplicate
    next_frontier <- setdiff(unique(next_frontier), frontier)
    # avoid infinite cycling: do not expand nodes we've already expanded before
    next_frontier <- setdiff(next_frontier, visited_nodes)
    visited_nodes <- unique(c(visited_nodes, next_frontier))
    frontier <- next_frontier
  }
  
  kept_eids <- unique(kept_eids)
  subgraph.edges(g, eids = kept_eids, delete.vertices = TRUE)
}


for (Input_per_nodes in sss) {
Number_data[[Flow_size_threshold]][[Input_per_nodes]]<-list()
Numb_data[[Flow_size_threshold]][[Input_per_nodes]]<-list()


gplus <- build_k_input_subgraph(
  g,
  root = product4d,
  k = Input_per_nodes,
  depth = Max_stage,
  rank_by = "outdegree",       # or "outdegree"
  group_parallel = TRUE   # one input per source per node
)



Number_data[[Flow_size_threshold]][[Input_per_nodes]]<-length(unique(V(gplus)$name))
Numb_data[[Flow_size_threshold]][[Input_per_nodes]]<-gplus



}}




#gplus<-Number_data[[Flow_size_threshold]][[Input_per_nodes]]
#gplus<-Number_data[[1]][[15]]




# Suppose:
## 1. Identify which Flow_size_threshold indices exist
fst_idx <- which(!vapply(Number_data, is.null, logical(1)))
fst_idx
# e.g. might contain 18, 19, ...

## 2. For inner list, use a representative outer element
tmp_inner <- Number_data[[fst_idx[1]]]
ipn_idx <- which(!vapply(tmp_inner, is.null, logical(1)))
ipn_idx
# e.g. might contain 23:30

## 3. Initialize matrix
M <- matrix(
  NA_real_,
  nrow = length(ipn_idx),
  ncol = length(fst_idx),
  dimnames = list(
    Input_per_nodes = ipn_idx,
    Flow_size_threshold = fst_idx
  )
)

## 4. Fill matrix
for (c in seq_along(fst_idx)) {
  i <- fst_idx[c]        # outer index, Flow_size_threshold
  for (r in seq_along(ipn_idx)) {
    j <- ipn_idx[r]      # inner index, Input_per_nodes
    M[r, c] <- Number_data[[i]][[j]][[1]]
  }
}



saveRDS(Numb_data, "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/gplus.rds")
saveRDS(M, "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/gplus_brother_matrix.rds")

gplus[[4]][[5]]
gplus_brother_matrix
