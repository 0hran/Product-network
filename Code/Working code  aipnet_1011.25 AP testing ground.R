

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

Flow_size_threshold<-5 #Threshold for the weight of the flow
Input_per_nodes<-10# - k: number of inputs to keep per node, at each level
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




# if you do not have a graph yet
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




gplus <- build_k_input_subgraph(
  g,
  root = product4d,
  k = Input_per_nodes,
  depth = Max_stage,
  rank_by = "outdegree",       # or "outdegree"
  group_parallel = TRUE   # one input per source per node
)



##############################

root <- product4d
list_of_name <- V(gplus)$name

# reverse edges so paths flow outward from the root
g_rev <- igraph::reverse_edges(gplus)

# UNWEIGHTED distances (hops)
d <- distances(g_rev, v = root, to = list_of_name, mode = "out", weights = NA)
dist_vec <- as.numeric(d[1, ])
names(dist_vec) <- list_of_name

# attach
V(gplus)$dist_to_root <- dist_vec





#####VIsualisation of the network



stopifnot(vcount(gplus) > 0)

# Build nodes/edges from gplus
nodes <- data.frame(
  id    = V(gplus)$name,
  label = V(gplus)$name,
  stringsAsFactors = FALSE
)

if (ecount(gplus) > 0) {
  e_ends <- ends(gplus, E(gplus))
  edges <- data.frame(
    from  = as.character(e_ends[, 1]),
    to    = as.character(e_ends[, 2]),
    value = if (is.null(E(gplus)$weight)) 1 else E(gplus)$weight,
    stringsAsFactors = FALSE
  )
} else {
  edges <- data.frame(from = character(0), to = character(0), value = numeric(0))
}



# HS names
nodes <- nodes %>%
  left_join(hsnames, by = c("id" = "Code")) %>%
  mutate(
    # Two line label: first line code, second line description
    label = if_else(
      is.na(Description),
      id,
      paste0(id, "\n", Description)
    ),
    # Rich tooltip for hover
    title = if_else(
      is.na(Description),
      paste0("<b>", id, "</b><br>No HS name found"),
      paste0("<b>", id, "</b><br>", Description)
    )
  )

# Make nodes far apart: use repulsion physics and large distances
visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  #visIgraphLayout(layout = "layout_with_fr") %>%   # pure tree layout
  visEdges(arrows = "to", smooth = FALSE) %>%
  visNodes(shape = "dot") %>%
  visPhysics(
    enabled = TRUE,
    solver = "repulsion",
    repulsion = list(
      nodeDistance   = 400,   # push nodes far apart
      centralGravity = 0.01,
      springLength   = 300,
      springConstant = 0.01,
      damping        = 0.09,
      avoidOverlap   = 1
    ),
    stabilization = list(enabled = TRUE, iterations = 150)
  ) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


V(gplus)$dist_to_root

# Improved visualisation


stopifnot(vcount(gplus) > 0)

# Build nodes and edges from gplus
nodes <- data.frame(
  id    = V(gplus)$name,
  label = V(gplus)$name,
  stringsAsFactors = FALSE
)

if (ecount(gplus) > 0) {
  e_ends <- ends(gplus, E(gplus))
  edges <- data.frame(
    from  = as.character(e_ends[, 1]),
    to    = as.character(e_ends[, 2]),
    value = if (is.null(E(gplus)$weight)) 1 else E(gplus)$weight,
    stringsAsFactors = FALSE
  )
} else {
  edges <- data.frame(from = character(0), to = character(0), value = numeric(0))
}

# Ensure distances exist. If not, compute unweighted hops from root = product4d
if (is.null(V(gplus)$dist_to_root)) {
  g_rev <- igraph::reverse_edges(gplus)
  d <- distances(
    g_rev,
    v = V(g_rev)[name == product4d],
    to = V(g_rev),
    mode = "out",
    weights = NA
  )
  dist_vec <- as.numeric(d[1, ])
  dist_vec[is.infinite(dist_vec)] <- NA_real_
  names(dist_vec) <- V(gplus)$name
  V(gplus)$dist_to_root <- dist_vec
}

# Join HS names
nodes <- nodes %>%
  mutate(id = str_trim(as.character(id))) %>%
  left_join(
    hsnames %>%
      transmute(Code = str_trim(as.character(Code)),
                Description = str_trim(as.character(Description))) %>%
      distinct(Code, .keep_all = TRUE),
    by = c("id" = "Code")
  ) %>%
  mutate(
    label = if_else(
      is.na(Description),
      id,
      paste0(id, "\n", Description)
    ),
    title = if_else(
      is.na(Description),
      paste0("<b>", id, "</b><br>No HS name found"),
      paste0("<b>", id, "</b><br>", htmlEscape(Description))
    )
  )

# Bring distance into the nodes table
nodes$dist_to_root <- V(gplus)$dist_to_root[match(nodes$id, V(gplus)$name)]

# Build a colour palette by discrete distance
uniq_d <- sort(unique(na.omit(nodes$dist_to_root)))
pal <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))(max(3, length(uniq_d)))
col_map <- setNames(pal, uniq_d)

nodes$color <- unname(col_map[as.character(nodes$dist_to_root)])
nodes$color[is.na(nodes$color)] <- "#cccccc"  # fallback for NA distances

# Optional legend entries
legend_nodes <- if (length(uniq_d)) {
  data.frame(
    label = paste0("Distance ", uniq_d),
    shape = "dot",
    color = unname(col_map[as.character(uniq_d)]),
    stringsAsFactors = FALSE
  )
} else {
  NULL
}

# Visualisation
vis <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visEdges(arrows = "to", smooth = FALSE) %>%
  visNodes(shape = "dot") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

if (!is.null(legend_nodes)) {
  vis <- vis %>% visLegend(addNodes = legend_nodes, useGroups = FALSE)
}

vis %>%
  visPhysics(
    enabled = TRUE,
    solver = "repulsion",
    repulsion = list(
      nodeDistance   = 400,
      centralGravity = 0.01,
      springLength   = 300,
      springConstant = 0.01,
      damping        = 0.09,
      avoidOverlap   = 1
    ),
    stabilization = list(enabled = TRUE, iterations = 150)
  )

# Inspect distances if needed
V(gplus)$dist_to_root

# Export ####

#saveRDS(nodes, "C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Output/car_nodes.rds")


###########################################
###########################################

#Plot the tree

# Packages
library(igraph)
library(visNetwork)

# ============== OPTIONAL: filter by weight > 5 ==============
# If you already filtered, skip this block.
if (is.null(E(gplus)$weight)) E(gplus)$weight <- 1
gplus_f <- subgraph.edges(gplus, eids = E(gplus)[weight > 5], delete.vertices = TRUE)

# ============== Helper for stage tags ==============
.stage_code <- function(s) paste0("L", s, "|")

# ============== Unfolded tree builder ==============
# Duplicates nodes along different paths to produce a true tree
# g        directed igraph
# root     vertex name or id
# max_depth number of layers to expand
# mode     "out" to follow outgoing edges, "in" to follow incoming edges, "all" for undirected expansion
build_unfolded_tree <- function(g, root, max_depth = 5, mode = c("out","in","all")) {
  stopifnot(inherits(g, "igraph"), max_depth >= 0)
  mode <- match.arg(mode)
  if (!is_directed(g) && mode != "all") stop("Graph must be directed for 'in' or 'out'.")
  if (is.null(E(g)$weight)) E(g)$weight <- 1
  
  # Ensure names
  if (!("name" %in% vertex_attr_names(g))) V(g)$name <- as.character(seq_len(vcount(g)))
  V(g)$name <- as.character(V(g)$name)
  vnames <- V(g)$name
  
  # Resolve root
  if (is.numeric(root)) {
    stop("Please provide 'root' as a vertex name for clarity.")
  }
  root <- as.character(root)
  v_root <- which(vnames == root)
  if (length(v_root) != 1) stop("Root not found or not unique in V(g)$name.")
  
  # Edge table without tibble conflicts
  el  <- igraph::as_edgelist(g, names = TRUE)
  w   <- if (is.null(E(g)$weight)) rep(1, nrow(el)) else E(g)$weight
  edf <- data.frame(from_name = el[,1], to_name = el[,2], weight = w, stringsAsFactors = FALSE)
  # Aggregate parallel edges by sum
  agg <- aggregate(weight ~ from_name + to_name, data = edf, FUN = sum)
  
  # Fast lookup of weight(u,v)
  key <- paste(agg$from_name, agg$to_name, sep = "\r")
  w_lookup <- function(u, v) {
    m <- match(paste(u, v, sep = "\r"), key)
    ifelse(is.na(m), 0, agg$weight[m])
  }
  
  # Mapping tree labels -> original node name
  root_label <- paste0(.stage_code(0), root)
  label_orig <- setNames(root, root_label)
  
  # Layers store labels per stage
  layers <- vector("list", max_depth + 1L)
  names(layers) <- sapply(0:max_depth, .stage_code)
  layers[[1]] <- root_label
  
  # Output edges of the unfolded tree
  edges_out <- data.frame(Source = character(),
                          Target = character(),
                          weight = numeric(),
                          stringsAsFactors = FALSE)
  
  # Expand layer by layer
  if (max_depth >= 1) {
    for (s in 1:max_depth) {
      parents <- layers[[s]]
      if (length(parents) == 0) { layers[[s + 1]] <- character(0); next }
      
      stage_tag <- .stage_code(s)
      child_labels <- character(0)
      
      for (L in parents) {
        u_name <- label_orig[[L]]
        u_idx  <- which(vnames == u_name)
        
        nbrs <- neighbors(g, v = u_idx, mode = mode)
        if (length(nbrs) == 0) next
        
        tgt_names <- unique(vnames[as.integer(nbrs)])
        # Unique labels per child: stage tag + original name + parent label
        labs <- paste0(stage_tag, tgt_names, "||", L)
        
        # Edge weights from u_name to each tgt_name
        wts <- vapply(tgt_names, function(vn) {
          if (mode == "out") {
            w_lookup(u_name, vn)
          } else if (mode == "in") {
            w_lookup(vn, u_name)
          } else {
            # undirected style, pick max of both directions if present
            max(w_lookup(u_name, vn), w_lookup(vn, u_name))
          }
        }, numeric(1))
        
        edges_out <- rbind(edges_out,
                           data.frame(Source = L, Target = labs, weight = wts, stringsAsFactors = FALSE))
        
        child_labels <- c(child_labels, labs)
        label_orig   <- c(label_orig, setNames(tgt_names, labs))
      }
      
      layers[[s + 1]] <- unique(child_labels)
    }
  }
  
  # Build tree graph
  verts <- unique(unlist(layers, use.names = FALSE))
  g_tree <- graph_from_data_frame(
    d = edges_out,
    directed = TRUE,
    vertices = data.frame(name = verts, stringsAsFactors = FALSE)
  )
  E(g_tree)$weight <- edges_out$weight
  
  list(
    graph  = g_tree,
    edges  = edges_out,
    layers = layers,
    label_to_original = label_orig
  )
}

# ============== Choose direction automatically ==============
root <- "8703"
deg_out <- if (root %in% V(gplus_f)$name) degree(gplus_f, v = V(gplus_f)[name == root], mode = "out") else 0
deg_in  <- if (root %in% V(gplus_f)$name) degree(gplus_f, v = V(gplus_f)[name == root], mode = "in") else 0
mode_dir <- if (deg_out > 0 || deg_in == 0) "out" else "in"  # if root has outputs, expand out, otherwise expand in

# ============== Build unfolded tree ==============
unf <- build_unfolded_tree(gplus_f, root = root, max_depth = 5, mode = mode_dir)
g_tree <- unf$graph

# g_tree


# ============== Plot with visNetwork ==============
nodes <- data.frame(
  id    = V(g_tree)$name,
  label = unf$label_to_original[V(g_tree)$name],
  stringsAsFactors = FALSE
)

if (ecount(g_tree) > 0) {
  e_ends <- ends(g_tree, E(g_tree))
  edges <- data.frame(
    from   = e_ends[, 1],
    to     = e_ends[, 2],
    value  = E(g_tree)$weight,
    title  = paste("Weight:", E(g_tree)$weight),
    stringsAsFactors = FALSE
  )
} else {
  edges <- data.frame(from = character(0), to = character(0), value = numeric(0))
}

# Highlight root node label
root_label <- names(unf$label_to_original)[unf$label_to_original == root]
nodes$color.background <- ifelse(nodes$id == root_label, "#cfe8ff", "#e6e6e6")
nodes$font.size <- ifelse(nodes$id == root_label, 20, 14)

visNetwork(nodes, edges, width = "100%", height = "850px") %>%
  visEdges(smooth = FALSE, arrows = "to", scaling = list(min = 2, max = 8)) %>%
  visNodes(shape = "dot", scaling = list(min = 10, max = 30)) %>%
  visHierarchicalLayout(
    direction = "UD",
    sortMethod = "directed",
    levelSeparation = 170,
    nodeSpacing = 220,
    treeSpacing = 280
  ) %>%
  visPhysics(
    solver = "hierarchicalRepulsion",
    hierarchicalRepulsion = list(
      nodeDistance = 320,
      springLength = 260,
      damping = 0.09,
      avoidOverlap = 1
    ),
    stabilization = list(enabled = TRUE, iterations = 150)
  ) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
