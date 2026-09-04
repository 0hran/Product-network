

# Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(igraph)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readxl)
  library(purrr)
  
  
  
})

# Load data


#graphh <- read.csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/edge_list_hs2022_4digit.csv") # Arnaud
graphh <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx")
#hsnames <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS22")
hsnames <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS22")
#EC_database <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BEC database.xlsx")
BEC_database <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Data/BEC database.xlsx")

#hsnames <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS22")

# Functions

#  REV: is this function used at all?
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


# Product and level

product4d <- "8703"
#product4d <- "8470"
Lvl<-3



# -----------------------------
# 1) Data cleaning helpers
# -----------------------------

Capital_good<-BEC_database$HS6[BEC_database$BEC5EndUse=="CAP"]

# Function to clean - adding 0 to turn from three-digit to four digit
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
#    Direction: input (upstream) -> product (downstream)
#Select only for the input
# -----------------------------

build_hidden_graph <- function(ed, product) {
  # deduplicate edges
  ed <- ed[ed$hs2022_code_downstream == product, ]
}

# REV note we are not using graphh_clean?
# Hidden graphs takes all downstream inputs going to the Root
Hidden_network<-build_hidden_graph(graphh_clean,product4d)

# Potential_p1 takes all inputs from hidden - meaning that products which are not inputs are removed
potential_p1<-Hidden_network$hs2022_code_upstream

# -----------------------------
# 2) Identify at the first stage which of the input of P0 which have the fewest outputs
# -----------------------------

## tpp takes from the full network all inputs to the root's inputs (e.g. selects the rows where the output is the root's input, which means that the resulting df will have as downstream the root's inputs, and as upstream all their inputs)
tpp<-graphh_clean[graphh_clean$hs2022_code_downstream%in%potential_p1,]

# Then we take tpp and keep their inputs only if their are also inptus to Root
t1<-tpp[tpp$hs2022_code_upstream%in%potential_p1,]

# Removes temporary df
rm(tpp)

# Then we look at the distribution of inputs
distr <- as.matrix(table(t1$hs2022_code_downstream))
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

# Set a 5% threshold - select the inputs to the root as those which are mroe often an input
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

# Hidden_network_stage1<-rbind.data.frame(Hidden_network_stage1, t2)

Hidden_network_stage12<-rbind.data.frame(t1,t2)

##############################################################################################################


Hidden_network_stage12<-graph_from_data_frame(Hidden_network_stage12)

V(Hidden_network_stage12)$code<-V(Hidden_network_stage12)$name

# Build hierarchical "display_network" from upstream links only.
# - g_hidden: igraph with edges input -> product (vertex names are HS codes)
# - root: HS code string, e.g. "8703"
# - max_levels: positive integer or Inf

build_hierarchical_graph <- function(g_hidden, root, max) {
  # max is the maximum stage depth to expand (p1..pmax)
  max_stage <- max
  
  if (!inherits(g_hidden, "igraph")) stop("g_hidden must be an igraph.")
  if (is.null(igraph::V(g_hidden)$code)) igraph::V(g_hidden)$code <- igraph::V(g_hidden)$name
  if (!(root %in% igraph::V(g_hidden)$code)) stop("Root code not found in the hidden graph.")
  
  # Reverse edges so we traverse product -> inputs
  g_rev <- igraph::reverse_edges(g_hidden)
  igraph::V(g_rev)$code <- igraph::V(g_rev)$name
  
  # Shortest path distance from root to all vertices in g_rev
  dist_vec <- igraph::distances(g_rev, v = root, to = igraph::V(g_rev), mode = "out")
  dist_vec <- as.numeric(dist_vec[1, ])
  names(dist_vec) <- igraph::V(g_rev)$code
  
  # Keep only vertices within max_stage from root
  keep_codes <- names(dist_vec)[is.finite(dist_vec) & dist_vec <= max_stage]
  sub_rev <- igraph::induced_subgraph(g_rev, vids = keep_codes)
  sub_dist <- dist_vec[igraph::V(sub_rev)$code]
  
  # Keep only edges that advance exactly one stage
  ed_sub <- igraph::as_data_frame(sub_rev, what = "edges") |>
    tibble::as_tibble() |>
    dplyr::rename(parent_code = from, child_code = to) |>
    dplyr::mutate(
      parent_stage = sub_dist[parent_code],
      child_stage  = sub_dist[child_code]
    ) |>
    dplyr::filter(
      is.finite(parent_stage), is.finite(child_stage),
      child_stage == parent_stage + 1,
      child_stage >= 1, child_stage <= max_stage
    )
  
  # Root instance, concise naming with branch lineage
  nodes_inst <- tibble::tibble(
    instance_id     = paste0(root, "_P0_B1"),
    code            = root,
    stage           = 0,
    parent_instance = NA_character_,
    branch_id       = "1",
    path_codes      = root
  )
  edges_inst <- tibble::tibble(from = character(0), to = character(0))
  
  # Expand stage by stage, duplicate by branch, concise IDs
  for (s in 1:max_stage) {
    parent_tbl <- dplyr::filter(nodes_inst, stage == s - 1)
    if (nrow(parent_tbl) == 0) break
    
    new_children <- parent_tbl |>
      dplyr::rowwise() |>
      dplyr::mutate(children = list(dplyr::filter(ed_sub, parent_code == code, child_stage == s))) |>
      dplyr::ungroup()
    
    child_rows <- purrr::map_dfr(seq_len(nrow(new_children)), function(i) {
      parent_inst   <- new_children$instance_id[i]
      parent_branch <- new_children$branch_id[i]
      parent_path   <- new_children$path_codes[i]
      ch_df <- new_children$children[[i]]
      if (is.null(ch_df) || nrow(ch_df) == 0) return(tibble::tibble())
      
      idx <- seq_len(nrow(ch_df))
      child_branch <- paste0(parent_branch, ".", idx)
      
      tibble::tibble(
        instance_id     = paste0(ch_df$child_code, "_P", s, "_B", child_branch),
        code            = ch_df$child_code,
        stage           = s,
        parent_instance = parent_inst,
        branch_id       = child_branch,
        path_codes      = paste0(parent_path, ">", ch_df$child_code)
      )
    })
    
    if (nrow(child_rows) > 0) {
      nodes_inst <- dplyr::bind_rows(nodes_inst, child_rows) |>
        dplyr::distinct(instance_id, .keep_all = TRUE)
      edges_inst <- dplyr::bind_rows(
        edges_inst,
        dplyr::transmute(child_rows, from = parent_instance, to = instance_id)
      )
    }
  }
  
  # Build the hierarchical igraph with attributes
  g_hier <- igraph::graph_from_data_frame(d = edges_inst, directed = TRUE, vertices = nodes_inst$instance_id)
  igraph::V(g_hier)$code       <- nodes_inst$code[match(igraph::V(g_hier)$name, nodes_inst$instance_id)]
  igraph::V(g_hier)$stage      <- nodes_inst$stage[match(igraph::V(g_hier)$name, nodes_inst$instance_id)]
  igraph::V(g_hier)$branch_id  <- nodes_inst$branch_id[match(igraph::V(g_hier)$name, nodes_inst$instance_id)]
  igraph::V(g_hier)$path_codes <- nodes_inst$path_codes[match(igraph::V(g_hier)$name, nodes_inst$instance_id)]
  
  # Optional sanity check: any HS code assigned to multiple stages
  stage_conflicts <- nodes_inst |>
    dplyr::distinct(code, stage) |>
    dplyr::count(code, name = "n_stages") |>
    dplyr::filter(n_stages > 1)
  if (nrow(stage_conflicts) > 0) {
    warning("Some HS codes appear in multiple stages: ",
            paste(stage_conflicts$code, collapse = ", "))
  }
  
  list(graph = g_hier, nodes = nodes_inst, edges = edges_inst, edgelist_layered = ed_sub)
}


# ---------- Usage ----------
# Hidden_network_stage12: your upstream input graph (inputs -> product)
disp <- build_hierarchical_graph(Hidden_network_stage12, root = product4d, max=Lvl)
display_network <- disp$graph

V(display_network)$level<-as.numeric(substr(V(display_network)$name,start = 7,stop =7))

##############

list_of_target_node<-V(display_network)$name[V(display_network)$level==Lvl]



library(igraph)
library(dplyr)
library(tibble)

# Inputs:
# - display_network : igraph object
# - list_of_target_node : character vector of target node names
# - product4d : root product code (character)

g <- display_network
root_node <- paste(product4d, "P0_B1", sep = "_")

# log of all removed edges
removed_edges_log <- tibble(from = character(), to = character(), target_code = character())

for (Target_node in list_of_target_node) {
  
  # 1) Find shortest path from root to target
  sp <- shortest_paths(g, from = root_node, to = Target_node, mode = "out")$vpath[[1]]
  if (length(sp) < Lvl) next
  
  # 2) Nodes to filter: path without last two nodes
  Product_to_filter <- V(g)$name[sp][-c(length(sp)-1, length(sp))]
  
  # 3) Identify target code and its instances
  target_code <- sub("^([0-9]{4}).*$", "\\1", Target_node)
  target_vertices <- V(g)[grepl(paste0("^", target_code, "_"), V(g)$name)]
  if (length(target_vertices) == 0) next
  
  # 4) For each product to filter, remove direct edges to target instances
  for (s in Product_to_filter) {
    for (t in V(g)$name[target_vertices]) {
      eid <- igraph::get.edge.ids(g, vp = c(s, t), directed = TRUE, error = FALSE)
      if (eid != 0) {
        g <- igraph::delete_edges(g, eid)
        removed_edges_log <- bind_rows(
          removed_edges_log,
          tibble(from = s, to = t, target_code = target_code)
        )
      }
    }
  }
}

# Final network after trimming
final_network <- g



#Filtering part you build the graph from the root to the leaves


library(igraph)

# Inputs:
# g               # your graph (e.g., display_network or final_network)
# root_code       # e.g., "8703"
root_name <- paste(product4d, "P0_B1", sep = "_")

keep_root_component <- function(g, root_name) {
  if (!(root_name %in% V(g)$name)) {
    stop(sprintf("Root '%s' not found in graph.", root_name))
  }
  # Weakly connected components (appropriate for directed production graphs)
  comp <- components(g, mode = "weak")
  root_vid <- which(V(g)$name == root_name)[1]
  root_comp_id <- comp$membership[root_vid]
  
  # Vertices to keep: those in the same component as the root
  keep_idx <- which(comp$membership == root_comp_id)
  g_kept <- induced_subgraph(g, vids = keep_idx)
  
  message(sprintf(
    "Kept %d nodes in root component, removed %d.",
    vcount(g_kept), vcount(g) - vcount(g_kept)
  ))
  g_kept
}

# Usage:
final_network <- keep_root_component(g, root_name)


library(igraph)


# Check root exists
if (!(root_name %in% V(final_network)$name)) {
  stop(sprintf("Root '%s' not found in graph.", root_name))
}

root_vid <- V(final_network)[name == root_name][1]

set.seed(1)
lay <- layout_as_tree(final_network, root = root_vid, flip.y = TRUE)

plot(
  final_network,
  layout = lay,
  vertex.size = 12,
  vertex.label.cex = 0.6,
  edge.arrow.size = 0.3,
  main = paste("Hierarchical network from root", root_name)
)

### dynamic netowrk visualisation

library(visNetwork)
library(htmltools)

g <- final_network

# Ensure vertices have names to use as ids and labels
if (is.null(V(g)$name)) V(g)$name <- as.character(seq_len(vcount(g)))

# Build nodes and edges data frames for visNetwork
edges <- igraph::as_data_frame(g, what = "edges")  # columns: from, to
nodes <- data.frame(
  id        = V(g)$name,
  label     = V(g)$name,
  stage     = V(g)$stage,
  level     = V(g)$level,
  branch_id = V(g)$branch_id,
  title     = sprintf(
    "<b>%s</b><br>Stage: %s<br>Level: %s<br>Branch: %s",
    V(g)$name, V(g)$stage, V(g)$level, V(g)$branch_id
  ),
  stringsAsFactors = FALSE
)

# Extract the first four digits from the node name (eg "8703" from "8703_P0_B1")
nodes <- nodes %>%
  mutate(
    hs4 = ifelse(str_detect(label, "^[0-9]{4}"),
                 str_sub(label, 1, 4),
                 NA_character_)
  )

# Build a 4-digit HS lookup from your hsnames tibble
# Level "4" rows should be the 4-digit HS headings
hs4_lookup <- hsnames %>%
  mutate(Code = str_replace_all(Code, "\\D", "")) %>%  # keep digits only, just in case
  filter(nchar(Code) == 4 | Level == "4") %>%
  transmute(hs4 = str_sub(Code, 1, 4),
            product = Description) %>%
  distinct(hs4, .keep_all = TRUE)

# Join product names onto nodes
nodes <- nodes %>%
  left_join(hs4_lookup, by = "hs4")

# Tooltip with product included
nodes$title <- sprintf(
  "<b>%s</b><br>Product: %s<br>Stage: %s<br>Level: %s<br>Branch: %s",
  nodes$label,
  ifelse(is.na(nodes$product), "Unknown", htmlEscape(nodes$product)),
  nodes$stage, nodes$level, nodes$branch_id
)

# Optional groups by stage for colouring
nodes$group <- as.character(nodes$stage)

# Visualisation
visNetwork(nodes, edges, height = "700px", width = "100%") %>%
  visEdges(arrows = "to", smooth = FALSE) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
  ) %>%
  visInteraction(
    dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, multiselect = TRUE
  ) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(gravitationalConstant = -50),
    stabilization = list(enabled = TRUE, iterations = 500)
  ) %>%
  visIgraphLayout(layout = "layout_with_fr")




substr(V(final_network)$name,1,4)[substr(V(final_network)$name,9,20)=="B1.2"]

unique(substr(V(final_network)$name,1,4))
