

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

Flow_size_threshold<-10 #Threshold for the weight of the flow
Input_per_nodes<-5# - k: number of inputs to keep per node, at each level
Max_stage <-5# - depth: number of upstream stages (root is level 0)
product4d <- "8703" # car
product4d <- "0803"

#product4d <- "8712" #"8712" # bycicle



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

potential_p1<-c(potential_p1,product4d)


Hidden_net<-graphh_clean[graphh_clean$hs2002_code_upstream%in%potential_p1,]
Hidden_net<-Hidden_net[Hidden_net$hs2002_code_downstream%in%potential_p1,]

#Full list of linkages
edges<-Hidden_net

library(tidyverse)


df <- as.data.frame(Hidden_net)
colnames(df) <- c("col", "row")

# build square table
labs <- union(df$col, df$row)

res <- df %>%
  mutate(value = 1) %>%
  complete(row = labs, col = labs, fill = list(value = 0)) %>%
  pivot_wider(
    names_from = col,
    values_from = value
  ) %>%
  arrange(row)


res<-as.data.frame(res)
row.names(res)<-res$row
res$row<-NULL
res<-as.matrix(res)
diag(res) <- 1

res<-t(res)


in_degree_values <- colSums(res)
out_degree_values <- rowSums(res)

order_cols <- order(in_degree_values, decreasing = TRUE) # the important one
order_rows <- order(out_degree_values, decreasing = TRUE)

res_reordered <- res[order_cols, order_cols, drop = FALSE]

res_reordered[res_reordered]



build_path_edges <- function(M) {
  # M: square binary matrix with same row/col names
  
  stopifnot(is.matrix(M))
  rn <- rownames(M)
  cn <- colnames(M)
  stopifnot(!is.null(rn), !is.null(cn))
  
  all_edges <- lapply(rn, function(r) {
    v <- M[r, ]
    
    # index of the endpoint (column whose name == row name)
    end_idx <- match(r, cn)
    if (is.na(end_idx)) return(NULL)   # safety
    
    # positions with 1s up to and including the endpoint
    idx <- which(v == 1 & seq_along(v) <= end_idx)
    idx <- sort(idx)
    
    # need at least two nodes to form an edge
    if (length(idx) < 2) return(NULL)
    
    nodes <- cn[idx]
    data.frame(
      from = nodes[-length(nodes)],
      to   = nodes[-1],
      stringsAsFactors = FALSE
    )
  })
  
  edges <- do.call(rbind, all_edges)
  if (is.null(edges)) return(NULL)
  
  unique(edges)
}

edges <- build_path_edges(res_reordered)

## --- 4. Build igraph object -------------------------------------------

nodes <- data.frame(
  name  = colnames(res_reordered),
  label = colnames(res_reordered),
  stringsAsFactors = FALSE
)


g <- graph_from_data_frame(
  d        = edges,
  directed = TRUE,
  vertices = nodes
)

## --- 5. visNetwork visualisation ---------------------------------------

vis_nodes <- data.frame(
  id    = V(g)$name,
  label = V(g)$name,
  stringsAsFactors = FALSE
)

# we already have edges as a data.frame, no need for as_data_frame(g, ...)
vis_edges <- edges
colnames(vis_edges) <- c("from", "to")
vis_edges$arrows <- "to"

visNetwork(vis_nodes, vis_edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)





# hsnames: tibble with columns Code, Description

lab_map <- setNames(hsnames$Description, hsnames$Code)


library(visNetwork)

# vis_nodes: use code as id, description as label (fallback on code if missing)
vis_nodes <- data.frame(
  id    = V(g)$name,                                        # HS code
  label = ifelse(is.na(lab_map[V(g)$name]),
                 V(g)$name,                                 # fallback
                 lab_map[V(g)$name]),                       # description
  stringsAsFactors = FALSE
)

# vis_edges: as before
vis_edges <- edges              # your data.frame(from, to)
colnames(vis_edges) <- c("from", "to")
vis_edges$arrows <- "to"

visNetwork(vis_nodes, vis_edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
