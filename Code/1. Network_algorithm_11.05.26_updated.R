rm = list = ls()

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
  library(tidyverse)
  library(readxl)
  library(readr)
  library(openxlsx)
})


Key<- "C:/Users/ap115/OneDrive - SOAS University of London/Adria Rius's files - Research collab. AP-AR/"
#Key <-"C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/"

#product4d <- "8712" # bicycle
#product4d <- "8418" #Fridge
product4d <- "8703" # car

#readRDS(paste(Key,"Output/product4d.rds",sep = "" ))

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
                     
 graphh <- read_excel(paste(Key,"Code/edge_list_hs2002_4digit.xlsx",sep = "" )) # Arnaud
 hsnames <- read_excel(paste(Key,"Code/HSCodeandDescription.xlsx",sep = "" ), sheet = "HS02")
 BEC_database <- read_excel( paste(Key,"Data/BEC database.xlsx",sep = "" )) # TO update



Capital_good<-BEC_database$HS6[BEC_database$BEC5EndUse=="CAP"] #Adapter BEC

Capital_good<-Capital_good[product4d != substr(Capital_good,1,4)]

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
# 2) Step1 identify all the inputs from the car 
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
Hidden_net <- as.data.frame(Hidden_net)
colnames(Hidden_net) <- c("col", "row")
Full_net<-Hidden_net

Fish_network<-cbind.data.frame("id"=1:length(Full_net$col),Full_net)

colnames(Full_net) <- c("Source","Target")

write.xlsx(Full_net,file = paste(Key,"Data/Full_network",product4d,".xlsx",sep = "" ))

# -----------------------------
# 3) All chain of product
# -----------------------------
#Ranking global



Dta <- c()
Dta2 <- c()
for(y in 1:length(potential_p1)){
  Dta1 <-length(Hidden_net$col[Hidden_net$row==potential_p1[y]])/length(Hidden_net$row[Hidden_net$col==potential_p1[y]])
  Dta<-c(Dta,Dta1) 
  Dta3 <-potential_p1[y]
  Dta2<-c(Dta2,Dta3) 
}

Full_dta<-cbind.data.frame(Dta2,Dta)
colnames(Full_dta)<-c("Id","Upstream2")

FDU<-Full_dta

#Full
Full_list<-list()

for(t in 1:length(potential_p1)){
  
kuj1<-Hidden_net$row[Hidden_net$col==potential_p1[t]]
kuj<-c(potential_p1[1],kuj1)
Hidden_net_kuj<-Hidden_net[Hidden_net$col%in%kuj,]
Hidden_net_kuj<-Hidden_net_kuj[Hidden_net_kuj$row%in%kuj,]

kuj2<-kuj1[!kuj1==potential_p1[t]]

Dta <- c()
Dta2 <- c()

for(y in 1:length(kuj2)){
 Dta1 <-length(Hidden_net_kuj$col[Hidden_net_kuj$row==kuj[y]])/length(Hidden_net_kuj$row[Hidden_net_kuj$col==kuj[y]])
 Dta<-c(Dta,Dta1) 
 Dta3 <-kuj[y]
 Dta2<-c(Dta2,Dta3) 
}

KUJ_dta<-cbind.data.frame(Dta2,Dta)
colnames(KUJ_dta)<-c("Id","Upstream1")
Full_list[[t]]<-KUJ_dta

}

for(t in 1:length(potential_p1)){
Full_list[[t]] <- merge(
  Full_list[[t]],
  Full_dta,
  by = "Id",
  all.x = TRUE
)}

# -----------------------------
# 4) Ranger par upstream
# -----------------------------

for(t in 1:length(potential_p1)){
  Full_list[[t]] <- Full_list[[t]] %>%
    arrange(desc(Upstream1), desc(Upstream2))}

hsnames$id<-hsnames$Code

# Loop through each dataframe in the list
for (i in seq_along(Full_list)) {
  # Replace Inf with 100 in both Upstream1 and Upstream2 columns
  Full_list[[i]]$Upstream1[Full_list[[i]]$Upstream1 == Inf] <- 100
  Full_list[[i]]$Upstream2[Full_list[[i]]$Upstream2 == Inf] <- 100
  
  
  Full_list[[i]]$Upstream1[is.nan(Full_list[[i]]$Upstream1)] <- 0
  Full_list[[i]]$Upstream2[is.nan(Full_list[[i]]$Upstream2)] <- 0
  
}


# 1) Collect all unique node Ids across the list
all_nodes <- Full_list %>%
  map(~ .x %>% select(Id) %>% distinct()) %>%
  bind_rows() %>%
  distinct(Id)


# 2) Build edges: consecutive Ids within each list element (chain)
library(dplyr)
library(purrr)

edges <- imap_dfr(Full_list, function(df, chain_id) {
  df <- df %>%
    select(Id) %>%
    distinct() %>%
    mutate(Id = as.character(Id))  # Ensure Id is character
  
  if (nrow(df) < 2) {
    return(tibble(from = character(), to = character(), chain_id = integer(), step = integer()))
  }
  
  tibble(
    from = as.character(df$Id[-nrow(df)]),
    to   = as.character(df$Id[-1]),
    chain_id = as.integer(chain_id),
    step = seq_len(nrow(df) - 1)
  )
})





# 3) (Optional) collapse identical edges across multiple chains, count how often they appear
edge_summary <- edges %>%
  group_by(from, to) %>%
  summarise(
    weight = n(),                          # number of times the edge appears across chains
    chains = paste(sort(unique(chain_id)), collapse = ","),
    .groups = "drop"
  )

# 4) Build igraph object (choose edges or edge_summary)
g <- graph_from_data_frame(
  d = edge_summary,                        # or use `edges` if you want chain-level edges
  vertices = all_nodes,
  directed = TRUE
)

gnetwork<-reverse_edges(g)

vertex_names <- get.vertex.attribute(gnetwork, "name")
upstream2_values <- setNames(FDU$Upstream2, FDU$Id)

# Add or update the Upstream2 attribute for all vertices
# For vertices not in FDU, set Upstream2 to NA or 0, as appropriate
gnetwork <- set.vertex.attribute(
  gnetwork,
  name = "Upstream2",
  value = upstream2_values[vertex_names]
)

###########################################################

# Get current vertex names
vertex_names <- get.vertex.attribute(gnetwork, "name")

# For each column in hsnames (except 'id'), add as a vertex attribute
for (col in setdiff(names(hsnames), "id")) {
  # Create a named vector for the current column
  attr_values <- setNames(hsnames[[col]], hsnames$id)
  
  # Add or update the vertex attribute
  gnetwork <- set.vertex.attribute(
    gnetwork,
    name = col,
    value = attr_values[vertex_names]
  )
}



# df is your dataframe with the two columns
# hs2002_code_upstream, hs2002_code_downstream

edges <- graphh_clean %>%
  transmute(from = hs2002_code_upstream,
            to   = hs2002_code_downstream)

Hidd_network <- graph_from_data_frame(edges, directed = TRUE)


edges_full<-get.edgelist(gnetwork)
colnames(edges_full)<-c("from","to")
edges_full<-as.data.frame(edges_full)

Hidd_network_full<-get.edgelist(Hidd_network)
colnames(Hidd_network_full)<-c("from","to")
Hidd_network_full<-as.data.frame(Hidd_network_full)

# keep only edges that appear in Hidd_network
edges_kept <- semi_join(edges_full, Hidd_network_full, by = c("from", "to"))

gnetwork_filtered <- graph_from_data_frame(
  edges_kept,   directed = TRUE)

gnetwork_filtered
gnetwork

# Get the edge list from gnetwork_filtered
filtered_edges <- as_edgelist(gnetwork_filtered)

# Filter gnetwork to keep only edges that exist in gnetwork_filtered
edges_full <- as_edgelist(gnetwork)

# Find matching edges (undirected: check both directions)
matches <- apply(edges_full, 1, function(e) {
  any(filtered_edges[,1] == e[1] & filtered_edges[,2] == e[2])
})

# Delete non-matching edges
gnetwork_new <- delete_edges(gnetwork, which(!matches))

write.xlsx(as.matrix(get.adjacency(gnetwork_new)), rowNames = T, file = paste(Key,"Code/gnetwork_filtered",product4d,".xlsx",sep = "" ))

gnetwork_new <- delete_vertices(gnetwork_new, which(degree(gnetwork_new) == 0))

vertex_df <- as.data.frame(vertex_attr(gnetwork_new))
vdf<-cbind.data.frame(id=vertex_df$name,vertex_df)

paste(Key,"Code/gnetwork_filtered",product4d,".xlsx",sep = "" )

write.xlsx(vdf, rowNames = F, file = paste(Key,"/Output/Vertex_Data",product4d,".xlsx",sep=""))

saveRDS(gnetwork_new, paste(Key,"/Output/PN_links_",product4d,".rds",sep=""))


###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################






Fish_network$id<-NULL

Hidden_network<-graph_from_edgelist(as.matrix(Fish_network),directed = T)
head(Fish_network)

length(V(gnetwork_new))
length(V(Hidden_network))

length(E(gnetwork_new))
length(E(Hidden_network))

mean(degree(gnetwork_new,mode = "in"))
mean(degree(Hidden_network,mode = "in"))

max(degree(gnetwork_new,mode = "in"))
max(degree(Hidden_network,mode = "in"))

max(degree(gnetwork_new,mode = "out"))
max(degree(Hidden_network,mode = "out"))

diameter(gnetwork_new,directed = T, unconnected = T)
diameter(Hidden_network,directed = T, unconnected = T)

reciprocity(gnetwork_new,ignore.loops = T)
reciprocity(Hidden_network,ignore.loops = T)

mean_distance(gnetwork_new,directed = T)
mean_distance(Hidden_network,directed = T)

edge_density(gnetwork_new, loops = FALSE)
edge_density(Hidden_network, loops = FALSE)

transitivity(gnetwork_new, type = "average")
transitivity(Hidden_network, type = "average")

assortativity_degree(gnetwork_new, directed = TRUE)
assortativity_degree(Hidden_network, directed = TRUE)

g_undirected <- as.undirected(gnetwork_new, mode = "collapse")
NN1<-cluster_louvain(g_undirected)

g_undirected <- as.undirected(Hidden_network, mode = "collapse")
NN2<-cluster_louvain(g_undirected)
 
HN_AIPNET<-as.matrix(get.adjacency(Hidden_network))
PN_PROD<-as.matrix(get.adjacency(gnetwork_new))

 
heatmap(
  HN_AIPNET,
  Rowv = NA,
  Colv = NA,
  col = c("white","red"),
  scale = "none"
)


heatmap(
  PN_PROD,
  Rowv = NA,
  Colv = NA,
  col = c("white","red"),
  scale = "none"
)