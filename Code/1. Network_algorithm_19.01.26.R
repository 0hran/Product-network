

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

rm = list = ls()

Flow_size_threshold<-10 #Threshold for the weight of the flow
Input_per_nodes<-5# - k: number of inputs to keep per node, at each level
Max_stage <-5# - depth: number of upstream stages (root is level 0)

#product4d <- "8712" # bycicle
product4d <- "8703" # car
# product4d <- "9018" # doesn't work
readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/product4d.rds")

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

#graphh <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx")
#hsnames <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS02")
#BEC_database <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Data/BEC database.xlsx") # TO update

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


#write.xlsx(hsnames, rowNames = F, file = "Fullmatrix1.xlsx")

















library(dplyr)
library(purrr)
library(igraph)

# 1) Collect all unique node Ids across the list
all_nodes <- Full_list %>%
  map(~ .x %>% select(Id) %>% distinct()) %>%
  bind_rows() %>%
  distinct(Id)

# 2) Build edges: consecutive Ids within each list element (chain)
edges <- imap_dfr(Full_list, function(df, chain_id) {
  df <- df %>% select(Id) %>% distinct()  # drop duplicate Id within the same chain, if any
  if (nrow(df) < 2) return(tibble(from = integer(), to = integer(), chain_id = integer(), step = integer()))
  
  tibble(
    from = df$Id[-nrow(df)],
    to   = df$Id[-1],
    chain_id = as.integer(chain_id),
    step = seq_len(nrow(df) - 1)          # 1..(k-1)
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

#saveRDS(gnetwork, "PN_links.rds")

















###########################################################













Fulldatabase<-as.matrix(get.adjacency(gnetwork))

Fulldatabase11<-Fulldatabase

write.xlsx(Fulldatabase, rowNames = TRUE, file = "Fullmatrix.xlsx")






library(dplyr)
library(igraph)

# df is your dataframe with the two columns
# hs2002_code_upstream, hs2002_code_downstream

edges <- graphh_clean %>%
  transmute(from = hs2002_code_upstream,
            to   = hs2002_code_downstream)

g12 <- graph_from_data_frame(edges, directed = TRUE)

##########################################



library(igraph)
library(dplyr)

# extract edge lists as data frames
edges_full <- as_data_frame(gnetwork, what = "edges") %>%
  select(from, to)

edges_g12 <- as_data_frame(g12, what = "edges") %>%
  select(from, to)

# keep only edges that appear in g12
edges_kept <- semi_join(edges_full, edges_g12, by = c("from", "to"))

# rebuild Fulldatabase with filtered edges
Fulldatabase_filtered <- graph_from_data_frame(
  edges_kept,
  directed = TRUE,
  vertices = as_data_frame(Fulldatabase, what = "vertices")
)





















library(igraph)
library(dplyr)

edges_full <- as_data_frame(gnetwork, what = "edges") %>% select(from, to)
edges_g12  <- as_data_frame(g12,      what = "edges") %>% select(from, to)

edges_kept <- semi_join(edges_full, edges_g12, by = c("from", "to"))

gnetwork_filtered <- graph_from_data_frame(
  edges_kept,
  directed = TRUE,
  vertices = as_data_frame(gnetwork, what = "vertices")
)

gnetwork_filtered1<-as.matrix(get.adjacency(gnetwork_filtered))





