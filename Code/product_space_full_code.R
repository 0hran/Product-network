#rm(list = ls())
library(readr)
library(visNetwork)
library("igraph")


BACI2022<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2022_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2022)
BACI2022$j=NULL
BACI2022$q=NULL

library(dplyr)
BACI2022$hs4=substr(BACI2022$k,1,4)
BACI2022$k=NULL
bacii = summarise(group_by(BACI2022, hs4, i), v=sum(v))
bacii2022<-bacii
rm(BACI2022,bacii)



BACI2017<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2017_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2017)
BACI2017$j=NULL
BACI2017$q=NULL

library(dplyr)
BACI2017$hs4=substr(BACI2017$k,1,4)
BACI2017$k=NULL
bacii = summarise(group_by(BACI2017, hs4, i), v=sum(v))
bacii2017<-bacii
rm(BACI2017,bacii)


BACI2012<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2012_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2012)
BACI2012$j=NULL
BACI2012$q=NULL

library(dplyr)
BACI2012$hs4=substr(BACI2012$k,1,4)
BACI2012$k=NULL
bacii = summarise(group_by(BACI2012, hs4, i), v=sum(v))
bacii2012<-bacii
rm(BACI2012,bacii)


BACI2007<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2007_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2007)
BACI2007$j=NULL
BACI2007$q=NULL

library(dplyr)
BACI2007$hs4=substr(BACI2007$k,1,4)
BACI2007$k=NULL
bacii = summarise(group_by(BACI2007, hs4, i), v=sum(v))
bacii2007<-bacii
rm(BACI2007,bacii)


BACI2002<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2002_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2002)
BACI2002$j=NULL
BACI2002$q=NULL

library(dplyr)
BACI2002$hs4=substr(BACI2002$k,1,4)
BACI2002$k=NULL
bacii = summarise(group_by(BACI2002, hs4, i), v=sum(v))
bacii2002<-bacii
rm(BACI2002,bacii)

bacii2002$ID<-paste(bacii2002$hs4,bacii2002$i,sep = "-")
bacii2007$ID<-paste(bacii2007$hs4,bacii2007$i,sep = "-")
bacii2012$ID<-paste(bacii2012$hs4,bacii2012$i,sep = "-")
bacii2017$ID<-paste(bacii2017$hs4,bacii2017$i,sep = "-")
bacii2022$ID<-paste(bacii2022$hs4,bacii2022$i,sep = "-")

Full_baci<-rbind.data.frame(bacii2002,bacii2007,bacii2012,bacii2017,bacii2022)
Full_baci<-bacii2022


bacii <- Full_baci %>%
  group_by(hs4, i) %>%
  summarise(v = mean(v, na.rm = TRUE))

####################Delete NA value########################################
bacii=na.omit(bacii)
####################rank by country########################################
bacii=arrange(bacii, i)
#######################RCA#################################################
attach(bacii)
ucountry = unique(i) # get unique countries in database
ncountries = length(ucountry) # get total number of distinct countries

a = matrix(nrow = ncountries, ncol = 1)
colnames(a) <- c("totalProduction")
for (j in 1:ncountries) {
  idCountry = ucountry[j]
  a[j] = sum(v[which(i==idCountry)])
}

uproduct = unique(hs4) # get unique products in database
nproducts = length(uproduct) # get total number of distinct products

b = matrix(nrow = nproducts, ncol = 1)
for (j in 1:nproducts) {
  idProduct = uproduct[j]
  b[j] = sum(v[which(hs4==idProduct)])
}

bacii$RCA <- lapply(seq_along(v), function(j) {
  idCountry = match(i[j], ucountry)
  idProduct = match(hs4[j], uproduct)
  return((v[j]/a[idCountry])/(b[idProduct]/sum(v)))
})
######################presence#####################
bacii$RCAbin<-as.numeric(bacii$RCA>1)
#############################cp matrix#############################

cp=cbind((bacii$hs4[which(bacii$RCAbin==1)]),bacii$i[which(bacii$RCAbin==1)])
cp=as.data.frame(cp)

attach(cp)
cp=table(V1,V2)
cp=as.data.frame(cp)

library(reshape)
cp=cast(cp, V1 ~ V2)
row.names(cp)<-cp$V1
cp$V1=NULL

#############Cette partie l? je n'arrete pas de trouver les philippine dans le top 10###############
cp_2022=as.data.frame(cp)

China=cp_2022$`156`
South=cp_2022$`710`
Morocco=cp_2022$`504`
Japan=cp_2022$`392`
Poland=cp_2022$`616`
Mexico=cp_2022$`484`

auto_producer=cbind.data.frame(China,South,Morocco,Japan,Poland,Mexico)

row.names(auto_producer)<-row.names(cp_2022)
auto_producer_2017<-auto_producer


Industry=row.names(auto_producer)
Sector=substr(Industry,1,2)



#######################################################################
####Espace produit des produits hausman-hidalgo########################
######################Matrice de proximit?######################################

cp=as.matrix(cp)

cp2=t(cp)
pp<-cp%*%cp2

#Matrice de Proximit? HH
dcM <-matrix(nrow=length(Industry),ncol=length(Industry))
for (cpii in 1:nrow(pp)){
  for (cpij in 1:ncol(pp)){
    dcM[cpii,cpij]<-pp[cpii,cpij]/(max(pp[cpii,cpii],pp[cpij,cpij]))
  }
}

# Nom sur les lignes/colonnes de la matrice :
rownames(dcM)<-Industry
colnames(dcM) <- Industry

dcM = ifelse(test=dcM > 0.5 , yes = 1 , no=0 )
diag(dcM) <- 0

adjpp2022 =graph.adjacency(dcM )

write.graph(adjpp2022, file="HHgraph2022_absolute.graphml", format="graphml")


netty <- toVisNetworkData(adjpp2022)
visNetwork(nodes = netty$nodes, edges = netty$edges, width = "100%" , height = "1000px") %>% visNodes(shape = "circle") %>%  visEdges(arrows = 'to', smooth =T)  %>%  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -250))
Graph_network<-adjpp2022

V(Graph_network)$deg<-degree(Graph_network, mode = "all")
#Merge_both of them
auto_producer


#############################################################
#############################################################
#rm(list = ls())
library(readr)
BACI2022<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2022_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2022)
BACI2022$j=NULL
BACI2022$q=NULL

library(dplyr)
BACI2022$hs4=substr(BACI2022$k,1,4)
BACI2022$k=NULL
bacii = summarise(group_by(BACI2022, hs4, i), v=sum(v))
bacii2022<-bacii
rm(BACI2022,bacii)

BACI2017<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2017_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2017)
BACI2017$j=NULL
BACI2017$q=NULL

library(dplyr)
BACI2017$hs4=substr(BACI2017$k,1,4)
BACI2017$k=NULL
bacii = summarise(group_by(BACI2017, hs4, i), v=sum(v))
bacii2017<-bacii
rm(BACI2017,bacii)


BACI2012<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2012_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2012)
BACI2012$j=NULL
BACI2012$q=NULL

library(dplyr)
BACI2012$hs4=substr(BACI2012$k,1,4)
BACI2012$k=NULL
bacii = summarise(group_by(BACI2012, hs4, i), v=sum(v))
bacii2012<-bacii
rm(BACI2012,bacii)


BACI2007<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2007_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2007)
BACI2007$j=NULL
BACI2007$q=NULL

library(dplyr)
BACI2007$hs4=substr(BACI2007$k,1,4)
BACI2007$k=NULL
bacii = summarise(group_by(BACI2007, hs4, i), v=sum(v))
bacii2007<-bacii
rm(BACI2007,bacii)


BACI2002<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2002_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2002)
BACI2002$j=NULL
BACI2002$q=NULL

library(dplyr)
BACI2002$hs4=substr(BACI2002$k,1,4)
BACI2002$k=NULL
bacii = summarise(group_by(BACI2002, hs4, i), v=sum(v))
bacii2002<-bacii
rm(BACI2002,bacii)

bacii<-bacii2022

####################Delete NA value########################################
bacii=na.omit(bacii)
####################rank by country########################################
bacii=arrange(bacii, i)
#######################RCA#################################################
attach(bacii)
ucountry = unique(i) # get unique countries in database
ncountries = length(ucountry) # get total number of distinct countries

a = matrix(nrow = ncountries, ncol = 1)
colnames(a) <- c("totalProduction")
for (j in 1:ncountries) {
  idCountry = ucountry[j]
  a[j] = sum(v[which(i==idCountry)])
}

uproduct = unique(hs4) # get unique products in database
nproducts = length(uproduct) # get total number of distinct products

b = matrix(nrow = nproducts, ncol = 1)
for (j in 1:nproducts) {
  idProduct = uproduct[j]
  b[j] = sum(v[which(hs4==idProduct)])
}

bacii$RCA <- lapply(seq_along(v), function(j) {
  idCountry = match(i[j], ucountry)
  idProduct = match(hs4[j], uproduct)
  return((v[j]/a[idCountry])/(b[idProduct]/sum(v)))
})
######################presence#####################
bacii$RCAbin<-as.numeric(bacii$RCA>1)
#############################cp matrix#############################

cp=cbind((bacii$hs4[which(bacii$RCAbin==1)]),bacii$i[which(bacii$RCAbin==1)])
cp=as.data.frame(cp)

attach(cp)
cp=table(V1,V2)
cp=as.data.frame(cp)

library(reshape)
cp=cast(cp, V1 ~ V2)
row.names(cp)<-cp$V1
cp$V1=NULL

#############Cette partie l? je n'arrete pas de trouver les philippine dans le top 10###############
cp_2022=as.data.frame(cp)

China=cp_2022$`156`
South=cp_2022$`710`
Morocco=cp_2022$`504`
Japan=cp_2022$`392`
Poland=cp_2022$`616`
Mexico=cp_2022$`484`

auto_producer=cbind.data.frame(China,South,Morocco,Japan,Poland,Mexico)
row.names(auto_producer)<-row.names(cp_2022)

write.csv(cp_2022, "cp_2022.csv", row.names = TRUE)
write.csv(auto_producer, "auto_producer_2022.csv", row.names = TRUE)
####################################################################################
####################################################################################



for(attr_name in colnames(auto_producer)) {
  Graph_network <- set_vertex_attr(
    Graph_network,
    name = attr_name,
    value = auto_producer[V(Graph_network)$name, attr_name]
  )
}



count_China_neigh <- sapply(V(Graph_network), function(v) {
  neigh <- neighbors(Graph_network, v)
  sum(V(Graph_network)[neigh]$China == 1)
})

V(Graph_network)$China_neigh_count <- count_China_neigh
V(Graph_network)$Share_activated_neighbour_china<-V(Graph_network)$China_neigh_count/V(Graph_network)$deg
V(Graph_network)$Share_activated_neighbour_china[is.na(V(Graph_network)$Share_activated_neighbour_china)] <- 0

###############


count_South_neigh <- sapply(V(Graph_network), function(v) {
  neigh <- neighbors(Graph_network, v)
  sum(V(Graph_network)[neigh]$South == 1)
})

V(Graph_network)$South_neigh_count <- count_South_neigh
V(Graph_network)$Share_activated_neighbour_South<-V(Graph_network)$South_neigh_count/V(Graph_network)$deg
V(Graph_network)$Share_activated_neighbour_South[is.na(V(Graph_network)$Share_activated_neighbour_South)] <- 0

###############


count_Morocco_neigh <- sapply(V(Graph_network), function(v) {
  neigh <- neighbors(Graph_network, v)
  sum(V(Graph_network)[neigh]$Morocco == 1)
})

V(Graph_network)$Morocco_neigh_count <- count_Morocco_neigh
V(Graph_network)$Share_activated_neighbour_Morocco<-V(Graph_network)$Morocco_neigh_count/V(Graph_network)$deg
V(Graph_network)$Share_activated_neighbour_Morocco[is.na(V(Graph_network)$Share_activated_neighbour_Morocco)] <- 0

###############


count_Japan_neigh <- sapply(V(Graph_network), function(v) {
  neigh <- neighbors(Graph_network, v)
  sum(V(Graph_network)[neigh]$Japan == 1)
})

V(Graph_network)$Japan_neigh_count <- count_Japan_neigh
V(Graph_network)$Share_activated_neighbour_Japan<-V(Graph_network)$Japan_neigh_count/V(Graph_network)$deg
V(Graph_network)$Share_activated_neighbour_Japan[is.na(V(Graph_network)$Share_activated_neighbour_Japan)] <- 0

###############

count_Poland_neigh <- sapply(V(Graph_network), function(v) {
  neigh <- neighbors(Graph_network, v)
  sum(V(Graph_network)[neigh]$Poland == 1)
})

V(Graph_network)$Poland_neigh_count <- count_Poland_neigh
V(Graph_network)$Share_activated_neighbour_Poland<-V(Graph_network)$Poland_neigh_count/V(Graph_network)$deg
V(Graph_network)$Share_activated_neighbour_Poland[is.na(V(Graph_network)$Share_activated_neighbour_Poland)] <- 0



###############

count_Mexico_neigh <- sapply(V(Graph_network), function(v) {
  neigh <- neighbors(Graph_network, v)
  sum(V(Graph_network)[neigh]$Mexico == 1)
})

V(Graph_network)$Mexico_neigh_count <- count_Mexico_neigh
V(Graph_network)$Share_activated_neighbour_Mexico<-V(Graph_network)$Mexico_neigh_count/V(Graph_network)$deg
V(Graph_network)$Share_activated_neighbour_Mexico[is.na(V(Graph_network)$Share_activated_neighbour_Mexico)] <- 0



Full_database_network <- data.frame(
  name  = V(Graph_network)$name,
  deg   = V(Graph_network)$deg,
  Mexico   = V(Graph_network)$Mexico,
  Poland   = V(Graph_network)$Poland,
  Japan    = V(Graph_network)$Japan,
  Morocco  = V(Graph_network)$Morocco,
  South    = V(Graph_network)$South,
  China    = V(Graph_network)$China,
  Mexico_neigh_count   = V(Graph_network)$Mexico_neigh_count,
  Poland_neigh_count   = V(Graph_network)$Poland_neigh_count,
  Japan_neigh_count    = V(Graph_network)$Japan_neigh_count,
  Morocco_neigh_count  = V(Graph_network)$Morocco_neigh_count,
  South_neigh_count    = V(Graph_network)$South_neigh_count,
  China_neigh_count    = V(Graph_network)$China_neigh_count,
  Share_activated_neighbour_Mexico   = V(Graph_network)$Share_activated_neighbour_Mexico,
  Share_activated_neighbour_Poland   = V(Graph_network)$Share_activated_neighbour_Poland,
  Share_activated_neighbour_Japan    = V(Graph_network)$Share_activated_neighbour_Japan,
  Share_activated_neighbour_Morocco  = V(Graph_network)$Share_activated_neighbour_Morocco,
  Share_activated_neighbour_South    = V(Graph_network)$Share_activated_neighbour_South,
  Share_activated_neighbour_China    = V(Graph_network)$Share_activated_neighbour_china
)

write.csv(Full_database_network, "Full_database_network_auto_maker.csv", row.names = TRUE)
coords <- vis$x$nodes[, c("id", "x", "y","size")]
write.csv(coords, "coords.csv", row.names = TRUE)






Short_auto_maker<-Full_database_network_auto_maker[Full_database_network_auto_maker$name%in%V(network)$name,]

Short_auto_maker_df <- as.data.frame(Short_auto_maker)
rownames(Short_auto_maker_df) <- Short_auto_maker_df$name



network <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/network.rds")
idx <- match(V(network)$name, coords$id)
V(network)$x <- coords$x[idx]
V(network)$y <- coords$y[idx]
V(network)$size <- coords$size[idx]

## All columns except the key "name"
attr_list <- setdiff(colnames(Short_auto_maker_df), "name")

for (attr_name in attr_list) {
  network <- set_vertex_attr(
    network,
    name  = attr_name,
    value = Short_auto_maker_df[V(network)$name, attr_name]
  )
}



library(visNetwork)

vis_data <- toVisNetworkData(network)

vis_data$nodes$x     <- V(network)$x
vis_data$nodes$y     <- V(network)$y
vis_data$nodes$fixed <- TRUE   # do not let physics move them

visNetwork(vis_data$nodes, vis_data$edges) %>%
  visNodes(shape = "dot", size = 30) %>%
  visPhysics(enabled = FALSE)     # no movement, pure frozen layout









## Color nodes by China
vis_data$nodes$color <- ifelse(
  vis_data$nodes$China == 1,
  "black",
  "lightgrey"
)

## Labels from Share_activated_neighbour_China
vis_data$nodes$label <- sprintf(
  "%.2f",
  vis_data$nodes$Share_activated_neighbour_China
)
vis_data$nodes$label <- as.character(vis_data$nodes$label)


visNetwork(vis_data$nodes, vis_data$edges) |>
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
  ) |>
  visEvents(
    stabilizationIterationsDone =
      "function () {
         this.setOptions({ physics: false });
       }"
  ) |>
  visNodes(
    shape = "dot",
    size  = 30,
    font  = list(
      size  = 30,     # big labels
      color = "black"
    ),
    scaling = list(
      label = list(
        enabled = TRUE,
        min     = 30,
        max     = 30
      )
    )
  )


##################################################################################################
############################################Grid data#############################################
##################################################################################################
Database_distroot<-cbind.data.frame("Name"=get.vertex.attribute(network)$name, "Dist_to_root"=V(network)$dist_to_root)

Full_database_merged <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Full_database_merged.rds")

Database_distroot$Name <- as.character(Database_distroot$Name)
Full_database_merged$name <- as.character(Full_database_merged$name)

# Rename for consistency
colnames(Database_distroot)[colnames(Database_distroot) == "Name"] <- "name"


Merged_DB <- merge(
  Database_distroot,
  Full_database_merged,
  by = "name",
  all.x = TRUE   # keep all rows from Database_distroot
)

saveRDS(Merged_DB,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Car_Full_database_merged.rds")


p_china <- ggplot(
  Merged_DB,
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_China,
    color = factor(China, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "Product relatedness in China",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "RCA>1"
  ) +
  theme_minimal()



p_morocco <- ggplot(
  Merged_DB,
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_Morocco,
    color = factor(Morocco, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "Product relatedness in Morocco",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "RCA>1"
  ) +
  theme_minimal()

p_japan <- ggplot(
  Merged_DB,
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_Japan,
    color = factor(Japan, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "Product relatedness in Japan",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "RCA>1"
  ) +
  theme_minimal()

p_south <- ggplot(
  Merged_DB,
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_South,
    color = factor(South, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "Product relatedness in South Africa",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "RCA>1"
  ) +
  theme_minimal()

gridExtra::grid.arrange(
  p_china, p_morocco,
  p_japan, p_south,
  ncol = 2
)
######################################
#############Last_section_diversification
Edge_list_from_PN<-get.edgelist(network)
Edge_list_from_PN<-as.data.frame(Edge_list_from_PN)

Edge_list_PN<-cbind.data.frame("Upstream"=Edge_list_from_PN$V2,"Downstream"=Edge_list_from_PN$V1)
write.csv(Edge_list_PN, "Edge_list_PN.csv", row.names = TRUE)

g_undirected <- as.undirected(adjpp2022, mode = "collapse")
g_undirected <- simplify(g_undirected,
                         remove.multiple = TRUE,
                         remove.loops = TRUE)
Edge_list_from_g_undirected_PN<-get.edgelist(g_undirected)
write.csv(Edge_list_from_g_undirected_PN, "product_space.csv", row.names = TRUE)

write.csv(cp_2002, "cp_2002.csv", row.names = TRUE)
write.csv(cp_2007, "cp_2007.csv", row.names = TRUE)
write.csv(cp_2012, "cp_2012.csv", row.names = TRUE)
write.csv(cp_2017, "cp_2017.csv", row.names = TRUE)
write.csv(cp_2022, "cp_2022.csv", row.names = TRUE)

Edge_list_PN <- read_csv("Edge_list_PN.csv")

##China=cp_2022$`156`
#South=cp_2022$`710`
#Morocco=cp_2022$`504`
#Japan=cp_2022$`392`
## 1. Put all auto_producer data frames in a named list ----------------------

auto_list_raw <- list(
  `2002` = auto_producer_2002,
  `2007` = auto_producer_2007,
  `2012` = auto_producer_2012,
  `2017` = auto_producer_2017,
  `2022` = auto_producer_2022
)

## 2. For each year:
##    - convert rownames to a "name" column
##    - add year suffix to country columns -----------------------------------

auto_list_prepared <- lapply(names(auto_list_raw), function(yr) {
  df <- as.data.frame(auto_list_raw[[yr]])
  
  df$name <- rownames(df)
  
  country_cols <- setdiff(colnames(df), "name")
  colnames(df)[colnames(df) != "name"] <- paste0(country_cols, "_", yr)
  
  df
})

## 3. Merge everything into Full_database_network by "name" ------------------

Full_database_merged <- Reduce(
  function(x, y) merge(x, y, by = "name", all.x = TRUE),
  c(list(Full_database_network), auto_list_prepared)
)

## 4. Inspect result --------------------------------------------------------


Full_database_merged$China2007_2002<-ifelse((Full_database_merged$China_2007-Full_database_merged$China_2002)>0.5,1,0)
Full_database_merged$China2012_2007<-ifelse((Full_database_merged$China_2012-Full_database_merged$China_2007)>0.5,1,0)
Full_database_merged$China2017_2012<-ifelse((Full_database_merged$China_2017-Full_database_merged$China_2012)>0.5,1,0)
Full_database_merged$China2022_2017<-ifelse((Full_database_merged$China_2022-Full_database_merged$China_2017)>0.5,1,0)

Full_database_merged$South2007_2002<-ifelse((Full_database_merged$South_2007-Full_database_merged$South_2002)>0.5,1,0)
Full_database_merged$South2012_2007<-ifelse((Full_database_merged$South_2012-Full_database_merged$South_2007)>0.5,1,0)
Full_database_merged$South2017_2012<-ifelse((Full_database_merged$South_2017-Full_database_merged$South_2012)>0.5,1,0)
Full_database_merged$South2022_2017<-ifelse((Full_database_merged$South_2022-Full_database_merged$South_2017)>0.5,1,0)

Full_database_merged$Morocco2007_2002<-ifelse((Full_database_merged$Morocco_2007-Full_database_merged$Morocco_2002)>0.5,1,0)
Full_database_merged$Morocco2012_2007<-ifelse((Full_database_merged$Morocco_2012-Full_database_merged$Morocco_2007)>0.5,1,0)
Full_database_merged$Morocco2017_2012<-ifelse((Full_database_merged$Morocco_2017-Full_database_merged$Morocco_2012)>0.5,1,0)
Full_database_merged$Morocco2022_2017<-ifelse((Full_database_merged$Morocco_2022-Full_database_merged$Morocco_2017)>0.5,1,0)

Full_database_merged$Japan2007_2002<-ifelse((Full_database_merged$Japan_2007-Full_database_merged$Japan_2002)>0.5,1,0)
Full_database_merged$Japan2012_2007<-ifelse((Full_database_merged$Japan_2012-Full_database_merged$Japan_2007)>0.5,1,0)
Full_database_merged$Japan2017_2012<-ifelse((Full_database_merged$Japan_2017-Full_database_merged$Japan_2012)>0.5,1,0)
Full_database_merged$Japan2022_2017<-ifelse((Full_database_merged$Japan_2022-Full_database_merged$Japan_2017)>0.5,1,0)

network <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/network.rds")

## 1. Use product code as rownames in the table
rownames(Full_database_merged) <- Full_database_merged$name

## 2. Subset rows to match the graph vertices (by name)
matched <- Full_database_merged[V(network)$name, ]

## Quick sanity check (should be TRUE)
stopifnot(nrow(matched) == vcount(network))

## 3. Add all columns except 'name' as vertex attributes
attr_list <- setdiff(colnames(Full_database_merged), "name")

for (attr_name in attr_list) {
  network <- set_vertex_attr(
    network,
    name  = attr_name,
    value = matched[[attr_name]]
  )
}



##############################################################
library(igraph)

## ------------------------------------------------------------------
## 0. Graph object
## ------------------------------------------------------------------
library(igraph)

g <- network

## 1. Basic info and adjacency lists ----------------------------------------

n  <- vcount(g)
el <- as_edgelist(g, names = FALSE)

out_list <- vector("list", n)
in_list  <- vector("list", n)

for (e in seq_len(nrow(el))) {
  i <- el[e, 1]
  j <- el[e, 2]
  out_list[[i]] <- c(out_list[[i]], j)
  in_list[[j]]  <- c(in_list[[j]],  i)
}

countries <- c("China", "South", "Morocco", "Japan")
periods   <- c("2007_2002", "2012_2007", "2017_2012", "2022_2017")

all_attr_names <- vertex_attr_names(g)
prod_names     <- V(g)$name

results <- list()
no_link_products <- list()   # to store product lists by country and period


## 2. Loop over countries and periods ---------------------------------------

for (cty in countries) {
  for (prd in periods) {
    
    col_delta <- paste0(cty, prd)                  # e.g. "China2007_2002"
    base_year <- sub(".*_", "", prd)               # e.g. "2002"
    col_base  <- paste0(cty, "_", base_year)       # e.g. "China_2002"
    
    if (!(col_delta %in% all_attr_names)) next
    if (!(col_base  %in% all_attr_names)) next
    
    delta_attr <- as.numeric(vertex_attr(g, col_delta))
    base_attr  <- as.numeric(vertex_attr(g, col_base))
    
    idx_new <- which(delta_attr == 1)
    
    # If no new nodes, record zeros and an empty product list
    if (length(idx_new) == 0) {
      results[[length(results) + 1]] <- data.frame(
        Country        = cty,
        Period         = prd,
        Yes_downstream = 0,
        No_downstream  = 0,
        Yes_upstream   = 0,
        No_upstream    = 0,
        No_linkages    = 0
      )
      if (is.null(no_link_products[[cty]])) no_link_products[[cty]] <- list()
      no_link_products[[cty]][[prd]] <- character(0)
      next
    }
    
    has_out <- logical(n)
    has_in  <- logical(n)
    
    # Downstream (outgoing)
    for (v in idx_new) {
      targets <- out_list[[v]]
      if (length(targets) > 0) {
        has_out[v] <- any(base_attr[targets] == 1)
      } else {
        has_out[v] <- FALSE
      }
    }
    
    # Upstream (incoming)
    for (v in idx_new) {
      sources <- in_list[[v]]
      if (length(sources) > 0) {
        has_in[v] <- any(base_attr[sources] == 1)
      } else {
        has_in[v] <- FALSE
      }
    }
    
    # Counts
    Yes_down <- sum(has_out[idx_new])
    No_down  <- sum(!has_out[idx_new])
    Yes_up   <- sum(has_in[idx_new])
    No_up    <- sum(!has_in[idx_new])
    
    # Nodes with neither upstream nor downstream link to base-year exporters
    no_link_flags <- !(has_out | has_in)
    idx_no_link   <- idx_new[no_link_flags[idx_new]]
    n_no_link     <- length(idx_no_link)
    
    # Store product codes for these nodes
    if (is.null(no_link_products[[cty]])) no_link_products[[cty]] <- list()
    no_link_products[[cty]][[prd]] <- prod_names[idx_no_link]
    
    # Store summary row
    results[[length(results) + 1]] <- data.frame(
      Country        = cty,
      Period         = prd,
      Yes_downstream = Yes_down,
      No_downstream  = No_down,
      Yes_upstream   = Yes_up,
      No_upstream    = No_up,
      No_linkages    = n_no_link
    )
  }
}

## 3. Final summary table ----------------------------------------------------

summary_table <- do.call(rbind, results)

saveRDS(summary_table,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/summary_table.rds")

saveRDS(Full_database_merged,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Full_database_merged.rds")
################################################################################
rm(list = ls())

load("~/full_database_rca_prod_space_bis_just_in_case.RData")

saveRDS(Graph_network,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Graph_network.rds")

saveRDS(no_link_products,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/no_link_products.rds")

Full_database_merged <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Full_database_merged.rds")




###################################################################
library(igraph)

## 1. Use product code as rownames in the table
rownames(Full_database_merged) <- Full_database_merged$name

## 2. Subset rows to match the vertices of Graph_network (by name)
matched <- Full_database_merged[V(Graph_network)$name, ]

## Optional sanity check
stopifnot(nrow(matched) == vcount(Graph_network))

## 3. Add all attributes except 'name' to the graph
attr_list <- setdiff(colnames(Full_database_merged), "name")

for (attr_name in attr_list) {
  Graph_network <- set_vertex_attr(
    Graph_network,
    name  = attr_name,
    value = matched[[attr_name]]
  )
}


node_atrib
V(Graph_network)$china


##############################################################################
library(igraph)

g <- Graph_network

## 1. Build undirected adjacency (any link, upstream or downstream) ---------

n  <- vcount(g)
el <- as_edgelist(g, names = FALSE)

# neighbour_list[[v]] = all neighbours (both directions)
neighbour_list <- vector("list", n)
for (e in seq_len(nrow(el))) {
  i <- el[e, 1]
  j <- el[e, 2]
  neighbour_list[[i]] <- c(neighbour_list[[i]], j)
  neighbour_list[[j]] <- c(neighbour_list[[j]], i)
}

## 2. Define countries and periods ------------------------------------------

countries <- c("China", "South", "Morocco", "Japan")
periods   <- c("2007_2002", "2012_2007", "2017_2012", "2022_2017")

all_attr_names <- vertex_attr_names(g)
prod_names     <- V(g)$name

summary_list   <- list()
no_link_list   <- list()   # products without any such link, by country and period

## 3. Loop over all country–period combinations -----------------------------

for (cty in countries) {
  for (prd in periods) {
    
    # delta attribute, e.g. "China2007_2002"
    col_delta <- paste0(cty, prd)
    
    # base year, e.g. "2002" from "2007_2002"
    base_year <- sub(".*_", "", prd)
    
    # base attribute, e.g. "China_2002"
    col_base  <- paste0(cty, "_", base_year)
    
    # skip if attributes do not exist
    if (!(col_delta %in% all_attr_names)) next
    if (!(col_base  %in% all_attr_names)) next
    
    delta_attr <- as.numeric(vertex_attr(g, col_delta))
    base_attr  <- as.numeric(vertex_attr(g, col_base))
    
    # adopted products in this period
    idx_adopted <- which(delta_attr == 1)
    n_adopted   <- length(idx_adopted)
    
    if (n_adopted == 0) {
      # no adopted products in this country–period
      summary_list[[length(summary_list) + 1]] <- data.frame(
        Country      = cty,
        Period       = prd,
        Adopted      = 0,
        Yes_link     = 0,
        No_link      = 0
      )
      if (is.null(no_link_list[[cty]])) no_link_list[[cty]] <- list()
      no_link_list[[cty]][[prd]] <- character(0)
      next
    }
    
    # for each adopted product, check if it has at least one neighbour
    # with base_attr == 1 at the beginning of the period
    has_link <- logical(n)
    
    for (v in idx_adopted) {
      neigh <- neighbour_list[[v]]
      if (length(neigh) > 0) {
        has_link[v] <- any(base_attr[neigh] == 1)
      } else {
        has_link[v] <- FALSE
      }
    }
    
    yes_link_count <- sum(has_link[idx_adopted])
    no_link_count  <- sum(!has_link[idx_adopted])
    
    # products with no such link
    idx_no_link    <- idx_adopted[!has_link[idx_adopted]]
    products_no_link <- prod_names[idx_no_link]
    
    # store in summary table
    summary_list[[length(summary_list) + 1]] <- data.frame(
      Country      = cty,
      Period       = prd,
      Adopted      = n_adopted,
      Yes_link     = yes_link_count,
      No_link      = no_link_count
    )
    
    # store product list
    if (is.null(no_link_list[[cty]])) no_link_list[[cty]] <- list()
    no_link_list[[cty]][[prd]] <- products_no_link
  }
}

## 4. Final summary table and product lists ---------------------------------

summary_table_links <- do.call(rbind, summary_list)
summary_table_links

# Example access to products with no link:
# no_link_list[["China"]][["2007_2002"]]
# no_link_list[["Morocco"]][["2017_2012"]]

saveRDS(summary_table_links,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/summary_table_product_space.rds")

########################################################################################
###########################################################################################
library(igraph)

g_ps <- Graph_network   # product space graph

## 1. Build undirected adjacency in product space ---------------------------

n_ps  <- vcount(g_ps)
el_ps <- as_edgelist(g_ps, names = FALSE)

# neighbour_list_ps[[v]] = all neighbours (undirected)
neighbour_list_ps <- vector("list", n_ps)
for (e in seq_len(nrow(el_ps))) {
  i <- el_ps[e, 1]
  j <- el_ps[e, 2]
  neighbour_list_ps[[i]] <- c(neighbour_list_ps[[i]], j)
  neighbour_list_ps[[j]] <- c(neighbour_list_ps[[j]], i)
}

countries <- c("China", "South", "Morocco", "Japan")
periods   <- c("2007_2002", "2012_2007", "2017_2012", "2022_2017")

all_attr_names_ps <- vertex_attr_names(g_ps)
prod_names_ps     <- V(g_ps)$name

results_ps        <- list()

## 2. Loop over all country–period combinations -----------------------------

for (cty in countries) {
  for (prd in periods) {
    
    # delta attribute, e.g. "China2007_2002"
    col_delta <- paste0(cty, prd)
    
    # base year, e.g. "2002" from "2007_2002"
    base_year <- sub(".*_", "", prd)
    
    # base attribute, e.g. "China_2002"
    col_base  <- paste0(cty, "_", base_year)
    
    # skip if attributes do not exist in product space graph
    if (!(col_delta %in% all_attr_names_ps)) next
    if (!(col_base  %in% all_attr_names_ps)) next
    
    delta_attr <- as.numeric(vertex_attr(g_ps, col_delta))
    base_attr  <- as.numeric(vertex_attr(g_ps, col_base))
    
    # adopted products in this period (in product space)
    idx_adopted <- which(delta_attr == 1)
    n_adopted   <- length(idx_adopted)
    
    # if no adopted, still record a row
    if (n_adopted == 0) {
      
      # previously no-link products from original network
      prev_no_vec <- character(0)
      if (!is.null(no_link_products[[cty]]) &&
          !is.null(no_link_products[[cty]][[prd]])) {
        prev_no_vec <- no_link_products[[cty]][[prd]]
      }
      
      # intersect with nodes present in product space
      idx_prev_no_ps <- match(prev_no_vec, prod_names_ps)
      idx_prev_no_ps <- idx_prev_no_ps[!is.na(idx_prev_no_ps)]
      prev_no_total  <- length(idx_prev_no_ps)
      
      results_ps[[length(results_ps) + 1]] <- data.frame(
        Country                  = cty,
        Period                   = prd,
        Adopted                  = 0,
        Yes_link                 = 0,
        No_link                  = 0,
        PrevNoLink_total         = prev_no_total,
        PrevNoLink_with_link_PS  = 0,
        PrevNoLink_no_link_PS    = prev_no_total
      )
      next
    }
    
    # has_link_ps[v] = TRUE if v has at least one neighbour with base_attr == 1
    has_link_ps <- logical(n_ps)
    
    for (v in idx_adopted) {
      neigh <- neighbour_list_ps[[v]]
      if (length(neigh) > 0) {
        has_link_ps[v] <- any(base_attr[neigh] == 1)
      } else {
        has_link_ps[v] <- FALSE
      }
    }
    
    yes_link_count <- sum(has_link_ps[idx_adopted])
    no_link_count  <- sum(!has_link_ps[idx_adopted])
    
    ## 3. Among the products that had NO link in the original network,
    ##    check how many now have a link in the product space ------------
    
    prev_no_vec <- character(0)
    if (!is.null(no_link_products[[cty]]) &&
        !is.null(no_link_products[[cty]][[prd]])) {
      prev_no_vec <- no_link_products[[cty]][[prd]]
    }
    
    # Map these product codes to indices in Graph_network
    idx_prev_no_ps <- match(prev_no_vec, prod_names_ps)
    idx_prev_no_ps <- idx_prev_no_ps[!is.na(idx_prev_no_ps)]
    prev_no_total  <- length(idx_prev_no_ps)
    
    # among these, count how many have link in product space
    if (prev_no_total > 0) {
      prev_no_with_link_ps <- sum(has_link_ps[idx_prev_no_ps])
      prev_no_no_link_ps   <- sum(!has_link_ps[idx_prev_no_ps])
    } else {
      prev_no_with_link_ps <- 0
      prev_no_no_link_ps   <- 0
    }
    
    # store row
    results_ps[[length(results_ps) + 1]] <- data.frame(
      Country                  = cty,
      Period                   = prd,
      Adopted                  = n_adopted,
      Yes_link                 = yes_link_count,
      No_link                  = no_link_count,
      PrevNoLink_total         = prev_no_total,
      PrevNoLink_with_link_PS  = prev_no_with_link_ps,
      PrevNoLink_no_link_PS    = prev_no_no_link_ps
    )
  }
}

## 4. Final summary table for Graph_network ---------------------------------

summary_table_productspace <- do.call(rbind, results_ps)
#rm(list = ls())

summary_table <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/summary_table.rds")

colnames(summary_table)<-c("Country","Period","Yes_upstream","No_upstream","Yes_downstream","No_downstream","No_linkages")

summary_table$Yes_PS<-summary_table_productspace$Yes_link
summary_table$No_PS<-summary_table_productspace$No_link
summary_table$PrevNoLink_total<-summary_table_productspace$PrevNoLink_total
summary_table$No_linkages_yesPS<-summary_table_productspace$PrevNoLink_with_link_PS
summary_table$Fullyunrelated<-summary_table_productspace$PrevNoLink_no_link_PS



library(writexl)

# Write to Excel in your Output folder
write_xlsx(
  summary_table,
  path = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/full_summary_table.xlsx"
)





###############################################################################################################################
###############################################################################################################################

#rm(list = ls())
library(readr)
library(visNetwork)
library("igraph")
library(dplyr)


BACI2022<- read_csv("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501/BACI_HS02_Y2022_V202501.csv",  col_types = cols(i = col_character(), j = col_character()))

#################################################
#Mission1 agreger les resultats
attach(BACI2022)
BACI2022$q=NULL

BACI2022$hs4=substr(BACI2022$k,1,4)
BACI2022$k=NULL
bacii2022_exp = summarise(group_by(BACI2022, hs4, i), v=sum(v))
bacii2022_imp = summarise(group_by(BACI2022, hs4, j), v=sum(v))

bacii2022_exp <- dplyr::rename(bacii2022_exp, country = i)
bacii2022_exp <- dplyr::rename(bacii2022_exp, v_exp = v)

bacii2022_imp <- dplyr::rename(bacii2022_imp, country = j)
bacii2022_imp <- dplyr::rename(bacii2022_imp, v_imp = v)

bacii2022_merged <- merge(
  bacii2022_exp,
  bacii2022_imp,
  by = c("hs4", "country"),
  all = TRUE
)

bacii2022_merged$v_exp[is.na(bacii2022_merged$v_exp)]<-0
bacii2022_merged$v_imp[is.na(bacii2022_merged$v_imp)]<-0
bacii2022_merged$IC_2022<-bacii2022_merged$v_exp/(bacii2022_merged$v_exp+bacii2022_merged$v_imp)

China_IC_2022<-cbind.data.frame("hs4"=bacii2022_merged$hs4[bacii2022_merged$country=="156"],"China_IC_2022"=bacii2022_merged$IC_2022[bacii2022_merged$country=="156"])
South_IC_2022<-cbind.data.frame("hs4"=bacii2022_merged$hs4[bacii2022_merged$country=="710"],"South_IC_2022"=bacii2022_merged$IC_2022[bacii2022_merged$country=="710"])
Morocco_IC_2022<-cbind.data.frame("hs4"=bacii2022_merged$hs4[bacii2022_merged$country=="504"],"Morocco_IC_2022"=bacii2022_merged$IC_2022[bacii2022_merged$country=="504"])
Japan_IC_2022<-cbind.data.frame("hs4"=bacii2022_merged$hs4[bacii2022_merged$country=="392"],"Japan_IC_2022"=bacii2022_merged$IC_2022[bacii2022_merged$country=="392"])

merged_IC_2022 <- Reduce(function(x, y) merge(x, y, by = "hs4", all = TRUE),
                         list(China_IC_2022,
                              South_IC_2022,
                              Morocco_IC_2022,
                              Japan_IC_2022))

library(dplyr)

Full_database_IC_2022 <- Full_database_merged %>%
  left_join(merged_IC_2022, by = c("name" = "hs4"))


saveRDS(Full_database_IC_2022,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Full_database_IC_2022.rds")

#############################################################################################################################
#############################################################################################################################
########################################################
Database_distroot<-cbind.data.frame("Name"=get.vertex.attribute(network)$name, "Dist_to_root"=V(network)$dist_to_root)

#Full_database_merged <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Full_database_merged.rds")

Database_distroot$Name <- as.character(Database_distroot$Name)
Full_database_merged$name <- as.character(Full_database_merged$name)

# Rename for consistency
colnames(Database_distroot)[colnames(Database_distroot) == "Name"] <- "name"


Merged_DB <- merge(
  Database_distroot,
  Full_database_IC_2022,
  by = "name",
  all.x = TRUE   # keep all rows from Database_distroot
)


saveRDS(Merged_DB,
        file = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/Car_Full_database_IC_2022.rds")



Merged_DB$China_IC_2022_exp<-ifelse(Merged_DB$China_IC_2022>0.5,1,0)
Merged_DB$South_IC_2022_exp<-ifelse(Merged_DB$South_IC_2022>0.5,1,0)
Merged_DB$Morocco_IC_2022_exp<-ifelse(Merged_DB$Morocco_IC_2022>0.5,1,0)
Merged_DB$Japan_IC_2022_exp<-ifelse(Merged_DB$Japan_IC_2022>0.5,1,0)



p_china <- ggplot(
  Merged_DB[!is.na(Merged_DB$China_IC_2022_exp),],
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_China,
    color = factor(China_IC_2022_exp, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "China",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "Net Exporter"
  ) +
  theme_minimal()






p_morocco <- ggplot(
  Merged_DB[!is.na(Merged_DB$Morocco_IC_2022_exp),],
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_Morocco,
    color = factor(Morocco_IC_2022_exp, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "Morocco",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "Net Exporter"
  ) +
  theme_minimal()


p_japan <- ggplot(
  Merged_DB[!is.na(Merged_DB$Japan_IC_2022_exp),],
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_Japan,
    color = factor(Japan_IC_2022_exp, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "Japan",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "Net Exporter"
  ) +
  theme_minimal()


p_south <- ggplot(
  Merged_DB[!is.na(Merged_DB$South_IC_2022_exp),],
  aes(
    x = Dist_to_root,
    y = Share_activated_neighbour_South,
    color = factor(South_IC_2022_exp, levels = c(0, 1), labels = c("No", "Yes"))
  )
) +
  geom_point() +
  scale_color_manual(values = c("No" = "grey", "Yes" = "black")) +
  labs(
    title = "South Africa",
    x     = "Distance to root",
    y     = "Product relatedness",
    color = "Net Exporter"
  ) +
  theme_minimal()

gridExtra::grid.arrange(
  p_china, p_morocco,
  p_japan, p_south,
  ncol = 2
)

#rm(list = ls())
load("~/Full_database_to generate_updated_IC.RData")

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

## ---------------------------------------------------------------
## 0. Assumptions
## ---------------------------------------------------------------
## You already have in your environment:
## merged_IC_2002, merged_IC_2007, merged_IC_2012, merged_IC_2017, merged_IC_2022
## each with columns: hs4, China_IC_YYYY, South_IC_YYYY, Morocco_IC_YYYY, Japan_IC_YYYY
## (or at least hs4 + country IC columns)

## ---------------------------------------------------------------
## 1. Put all datasets in a list and ensure hs4 is character
## ---------------------------------------------------------------

datasets <- list(
  "2002" = merged_IC_2002,
  "2007" = merged_IC_2007,
  "2012" = merged_IC_2012,
  "2017" = merged_IC_2017,
  "2022" = merged_IC_2022
)

datasets <- lapply(datasets, function(df) {
  df$hs4 <- as.character(df$hs4)
  df
})

## ---------------------------------------------------------------
## 2. Rename IC columns to be year specific
##    e.g. China_IC_2002, China_IC_2007, etc.
## ---------------------------------------------------------------

rename_with_year <- function(df, year) {
  cols_to_rename <- setdiff(names(df), "hs4")
  names(df)[names(df) != "hs4"] <- paste0(cols_to_rename, "_", year)
  df
}

merged_IC_2002 <- rename_with_year(datasets[["2002"]], "2002")
merged_IC_2007 <- rename_with_year(datasets[["2007"]], "2007")
merged_IC_2012 <- rename_with_year(datasets[["2012"]], "2012")
merged_IC_2017 <- rename_with_year(datasets[["2017"]], "2017")
merged_IC_2022 <- rename_with_year(datasets[["2022"]], "2022")

## ---------------------------------------------------------------
## 3. Merge all years by hs4
## ---------------------------------------------------------------

merged_IC_all <- Reduce(
  function(x, y) merge(x, y, by = "hs4", all = TRUE),
  list(merged_IC_2002,
       merged_IC_2007,
       merged_IC_2012,
       merged_IC_2017,
       merged_IC_2022)
)

colnames(merged_IC_all)<-c("hs4","China_IC_2002","South_IC_2002","Morocco_IC_2002","Japan_IC_2002",
                           "China_IC_2007","South_IC_2007","Morocco_IC_2007","Japan_IC_2007","China_IC_2012",  
                           "South_IC_2012","Morocco_IC_2012","Japan_IC_2012","China_IC_2017","South_IC_2017",
                           "Morocco_IC_2017","Japan_IC_2017","China_IC_2022","South_IC_2022","Morocco_IC_2022",
                           "Japan_IC_2022")


## ---------------------------------------------------------------
## 4. (Optional) Merge with Full_database_merged by product code
##    Full_database_merged$name matches merged_IC_all$hs4
## ---------------------------------------------------------------

## Ensure types are compatible
Full_database_merged$name <- as.character(Full_database_merged$name)

Full_database_IC_all <- merge(
  Full_database_merged,
  merged_IC_all,
  by.x = "name",
  by.y = "hs4",
  all.x = TRUE
)

## ---------------------------------------------------------------
## 5. Save to disk
## ---------------------------------------------------------------

# saveRDS(merged_IC_all, "merged_IC_all.rds")
 saveRDS(Full_database_IC_all, "Full_database_IC_all.rds")

Full_database_IC_all$China_IC_2022<-Full_database_IC_all$China_IC_2002_2002
Full_database_IC_all$China_IC_2022_2022<-NULL
Full_database_IC_all$Japan_IC_2022<-Full_database_IC_all$Japan_IC_2022_2022
Full_database_IC_all$Japan_IC_2022_2022<-NULL
Full_database_IC_all$Morocco_IC_2022<-Full_database_IC_all$Morocco_IC_2022_2022
Full_database_IC_all$Morocco_IC_2022_2022<-NULL
Full_database_IC_all$South_IC_2022<-Full_database_IC_all$South_IC_2022_2022
Full_database_IC_all$South_IC_2022_2022<-NULL

Full_database_IC_all$China_IC_2017<-Full_database_IC_all$China_IC_2002_2002
Full_database_IC_all$China_IC_2017_2017<-NULL
Full_database_IC_all$Japan_IC_2017<-Full_database_IC_all$Japan_IC_2017_2017
Full_database_IC_all$Japan_IC_2017_2017<-NULL
Full_database_IC_all$Morocco_IC_2017<-Full_database_IC_all$Morocco_IC_2017_2017
Full_database_IC_all$Morocco_IC_2017_2017<-NULL
Full_database_IC_all$South_IC_2017<-Full_database_IC_all$South_IC_2017_2017
Full_database_IC_all$South_IC_2017_2017<-NULL

Full_database_IC_all$China_IC_2012<-Full_database_IC_all$China_IC_2002_2002
Full_database_IC_all$China_IC_2012_2012<-NULL
Full_database_IC_all$Japan_IC_2012<-Full_database_IC_all$Japan_IC_2012_2012
Full_database_IC_all$Japan_IC_2012_2012<-NULL
Full_database_IC_all$Morocco_IC_2012<-Full_database_IC_all$Morocco_IC_2012_2012
Full_database_IC_all$Morocco_IC_2012_2012<-NULL
Full_database_IC_all$South_IC_2012<-Full_database_IC_all$South_IC_2012_2012
Full_database_IC_all$South_IC_2012_2012<-NULL

Full_database_IC_all$China_IC_2007<-Full_database_IC_all$China_IC_2002_2002
Full_database_IC_all$China_IC_2007_2007<-NULL
Full_database_IC_all$Japan_IC_2007<-Full_database_IC_all$Japan_IC_2007_2007
Full_database_IC_all$Japan_IC_2007_2007<-NULL
Full_database_IC_all$Morocco_IC_2007<-Full_database_IC_all$Morocco_IC_2007_2007
Full_database_IC_all$Morocco_IC_2007_2007<-NULL
Full_database_IC_all$South_IC_2007<-Full_database_IC_all$South_IC_2007_2007
Full_database_IC_all$South_IC_2007_2007<-NULL

Full_database_IC_all$China_IC_2002<-Full_database_IC_all$China_IC_2002_2002
Full_database_IC_all$China_IC_2002_2002<-NULL
Full_database_IC_all$Japan_IC_2002<-Full_database_IC_all$Japan_IC_2002_2002
Full_database_IC_all$Japan_IC_2002_2002<-NULL
Full_database_IC_all$Morocco_IC_2002<-Full_database_IC_all$Morocco_IC_2002_2002
Full_database_IC_all$Morocco_IC_2002_2002<-NULL
Full_database_IC_all$South_IC_2002<-Full_database_IC_all$South_IC_2002_2002
Full_database_IC_all$South_IC_2002_2002<-NULL

#save.image("~/Full_database_to generate_updated_IC.RData")

Full_database_IC_all$South_IC_2002bin<- ifelse(Full_database_IC_all$South_IC_2002>0.5,1,0)
Full_database_IC_all$Morocco_IC_2002bin<- ifelse(Full_database_IC_all$Morocco_IC_2002>0.5,1,0)
Full_database_IC_all$Japan_IC_2002bin<- ifelse(Full_database_IC_all$Japan_IC_2002>0.5,1,0)
Full_database_IC_all$China_IC_2002bin<- ifelse(Full_database_IC_all$China_IC_2002>0.5,1,0)

Full_database_IC_all$South_IC_2007bin<- ifelse(Full_database_IC_all$South_IC_2007>0.5,1,0)
Full_database_IC_all$Morocco_IC_2007bin<- ifelse(Full_database_IC_all$Morocco_IC_2007>0.5,1,0)
Full_database_IC_all$Japan_IC_2007bin<- ifelse(Full_database_IC_all$Japan_IC_2007>0.5,1,0)
Full_database_IC_all$China_IC_2007bin<- ifelse(Full_database_IC_all$China_IC_2007>0.5,1,0)

Full_database_IC_all$South_IC_2012bin<- ifelse(Full_database_IC_all$South_IC_2012>0.5,1,0)
Full_database_IC_all$Morocco_IC_2012bin<- ifelse(Full_database_IC_all$Morocco_IC_2012>0.5,1,0)
Full_database_IC_all$Japan_IC_2012bin<- ifelse(Full_database_IC_all$Japan_IC_2012>0.5,1,0)
Full_database_IC_all$China_IC_2012bin<- ifelse(Full_database_IC_all$China_IC_2012>0.5,1,0)

Full_database_IC_all$South_IC_2017bin<- ifelse(Full_database_IC_all$South_IC_2017>0.5,1,0)
Full_database_IC_all$Morocco_IC_2017bin<- ifelse(Full_database_IC_all$Morocco_IC_2017>0.5,1,0)
Full_database_IC_all$Japan_IC_2017bin<- ifelse(Full_database_IC_all$Japan_IC_2017>0.5,1,0)
Full_database_IC_all$China_IC_2017bin<- ifelse(Full_database_IC_all$China_IC_2017>0.5,1,0)

Full_database_IC_all$South_IC_2022bin<- ifelse(Full_database_IC_all$South_IC_2022>0.5,1,0)
Full_database_IC_all$Morocco_IC_2022bin<- ifelse(Full_database_IC_all$Morocco_IC_2022>0.5,1,0)
Full_database_IC_all$Japan_IC_2022bin<- ifelse(Full_database_IC_all$Japan_IC_2022>0.5,1,0)
Full_database_IC_all$China_IC_2022bin<- ifelse(Full_database_IC_all$China_IC_2022>0.5,1,0)
#saveRDS(Full_database_IC_all, "Full_database_IC_all.rds")



library(igraph)

# 1. Prepare keys
Full_database_IC_all$name <- as.character(Full_database_IC_all$name)
V(Cor_Dir_network)$name   <- as.character(V(Cor_Dir_network)$name)

# 2. Match rows in the table to graph vertices by name
#    idx[i] = row index in Full_database_IC_all corresponding to vertex i
idx <- match(V(Cor_Dir_network)$name, Full_database_IC_all$name)

# Optional sanity check
# Any NA in idx means that some vertices are not in the table
# print(V(Cor_Dir_network)$name[is.na(idx)])

# 3. List attributes to add (everything except 'name')
attr_list <- setdiff(colnames(Full_database_IC_all), "name")

# 4. Assign attributes to graph vertices
for (att in attr_list) {
  vals <- Full_database_IC_all[[att]][idx]  # vector aligned with vertex order
  Cor_Dir_network <- set_vertex_attr(
    Cor_Dir_network,
    name  = att,
    value = vals
  )
}

saveRDS(Cor_Dir_network, "Cor_Dir_network_full_network.rds")




V(Cor_Dir_network)$China_IC_2002bin<- ifelse(V(Cor_Dir_network)$China_IC_2002>0.5,1,0)
V(Cor_Dir_network)$China_IC_2007bin<- ifelse(V(Cor_Dir_network)$China_IC_2007>0.5,1,0)
V(Cor_Dir_network)$China_IC_2012bin<- ifelse(V(Cor_Dir_network)$China_IC_2012>0.5,1,0)
V(Cor_Dir_network)$China_IC_2017bin<- ifelse(V(Cor_Dir_network)$China_IC_2017>0.5,1,0)


titi<-V(Cor_Dir_network)$South_IC_2022bin-V(Cor_Dir_network)$South_IC_2017bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$South_IC_2022_2017<-titi

titi<-V(Cor_Dir_network)$Morocco_IC_2022bin-V(Cor_Dir_network)$Morocco_IC_2017bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Morocco_IC_2022_2017<-titi


titi<-V(Cor_Dir_network)$Japan_IC_2022bin-V(Cor_Dir_network)$Japan_IC_2017bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Japan_IC_2022_2017<-titi


titi<-V(Cor_Dir_network)$China_IC_2022bin-V(Cor_Dir_network)$China_IC_2017bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$China_IC_2022_2017<-titi







titi<-V(Cor_Dir_network)$South_IC_2017bin-V(Cor_Dir_network)$South_IC_2012bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$South_IC_2017_2012<-titi

titi<-V(Cor_Dir_network)$Morocco_IC_2017bin-V(Cor_Dir_network)$Morocco_IC_2012bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Morocco_IC_2017_2012<-titi


titi<-V(Cor_Dir_network)$Japan_IC_2017bin-V(Cor_Dir_network)$Japan_IC_2012bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Japan_IC_2017_2012<-titi

#saveRDS(Cor_Dir_network, "Cor_Dir_network_full_network.rds")

titi<-V(Cor_Dir_network)$South_IC_2012bin-V(Cor_Dir_network)$South_IC_2007bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$South_IC_2012_2007<-titi

titi<-V(Cor_Dir_network)$Morocco_IC_2012bin-V(Cor_Dir_network)$Morocco_IC_2007bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Morocco_IC_2012_2007<-titi


titi<-V(Cor_Dir_network)$Japan_IC_2012bin-V(Cor_Dir_network)$Japan_IC_2007bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Japan_IC_2012_2007<-titi

#saveRDS(Cor_Dir_network, "Cor_Dir_network_full_network.rds")

titi<-V(Cor_Dir_network)$South_IC_2007bin-V(Cor_Dir_network)$South_IC_2002bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$South_IC_2007_2002<-titi

titi<-V(Cor_Dir_network)$Morocco_IC_2007bin-V(Cor_Dir_network)$Morocco_IC_2002bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Morocco_IC_2007_2002<-titi


titi<-V(Cor_Dir_network)$Japan_IC_2007bin-V(Cor_Dir_network)$Japan_IC_2002bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$Japan_IC_2007_2002<-titi



titi<-V(Cor_Dir_network)$China_IC_2007bin-V(Cor_Dir_network)$China_IC_2002bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$China_IC_2007_2002<-titi

titi<-V(Cor_Dir_network)$China_IC_2012bin-V(Cor_Dir_network)$China_IC_2007bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$China_IC_2012_2007<-titi

titi<-V(Cor_Dir_network)$China_IC_2017bin-V(Cor_Dir_network)$China_IC_2012bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$China_IC_2017_2012<-titi

titi<-V(Cor_Dir_network)$China_IC_2022bin-V(Cor_Dir_network)$China_IC_2017bin
titi<-ifelse(titi==1,1,0)
titi[is.na(titi)]<-0
V(Cor_Dir_network)$China_IC_2022_2017<-titi

saveRDS(Cor_Dir_network, "Cor_Dir_network_full_network.rds")


###########################################################################################
#########################Build the table
library(igraph)


V(Cor_Dir_network)$name <- as.character(V(Cor_Dir_network)$name)

library(igraph)

# 1. Make sure keys are character
Full_database_IC_all$name <- as.character(Full_database_IC_all$name)
V(Graph_network)$name     <- as.character(V(Graph_network)$name)

# 2. For each vertex, find the corresponding row in Full_database_IC_all
idx <- match(V(Graph_network)$name, Full_database_IC_all$name)
# idx has length vcount(Graph_network); can contain NA for unmatched products

# Optional: see which vertices have no data
# V(Graph_network)$name[is.na(idx)]

# 3. List attributes to add (everything except 'name')
attr_list <- setdiff(colnames(Full_database_IC_all), "name")

# 4. Assign attributes as vertex attributes
for (att in attr_list) {
  vals <- Full_database_IC_all[[att]][idx]  # length == vcount(Graph_network)
  Graph_network <- set_vertex_attr(
    Graph_network,
    name  = att,
    value = vals
  )
}






library(igraph)

## --------------------------------------------------
## 1. Graphs
## --------------------------------------------------

g_ps <- Graph_network      # product space
g_sc <- Cor_Dir_network    # supply chain (directed, but we will ignore direction)

V(g_ps)$name <- as.character(V(g_ps)$name)
V(g_sc)$name <- as.character(V(g_sc)$name)

## --------------------------------------------------
## 2. Build undirected neighbour lists
## --------------------------------------------------

## 2.1 Product space
el_ps <- as_edgelist(g_ps, names = FALSE)
n_ps  <- vcount(g_ps)

neigh_ps <- vector("list", n_ps)
for (e in seq_len(nrow(el_ps))) {
  i <- el_ps[e, 1]
  j <- el_ps[e, 2]
  neigh_ps[[i]] <- c(neigh_ps[[i]], j)
  neigh_ps[[j]] <- c(neigh_ps[[j]], i)
}

## 2.2 Supply chain (ignore direction)
el_sc <- as_edgelist(g_sc, names = FALSE)
n_sc  <- vcount(g_sc)

neigh_sc <- vector("list", n_sc)
for (e in seq_len(nrow(el_sc))) {
  i <- el_sc[e, 1]
  j <- el_sc[e, 2]
  neigh_sc[[i]] <- c(neigh_sc[[i]], j)
  neigh_sc[[j]] <- c(neigh_sc[[j]], i)
}

## --------------------------------------------------
## 3. Countries and years (RCA dummies)
## --------------------------------------------------

countries <- c("China", "South", "Morocco", "Japan")
years     <- c("2002", "2007", "2012", "2017", "2022")

attr_ps <- vertex_attr_names(g_ps)
attr_sc <- vertex_attr_names(g_sc)

## --------------------------------------------------
## 4. Loop: for each country × year, count neighbours
## --------------------------------------------------

for (cty in countries) {
  for (yr in years) {
    
    base_col <- paste0(cty, "_", yr)   # e.g. "China_2002"
    
    ## ---------- Product space ----------
    if (base_col %in% attr_ps) {
      base_ps <- as.numeric(vertex_attr(g_ps, base_col))
      base_ps[is.na(base_ps)] <- 0
      
      count_ps <- integer(n_ps)
      for (v in seq_len(n_ps)) {
        nb <- neigh_ps[[v]]
        if (length(nb) > 0) {
          count_ps[v] <- sum(base_ps[nb] == 1)
        }
      }
      
      new_att_ps <- paste0(base_col, "_PS_neigh_count")
      g_ps <- set_vertex_attr(g_ps, new_att_ps, value = count_ps)
    }
    
    ## ---------- Supply chain ----------
    if (base_col %in% attr_sc) {
      base_sc <- as.numeric(vertex_attr(g_sc, base_col))
      base_sc[is.na(base_sc)] <- 0
      
      count_sc <- integer(n_sc)
      for (v in seq_len(n_sc)) {
        nb <- neigh_sc[[v]]
        if (length(nb) > 0) {
          count_sc[v] <- sum(base_sc[nb] == 1)
        }
      }
      
      new_att_sc <- paste0(base_col, "_SC_neigh_count")
      g_sc <- set_vertex_attr(g_sc, new_att_sc, value = count_sc)
    }
  }
}

## --------------------------------------------------
## 5. Quick checks
## --------------------------------------------------

# Example: China 2002 neighbour counts in product space vs supply chain
head(vertex_attr(g_ps, "China_2002_PS_neigh_count"))
head(vertex_attr(g_sc, "China_2002_SC_neigh_count"))



vertex_attr_names(g_ps)
grep("neigh", vertex_attr_names(g_ps), value = TRUE)

library(igraph)

g_ps <- g_ps              # product space, already has *_PS_neigh_count
g_sc <- Cor_Dir_network   # supply chain

V(g_ps)$name <- as.character(V(g_ps)$name)
V(g_sc)$name <- as.character(V(g_sc)$name)

# Map each supply-chain node to its index in the product-space graph
idx_ps_for_sc <- match(V(g_sc)$name, V(g_ps)$name)

nodes_sc_df <- as_data_frame(g_sc, what = "vertices")
ps_neigh_attrs <- grep("_PS_neigh_count$", vertex_attr_names(g_ps), value = TRUE)

for (att in ps_neigh_attrs) {
  
  # values on the product-space graph (one per node in g_ps)
  vals_ps <- vertex_attr(g_ps, att)
  
  # vector aligned with supply-chain nodes (one per node in g_sc)
  vals_sc <- rep(NA_real_, vcount(g_sc))
  valid   <- which(!is.na(idx_ps_for_sc))
  vals_sc[valid] <- vals_ps[idx_ps_for_sc[valid]]
  
  # add as new column in the supply-chain node dataframe
  nodes_sc_df[[att]] <- vals_sc
}


nodes_sc_df$China2022_2017[is.na(nodes_sc_df$China2022_2017)]<-0
nodes_sc_df$Benefited_from_PS_China_2022_2017<-nodes_sc_df$China2022_2017*ifelse(nodes_sc_df$China_2017_PS_neigh_count>0,1,0)

nodes_sc_df$South2022_2017[is.na(nodes_sc_df$South2022_2017)]<-0
nodes_sc_df$Benefited_from_PS_South_2022_2017<-nodes_sc_df$South2022_2017*ifelse(nodes_sc_df$South_2017_PS_neigh_count>0,1,0)

nodes_sc_df$Japan2022_2017[is.na(nodes_sc_df$Japan2022_2017)]<-0
nodes_sc_df$Benefited_from_PS_Japan_2022_2017<-nodes_sc_df$Japan2022_2017*ifelse(nodes_sc_df$Japan_2017_PS_neigh_count>0,1,0)

nodes_sc_df$Morocco2022_2017[is.na(nodes_sc_df$Morocco2022_2017)]<-0
nodes_sc_df$Benefited_from_PS_Morocco2022_2017<-nodes_sc_df$Morocco2022_2017*ifelse(nodes_sc_df$Morocco_2017_PS_neigh_count>0,1,0)


nodes_sc_df$China2017_2012[is.na(nodes_sc_df$China2017_2012)]<-0
nodes_sc_df$Benefited_from_PS_China_2017_2012<-nodes_sc_df$China2017_2012*ifelse(nodes_sc_df$China_2012_PS_neigh_count>0,1,0)

nodes_sc_df$South2017_2012[is.na(nodes_sc_df$South2017_2012)]<-0
nodes_sc_df$Benefited_from_PS_South_2017_2012<-nodes_sc_df$South2017_2012*ifelse(nodes_sc_df$South_2012_PS_neigh_count>0,1,0)

nodes_sc_df$Japan2017_2012[is.na(nodes_sc_df$Japan2017_2012)]<-0
nodes_sc_df$Benefited_from_PS_Japan_2017_2012<-nodes_sc_df$Japan2017_2012*ifelse(nodes_sc_df$Japan_2012_PS_neigh_count>0,1,0)

nodes_sc_df$Morocco2017_2012[is.na(nodes_sc_df$Morocco2017_2012)]<-0
nodes_sc_df$Benefited_from_PS_Morocco2017_2012<-nodes_sc_df$Morocco2017_2012*ifelse(nodes_sc_df$Morocco_2012_PS_neigh_count>0,1,0)

nodes_sc_df$China2012_2007[is.na(nodes_sc_df$China2012_2007)]<-0
nodes_sc_df$Benefited_from_PS_China_2012_2007<-nodes_sc_df$China2012_2007*ifelse(nodes_sc_df$China_2007_PS_neigh_count>0,1,0)

nodes_sc_df$South2012_2007[is.na(nodes_sc_df$South2012_2007)]<-0
nodes_sc_df$Benefited_from_PS_South_2012_2007<-nodes_sc_df$South2012_2007*ifelse(nodes_sc_df$South_2007_PS_neigh_count>0,1,0)

nodes_sc_df$Japan2012_2007[is.na(nodes_sc_df$Japan2012_2007)]<-0
nodes_sc_df$Benefited_from_PS_Japan_2012_2007<-nodes_sc_df$Japan2012_2007*ifelse(nodes_sc_df$Japan_2007_PS_neigh_count>0,1,0)

nodes_sc_df$Morocco2012_2007[is.na(nodes_sc_df$Morocco2012_2007)]<-0
nodes_sc_df$Benefited_from_PS_Morocco2012_2007<-nodes_sc_df$Morocco2012_2007*ifelse(nodes_sc_df$Morocco_2007_PS_neigh_count>0,1,0)

nodes_sc_df$China2007_2002[is.na(nodes_sc_df$China2007_2002)]<-0
nodes_sc_df$Benefited_from_PS_China_2007_2002<-nodes_sc_df$China2007_2002*ifelse(nodes_sc_df$China_2002_PS_neigh_count>0,1,0)

nodes_sc_df$South2007_2002[is.na(nodes_sc_df$South2007_2002)]<-0
nodes_sc_df$Benefited_from_PS_South_2007_2002<-nodes_sc_df$South2007_2002*ifelse(nodes_sc_df$South_2002_PS_neigh_count>0,1,0)

nodes_sc_df$Japan2007_2002[is.na(nodes_sc_df$Japan2007_2002)]<-0
nodes_sc_df$Benefited_from_PS_Japan_2007_2002<-nodes_sc_df$Japan2007_2002*ifelse(nodes_sc_df$Japan_2002_PS_neigh_count>0,1,0)

nodes_sc_df$Morocco2007_2002[is.na(nodes_sc_df$Morocco2007_2002)]<-0
nodes_sc_df$Benefited_from_PS_Morocco2007_2002<-nodes_sc_df$Morocco2007_2002*ifelse(nodes_sc_df$Morocco_2002_PS_neigh_count>0,1,0)

saveRDS(nodes_sc_df, "nodes_sc_df.rds")

################################################################################
################################################################################
################################################################################
################################################################################
library(igraph)

g_sc <- Cor_Dir_network
V(g_sc)$name <- as.character(V(g_sc)$name)

# Build undirected neighbours
el_sc <- as_edgelist(g_sc, names = FALSE)
n_sc  <- vcount(g_sc)
neigh_sc <- vector("list", n_sc)

for (e in seq_len(nrow(el_sc))) {
  i <- el_sc[e, 1]
  j <- el_sc[e, 2]
  neigh_sc[[i]] <- c(neigh_sc[[i]], j)
  neigh_sc[[j]] <- c(neigh_sc[[j]], i)
}

countries <- c("China","South","Morocco","Japan")
years     <- c("2002","2007","2012","2017","2022")

for (cty in countries) {
  for (yr in years) {
    
    base_col <- paste0(cty, "_", yr)
    if (!(base_col %in% vertex_attr_names(g_sc))) next
    
    base_vec <- vertex_attr(g_sc, base_col)
    base_vec[is.na(base_vec)] <- 0
    
    count_sc <- integer(n_sc)
    for (v in seq_len(n_sc)) {
      nb <- neigh_sc[[v]]
      if (length(nb) > 0) count_sc[v] <- sum(base_vec[nb] == 1)
    }
    
    new_att <- paste0(base_col, "_SC_neigh_count")
    
    g_sc <- set_vertex_attr(
      graph = g_sc,
      name  = new_att,
      value = count_sc
    )
  }
}

Cor_Dir_network <- g_sc


new_sc_attrs <- grep("_SC_neigh_count$", vertex_attr_names(Cor_Dir_network), value = TRUE)

for (att in new_sc_attrs) {
  nodes_sc_df[[att]] <- vertex_attr(Cor_Dir_network, att)
}




nodes_sc_df$Benefited_from_SC_China_2022_2017<-nodes_sc_df$China2022_2017*ifelse(nodes_sc_df$China_2017_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_South_2022_2017<-nodes_sc_df$South2022_2017*ifelse(nodes_sc_df$South_2017_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Japan_2022_2017<-nodes_sc_df$Japan2022_2017*ifelse(nodes_sc_df$Japan_2017_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Morocco2022_2017<-nodes_sc_df$Morocco2022_2017*ifelse(nodes_sc_df$Morocco_2017_SC_neigh_count>0,1,0)

nodes_sc_df$Benefited_from_SC_China_2017_2012<-nodes_sc_df$China2017_2012*ifelse(nodes_sc_df$China_2012_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_South_2017_2012<-nodes_sc_df$South2017_2012*ifelse(nodes_sc_df$South_2012_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Japan_2017_2012<-nodes_sc_df$Japan2017_2012*ifelse(nodes_sc_df$Japan_2012_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Morocco2017_2012<-nodes_sc_df$Morocco2017_2012*ifelse(nodes_sc_df$Morocco_2012_SC_neigh_count>0,1,0)

nodes_sc_df$Benefited_from_SC_China_2012_2007<-nodes_sc_df$China2012_2007*ifelse(nodes_sc_df$China_2007_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_South_2012_2007<-nodes_sc_df$South2012_2007*ifelse(nodes_sc_df$South_2007_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Japan_2012_2007<-nodes_sc_df$Japan2012_2007*ifelse(nodes_sc_df$Japan_2007_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Morocco2012_2007<-nodes_sc_df$Morocco2012_2007*ifelse(nodes_sc_df$Morocco_2007_SC_neigh_count>0,1,0)

nodes_sc_df$Benefited_from_SC_China_2007_2002<-nodes_sc_df$China2007_2002*ifelse(nodes_sc_df$China_2002_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_South_2007_2002<-nodes_sc_df$South2007_2002*ifelse(nodes_sc_df$South_2002_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Japan_2007_2002<-nodes_sc_df$Japan2007_2002*ifelse(nodes_sc_df$Japan_2002_SC_neigh_count>0,1,0)
nodes_sc_df$Benefited_from_SC_Morocco2007_2002<-nodes_sc_df$Morocco2007_2002*ifelse(nodes_sc_df$Morocco_2002_SC_neigh_count>0,1,0)

countries <- c("China", "South", "Morocco", "Japan")
periods   <- c("2007_2002", "2012_2007", "2017_2012", "2022_2017")

for (cty in countries) {
  for (prd in periods) {
    
    # diversification dummy, e.g. "Morocco2007_2002"
    delta_col <- paste0(cty, prd)
    if (!delta_col %in% names(nodes_sc_df)) next
    
    base_year    <- sub(".*_", "", prd)  # "2002" from "2007_2002"
    ps_neigh_col <- paste0(cty, "_", base_year, "_PS_neigh_count")
    sc_neigh_col <- paste0(cty, "_", base_year, "_SC_neigh_count")
    
    if (!(ps_neigh_col %in% names(nodes_sc_df))) next
    if (!(sc_neigh_col %in% names(nodes_sc_df))) next
    
    # ensure no NA in diversification dummy
    nodes_sc_df[[delta_col]][is.na(nodes_sc_df[[delta_col]])] <- 0
    
    ps_flag <- ifelse(nodes_sc_df[[ps_neigh_col]] > 0, 1, 0)
    sc_flag <- ifelse(nodes_sc_df[[sc_neigh_col]] > 0, 1, 0)
    
    ben_ps_col <- paste0("Benefited_from_PS_", cty, prd)
    ben_sc_col <- paste0("Benefited_from_SC_", cty, prd)
    
    nodes_sc_df[[ben_ps_col]] <- nodes_sc_df[[delta_col]] * ps_flag
    nodes_sc_df[[ben_sc_col]] <- nodes_sc_df[[delta_col]] * sc_flag
  }
}



results <- list()

for (cty in countries) {
  for (prd in periods) {
    
    delta_col  <- paste0(cty, prd)
    ben_sc_col <- paste0("Benefited_from_SC_", cty, prd)
    ben_ps_col <- paste0("Benefited_from_PS_", cty, prd)
    
    if (!all(c(delta_col, ben_sc_col, ben_ps_col) %in% names(nodes_sc_df))) next
    
    delta  <- nodes_sc_df[[delta_col]]
    ben_sc <- nodes_sc_df[[ben_sc_col]]
    ben_ps <- nodes_sc_df[[ben_ps_col]]
    
    delta[is.na(delta)]   <- 0
    ben_sc[is.na(ben_sc)] <- 0
    ben_ps[is.na(ben_ps)] <- 0
    
    New_products <- sum(delta == 1, na.rm = TRUE)
    
    SC_only  <- sum(ben_sc == 1 & ben_ps == 0, na.rm = TRUE)
    PS_only  <- sum(ben_sc == 0 & ben_ps == 1, na.rm = TRUE)
    Both     <- sum(ben_sc == 1 & ben_ps == 1, na.rm = TRUE)
    Neither  <- sum(delta == 1 & ben_sc == 0 & ben_ps == 0, na.rm = TRUE)
    
    results[[length(results) + 1]] <- data.frame(
      Country           = cty,
      Period            = prd,
      New_products      = New_products,
      SC_only           = SC_only,
      PS_only           = PS_only,
      Both_SC_and_PS    = Both,
      Neither_SC_nor_PS = Neither
    )
  }
}

summary_SC_PS_all <- do.call(rbind, results)
summary_SC_PS_all
saveRDS(summary_SC_PS_all, "summary_SC_PS_all.rds")

library(writexl)

# Write to Excel in your Output folder
write_xlsx(
  summary_SC_PS_all,
  path = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/full_summary_table_summary_SC_PS_all.xlsx"
)


###########################Untapped 
countries <- c("China", "South", "Morocco", "Japan")
years     <- c("2002", "2007", "2012", "2017", "2022")

# 1. Node-level variables: "untapped despite SC+PS"
for (cty in countries) {
  for (yr in years) {
    
    base_col <- paste0(cty, "_", yr)                 # e.g. "China_2007"
    sc_col   <- paste0(base_col, "_SC_neigh_count")  # "China_2007_SC_neigh_count"
    ps_col   <- paste0(base_col, "_PS_neigh_count")  # "China_2007_PS_neigh_count"
    
    if (!all(c(base_col, sc_col, ps_col) %in% names(nodes_sc_df))) next
    
    base_vec <- nodes_sc_df[[base_col]]
    base_vec[is.na(base_vec)] <- 0                   # treat NA as not exported
    
    sc_flag <- nodes_sc_df[[sc_col]] > 0
    ps_flag <- nodes_sc_df[[ps_col]] > 0
    
    new_col <- paste0("Untapped_with_SC_PS_", cty, "_", yr)
    
    # 1 if not exported but has both SC and PS neighbours with RCA>1
    nodes_sc_df[[new_col]] <- ifelse(
      base_vec == 0 & sc_flag & ps_flag,
      1, 0
    )
  }
}

# 2. Summary table: count per country × year
results_untapped <- list()

for (cty in countries) {
  for (yr in years) {
    
    new_col <- paste0("Untapped_with_SC_PS_", cty, "_", yr)
    base_col <- paste0(cty, "_", yr)
    
    if (!all(c(new_col, base_col) %in% names(nodes_sc_df))) next
    
    base_vec <- nodes_sc_df[[base_col]]
    base_vec[is.na(base_vec)] <- 0
    
    untapped <- nodes_sc_df[[new_col]]
    untapped[is.na(untapped)] <- 0
    
    total_not_exported <- sum(base_vec == 0, na.rm = TRUE)
    n_untapped         <- sum(untapped == 1, na.rm = TRUE)
    
    results_untapped[[length(results_untapped) + 1]] <- data.frame(
      Country              = cty,
      Year                 = yr,
      Not_exported         = total_not_exported,
      Untapped_with_SC_PS  = n_untapped
    )
  }
}

saveRDS(summary_untapped_SC_PS, "summary_untapped_SC_PSdf.rds")


saveRDS(summary_untapped_SC_PS, "summary_untapped_SC_PSdf.rds")


# Write to Excel in your Output folder
write_xlsx(
  summary_untapped_SC_PS,
  path = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/full_summary_table_summary_untapped_SC_PS.xlsx"
)



countries <- c("China", "South", "Morocco", "Japan")
years     <- c("2002", "2007", "2012", "2017", "2022")

# Create node-level PS/SC-only/both/neither indicators for not-exported products
for (cty in countries) {
  for (yr in years) {
    
    base_col <- paste0(cty, "_", yr)                 # e.g. "China_2007"
    sc_col   <- paste0(base_col, "_SC_neigh_count")  # e.g. "China_2007_SC_neigh_count"
    ps_col   <- paste0(base_col, "_PS_neigh_count")  # e.g. "China_2007_PS_neigh_count"
    
    if (!all(c(base_col, sc_col, ps_col) %in% names(nodes_sc_df))) next
    
    base_vec <- nodes_sc_df[[base_col]]
    base_vec[is.na(base_vec)] <- 0
    
    sc_flag <- nodes_sc_df[[sc_col]] > 0
    ps_flag <- nodes_sc_df[[ps_col]] > 0
    
    # a product is "not exported" if RCA ≤ 1 → value == 0
    not_exported <- base_vec == 0
    
    prefix <- paste0(cty, "_", yr, "_")
    
    nodes_sc_df[[paste0(prefix, "PS_only_not_exported")]]     <- ifelse(not_exported & ps_flag & !sc_flag, 1, 0)
    nodes_sc_df[[paste0(prefix, "SC_only_not_exported")]]     <- ifelse(not_exported & sc_flag & !ps_flag, 1, 0)
    nodes_sc_df[[paste0(prefix, "Both_SC_PS_not_exported")]]  <- ifelse(not_exported & sc_flag & ps_flag, 1, 0)
    nodes_sc_df[[paste0(prefix, "Neither_SC_PS_not_exported")]] <- ifelse(not_exported & !sc_flag & !ps_flag, 1, 0)
  }
}




results_not_exported <- list()

for (cty in countries) {
  for (yr in years) {
    
    prefix <- paste0(cty, "_", yr, "_")
    
    cols_needed <- paste0(prefix, c(
      "PS_only_not_exported",
      "SC_only_not_exported",
      "Both_SC_PS_not_exported",
      "Neither_SC_PS_not_exported"
    ))
    
    if (!all(cols_needed %in% names(nodes_sc_df))) next
    
    df_sub <- nodes_sc_df[, cols_needed]
    
    results_not_exported[[length(results_not_exported) + 1]] <- data.frame(
      Country              = cty,
      Year                 = yr,
      PS_only_not_exported     = sum(df_sub[[1]], na.rm = TRUE),
      SC_only_not_exported     = sum(df_sub[[2]], na.rm = TRUE),
      Both_SC_PS_not_exported  = sum(df_sub[[3]], na.rm = TRUE),
      Neither_SC_PS_not_exported = sum(df_sub[[4]], na.rm = TRUE)
    )
  }
}

summary_not_exported <- do.call(rbind, results_not_exported)
summary_not_exported
# Write to Excel in your Output folder
write_xlsx(
  summary_not_exported,
  path = "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/full_summary_table_summary_not_exported.xlsx"
)



library(dplyr)
library(tidyr)
library(ggplot2)

plot_df <- summary_not_exported %>%
  pivot_longer(
    cols = c(
      PS_only_not_exported,
      SC_only_not_exported,
      Both_SC_PS_not_exported,
      Neither_SC_PS_not_exported
    ),
    names_to = "Type",
    values_to = "Count"
  )


plot_df$Type <- factor(
  plot_df$Type,
  levels = c(
    "Both_SC_PS_not_exported",     # bottom
    "PS_only_not_exported",
    "SC_only_not_exported",
    "Neither_SC_PS_not_exported"   # top
  )
)



ggplot(plot_df, aes(x = Year, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country, ncol = 2, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "PS_only_not_exported"        = "steelblue",
      "SC_only_not_exported"        = "orange",
      "Both_SC_PS_not_exported"     = "darkgreen",
      "Neither_SC_PS_not_exported"  = "grey60"
    ),
    labels = c(
      "Both_SC_PS_not_exported"     = "PS + SC",
      "PS_only_not_exported"        = "PS only",
      "SC_only_not_exported"        = "SC only",
      "Neither_SC_PS_not_exported"  = "Neither"
    )
  ) +
  labs(
    title = "Unexploited Opportunities by Type of Relatedness",
    x = "Year",
    y = "Number of Products",
    fill = "Type of linkage"
  ) +
  theme_minimal(base_size = 14)


plot(x = nodes_sc_df$Morocco_IC_2022,y = nodes_sc_df$Morocco_2022)
