# Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(igraph)
  library(ggraph)
  library(tidygraph)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readxl)
  library(purrr)
  library(visNetwork)
  library(htmltools)
  library(webshot2)
  library(stringr)
  library(reshape2)
  library(RColorBrewer)
  library(writexl)
})

# Paths
rm(list = ls())

#Key<- "C:/Users/ap115/OneDrive - SOAS University of London/Adria Rius's files - Research collab. AP-AR/"
Key <-"C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/"

out_dir <- paste(Key,"Output",sep = "" )
data_dir <- paste(Key,"Data/BACI_HS02_V202501",sep = "" )
graphs_dir <- paste(Key,"Graphs",sep = "" )

# Load data
aipnet <- read_excel(paste(Key,"Code/edge_list_hs2002_4digit.xlsx",sep = "" ))
hsnames <- read_excel(paste(Key,"Code/HSCodeandDescription.xlsx",sep = "" ), sheet = "HS02")
BEC_database <- read_excel(paste(Key,"Data/BEC database.xlsx",sep = "" ))

#source(paste(Key,"Code/1. Network_algorithm_11.05.26_updated.R",sep = "" ))

network <- readRDS(paste(Key,"Output/PN_links_8703Final_version.rds",sep = "" ))
product4d <- "8703"

product_i <- aipnet %>%
  filter(hs2002_code_downstream == product4d) %>%
  distinct(hs2002_code_upstream)

# Prepare #########################################################

# Turn names into character
V(network)$name <- as.character(V(network)$name)

# Distance
focal_id <- which(V(network)$Code == "8703")
V(network)$stage_dist <- distances(network, v = focal_id, mode = "in")[1, ]

# Plot #########################################################

set.seed(100)
ggraph(network, layout = "fr") +
  geom_edge_link(
    aes(width = weight, alpha = weight),
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    end_cap = circle(1, "mm"),
    color = "grey40"
  ) +
  scale_edge_alpha(range = c(0.1, 0.7), guide = "none") +
  geom_node_point(
    aes(fill = stage_dist, size = Upstream2),
    shape = 21,
    color = "grey20",
    stroke = 0.4,
    alpha = 0.9
  ) +
  scale_edge_width(range = c(0.2, 1.8), guide = "none") +
  scale_size_continuous(range = c(2, 8), name = "Upstream2") +
  scale_fill_viridis_c(name = "Distance from\n8703 (car)", option = "plasma", direction = -1) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(
    title = "HS Code Network — Colored by Supply Chain Distance",
    subtitle = paste0(vcount(network), " nodes, ", ecount(network), " edges")
  )

# V2
set.seed(100)
ggraph(network, layout = "fr") +
  geom_edge_link(
    aes(width = weight, alpha = weight, color = node1.stage_dist),
    arrow = arrow(length = unit(1, "mm"), type = "closed"),
    end_cap = circle(1, "mm")
  ) +
  scale_edge_alpha(range = c(0.3, 0.9), guide = "none") +
  scale_edge_width(range = c(0.4, 2.5), guide = "none") +
  scale_edge_colour_distiller(palette = "Blues", direction = -1, guide = "none") +
  geom_node_point(
    aes(fill = stage_dist),
    size = 4,
    shape = 21,
    color = "grey10",
    stroke = 0.4,
    alpha = 0.9
  ) +
  scale_fill_distiller(name = "Distance from\n8703 (car)", palette = "Blues", direction = -1) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "none")
ggsave(paste0(graphs_dir, "/HS_network_8703_stage_dist.jpg"),
  width = 10, height = 8, units = "in"
)
