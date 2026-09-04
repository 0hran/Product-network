# Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(igraph)
  library(ggraph)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readxl)
  library(purrr)
  library(visNetwork)
  library(htmltools)
  library(webshot2)
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

# Trade
years <- 2002:2022

for (y in years) {
  file_t <- file.path(out_dir, sprintf("trade_%d.rda", y))
  load(file_t)
  assign(paste0("trade_net_", y), trade_net)
  rm(matrix_m, matrix_x, trade_net)
}

product_i <- aipnet %>%
  filter(hs2002_code_downstream == product4d) %>%
  distinct(hs2002_code_upstream)

# Prepare #########################################################


# Focal coutnries and years
focal_countries <- c("China", "Japan", "Morocco", "South Africa")

# Calculate distance to root (shortest path length)
dist_from_root <- igraph::distances(
  graph = network,
  v     = product4d,
  to    = V(network),
  mode  = "in"
)

dist_vec <- as.vector(dist_from_root) # create distance attribute

names(dist_vec) <- V(network)$name

# *** Replace Inf with max finite distance ***
#max_finite_dist <- max(dist_vec[is.finite(dist_vec)])
#dist_vec[is.infinite(dist_vec)] <- max_finite_dist

V(network)$dist_from_root <- dist_vec[V(network)$name]

vertex_attr_names(network)
table(V(network)$dist_from_root)

# Turn names into character  
V(network)$name <- as.character(V(network)$name)

# Create decile
V(network)$decile_dist <- NA_real_

# Now all nodes are finite so no need for reachable_mask
#V(network)$decile_dist <- ntile(V(network)$dist_from_root, 10)

reachable_mask <- is.finite(V(network)$dist_from_root) # assign deciles only to reachable nodes
V(network)$decile_dist[reachable_mask] <- ntile(
  V(network)$dist_from_root[reachable_mask], 10
)

table(V(network)$decile_dist, useNA = "ifany")

# Create merged dataset HPN-trade
nodes_df <- data.frame(
  hscode         = V(network)$name,
  description    = V(network)$Description,
  dist_from_root = V(network)$dist_from_root,
  decile_dist    = V(network)$decile_dist,
  stringsAsFactors = FALSE
)

trade_all <- map_dfr(years, ~ get(paste0("trade_net_", .x)))

trade_network <- trade_all %>%
  filter(hs4_c %in% nodes_df$hscode) %>%
  left_join(nodes_df, by = c("hs4_c" = "hscode")) %>% # nodes_df includes root product
  filter(!is.na(decile_dist), # Filter disconnected nodes
         hs4_c != product4d) %>% # Filter root product
  select(year, country, hscode = hs4_c, description, dist_from_root, decile_dist, imports, exports, net)

# Export nodes list, with decile
write_xlsx(nodes_df, file.path(out_dir, "products_by_decile.xlsx"))

# Compute indices: create IC variable
trade_network <- trade_network %>%
  mutate(
    total_trade = exports + imports,
    IC = ifelse(total_trade > 0, exports / total_trade, NA_real_)
  )

# Compute indices: trade-weighted IC
IC_weighted <- trade_network %>%
  filter(!is.na(IC), total_trade > 0) %>%
  group_by(country, year) %>%
  summarise(
    IC_bar       = sum(IC * total_trade) / sum(total_trade), # sum is by group, so sum, for all products 
    # (like E), the IC * total_trade of that product, divided by the sum of total
    # trade for each product which gives total trade for country-year
    # So it's a weighted sum / total trade
    n_components = n_distinct(hscode),
    .groups = "drop"
  )

# Compute indices: IC by decile
IC_decile <- trade_network %>%
  filter(!is.na(IC), total_trade > 0) %>%
  group_by(country, year, decile_dist) %>% # same as ic_weighted but grouping by decile, different level of aggregation
  summarise(
    IC_bar = sum(IC * total_trade) / sum(total_trade),
    .groups = "drop"
  )

# Compute indices: CG
CG <- trade_network %>%
  filter(total_trade > 0) %>% # total trade has already been made positive (e.g. when net is negative)
  mutate(ic_weighted_trade = IC * total_trade) %>% # weighted sum, for each product, IC * total_trade
  group_by(country, year, decile_dist) %>% # grouping by decile
  summarise(ic_trade_bin = sum(ic_weighted_trade), .groups = "drop") %>% # sum by country-year-decile
  group_by(country, year) %>% 
  mutate(weight = ic_trade_bin / sum(ic_trade_bin)) %>% # weight (%) for each decile, which is each decile trade-weighterd IC/the sum for all deciles
  summarise(CG = sum(decile_dist * weight), .groups = "drop") # weighted average of decile positions

## Analysis ####################################################################

## Analysis 0: IC for cars ####

trade_8703 <- trade_all %>%
  filter(hs4_c == product4d,
         country %in% focal_countries) %>%
  mutate(
    total_trade = exports + imports,
    IC = ifelse(total_trade > 0, exports / total_trade, NA_real_)
  )

ggplot(trade_8703, aes(x = year, y = IC)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x = "Year",
    y = "IC"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12)
  )

ggsave(file.path(graphs_dir, "car_IC.png"), width = 12, height = 4, dpi = 300)

## Analysis 1: CDF ####
focal_years <- c(2002, 2012, 2022)

cdf_data <- trade_network %>%
  filter(year %in% focal_years,
         country %in% focal_countries,
         IC > 0.5)

cdf_data_all <- trade_network %>%
  filter(year %in% focal_years)

ggplot(cdf_data, aes(x = dist_from_root, colour = country)) +
  stat_ecdf(data = cdf_data_all, aes(x = dist_from_root, colour = "Benchmark"),
            geom = "step", linewidth = 1.4, inherit.aes = FALSE) +
  stat_ecdf(geom = "step", linewidth = 1.2, alpha = 0.5) +
  scale_colour_manual(
    values = c("Benchmark" = "black",
               setNames(RColorBrewer::brewer.pal(4, "Set1"), focal_countries))
  ) +
  facet_wrap(~ year, nrow = 1) +
  labs(x = "Distance from root", y = "CDF", colour = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12),
    legend.text     = element_text(size = 13)
  )
ggsave(file.path(graphs_dir, "cdf.png"), width = 10, height = 4, dpi = 300)

## Analysis 2: IC weighted and CG ####

IC_weighted %>%
  filter(country %in% focal_countries) %>%
  ggplot(aes(x = year, y = IC_bar)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = trade_8703,
            aes(x = year, y = IC),
            linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey40") +
  facet_wrap(~ country, nrow = 1) +
  labs(x = "Year", y = "Trade-weighted IC") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12)
  )
#ggsave(file.path(graphs_dir, "ic_weighted.png"), width = 12, height = 4, dpi = 300)

CG %>%
  filter(country %in% focal_countries) %>%
  ggplot(aes(x = year, y = CG)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ country, nrow = 1) +
  labs(x = "Year", y = "Centre of gravity") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12)
  )
#ggsave(file.path(graphs_dir, "cg.png"), width = 12, height = 4, dpi = 300)

# Combine IC_weighted and CG into one long dataframe
combined <- IC_weighted %>%
  filter(country %in% focal_countries) %>%
  select(country, year, value = IC_bar) %>%
  mutate(measure = "Trade-weighted IC") %>%
  bind_rows(
    CG %>%
      filter(country %in% focal_countries) %>%
      select(country, year, value = CG) %>%
      mutate(measure = "Centre of gravity")
  ) %>%
  mutate(measure = factor(measure, levels = c("Trade-weighted IC", "Centre of gravity")))

# Prepare car IC line for the combined plot
car_ic_line <- trade_8703 %>%
  filter(country %in% focal_countries) %>%
  select(country, year, value = IC) %>%
  mutate(measure = factor("Trade-weighted IC", 
                          levels = c("Trade-weighted IC", "Centre of gravity")))

ggplot(combined, aes(x = year, y = value)) +
  geom_line(aes(linetype = "Supply chain"), linewidth = 0.8) +
  geom_line(data = car_ic_line,
            aes(x = year, y = value, linetype = "Finished product"),
            colour = "grey40", linewidth = 0.8) +
  geom_hline(data = filter(combined, measure == "Trade-weighted IC"),
             aes(yintercept = 0.5), linetype = "dotted", colour = "grey40") +
  scale_linetype_manual(values = c("Supply chain"    = "solid",
                                   "Finished product" = "dashed")) +
  facet_grid(measure ~ country, scales = "free_y", switch = "y") +
  labs(x = "Year", y = NULL, linetype = NULL) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    panel.grid       = element_blank(),
    panel.border     = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    strip.placement  = "outside",
    text             = element_text(size = 14),
    strip.text       = element_text(size = 16),
    axis.title       = element_text(size = 15),
    axis.text        = element_text(size = 12),
    legend.text      = element_text(size = 13)
  )

ggsave(file.path(graphs_dir, "ic_cg.png"), 
       width = 12, height = 6, dpi = 300)


## Analysis 3: line plots by distance bin #### 

# Filter to focal years
IC_decile_indexed <- IC_decile %>%
  filter(country %in% focal_countries) %>%
  group_by(country, decile_dist) %>%
  mutate(IC_bar_2002 = IC_bar[year == 2002],
         IC_diff = IC_bar - IC_bar_2002) %>%
  ungroup() %>%
  filter(year %in% c(2007, 2012, 2017, 2022))

# Plot
ggplot(IC_decile_indexed, aes(x = as.numeric(decile_dist), y = IC_diff,
                              colour = factor(year), group = factor(year))) +
  geom_smooth(linewidth = 0.8, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x      = "Distance bin",
    y      = "Change in trade-weighted IC (vs 2002)",
    colour = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave(file.path(graphs_dir, "lines_ic.png"), width = 12, height = 6, dpi = 300)
