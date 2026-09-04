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

# Change 04 Sept.

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

# Focal countries and years
focal_countries <- c("China", "Japan", "Morocco", "South Africa")

# Turn names into character
V(network)$name <- as.character(V(network)$name)

# Invert Upstream2 to get Downstream2: nodes far upstream get high values,
# nodes close to the root (final product) get low values.
V(network)$Downstream2 <- max(V(network)$Upstream2, na.rm = TRUE) - V(network)$Upstream2

# Create decile from Downstream2 (no NAs confirmed)
V(network)$decile_dist <- ntile(V(network)$Downstream2, 10)

table(V(network)$decile_dist, useNA = "ifany")

# Compute shortest path
root <- which(V(network)$name == product4d)
sp <- shortest.paths(network, to = root, mode = "out")
V(network)$sp_dist <- as.numeric(sp)
V(network)$sp_dist[is.infinite(V(network)$sp_dist)] <- NA

# Build nodes_df
nodes_df <- data.frame(
  hscode      = V(network)$name,
  description = V(network)$Description,
  downstream2 = V(network)$Downstream2,
  decile_dist = V(network)$decile_dist,
  sp_dist     = V(network)$sp_dist,
  stringsAsFactors = FALSE
)

trade_all <- map_dfr(years, ~ get(paste0("trade_net_", .x)))

trade_network <- trade_all %>%
  filter(hs4_c %in% nodes_df$hscode) %>%
  left_join(nodes_df, by = c("hs4_c" = "hscode")) %>%
  filter(!is.na(decile_dist),
         hs4_c != product4d) %>%
  select(year, country, hscode = hs4_c, description,
         downstream2, decile_dist, sp_dist,
         imports, exports, net)

# Export nodes list, with decile
write_xlsx(nodes_df, file.path(out_dir, "products_by_decile.xlsx"))

# Create IC variable to compute indices below
trade_network <- trade_network %>%
  mutate(
    total_trade = exports + imports,
    IC = ifelse(total_trade > 0, exports / total_trade, NA_real_)
  )

# Compute indices: IC Simple
IC_simple <- trade_network %>%
  filter(!is.na(IC), total_trade > 0) %>%
  group_by(country, year) %>%
  summarise(IC_bar = mean(IC), .groups = "drop")

# Compute indices: trade-weighted IC
IC_weighted <- trade_network %>%
  filter(!is.na(IC), total_trade > 0) %>%
  group_by(country, year) %>%
  summarise(
    IC_bar       = sum(IC * total_trade) / sum(total_trade),
    n_components = n_distinct(hscode),
    .groups = "drop"
  )

# Compute indices: IC by decile
IC_decile <- trade_network %>%
  filter(!is.na(IC), total_trade > 0) %>%
  group_by(country, year, decile_dist) %>%
  summarise(
    IC_bar = sum(IC * total_trade) / sum(total_trade),
    .groups = "drop"
  )

# Compute indices: CG
CG <- trade_network %>%
  filter(!is.na(IC), total_trade > 0) %>%
  mutate(ic_weighted_trade = IC * total_trade) %>%
  group_by(country, year) %>%
  mutate(weight = ic_weighted_trade / sum(ic_weighted_trade)) %>%
  summarise(CG = sum(downstream2 * weight), .groups = "drop")

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
  labs(x = "Year", y = "IC") +
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

ggplot(cdf_data, aes(x = downstream2, colour = country)) +
  stat_ecdf(data = cdf_data_all, aes(x = downstream2, colour = "Benchmark"),
            geom = "step", linewidth = 1.4, inherit.aes = FALSE) +
  stat_ecdf(geom = "step", linewidth = 1.2, alpha = 0.5) +
  scale_colour_manual(
    values = c("Benchmark" = "black",
               setNames(RColorBrewer::brewer.pal(4, "Set1"), focal_countries))
  ) +
  facet_wrap(~ year, nrow = 1) +
  labs(x = "Upstream", y = "CDF", colour = NULL) +
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
  scale_linetype_manual(values = c("Supply chain"     = "solid",
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

#ggsave(file.path(graphs_dir, "ic_cg.png"),
#       width = 12, height = 6, dpi = 300)

# Combine countries in one plot, then plots side by side

cg_labels <- data.frame(
  measure = factor("Centre of gravity",
                   levels = c("Trade-weighted IC", "Centre of gravity")),
  x     = c(2022, 2022),
  y     = c(Inf, -Inf),
  label = c("Close to raw materials", "Close to root product"),
  vjust = c(1.5, -0.5)
)

combined %>%
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = country), size = 2) +
  geom_hline(data = filter(combined, measure == "Trade-weighted IC"),
             aes(yintercept = 0.5), linetype = "dotted", colour = "grey40") +
  geom_text(data = cg_labels,
            aes(x = x, y = y, label = label, vjust = vjust),
            hjust = 1, colour = "grey25", size = 3.5, inherit.aes = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(x = "Year", y = NULL, colour = NULL, shape = NULL) +
  facet_wrap(~ measure, scales = "free_y", nrow = 1,
             labeller = as_labeller(c("Trade-weighted IC" = "Trade-weighted IC",
                                      "Centre of gravity" = "Centre of Gravity"))) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    strip.placement = "outside",
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12),
    legend.text     = element_text(size = 13)
  )

ggsave(file.path(graphs_dir, "ic_cg2.jpg"),
       width = 10, height = 6, dpi = 300)

# CG using CG-strength, using only products with IC>0.5

CG_strength <- trade_network %>%
  filter(total_trade > 0, !is.na(IC), IC > 0.5) %>%
  mutate(ic_weighted_trade = IC * total_trade) %>%
  group_by(country, year, downstream2) %>%
  summarise(ic_trade_bin = sum(ic_weighted_trade), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(weight = ic_trade_bin / sum(ic_trade_bin)) %>%
  summarise(CG = sum(downstream2 * weight), .groups = "drop")

combined1 <- IC_weighted %>%
  filter(country %in% focal_countries) %>%
  select(country, year, value = IC_bar) %>%
  mutate(measure = "Trade-weighted IC") %>%
  bind_rows(
    CG %>%
      filter(country %in% focal_countries) %>%
      select(country, year, value = CG) %>%
      mutate(measure = "Centre of gravity")
  ) %>%
  bind_rows(
    CG_strength %>%
      filter(country %in% focal_countries) %>%
      select(country, year, value = CG) %>%
      mutate(measure = "Centre of gravity (IC-weighted)")
  ) %>%
  mutate(measure = factor(measure, levels = c("Trade-weighted IC", 
                                              "Centre of gravity",
                                              "Centre of gravity (IC-weighted)")))

combined1 %>%
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = country), size = 2) +
  geom_hline(data = filter(combined1, measure == "Trade-weighted IC"),
             aes(yintercept = 0.5), linetype = "dotted", colour = "grey40") +
  geom_text(data = cg_labels,
            aes(x = x, y = y, label = label, vjust = vjust),
            hjust = 1, colour = "grey25", size = 3.5, inherit.aes = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(x = "Year", y = NULL, colour = NULL, shape = NULL) +
  facet_wrap(~ measure, scales = "free_y", nrow = 1,
             labeller = as_labeller(c(
               "Trade-weighted IC"               = "Trade-weighted IC",
               "Centre of gravity"               = "Centre of Gravity",
               "Centre of gravity (IC-weighted)" = "Centre of Gravity\n(IC-weighted)"
             ))) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    strip.placement = "outside",
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12),
    legend.text     = element_text(size = 13)
  )

# With IC SImple

combined2 <- IC_simple %>%
  filter(country %in% focal_countries) %>%
  select(country, year, value = IC_bar) %>%
  mutate(measure = "IC") %>%
  bind_rows(
    IC_weighted %>%
      filter(country %in% focal_countries) %>%
      select(country, year, value = IC_bar) %>%
      mutate(measure = "Trade-weighted IC")
  ) %>%
  bind_rows(
    CG %>%
      filter(country %in% focal_countries) %>%
      select(country, year, value = CG) %>%
      mutate(measure = "Centre of gravity")
  ) %>%
  mutate(measure = factor(measure, levels = c("IC", "Trade-weighted IC", "Centre of gravity")))

combined2 %>%
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = country), size = 2) +
  geom_hline(data = filter(combined2, measure %in% c("IC", "Trade-weighted IC")),
             aes(yintercept = 0.5), linetype = "dotted", colour = "grey40") +
  geom_text(data = cg_labels,
            aes(x = x, y = y, label = label, vjust = vjust),
            hjust = 1, colour = "grey25", size = 3.5, inherit.aes = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(x = "Year", y = NULL, colour = NULL, shape = NULL) +
  facet_wrap(~ measure, scales = "free_y", nrow = 1,
             labeller = as_labeller(c("Trade-weighted IC" = "Trade-weighted IC",
                                      "IC"                = "IC",
                                      "Centre of gravity" = "Centre of Gravity"))) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    strip.placement = "outside",
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12),
    legend.text     = element_text(size = 13)
  )

ggsave(file.path(graphs_dir, "ic_cg3.jpg"),
       width = 10, height = 6, dpi = 300)


# Combine, with reference value at CG (mean of downstream)

cg_ref <- mean(nodes_df$downstream2, na.rm = TRUE)

cg_ref_line <- data.frame(
  measure = factor("Centre of gravity",
                   levels = c("IC", "Trade-weighted IC", "Centre of gravity")),
  yintercept = cg_ref
)

combined3 <- combined2
combined3 %>%
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = country), size = 2) +
  geom_hline(data = cg_ref_line,
             aes(yintercept = yintercept),
             linetype = "dashed", colour = "grey40") +
  geom_text(data = cg_labels,
            aes(x = x, y = y, label = label, vjust = vjust),
            hjust = 1, colour = "grey25", size = 3.5, inherit.aes = FALSE) +
  scale_colour_brewer(palette = "Set1") +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(x = "Year", y = NULL, colour = NULL, shape = NULL) +
  facet_wrap(~ measure, scales = "free_y", nrow = 1,
             labeller = as_labeller(c("Trade-weighted IC" = "Trade-weighted IC",
                                      "IC"                = "IC",
                                      "Centre of gravity" = "Centre of Gravity"))) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid      = element_blank(),
    panel.border    = element_rect(colour = "grey10", fill = NA, linewidth = 1),
    strip.placement = "outside",
    text            = element_text(size = 14),
    strip.text      = element_text(size = 16),
    axis.title      = element_text(size = 15),
    axis.text       = element_text(size = 12),
    legend.text     = element_text(size = 13)
  )

## Analysis 3: line plots by distance bin ####

# Filter to focal years
IC_decile_indexed <- IC_decile %>%
  filter(country %in% focal_countries) %>%
  group_by(country, decile_dist) %>%
  mutate(IC_bar_2002 = IC_bar[year == 2002],
         IC_diff = IC_bar - IC_bar_2002) %>%
  ungroup() %>%
  filter(year %in% c(2012, 2022))

# Plot
ggplot(IC_decile_indexed, aes(x = as.numeric(decile_dist), y = IC_diff,
                              colour = factor(year), group = factor(year))) +
  geom_smooth(linewidth = 0.8, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x      = "Downstream2 bin",
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

#ggsave(file.path(graphs_dir, "lines_ic.png"), width = 12, height = 6, dpi = 300)

# Analysis 4: without using deciles ###########################################

# Get 2002 baseline: trade-weighted IC per country x hscode
IC_2002_baseline <- trade_network %>%
  filter(year == 2002, !is.na(IC), total_trade > 0) %>%
  select(country, hscode, IC_2002 = IC, total_trade_2002 = total_trade)

# For 2012 and 2022, join baseline and compute IC_diff at product level
IC_loess_data <- trade_network %>%
  filter(year %in% c(2012, 2022),
         country %in% focal_countries,
         !is.na(IC), total_trade > 0) %>%
  left_join(IC_2002_baseline, by = c("country", "hscode")) %>%
  filter(!is.na(IC_2002)) %>%                # Keep only products observed in 2002 too
  mutate(IC_diff = IC - IC_2002)

# Plot: LOESS of IC_diff on downstream2, one line per year
ggplot(IC_loess_data, aes(x = downstream2, y = IC_diff,
                          colour = factor(year), group = factor(year))) +
  geom_smooth(method = "loess", span = 0.75,        
              linewidth = 0.8, se = FALSE, alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x      = "Downstream2 (continuous)",
    y      = "Change in IC (vs 2002)",
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

# showing components
ggplot(IC_loess_data, aes(x = downstream2, y = IC_diff,
                          colour = factor(year), group = factor(year))) +
  geom_point(alpha = 0.2, size = 2) +
  geom_smooth(method = "loess", span = 0.75,
              linewidth = 1, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("2012" = "#E69F00", "2022" = "#009E73")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x      = "Downstream2 (continuous)",
    y      = "Change in IC (vs 2002)",
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

# Using shortest path

IC_loess_sp <- IC_loess_data %>%
  filter(!is.na(sp_dist))

# Plot
ggplot(IC_loess_sp, aes(x = sp_dist, y = IC_diff,
                        colour = factor(year), group = factor(year))) +
  geom_point(alpha = 0.2, size = 2) +
  geom_smooth(method = "loess", span = 0.75,
              linewidth = 1, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("2012" = "#E69F00", "2022" = "#009E73")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  facet_wrap(~ country, nrow = 1) +
  labs(
    x      = "Shortest path to root",
    y      = "Change in IC (vs 2002)",
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

ggsave(file.path(graphs_dir, "lines_ic_shortestd0.png"), 
       width = 14, height = 6, dpi = 300)

# Using trade-weighted
IC_loess_sp_weighted <- IC_loess_data %>%
  filter(!is.na(sp_dist), total_trade > 0) %>%
  group_by(country, year, sp_dist) %>%
  summarise(
    IC_diff_bar = sum(IC_diff * total_trade) / sum(total_trade),
    .groups = "drop"
  )

ggplot(IC_loess_sp_weighted, aes(x = sp_dist, y = IC_diff_bar,
                                 colour = factor(year), group = factor(year))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", span = 0.75,
              linewidth = 1, se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("2012" = "#E69F00", "2022" = "#009E73")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  facet_wrap(~ country, nrow = 1) +
  labs(x = "Shortest path to root", y = "Change in trade-weighted IC (vs 2002)", colour = NULL) +
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

ggsave(file.path(graphs_dir, "lines_ic_shortestd.png"), 
       width = 14, height = 6, dpi = 300)
