

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
})

# Paths

  rm(list = ls())
  out_dir <- "C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Output"
  data_dir <- "C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Data/BACI_HS02_V202501"
  graphs_dir <- "C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Graphs"
  
# Load data

  # Aipnet
  aipnet <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx")
  # HS codes
  hsnames <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS02")
  BEC_database <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Data/BEC database.xlsx")
  gplus_all <- readRDS("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Output/gplus.rds")
  gplus_matrix <- readRDS(("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Output/gplus_brother_matrix.rds"))
    
    # Select gplus of choice
    gplus <- gplus_all[[10]][[25]] # first threshold, second threshold
  
    
  # Trade
  
    # Years you created
    years <- 2002:2022
    
    # Loop through years and load each file as a separate data frame
    for (y in years) {
      
      # Build filenames
      
        # file_x <- file.path(out_dir, sprintf("matrix_x_%d.rda", y))
        # file_m <- file.path(out_dir, sprintf("matrix_m_%d.rda", y))
        file_t <- file.path(out_dir, sprintf("trade_%d.rda", y))
      
      # Load and assign each to a unique object name
        
        # load(file_x)   # loads matrix_x
        # assign(paste0("matrix_x_", y), matrix_x)
        # 
        # load(file_m)   # loads matrix_m
        # assign(paste0("matrix_m_", y), matrix_m)
      
        load(file_t)   # loads trade_net
        assign(paste0("trade_net_", y), trade_net)
      
      # Clean up temporary objects
      rm(matrix_m, matrix_x, trade_net)
      
    }
    
    # Set product root
    product4d <- "8703"
    
    # Get inputs 
    product_i <- aipnet %>%
      filter(hs2002_code_downstream == product4d) %>%
      distinct(hs2002_code_upstream)
  
    # Vector of HS codes for car inputs
    hs_keep <- sprintf("%04d", unique(product_i$hs2002_code_upstream))
    
    # Helper: compute IC per product and overall IC
    prep_ic <- function(data, country_name) {
      df <- data %>%
        filter(country == country_name, hs4_c %in% hs_keep) %>%
        mutate(ic = if_else(
          exports + imports > 0,
          exports / (exports + imports),
          NA_real_
        ))
      
      total_ic <- df %>%
        summarise(
          total_exports = sum(exports, na.rm = TRUE),
          total_imports = sum(imports, na.rm = TRUE),
          IC = total_exports / (total_exports + total_imports)
        ) %>%
        pull(IC)
      
      list(data = df, total_ic = total_ic)
    }
    
    # --- Countries and years ---
    countries <- c("China", "Rep. of Korea", "South Africa")
    years <- c(2002, 2012, 2022)
    
    # --- Set up plotting layout: 4 rows (countries) × 3 columns (years) ---
    par(mfrow = c(length(countries), length(years)), mar = c(5, 4, 4, 2) + 0.1)
    
    # --- Loop: rows = countries, columns = years ---
    for (c in countries) {
      
      # Choose colour per country
      col_country <- case_when(
        c == "Rep. of Korea" ~ "steelblue",
        c == "China"   ~ "darkred",
        #c == "Morocco" ~ "darkgreen",
        c == "South Africa"   ~ "goldenrod",
        TRUE           ~ "black"
      )
      
      for (y in years) {
        
        # Dynamically load the correct trade data
        trade_data <- get(paste0("trade_net_", y))
        
        # Compute ICs
        tmp <- prep_ic(trade_data, c)
        
        # Plot
        plot(tmp$data$ic,
             pch = 19,
             col = col_country,
             main = paste(c, "–", y),
             xlab = "HS product (sorted)",
             ylab = if (y == 2002) "IC = Exports / (Exports + Imports)" else "",
             ylim = c(0, 1))
        
        # Add horizontal line for overall IC
        abline(h = tmp$total_ic, lty = 2, lwd = 2, col = "grey40")
      }
    }
    
    # Now plot evolution of IC by supply chain stage
    
    root <- product4d
    list_of_name <- V(gplus)$name
    # reverse edges so paths flow outward from the root
    g_rev <- igraph::reverse_edges(gplus)
    
    # UNWEIGHTED distances (hops)
  
      # d <- distances(g_rev, v = root, to = list_of_name, mode = "out", weights = NA)
      # dist_vec <- as.numeric(d[1, ])
      # names(dist_vec) <- list_of_name
  
    # WEIGHTED distances (hops)
    d <- distances(
      g_rev,
      v       = root,
      to      = list_of_name,
      mode    = "out",
      weights = E(g_rev)$weight   # or NULL if the attribute is named "weight"
    )
    
    dist_vec <- as.numeric(d[1, ])
    names(dist_vec) <- list_of_name
    
    finite <- is.finite(dist_vec)
    
    # quantiles
    q <- quantile(dist_vec[finite], probs = seq(0, 1, 0.2), na.rm = TRUE)
    
    # keep only unique break points, in order
    q_uniq <- unique(q)
    
    if (length(q_uniq) == 1L) {
      # everything has the same distance
      dist_groups <- rep(1L, length(dist_vec))
      dist_groups[!finite] <- NA_integer_
    } else {
      # number of actual intervals
      n_int <- length(q_uniq) - 1L
      
      dist_groups <- rep(NA_integer_, length(dist_vec))
      dist_groups[finite] <- as.integer(cut(
        dist_vec[finite],
        breaks         = q_uniq,
        include.lowest = TRUE,
        labels         = seq_len(n_int)
      ))
    }
    
    # store raw weighted distance
    V(gplus)$dist_to_root <- dist_groups
    
    # Build nodes and edges from gplus
    nodes <- data.frame(
      id    = V(gplus)$name,
      label = V(gplus)$name,
      dist_to_root = V(gplus)$dist_to_root,
      stringsAsFactors = FALSE
    )
    
    # Get stage of inputs
    product_i1 <- product_i %>%
      mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
      inner_join(nodes, by = c("hs2002_code_upstream" = "id")) %>%
      rename(stage = dist_to_root)
    
    years <- 2002:2022
    country_name <- "Rep. of Korea"
    stages <- c("stage1", "stage2", "stage3", "stage4", "stage5")
    
    product_stage <- product_i1 %>%
      mutate(hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")) %>%
      select(hs4_c, stage)
    
    trade_ctr <- map_df(years, ~{
      get(paste0("trade_net_", .x)) %>%
        filter(country == country_name) %>%
        select(year, hs4_c, exports, imports)
    })
    
    # Item-level IC values
    ic_items <- trade_ctr %>%
      left_join(product_stage, by = "hs4_c") %>%
      filter(!is.na(stage)) %>%
      mutate(ic = if_else(exports + imports > 0, exports / (exports + imports), NA_real_)) %>%
      filter(!is.na(ic)) %>%
      mutate(stage = factor(paste0("stage", stage), levels = stages))
    
    # Layout parameters
    nY  <- length(years)
    gap <- 1
    block_width <- nY + gap
    
    ic_items <- ic_items %>%
      mutate(
        stage_idx = as.integer(stage),
        year_idx  = match(year, years),
        x_pos     = (stage_idx - 1) * block_width + year_idx
      )
    
    vlines_at <- (1:(length(stages) - 1)) * block_width + 0.5
    stage_centres <- (0:(length(stages) - 1)) * block_width + (nY + 1) / 2
    
    # Compute mean IC per stage-year for overlay line
    ic_means <- ic_items %>%
      group_by(stage, year, x_pos) %>%
      summarise(mean_ic = mean(ic, na.rm = TRUE), .groups = "drop")
    
    # keep only stages 1–3
    ic_items_f <- ic_items %>% dplyr::filter(!stage %in% c("stage5"))
    ic_means_f <- ic_means %>% dplyr::filter(!stage %in% c("stage5"))
    
    ggplot(
      ic_items_f,
      aes(x = year, y = ic, group = year)
    ) +
      geom_boxplot(width = 0.6, outlier.shape = NA, fill = "grey70", colour = "grey70", alpha = 0.1) +
      geom_jitter(width = 0.2, height = 0, alpha = 0.25, size = 0.8) +
      geom_smooth(
        data = ic_means_f,
        aes(x = year, y = mean_ic),
        method = "loess",
        se = FALSE,
        linewidth = 1,
        span = 0.6,
        colour = "steelblue",
        inherit.aes = FALSE
      ) +
      labs(title = country_name, 
           x = NULL, 
           y = "Export Share in Total Trade") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11),
        #axis.title.y = element_text(angle = 0, margin = margin (r = 12)),
        strip.text = element_text(size = 11, face = "bold")
      ) +
      facet_wrap(~ stage, nrow = 1) 
    
##### For more than one country ################################################
    
    years     <- 2002:2022
    countries <- c("Rep. of Korea", "South Africa", "China")
    stages    <- c("stage1", "stage2", "stage3", "stage4", "stage5")
    
    product_stage <- product_i1 %>%
      mutate(hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")) %>%
      select(hs4_c, stage)
    
    # trade data for all countries and years
    trade_ctr <- map_df(countries, \(ctry) {
      map_df(years, \(yr) {
        get(paste0("trade_net_", yr)) %>%
          filter(country == ctry) %>%
          transmute(
            country = ctry,
            year,
            hs4_c,
            exports,
            imports
          )
      })
    })
    
    # item level IC values
    ic_items <- trade_ctr %>%
      left_join(product_stage, by = "hs4_c") %>%
      filter(!is.na(stage)) %>%
      mutate(
        ic = if_else(exports + imports > 0, exports / (exports + imports), NA_real_),
        stage = factor(paste0("stage", stage), levels = stages,
                       labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Stage 5")),
        country = factor(country, levels = countries)
      ) %>%
      filter(!is.na(ic))
    
    # keep only stages 1 to 3
    ic_items_f <- ic_items %>% filter(!stage %in% c("Stage 5"))
    
    # mean IC per country stage year for the smooth line
    ic_means_f <- ic_items_f %>%
      group_by(country, stage, year) %>%
      summarise(mean_ic = mean(ic, na.rm = TRUE), .groups = "drop")
    
    ggplot(
      ic_items_f,
      aes(x = year, y = ic, group = year)
    ) +
      geom_boxplot(
        width = 0.6,
        outlier.shape = NA,
        fill = "grey70",
        colour = "grey70",
        alpha = 0.1
      ) +
      geom_jitter(width = 0.2, height = 0, alpha = 0.25, size = 0.8) +
      geom_smooth(
        data = ic_means_f,
        aes(x = year, y = mean_ic, group = 1),
        method = "loess",
        se = FALSE,
        linewidth = 1,
        span = 0.6,
        colour = "steelblue",
        inherit.aes = FALSE
      ) +
      scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1)) +
      labs(
        title = "Automotive Supply Chain",
        x = NULL,
        y = "Integration Coefficient"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position   = "none",
        panel.background  = element_rect(fill = "white", colour = "black", linewidth = 1),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y       = element_text(size = 11),
        strip.text        = element_text(size = 11, face = "bold")
      ) +
      facet_grid(country ~ stage)
   ggsave(file.path(graphs_dir, "IC by stages.png"),
          width = 8, height = 6)
  

