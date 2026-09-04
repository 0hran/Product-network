
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
 #out_dir <- "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output"
out_dir <- "C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Output"

 #data_dir <- "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BACI_HS02_V202501"
data_dir <- "C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Data/BACI_HS02_V202501"

# graphs_dir <- "C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Graphs"
graphs_dir <- "C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Graphs"

# Load data

# Aipnet
  
  # aipnet <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/edge_list_hs2002_4digit.xlsx")
  aipnet <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Code/edge_list_hs2002_4digit.xlsx")

# HS codes
  
  # hsnames <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS02")
  hsnames <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Code/HSCodeandDescription.xlsx", sheet = "HS02")
  # BEC_database <- read_excel("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Data/BEC database.xlsx")
  BEC_database <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Data/BEC database.xlsx")

# Run network

  # source("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Code/1. Network_algorithm_24.11.25.R")
  # source("C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Code/1. Network_algorithm_24.11.25.R")

# supply chain network

  # network <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/network.rds")
  network <- readRDS("C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Output/PN_links_8703.rds")

  
  V(network)
# Root
  
  # product4d <- readRDS("C:/Users/ap115/OneDrive - SOAS University of London/CSST Arnaud_Adria/Output/product4d.rds")
  product4d <- readRDS("C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/Output/product4d.rds")

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
  
  # list of inputs
  
  product_i <- aipnet %>%
    filter(hs2002_code_downstream == product4d) %>%
    distinct(hs2002_code_upstream)
  
# Prepare #########################################################

# Calculate distance to root

  root <- product4d
  list_of_name <- V(network)$name
  
  d <- distances(network, v = root, to = list_of_name, mode = "out", weights = NA)
  
  dist_vec <- as.numeric(d[1, ])
  names(dist_vec) <- list_of_name
  
  V(network)$dist_to_root <- dist_vec

  # Export stages
  stages_list <- tibble(
    hscode = names(dist_vec),
    stages = as.numeric(dist_vec)
  ) %>%
    left_join(hsnames %>% 
                select(Code, Description),
              by = c("hscode" = "Code")) %>%
    transmute(
      hscode,
      hscode_description = Description,
      stages
    )
  
  stages_list <- stages_list %>%
    arrange(stages, hscode)
  
  write_xlsx(stages_list, file.path(graphs_dir, "stages_list.xlsx"))
  
  
# Car TOtal Exports  ###################################################
  
  countries_to_plot <- c("China", "Morocco", "South Africa", "Japan")
  years_to_plot <- 2002:2022
  hs_code_target <- 8703
  
  # Function to load and filter a single year
  load_year_data <- function(yr) {
    tbl_name <- paste0("trade_net_", yr)
    if (!exists(tbl_name, inherits = TRUE)) return(NULL)
    
    get(tbl_name) %>%
      filter(
        country %in% countries_to_plot,
        hs4_c == hs_code_target
      ) %>%
      transmute(
        country,
        year = yr,
        exports,
        imports
      )
  }
  
  # Build one data frame for all years
  trade_all <- map_df(years_to_plot, load_year_data)
  
  trade_all <- trade_all %>%
    mutate(export_share = exports / (exports + imports))
  
  # Plot
  ggplot(trade_all, aes(x = year, y = export_share)) +
    geom_line(colour = "grey30", size = 1) +
    facet_wrap(~ country, nrow = 1) +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16),
      plot.title = element_text(size = 18)
    )  
  
  ggsave(file.path(graphs_dir, "car_IC.png"),
         width = 12, height = 4, dpi = 600)
  
# Visualise network ###################################################
  
  network_ic_vis_period <- function(ctry, start_yr, end_yr, out_file,
                                    stages = paste0("stage", 0:5)) {
    # Uses global objects:
    #   network      : igraph object of the full product network
    #   product4d    : root product code (vertex name in network)
    #   product_i    : data frame with column hs2002_code_upstream
    #   hsnames      : data frame with Code and Description
    #   trade_net_YY : one data frame per year, for example trade_net_2002
    
    # 1. Distance from root: raw and capped]
    
    # Compute raw distances once and store as vertex attribute
    if (is.null(igraph::vertex_attr(network, "dist_to_root_raw"))) {
      
      d <- igraph::distances(
        network,
        v = igraph::V(network)[name == product4d],
        to = igraph::V(network),
        mode = "out",
        weights = NA
      )
      
      dist_vec <- as.numeric(d[1, ])
      dist_vec[is.infinite(dist_vec)] <- NA_real_
      names(dist_vec) <- igraph::V(network)$name
      
      igraph::V(network)$dist_to_root_raw <- dist_vec
    }
    
    # Capped distance 6 and above becomes 5 for the visual
    dist_raw <- igraph::V(network)$dist_to_root_raw
    dist_cap <- ifelse(is.na(dist_raw), NA_real_, pmin(dist_raw, 5))
    igraph::V(network)$dist_to_root_cap <- dist_cap
    
    # 2. Nodes and edges from full network
    
    nodes <- data.frame(
      id              = igraph::V(network)$name,
      label           = igraph::V(network)$name,
      dist_to_root    = igraph::V(network)$dist_to_root_cap,
      stringsAsFactors = FALSE
    )
    
    if (igraph::ecount(network) > 0) {
      e_ends <- igraph::ends(network, igraph::E(network))
      edges <- data.frame(
        from  = as.character(e_ends[, 1]),
        to    = as.character(e_ends[, 2]),
        value = 0.5,
        stringsAsFactors = FALSE
      )
    } else {
      edges <- data.frame(
        from  = character(0),
        to    = character(0),
        value = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    
    # Stable ordering for reproducible layouts
    nodes <- nodes[order(nodes$id), ]
    edges <- edges[order(edges$from, edges$to), ]
    
    # 3. Stages from network distance
    
    product_i1 <- product_i %>%
      dplyr::mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
      dplyr::inner_join(
        nodes,
        by = c("hs2002_code_upstream" = "id")
      ) %>%
      dplyr::rename(stage = dist_to_root)
    
    product_stage <- product_i1 %>%
      dplyr::mutate(
        hs4_c = stringr::str_pad(hs2002_code_upstream, 4, pad = "0"),
        stage = pmin(stage, 5)
      ) %>%
      dplyr::select(hs4_c, stage)
    
    # 4. Trade data for this country and period
    # Uses all years from start_yr to end_yr inclusive
    
    years_seq <- seq.int(start_yr, end_yr)
    
    trade_list <- lapply(years_seq, function(yr) {
      trade_tbl_name <- paste0("trade_net_", yr)
      if (!exists(trade_tbl_name, inherits = TRUE)) {
        stop("Object ", trade_tbl_name, " not found in the environment")
      }
      trade_tbl <- get(trade_tbl_name)
      
      trade_tbl %>%
        dplyr::filter(country == ctry) %>%
        dplyr::mutate(year = yr)
    })
    
    trade_period <- dplyr::bind_rows(trade_list)
    
    trade_ctr <- trade_period %>%
      dplyr::group_by(country, hs4_c) %>%
      dplyr::summarise(
        exports = sum(exports, na.rm = TRUE),
        imports = sum(imports, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        period_start = start_yr,
        period_end   = end_yr,
        period_label = paste0(start_yr, "-", end_yr)
      )
    
    # 5. Item level IC for all HS4 in trade data
    
    ic_items <- trade_ctr %>%
      dplyr::left_join(product_stage, by = "hs4_c") %>%
      dplyr::mutate(
        ic = dplyr::if_else(
          exports + imports > 0,
          exports / (exports + imports),
          NA_real_
        ),
        stage        = factor(paste0("stage", stage), levels = stages),
        country      = factor(country, levels = c(ctry)),
        period_label = period_label
      )
    
    # 6. Add IC and trade variables to nodes
    
    nodes <- nodes %>%
      dplyr::left_join(
        ic_items %>%
          dplyr::select(
            id = hs4_c,
            country,
            exports,
            imports,
            stage,
            ic,
            period_start,
            period_end,
            period_label
          ),
        by = "id"
      )
    
    # 7. HS names and tooltips
    
    nodes <- nodes %>%
      dplyr::mutate(id = stringr::str_trim(as.character(id))) %>%
      dplyr::left_join(
        hsnames %>%
          dplyr::transmute(
            Code        = stringr::str_trim(as.character(Code)),
            Description = stringr::str_trim(as.character(Description))
          ) %>%
          dplyr::distinct(Code, .keep_all = TRUE),
        by = c("id" = "Code")
      ) %>%
      dplyr::mutate(
        label = id,
        title = dplyr::if_else(
          is.na(Description),
          paste0(
            "<b>", id, "</b><br>",
            "No HS name found<br>",
            "Period: ", period_label
          ),
          paste0(
            "<b>", id, "</b><br>",
            htmltools::htmlEscape(Description),
            "<br>Period: ", period_label
          )
        )
      )
    
    # 8. Synchronise capped distance with nodes
    
    nodes$dist_to_root <- igraph::V(network)$dist_to_root_cap[
      match(nodes$id, igraph::V(network)$name)
    ]
    
    # 9. Border colour by distance 0 to 5
    
    uniq_d <- sort(unique(stats::na.omit(nodes$dist_to_root)))
    pal_dist <- grDevices::colorRampPalette(
      c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#f46d43")
    )(max(3, length(uniq_d)))
    col_map_dist <- stats::setNames(pal_dist, uniq_d)
    
    nodes$color.border <- unname(col_map_dist[as.character(nodes$dist_to_root)])
    nodes$color.border[is.na(nodes$color.border)] <- "#999999"
    
    # 10. Fill colour by IC: white if ic <= 0.5 or NA, black if ic > 0.5 
    
    ic_vals <- nodes$ic
    nodes$color.background <- ifelse(
      is.na(ic_vals),
      "#ffffff",
      ifelse(ic_vals > 0.5, "#000000", "#ffffff")
    )
    
    # 11. Visual construction
    
    # reverse edges only for plotting
    edges <- edges[c("to", "from", "value")]
    names(edges) <- c("from", "to", "value")
  
    vis <- toVisNetworkData(network)
    
      
    vis <- visNetwork::visNetwork(nodes, edges, width = "100%", height = "800px") %>%
      visNetwork::visEdges(arrows = "to", smooth = FALSE) %>%
      visNetwork::visNodes(
        shape       = "dot",
        borderWidth = 2,
        size        = 55
      ) %>%
      visNetwork::visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visNetwork::visLayout(randomSeed = 99)
    
    set.seed(99)
    vis <- vis %>%
      visNetwork::visPhysics(
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
      ) %>%
      visNetwork::visEvents(
        stabilizationIterationsDone =
          "function () {
           this.setOptions({ physics: false });
         }"
      )
    
    htmlwidgets::saveWidget(vis, file = out_file, selfcontained = TRUE)
    
    invisible(vis)
  }
  
  # PLOT 
  
  country_codes <- c(
    "China"        = "chn",
    "Morocco"      = "mar",
    "South Africa" = "sa",
    "Japan"        = "jp"
  )
  
  countries_to_plot <- c("China", "Morocco", "South Africa", "Japan")
  year_breaks <- c(2002, 2007, 2012, 2017, 2022)
  
  # Example: graphs_dir must exist already
  # graphs_dir <- "path/to/output/folder"
  
  for (i in seq_len(length(year_breaks) - 1)) {
    start_yr <- year_breaks[i]
    end_yr   <- year_breaks[i + 1]
    
    for (ctry in countries_to_plot) {
      code <- country_codes[ctry]
      
      out_file <- file.path(
        graphs_dir,
        sprintf("network_vis_%s_%d_%d.html", code, start_yr, end_yr)
      )
      
      network_ic_vis_period(ctry, start_yr, end_yr, out_file)
    }
  }

# Annual plot #######################################################

  # Build nodes and edges from gplus
  nodes <- data.frame(
    id    = V(network)$name,
    label = V(network)$name,
    dist_to_root = V(network)$dist_to_root,
    stringsAsFactors = FALSE
  )
  
  product_i1 <- product_i %>%
    dplyr::mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
    dplyr::inner_join(nodes, by = c("hs2002_code_upstream" = "id")) %>%
    dplyr::rename(stage = dist_to_root) %>%
    dplyr::mutate(stage = pmin(stage, 5)) # set more than 5 to 5
  
  years     <- 2002:2022
  countries <- c("China", "Japan", "Morocco", "South Africa")
  stages    <- paste0("stage", 0:5)
  
  product_stage <- product_i1 %>%
    mutate(
      hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")
    ) %>%
    select(hs4_c, stage)
  
  # trade data for all countries and years
  trade_ctr <- map_df(countries, \(ctry) {
    map_df(years, \(yr) {
      get(paste0("trade_net_", yr)) %>%
        filter(country == ctry) %>%
        transmute(
          country = ctry,
          year    = yr,
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
      ic = if_else(exports + imports > 0,
                   exports / (exports + imports),
                   NA_real_),
      stage = factor(
        paste0("stage", stage),
        levels = stages,
        labels = c("Stage 0", "Stage 1", "Stage 2", "Stage 3", "Stage 4", "Stage 5")
      ),
      country = factor(country, levels = countries)
    ) %>%
    filter(!is.na(ic))
  
  # if you want to drop Stage 5 only
  #ic_items_f <- ic_items %>% filter(stage != "Stage 5")
  ic_items_f <- ic_items
  
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
      title = NULL,
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

# Distribution dist to root ###################################################
  
  # settings
  countries_to_plot <- c("China", "Morocco", "South Africa", "Japan")
  year_breaks <- c(2002, 2007, 2012, 2017, 2022)
  
  if (is.null(igraph::vertex_attr(network, "dist_to_root_raw"))) {
    d <- igraph::distances(
      network,
      v   = igraph::V(network)[name == product4d],
      to  = igraph::V(network),
      mode = "out",
      weights = NA
    )
    
    dist_vec <- as.numeric(d[1, ])
    dist_vec[is.infinite(dist_vec)] <- NA_real_
    names(dist_vec) <- igraph::V(network)$name
    
    igraph::V(network)$dist_to_root_raw <- dist_vec
  }
  
  # 1. nodes with uncapped dist_to_root 
  
  nodes <- data.frame(
    id           = V(network)$name,
    dist_to_root = V(network)$dist_to_root_raw,
    stringsAsFactors = FALSE
  )
  
  # 2. attach distance to product inputs
  
  product_i1 <- product_i %>%
    mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
    inner_join(nodes, by = c("hs2002_code_upstream" = "id"))
  
  # 3. HS4 to dist_to_root 
  
  product_dist <- product_i1 %>%
    mutate(hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")) %>%
    select(hs4_c, dist_to_root)
  
  # 4. define periods based on year_breaks
  
  periods <- tibble::tibble(
    period_id    = seq_len(length(year_breaks) - 1),
    period_start = year_breaks[-length(year_breaks)],
    period_end   = year_breaks[-1]
  )
  
  # 5. build period summed trade for each country
  
  trade_ctr <- purrr::map_df(countries_to_plot, function(ctry) {
    purrr::map_df(seq_len(nrow(periods)), function(i) {
      
      years_seq <- periods$period_start[i] : periods$period_end[i]
      
      trade_list <- purrr::map_df(years_seq, function(yr) {
        get(paste0("trade_net_", yr)) %>%
          dplyr::filter(country == ctry) %>%
          dplyr::mutate(year = yr)
      })
      
      trade_list %>%
        dplyr::group_by(country, hs4_c) %>%
        dplyr::summarise(
          exports = sum(exports, na.rm = TRUE),
          imports = sum(imports, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          period_start = periods$period_start[i],
          period_end   = periods$period_end[i],
          period_label = paste0(periods$period_start[i], "-", periods$period_end[i])
        )
    })
  })
  
  # 6. compute IC and join true distance 
  
  ic_items <- trade_ctr %>%
    dplyr::left_join(product_dist, by = "hs4_c") %>%
    dplyr::filter(!is.na(dist_to_root)) %>%
    dplyr::mutate(
      ic = dplyr::if_else(
        exports + imports > 0,
        exports / (exports + imports),
        NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(ic))
  
  # 7. keep only products with ic > 0.5 
  
  ic_gt_05 <- ic_items %>%
    dplyr::filter(ic > 0.5)
  
  mean_lines <- ic_items %>%
    group_by(country) %>%
    summarise(
      mean_dist = mean(dist_to_root, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 8. density plot facetted by country and period
  
  ggplot() +
    # baseline: all products, all periods, per country
    # geom_density(
    #   data = ic_items,
    #   aes(x = dist_to_root, group = country),
    #   colour = "grey20",
    #   linewidth = 0.8,
    #   alpha = 0.4
    # ) +
    
    # coloured curves: IC > 0.5 by period
    # geom_vline(
    #   data = mean_lines,
    #   aes(xintercept = mean_dist),
    #   colour = "black",
    #   linewidth = 0.8,
    #   linetype = "longdash"
    # ) +
    geom_density(
      data = ic_gt_05,
      aes(
        x        = dist_to_root,
        colour   = period_label,
        linetype = period_label
      ),
      linewidth = 0.9
    ) +
    
    scale_colour_manual(
      values = c(
        "2002-2007" = "#8da0cb",
        "2007-2012" = "#66c2a5",
        "2012-2017" = "#fc8d62",
        "2017-2022" = "#e41a1c"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "2002-2007" = "solid",
        "2007-2012" = "dashed",
        "2012-2017" = "dotted",
        "2017-2022" = "dotdash"
      )
    ) +
    scale_x_continuous(
      breaks = 0:max(ic_items$dist_to_root, na.rm = TRUE)
    ) +
    facet_wrap(~ country, nrow = 1) +
    labs(
      title    = NULL,
      x        = "Distance to root",
      y        = NULL,
      colour   = "Period (IC > 0.5)",
      linetype = "Period (IC > 0.5)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = 12, face = "bold"),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 1),
      legend.position  = "bottom"
    )

  ggsave(file.path(graphs_dir, "kernel density_distnaceRoot.jpg"),
         width = 14, height = 6)
  
  
## Correlations ################################################################
  
  countries_to_plot <- c("China", "Japan", "South Africa", "Morocco")
  years <- 2002:2021
  
  ## 0. distance from root and HS4 distance table
  if (is.null(igraph::vertex_attr(network, "dist_to_root_raw"))) {
    d <- igraph::distances(
      network,
      v   = igraph::V(network)[name == product4d],
      to  = igraph::V(network),
      mode = "out",
      weights = NA
    )
    
    dist_vec <- as.numeric(d[1, ])
    dist_vec[is.infinite(dist_vec)] <- NA_real_
    names(dist_vec) <- igraph::V(network)$name
    
    igraph::V(network)$dist_to_root_raw <- dist_vec
  }
  
  nodes <- data.frame(
    id           = igraph::V(network)$name,
    dist_to_root = igraph::V(network)$dist_to_root_raw,
    stringsAsFactors = FALSE
  )
  
  product_i1 <- product_i %>%
    mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
    inner_join(nodes, by = c("hs2002_code_upstream" = "id"))
  
  product_dist <- product_i1 %>%
    mutate(hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")) %>%
    select(hs4_c, dist_to_root) %>%
    distinct()
  
  ## 1. build trade by country, year, product
  trade_ctr <- map_df(countries_to_plot, function(ctry) {
    map_df(years, function(yr) {
      get(paste0("trade_net_", yr)) %>%
        filter(country == ctry) %>%
        mutate(year = yr)
    })
  }) %>%
    group_by(country, year, hs4_c) %>%
    summarise(
      exports = sum(exports, na.rm = TRUE),
      imports = sum(imports, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## 2. join distance and compute IC per country year product
  ic_items <- trade_ctr %>%
    left_join(product_dist, by = "hs4_c") %>%
    filter(!is.na(dist_to_root)) %>%
    mutate(
      ic = if_else(
        exports + imports > 0,
        exports / (exports + imports),
        NA_real_
      )
    ) %>%
    filter(!is.na(ic))
  
  ## remove if distance greater than 5
  ic_items1 <- ic_items %>% filter(dist_to_root <= 5)
  #ic_items1 <- ic_items %>% filter(dist_to_root <= 4)
  
  ## rename stage variable for clarity
  ic_items1 <- ic_items1 %>% 
    mutate(stage = dist_to_root)
  
  ## compute yearly averages for each stage and country
  stage_ts <- ic_items1 %>%
    group_by(country, year, stage) %>%
    summarise(avg_ic = mean(ic, na.rm = TRUE), .groups = "drop") %>%
    arrange(country, stage, year)
  
  ## wide format, one row per country year
  stage_wide <- stage_ts %>%
    pivot_wider(
      names_from = stage,
      values_from = avg_ic,
      names_prefix = "stage_"
    ) %>%
    arrange(country, year)
  
  ## year to year differences by country
  stage_diff <- stage_wide %>%
    group_by(country) %>%
    mutate(
      across(
        starts_with("stage_"),
        ~ .x - lag(.x),
        .names = "{.col}_diff"
      )
    ) %>%
    ungroup()
  
  ## build correlation heatmap data for each country
  movement_cols <- grep("^stage_.*_diff$", names(stage_diff), value = TRUE)
  
  country_list <- split(stage_diff, stage_diff$country)
  
  cor_df_all <- map_dfr(country_list, function(df) {
    mv <- df %>%
      select(all_of(movement_cols)) %>%
      na.omit()
    
    cm <- cor(mv, method = "pearson")
    
    m <- melt(cm,
              varnames = c("stage_x", "stage_y"),
              value.name = "correlation")
    
    m$country <- df$country[1]
    m
  })
  
  ## simplify stage labels by removing "_diff"
  cor_df_all$stage_x <- gsub("_diff", "", cor_df_all$stage_x)
  cor_df_all$stage_y <- gsub("_diff", "", cor_df_all$stage_y)
  
  # simplify further
  cor_df_all <- cor_df_all %>%
    mutate(
      stage_x = paste0("S", gsub("stage_", "", stage_x)),
      stage_y = paste0("S", gsub("stage_", "", stage_y))
    )
  
  ## combined heatmap with one panel per country
  ggplot(cor_df_all, aes(x = stage_x, y = stage_y, fill = correlation)) +
    geom_tile(colour = "white") +                       # thin borders between cells
    geom_text(aes(label = round(correlation, 2)), colour = "black", size = 3) +
    scale_fill_distiller(
      palette = "RdBu", 
      limits  = c(-1, 1),
      direction = -1,                                   # so blue is negative, red positive
      name = "Correlation"
    ) +
    labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +
    facet_wrap(~ country, nrow = 1) +
    coord_equal() +                                     # square tiles
    theme_minimal() +
    theme(
      panel.grid = element_blank(),                     # remove background grid
      panel.border = element_rect(
        colour = "black", fill = NA, linewidth = 1    # solid black border around each heatmap
      ),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      axis.text.x  = element_text(angle = 0, hjust = 1),
      strip.text       = element_text(size = 12, face = "bold"),
      legend.position  = "bottom"
    )
  
  ggsave(file.path(graphs_dir, "correlation heatmap.jpg"),
         width = 12, height = 5)
  
  
  