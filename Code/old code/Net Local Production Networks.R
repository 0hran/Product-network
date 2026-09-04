
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
# supply chain network
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

  # Calculate distance to node
  root <- product4d
  list_of_name <- V(gplus)$name
  # reverse edges so paths flow outward from the root
  g_rev <- igraph::reverse_edges(gplus)
  
  # UNWEIGHTED distances (hops)
  
    # d <- distances(g_rev, v = root, to = list_of_name, mode = "out", weights = NA)
    # dist_vec <- as.numeric(d[1, ])
    # names(dist_vec) <- list_of_name
    # 
    # # attach
    # V(gplus)$dist_to_root <- dist_vec

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
  
  # get inputs
  product_i <- aipnet %>%
    filter(hs2002_code_downstream == product4d) %>%
    distinct(hs2002_code_upstream)
  
################################################################################
# Basic Network visualisation  #################################################
################################################################################

network_ic_vis <- function(ctry, yr, out_file, stages = paste0("stage", 1:5)) {
    # Using global gplus, include IC into node attributes
    
    # 1. Ensure distances exist on gplus
    if (is.null(igraph::vertex_attr(gplus, "dist_to_root"))) {
      g_rev <- igraph::reverse_edges(gplus)
      d <- distances(
        g_rev,
        v = V(g_rev)[name == product4d],
        to = V(g_rev),
        mode = "out",
        weights = NA
      )
      dist_vec <- as.numeric(d[1, ])
      dist_vec[is.infinite(dist_vec)] <- NA_real_
      names(dist_vec) <- V(gplus)$name
      V(gplus)$dist_to_root <- dist_vec
    }
    
    # 2. Build nodes and edges from gplus
    nodes <- data.frame(
      id          = V(gplus)$name,
      label       = V(gplus)$name,
      dist_to_root = V(gplus)$dist_to_root,
      stringsAsFactors = FALSE
    )
    
    if (ecount(gplus) > 0) {
      e_ends <- ends(gplus, E(gplus))
      edges <- data.frame(
        from  = as.character(e_ends[, 1]),
        to    = as.character(e_ends[, 2]),
        value = if (is.null(E(gplus)$weight)) 1 else E(gplus)$weight,
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
    
    # 3. attach stages into root input list
    product_i1 <- product_i %>%
      mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
      inner_join(nodes, by = c("hs2002_code_upstream" = "id")) %>%
      rename(stage = dist_to_root)
    
    product_stage <- product_i1 %>%
      mutate(hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")) %>%
      select(hs4_c, stage)
    
    # 4. Trade data for this country and year
    trade_tbl_name <- paste0("trade_net_", yr)
    if (!exists(trade_tbl_name, inherits = TRUE)) {
      stop("Object ", trade_tbl_name, " not found in the environment")
    }
    
    trade_tbl <- get(trade_tbl_name)
    
    trade_ctr <- trade_tbl %>%
      filter(country == ctry) %>%
      transmute(
        country = ctry,
        year    = yr,
        hs4_c,
        exports,
        imports
      )
    
    # 5. Item level IC values
    ic_items <- trade_ctr %>%
      left_join(product_stage, by = "hs4_c") %>%
      filter(!is.na(stage) | hs4_c == product4d) %>%
      mutate(
        ic      = if_else(exports + imports > 0, exports / (exports + imports), NA_real_),
        stage   = factor(paste0("stage", stage), levels = stages),
        country = factor(country, levels = c(ctry))
      ) %>%
      filter(!is.na(ic))
    
    # 6. Add IC and related variables to nodes by HS code
    nodes <- nodes %>%
      left_join(
        ic_items %>%
          select(id = hs4_c, country, year, exports, imports, stage, ic),
        by = "id"
      )
    
    # 7. Join HS names
    nodes <- nodes %>%
      mutate(id = str_trim(as.character(id))) %>%
      left_join(
        hsnames %>%
          transmute(
            Code        = str_trim(as.character(Code)),
            Description = str_trim(as.character(Description))
          ) %>%
          distinct(Code, .keep_all = TRUE),
        by = c("id" = "Code")
      ) %>%
      mutate(
        label = if_else(
          is.na(Description),
          id,
          paste0(id, "\n", Description)
        ),
        title = if_else(
          is.na(Description),
          paste0("<b>", id, "</b><br>No HS name found"),
          paste0("<b>", id, "</b><br>", htmlEscape(Description))
        )
      )
    
    # 8. Bring distance into the nodes table (synchronise with gplus)
    nodes$dist_to_root <- V(gplus)$dist_to_root[match(nodes$id, V(gplus)$name)]
    
    # 9. Border colour by discrete distance
    uniq_d <- sort(unique(na.omit(nodes$dist_to_root)))
    pal_dist <- colorRampPalette(
      c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
    )(max(3, length(uniq_d)))
    col_map_dist <- setNames(pal_dist, uniq_d)
    
    nodes$color.border <- unname(col_map_dist[as.character(nodes$dist_to_root)])
    nodes$color.border[is.na(nodes$color.border)] <- "#999999"
    
    # 10. Fill colour by IC, graded grey
    ic_vals <- nodes$ic
    
    if (all(is.na(ic_vals))) {
      nodes$color.background <- "#dddddd"
    } else {
      ic_range <- range(ic_vals, na.rm = TRUE)
      
      if (ic_range[1] == ic_range[2]) {
        nodes$color.background <- "#bdbdbd"
      } else {
        pal_ic <- colorRampPalette(c("#f7f7f7", "#252525"))(100)
        
        norm_ic <- (ic_vals - ic_range[1]) / diff(ic_range)
        norm_ic[norm_ic < 0] <- 0
        norm_ic[norm_ic > 1] <- 1
        
        idx <- round(norm_ic * (length(pal_ic) - 1)) + 1
        nodes$color.background <- pal_ic[idx]
      }
      
      nodes$color.background[is.na(nodes$color.background)] <- "#f0f0f0"
    }
    
    # 11. Optional legend entries for distance
    legend_nodes <- if (length(uniq_d)) {
      data.frame(
        label = paste0("Distance ", uniq_d),
        shape = "dot",
        color = unname(col_map_dist[as.character(uniq_d)]),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
    
    # 12. Visualisation
    vis <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
      visEdges(arrows = "to", smooth = FALSE) %>%
      visNodes(
        shape       = "dot",
        borderWidth = 2
      ) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
    
    if (!is.null(legend_nodes)) {
      vis <- vis %>% visLegend(addNodes = legend_nodes, useGroups = FALSE)
    }
    
    set.seed(99)
    vis <- vis %>%
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
      )
    
    # save html
    htmlwidgets::saveWidget(vis, file = out_file, selfcontained = TRUE)
    
    invisible(vis)
  }

  # country_codes <- c(
  #   "South Africa" = "sa",
  #   "China"        = "chn",
  #   "Rep. of Korea"      = "kor"
  # )
  # 
  # countries_to_plot <- c("South Africa", "China", "Rep. of Korea")
  # years_target <- c(2002, 2022)
  # 
  # for (yr in years_target) {
  #   for (ctry in countries_to_plot) {
  #     code <- country_codes[ctry]
  #     out_file <- file.path(
  #       graphs_dir,
  #       sprintf("network_vis_%s%02d.html", code, yr)
  #     )
  #     network_ic_vis(ctry, yr, out_file)
  #   }
  # }

## Concentric ################################################################

# you only need this once, outside the function
stages <- c("stage1", "stage2", "stage3", "stage4", "stage5")

# ensure distances exist once for gplus
if (is.null(V(gplus)$dist_to_root)) {
  g_rev <- igraph::reverse_edges(gplus)
  d <- distances(
    g_rev,
    v = V(g_rev)[name == product4d],
    to = V(g_rev),
    mode = "out",
    weights = NA
  )
  dist_vec <- as.numeric(d[1, ])
  dist_vec[is.infinite(dist_vec)] <- NA_real_
  names(dist_vec) <- V(gplus)$name
  V(gplus)$dist_to_root <- dist_vec
}

# FUNCTION

make_country_plot <- function(ctry, year, outfile) {
  
  # nodes and edges from gplus (fresh copy each time)
  nodes <- data.frame(
    id    = V(gplus)$name,
    label = V(gplus)$name,
    stringsAsFactors = FALSE
  )
  
  # attach distance to nodes straight away
  nodes$dist_to_root <- V(gplus)$dist_to_root[match(nodes$id, V(gplus)$name)]
  
  if (ecount(gplus) > 0) {
    e_ends <- ends(gplus, E(gplus))
    edges <- data.frame(
      from  = as.character(e_ends[, 1]),
      to    = as.character(e_ends[, 2]),
      stringsAsFactors = FALSE
    )
  } else {
    edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  }
  
  # attach stages into root input list
  product_i1 <- product_i %>%
    mutate(hs2002_code_upstream = as.character(hs2002_code_upstream)) %>%
    inner_join(nodes, by = c("hs2002_code_upstream" = "id")) %>%
    rename(stage = dist_to_root)
  
  product_stage <- product_i1 %>%
    mutate(hs4_c = str_pad(hs2002_code_upstream, 4, pad = "0")) %>%
    select(hs4_c, stage)
  
  # trade data for this country and year
  trade_ctr <- get(paste0("trade_net_", year)) %>%
    filter(country == ctry) %>%
    transmute(
      country = ctry,
      year    = year,
      hs4_c,
      exports,
      imports
    )
  
  # item level IC
  ic_items <- trade_ctr %>%
    left_join(product_stage, by = "hs4_c") %>%
    filter(!is.na(stage) | hs4_c == product4d) %>%
    mutate(
      ic      = if_else(exports + imports > 0, exports / (exports + imports), NA_real_),
      stage   = factor(paste0("stage", stage), levels = stages),
      country = factor(country, levels = c(ctry))
    ) %>%
    filter(!is.na(ic))
  
  # add IC etc to nodes
  nodes <- nodes %>%
    left_join(
      ic_items %>%
        select(id = hs4_c, country, year, exports, imports, stage, ic),
      by = "id"
    )
  
  # join HS names
  nodes <- nodes %>%
    mutate(id = str_trim(as.character(id))) %>%
    left_join(
      hsnames %>%
        transmute(
          Code        = str_trim(as.character(Code)),
          Description = str_trim(as.character(Description))
        ) %>%
        distinct(Code, .keep_all = TRUE),
      by = c("id" = "Code")
    )
  
  # distance and colours
  nodes$dist_to_root <- V(gplus)$dist_to_root[match(nodes$id, V(gplus)$name)]
  
  # layout distance: copy of dist_to_root (root forced to centre)
  nodes$dist_for_layout <- nodes$dist_to_root
  nodes$dist_for_layout[nodes$id == product4d] <- 0
  
  uniq_d <- sort(unique(na.omit(nodes$dist_to_root)))
  pal_dist <- colorRampPalette(
    c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  )(max(3, length(uniq_d)))
  col_map_dist <- setNames(pal_dist, uniq_d)
  
  nodes$color.border <- unname(col_map_dist[as.character(nodes$dist_to_root)])
  nodes$color.border[is.na(nodes$color.border)] <- "#999999"
  
  # fill by IC, grey gradient
  # ic_vals <- nodes$ic
  # 
  # if (all(is.na(ic_vals))) {
  #   nodes$color.background <- "#dddddd"
  # } else {
  #   ic_range <- range(ic_vals, na.rm = TRUE)
  #   
  #   if (ic_range[1] == ic_range[2]) {
  #     nodes$color.background <- "#bdbdbd"
  #   } else {
  #     pal_ic <- colorRampPalette(c("#f7f7f7", "#252525"))(100)
  #     
  #     norm_ic <- (ic_vals - ic_range[1]) / diff(ic_range)
  #     norm_ic[norm_ic < 0] <- 0
  #     norm_ic[norm_ic > 1] <- 1
  #     
  #     idx <- round(norm_ic * (length(pal_ic) - 1)) + 1
  #     nodes$color.background <- pal_ic[idx]
  #   }
  #   
  #   nodes$color.background[is.na(nodes$color.background)] <- "#f0f0f0"
  # }
  
  # fill by IC, dichotomic at 0.5 threshold
  # fill by IC simple two level colour
  ic_vals <- nodes$ic
  
  nodes$color.background <- ifelse(
    is.na(ic_vals),
    "#f0f0f0",            # colour for missing IC
    ifelse(ic_vals < 0.5,
           "#ffffff",     # white
           "#252525")     # dark grey
  )
  
  # labels and tooltip with IC
  nodes$ic_fmt <- ifelse(is.na(nodes$ic), "NA", sprintf("%.4f", nodes$ic))
  
  nodes <- nodes %>%
    mutate(
      label = if_else(
        is.na(Description),
        id,
        paste0(id, "\n", Description)
      ),
      title = if_else(
        is.na(Description),
        paste0(
          "<b>", id, "</b>",
          "<br>No HS name found",
          "<br>IC: ", ic_fmt
        ),
        paste0(
          "<b>", id, "</b>",
          "<br>", htmlEscape(Description),
          "<br>IC: ", ic_fmt
        )
      )
    )
  
  # concentric layout
  base_radius <- 300
  nodes$x <- 0
  nodes$y <- 0
  
  ring_vals <- sort(unique(na.omit(nodes$dist_for_layout)))
  
  for (d in ring_vals) {
    idx <- which(nodes$dist_for_layout == d)
    n_ring <- length(idx)
    radius <- as.numeric(d) * base_radius
    
    if (n_ring == 1) {
      nodes$x[idx] <- radius
      nodes$y[idx] <- 0
    } else {
      angles <- seq(0, 2 * pi, length.out = n_ring + 1)[1:n_ring]
      nodes$x[idx] <- radius * cos(angles)
      nodes$y[idx] <- radius * sin(angles)
    }
  }
  
  if (any(is.na(nodes$dist_for_layout))) {
    idx <- which(is.na(nodes$dist_for_layout))
    n_ring <- length(idx)
    radius <- (max(ring_vals, na.rm = TRUE) + 1) * base_radius
    
    angles <- seq(0, 2 * pi, length.out = n_ring + 1)[1:n_ring]
    nodes$x[idx] <- radius * cos(angles)
    nodes$y[idx] <- radius * sin(angles)
  }
  
  # make sure no weight column
  if ("value" %in% names(edges)) {
    edges$value <- NULL
  }
  
  # CHANGED: keep only edges that go within same ring or to next ring
  edges$dist_from <- nodes$dist_for_layout[match(edges$from, nodes$id)]
  edges$dist_to   <- nodes$dist_for_layout[match(edges$to,   nodes$id)]
  
  edges <- edges %>%
    dplyr::filter(
      !is.na(dist_from),
      !is.na(dist_to),
      dist_from <= dist_to + 1,   # from is not more than one ring outward
      dist_from >= dist_to        # from is same ring or one inner ring
    )
  
  edges$dist_from <- NULL
  edges$dist_to   <- NULL
  
  # edge colours still based on dist_to_root of the source
  edges$dist_to_root <- V(gplus)$dist_to_root[
    match(edges$from, V(gplus)$name)
  ]
  
  edge_col <- unname(col_map_dist[as.character(edges$dist_to_root)])
  edge_col[is.na(edge_col)] <- "#cccccc"
  
  # pack colour AND opacity per edge
  edges$color <- lapply(edge_col, function(col) {
    list(
      color     = col,
      highlight = col,
      hover     = col,
      opacity   = 0.25   # set your transparency here
    )
  })
  
  # remove label from visualisation
  nodes$label <- ""
  
  # visualise
  vis <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
    visEdges(
      arrows = "to",
      smooth = FALSE
    ) %>%
    visNodes(
      shape       = "dot",
      borderWidth = 1.5,
      size = 40,
    ) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(enabled = FALSE)
  
  # legend
  # if (!is.null(legend_nodes)) {
  #   vis <- vis %>% visLegend(addNodes = legend_nodes, useGroups = FALSE)
  # }
  
  visSave(vis, outfile)
  invisible(vis)
}

# mapping from country to short code for filenames

year_target <- c(2002,2022)

country_codes <- c(
  "South Africa" = "sa",
  "China"        = "chn",
  "Rep. of Korea"      = "kor"
)

countries_to_plot <- c("South Africa", "China", "Rep. of Korea")

years_target <- c(2002, 2022)

for (yr in years_target) {
  for (ctry in countries_to_plot) {
    code <- country_codes[ctry]
    out_file <- file.path(
      graphs_dir,
      sprintf("concentric_%s%02d.html", code, yr)
    )
    make_country_plot(ctry, yr, out_file)
  }
}

