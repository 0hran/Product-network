# Load packages

library(readxl)
library(igraph)
library(ggraph)
library(ggplot2)
library(igraph)
library(visNetwork)
library(tidyverse)
library(tidygraph)
library(plotly)
library(scales)

# Load functions

  ## Core logic to build direct (non-nested) links from a given product
  build_direct_network <- function(Product, TESTOI) { # Product represents the root product, TESTOI the IOT network
    
    # Below gets all inputs to Product by keeping only rows where Product is the output
    SSet <- TESTOI[TESTOI$hs2022_code_downstream == Product, ]
    
    if (nrow(SSet) == 0) {
      return(character(0))  # no upstream inputs / no inputs for that product
    }
    
    net_data <- c()
    
    # loop over each input product
    for (it in 1:length(SSet$hs2022_code_upstream)) {
      
      # get one input
      lookupproduct <- SSet$hs2022_code_upstream[it]
      # get all other inputs into new dataframe
      Other_upstream <- SSet$hs2022_code_upstream[SSet$hs2022_code_upstream != lookupproduct]
      # cehck whether the input we're considering is an input to any other inputs - if that's the case, make it an indirect input, not a direct one
      resulti <- lookupproduct %in% unlist(TESTOI[TESTOI$hs2022_code_downstream %in% Other_upstream, 1])
      
      if (!resulti) {
        net_data <- append(net_data, paste(Product, lookupproduct, sep = "-"))
      }
    }
    
    return(net_data)
  }

  ## Wrapper function to build tree-shaped network from a root product
  build_full_network <- function(root_product, TESTOI) {
    # Initialize output vector with direct inputs to the root produt ("root_producT")
    full_network <- build_direct_network(root_product, TESTOI)
    
    # Get unique upstream inputs of the root
    upstream_inputs <- unique(TESTOI$hs2022_code_upstream[TESTOI$hs2022_code_downstream == root_product])
    
    for (input in upstream_inputs) {
      # Check if this input is also a downstream (i.e. has its own inputs)
      if (input %in% TESTOI$hs2022_code_downstream) {
        child_network <- build_direct_network(input, TESTOI)
        full_network <- append(full_network, child_network)
      }
    }
    
    return(full_network)
  }

  ## Third wrapper function
  build_named_network <- function(network_vector, product_codes) {
    # Step 1: Parse vector into a 2-column data frame
    network_df <- do.call(rbind, strsplit(network_vector, "-"))
    network_df <- as.data.frame(network_df, stringsAsFactors = FALSE)
    names(network_df) <- c("product_1", "product_2")
    
    # Step 2: Merge product_1 with product_codes
    network_df <- merge(network_df, product_codes,
                        by.x = "product_1", by.y = "code",
                        all.x = TRUE)
    names(network_df)[names(network_df) == "description"] <- "product_1_name"
    
    # Step 3: Merge product_2 with product_codes
    network_df <- merge(network_df, product_codes,
                        by.x = "product_2", by.y = "code",
                        all.x = TRUE)
    names(network_df)[names(network_df) == "description"] <- "product_2_name"
    
    # Final column order
    network_df <- network_df[, c("product_1", "product_2", "product_1_name", "product_2_name")]
    
    return(network_df)
  }


# Import ####
  
  # AIPNET 6 digit
  graphh <- read_excel("C:/Users/arnau/OneDrive/Bureau/Aipnet hs22_6d.xlsx") # Arnaud
  graphh <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/Aipnet hs22_6d.xlsx") # Adria
  
  # AIPNET 4 digit
  graphh <- read.csv("C:/Users/arnau/OneDrive/Bureau/edge_list_hs2022_4digit.csv") # Arnaud
  graphh <- read.csv("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/edge_list_hs2022_4digit.csv")
  
  # Product codes 6 digit
  product_codes_HS22_V202501 <- read_csv("C:/Users/arnau/OneDrive/Bureau/product_codes_HS22_V202501.csv")
  product_codes_HS22_V202501 <- read_csv("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/product_codes_HS22_V202501.csv") # Adria
  
  # Product codes 4 digit
  hsnames <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/HSCodeandDescription.xlsx", sheet = "HS22")
  
  # Product categories (capital, consumption,)
  category <- read_excel("C:/Users/arnau/OneDrive/Bureau/AIPNET_Data_Pack_20241204.xlsx", sheet = "1a. Node List 6-digit HS02")
  category <- read_excel("C:/Users/ar86/OneDrive - SOAS University of London/CSST/Collaborations/CSST Arnaud_Adria/Code/AIPNET_Data_Pack_20241204.xlsx", sheet = "1a. Node List 6-digit HS02")
  
# Set root product of interest ####
  
  product4d <- "8703" 
    # cars: 8703; 
    # Radio Communication Devices: 8517
    # Computers: 8471
    # CNC machine tool: 8457
    # Freezer and refrigerator: 8418
    # Coffee macine, toasters, teamakers: 8516
  
  # product6d <- "870321"
  
  
# Clean ####
  
  # include zero into those with digit = 3 (zeroes were removed automatically when importing into Excel)
  graphh$hs2022_code_upstream <- as.character(graphh$hs2022_code_upstream)
  graphh$hs2022_code_downstream <- as.character(graphh$hs2022_code_downstream)
  
  # Add leading zero to 3-digit codes
  graphh$hs2022_code_upstream[nchar(graphh$hs2022_code_upstream) == 3] <- 
    paste0("0", graphh$hs2022_code_upstream[nchar(graphh$hs2022_code_upstream) == 3])
  
  graphh$hs2022_code_downstream[nchar(graphh$hs2022_code_downstream) == 3] <- 
    paste0("0", graphh$hs2022_code_downstream[nchar(graphh$hs2022_code_downstream) == 3])
  
  
  # Clean hs names so that the dataframe for 4d can be used with the function build named network
  hsnames1 <- hsnames %>% filter(Level == "4") %>% select(Code, Description)
  names(hsnames1) <- tolower(names(hsnames1))
  
  # Remove capital goods from AIPNET
  
    # get code and category
    
    exclude <- hsnames1 %>%
      filter(grepl("^84|^85", code) & code != product4d) %>% # machinery and equipment, filter out root product of interest if it has a code 84 85
      pull(code)
    
    #remove
    graphh_c <- graphh %>%
      filter(!hs2022_code_upstream %in% exclude,
             !hs2022_code_downstream %in% exclude)
    
# Create network ####

  # 6 digits
    
    #network_840721<-build_full_network("840721",graphh)
    #network_6d<-build_full_network(product6d,graphh_c)
    #product_codes_HS22_V202501

  # 4 digits
    
    network_4d<-build_full_network(product4d,graphh_c)
    #product_codes_HS22_V202501
    
  # name product codes and create final network data frame
  
    #named_network <- build_named_network(network_6d, product_codes_HS22_V202501)
    named_network <- build_named_network(network_4d, hsnames1)

  # Siwtch columns to have them sorted as input -to- output  (currently it's output to input)
  
    named_network <- named_network %>%
      rename(
        output = product_1,
        input = product_2,
        output_name = product_1_name,
        input_name = product_2_name
      )
    
    named_network <- named_network %>%
      select(input, output, input_name, output_name)

# Draw plot ####

  g <- graph_from_data_frame(named_network, directed = TRUE)
  
  plot(g,
       vertex.label = V(g)$name,
       vertex.size = 5,
       edge.arrow.size = 0.3,
       layout = layout_with_fr)
  
  # identiy cars
  g <- graph_from_data_frame(named_network, directed = TRUE)
  tg <- as_tbl_graph(g)
  tg <- tg %>%
    mutate(label = ifelse(name == "8706", "8706", NA),
           highlight = name == "8706")
  
  set.seed(11)
  ggraph(tg, layout = "fr") +
    geom_edge_link(arrow = arrow(length = unit(2, 'mm')), end_cap = circle(2, 'mm')) +
    geom_node_point(aes(colour = highlight), size = 3, show.legend = FALSE) +
    geom_node_text(aes(label = label), repel = TRUE, size = 3) +
    scale_colour_manual(values = c("FALSE" = "grey", "TRUE" = "blue")) +
    theme_void()
  
  # as tree
  plot(g,
       layout = layout_as_tree(g),
  #    vertex.label = NA,
       vertex.label = ifelse(V(g)$name %in% product4d, V(g)$name, NA),
       vertex.size = 3,
       edge.arrow.size = 0.2)
  
  # as dendogram
  
  # g_tbl <- as_tbl_graph(g)
  # ggraph(g_tbl, layout = "dendrogram", circular = FALSE) +
  #   geom_edge_diagonal() +
  #   geom_node_point() +
  #   geom_node_text(aes(label = name), hjust = -0.1, size = 2) +
  #   theme_void()

# Explore the issue of disconnected nodes

  # total number of nodes
  vcount(g) # 216 with 4d

  # How many nodes are linked to 8703 - directly, indirectly?
  target_node <- product4d
  direct_in <- neighbors(g, target_node, mode = "in")
  direct_out <- neighbors(g, target_node, mode = "out") # note that it captures cases where root product is an input
  direct_nodes <- union(direct_in, direct_out)

  component_nodes <- subcomponent(g, target_node, mode = "all")
  indirect_nodes <- setdiff(component_nodes, c(direct_nodes, V(g)[name == target_node]))

  all_nodes <- V(g)
  linked_nodes <- union(component_nodes, V(g)[name == target_node])
  not_linked_nodes <- setdiff(all_nodes, linked_nodes)

  length(V(g)[direct_nodes])       # Directly linked
  length(V(g)[indirect_nodes])     # Indirectly linked
  length(V(g)[not_linked_nodes])   # Not linked
  
# Explore if we can stratify into hierarchy using centrality etc. measures
  
  g_full <- graph_from_data_frame(
    d = graphh_c,
    directed = TRUE,
    vertices = NULL  # igraph automatically infers nodes from edges
  )
  

  #centr_val <- betweenness(g_full, directed = TRUE)
  centr_val <- page_rank(g_full, directed = TRUE)$vector
  
  # Step 3: Extract scores into a dataframe
  centrality_df <- data.frame(
    hs2022_code = names(centr_val),
    centrality = centr_val
  )
  
  # Step 1: Create two versions of centrality_df with distinct names
  centrality_input_df <- centrality_df %>%
    rename(es_input = centrality)
  
  centrality_output_df <- centrality_df %>%
    rename(es_output = centrality)
  
  # Step 2: Join each separately
  named_network1 <- named_network %>%
    left_join(centrality_input_df, by = c("input" = "hs2022_code")) %>%
    left_join(centrality_output_df, by = c("output" = "hs2022_code"))
  
  g_prod <- graph_from_data_frame(named_network1, directed = TRUE)
  
  V(g_prod)$centrality <- centrality_df$centrality[match(V(g_prod)$name, centrality_df$hs2022_code)]
  V(g_prod)$centrality[is.na(V(g_prod)$centrality)] <- 0
  
  # Step 3: Add product names for hover text
  product_names <- unique(named_network1 %>%
                            select(input, input_name) %>% rename(code = input, name = input_name) %>%
                            bind_rows(named_network1 %>%
                                        select(output, output_name) %>% rename(code = output, name = output_name))) %>%
    distinct()
  
  V(g_prod)$label <- product_names$name[match(V(g_prod)$name, product_names$code)]
  
  # Step 4: Compute node layout coordinates
  layout_coords <- layout_with_fr(g_prod)
  layout_df <- as.data.frame(layout_coords)
  names(layout_df) <- c("x", "y")
  layout_df$label <- V(g_prod)$label
  layout_df$centrality <- V(g_prod)$centrality
  layout_df$name <- V(g_prod)$name
  
  # Step 5: Extract edge coordinates using node names (FIXED)
  edges <- igraph::as_data_frame(g_prod, what = "edges")
  edge_coords <- data.frame(
    x = layout_coords[match(edges$from, V(g_prod)$name), 1],
    y = layout_coords[match(edges$from, V(g_prod)$name), 2],
    xend = layout_coords[match(edges$to, V(g_prod)$name), 1],
    yend = layout_coords[match(edges$to, V(g_prod)$name), 2]
  )
  
  # Step 6: Plot interactive network with plotly
  p <- plot_ly(type = "scatter", mode = "lines") %>%
    # Add edges
    add_segments(
      data = edge_coords,
      x = ~x, y = ~y, xend = ~xend, yend = ~yend,
      line = list(width = 0.3, color = "#cccccc"),
      showlegend = FALSE,
      inherit = FALSE
    ) %>%
    # Add nodes
    add_markers(
      data = layout_df,
      x = ~x, y = ~y,
      text = ~label,
      hoverinfo = "text",
      marker = list(
        size = ~centrality*10000,
        color = ~centrality,
        colorscale = "Plasma",
        showscale = TRUE,
        line = list(width = 0.5, color = "#222222")
      )
    ) %>%
    layout(
      title = "Car Supply Chain Network with Centrality",
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  
  p
  
  ## Reproduce the plot with arrows (cannot use plotly, use ggraph)
  
  # Assign PageRank to vertices
  V(g_prod)$centrality <- centrality_df$centrality[match(V(g_prod)$name, centrality_df$hs2022_code)]
  V(g_prod)$centrality[is.na(V(g_prod)$centrality)] <- 0
  
  # Add labels
  product_names <- unique(named_network1 %>%
                            select(input, input_name) %>% rename(code = input, name = input_name) %>%
                            bind_rows(named_network1 %>%
                                        select(output, output_name) %>% rename(code = output, name = output_name))) %>%
    distinct()
  
  V(g_prod)$label <- product_names$name[match(V(g_prod)$name, product_names$code)]
  
  # Step 4: Plot with ggraph
  set.seed(22)
  ggraph(g_prod, layout = "fr") +
    geom_edge_link(
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      end_cap = circle(2, "mm"),
      colour = "grey80"
    ) +
    geom_node_point(aes(size = centrality, colour = centrality)) +
    geom_node_text(
      aes(label = ifelse(name == product4d, name, NA_character_)),
      colour = "red",
      size = 3
    ) +
    geom_node_text(
      aes(label = label),
      repel = TRUE,
      size = 3,
      max.overlaps = 1  # adjust this number as needed
    ) +
    scale_colour_viridis_c(option = "plasma") +
    labs(title = "Car Supply Chain Network with PageRank Centrality") +
    theme_void() +
    theme(legend.position = "none")
  
# Checks ####
  
  # Does the algorithm remove or add nodes?
  # 1. Get all unique nodes from final network
  all_nodes <- unique(c(named_network$input, named_network$output))
  
  # 2. Get original upstream inputs to 8516 before filtering
  original_inputs <- graphh_c %>%
    filter(hs2022_code_downstream == product4d) %>%
    pull(hs2022_code_upstream) %>%
    unique()
  
  # 3. Identify which original inputs were removed (not in final network)
  removed_inputs <- setdiff(original_inputs, all_nodes)
  print(removed_inputs)
  added_inputs <- setdiff(all_nodes, original_inputs)
  print(added_inputs)
  
# To check ####
  
  # check that we're using the same version of AIPNET
  # note that the bec_desc is for 6d HS02


    