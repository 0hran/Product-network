# =============================================================================
# FULL SCRIPT: Supply Chain Network Builder with Iterative Node Rewiring
# =============================================================================
# OVERVIEW:
# This script builds a directed upstream supply chain graph for a target product
# (e.g., car = HS code 8703). The original code identifies all upstream inputs,
# scores their position in the value chain, and assembles a directed igraph object.
#
# REWIRING ALGORITHM:
# After constructing gnetwork, instead of simply deleting non-feasible edges and
# losing isolated nodes, we apply an iterative rewiring algorithm:
#
#   PRE-LOOP:
#     - Before any filtering, identify nodes already disconnected from root
#     - Wire each to its first downstream neighbour in the original graph
#     - If no downstream neighbour exists, wire directly to root
#
#   ITERATIVE LOOP:
#     1. Filter edges against Hidd_network_full (remove non-feasible edges)
#        Bridge edges added by rewiring are always preserved
#     2. Identify nodes that became disconnected after filtering
#     3. For each disconnected node, look up its first downstream neighbour
#        in gnetwork_original (the permanent unfiltered reference)
#     4. Wire orphan → first downstream candidate
#     5. Repeat until no disconnected nodes remain
#
#   KEY DESIGN PRINCIPLE:
#     - gnetwork_original is ALWAYS the snapshot reference — never the
#       evolving filtered copy. This ensures downstream neighbour memory
#       is never lost across iterations.
#     - The loop is the safety net: if a rewire target is itself disconnected,
#       the next iteration handles it. No reachability check needed.
#     - Only falls back to root if orphan has NO downstream neighbours at all
#       in gnetwork_original.
#
# REWIRING LOG:
#   At each pass a snapshot of rewired edges is saved in
#   rewiring_log_by_iteration (named list: "pre_loop", "iteration_1", ...).
#   Exported as a multi-sheet Excel workbook at the end.
# =============================================================================

rm(list = ls())

# ── Packages ──────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(igraph)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(readxl)
  library(visNetwork)
  library(htmltools)
  library(tidyverse)
  library(readr)
  library(openxlsx)
})

# ── Paths & product selection ──────────────────────────────────────────────────
Key <- "C:/Users/ap115/OneDrive - SOAS University of London/Adria Rius's files - Research collab. AP-AR/"
# Key <- "C:/Users/ar86/OneDrive - SOAS University of London/Research collab. AP-AR/"

#product4d <- "8712" # bicycle
#product4d <- "8503" #Battery
#product4d <- "8418" #Fridge
product4d <- "8703" # car

# ── Raw data loads ─────────────────────────────────────────────────────────────
graphh       <- read_excel(paste0(Key, "Code/edge_list_hs2002_4digit.xlsx"))
hsnames      <- read_excel(paste0(Key, "Code/HSCodeandDescription.xlsx"), sheet = "HS02")
BEC_database <- read_excel(paste0(Key, "Data/BEC database.xlsx"))

# ── Capital-goods exclusion list ───────────────────────────────────────────────
# Keep only BEC "CAP" goods, then remove the target product itself from the list
# so we never accidentally exclude it during cleaning.
Capital_good <- BEC_database$HS6[BEC_database$BEC5EndUse == "CAP"]
Capital_good <- Capital_good[product4d != substr(Capital_good, 1, 4)]

# Restrict hsnames to 4-digit level only
hsnames <- hsnames[hsnames$Level == 4, ]

# =============================================================================
# SECTION 1 — Data-cleaning helpers   (UNCHANGED)
# =============================================================================
clean_edges <- function(graphh) {
  # Coerce both edge columns to character, then left-pad 3-digit codes with "0"
  # so that e.g. "840" becomes "0840" and matches 4-digit HS codes consistently.
  graphh <- graphh %>%
    mutate(
      hs2002_code_upstream   = as.character(hs2002_code_upstream),
      hs2002_code_downstream = as.character(hs2002_code_downstream)
    )
  pad3 <- function(x) ifelse(nchar(x) == 3, paste0("0", x), x)
  graphh %>%
    mutate(
      hs2002_code_upstream   = pad3(hs2002_code_upstream),
      hs2002_code_downstream = pad3(hs2002_code_downstream)
    )
}

# Apply cleaning, then drop all edges where either endpoint is a capital good
graphh_clean <- clean_edges(graphh)
graphh_clean <- graphh_clean[
  !graphh_clean$hs2002_code_upstream   %in% unique(substr(Capital_good, 1, 4)), ]
graphh_clean <- graphh_clean[
  !graphh_clean$hs2002_code_downstream %in% unique(substr(Capital_good, 1, 4)), ]

# =============================================================================
# SECTION 2 — Identify all direct inputs of the target product  (UNCHANGED)
# =============================================================================
build_hidden_graph <- function(ed, product) {
  # Returns only the rows where the target product is the downstream node,
  # i.e. the set of all products that feed directly into `product`.
  ed[ed$hs2002_code_downstream == product, ]
}

Hidden_network <- build_hidden_graph(graphh_clean, product4d)
potential_p1   <- Hidden_network$hs2002_code_upstream
potential_p1   <- c(potential_p1, product4d)   # include the target product itself

# Keep only edges where BOTH endpoints belong to the direct-input set.
# This creates the closed subgraph of mutual linkages among direct inputs.
Hidden_net <- graphh_clean[
  graphh_clean$hs2002_code_upstream   %in% potential_p1 &
    graphh_clean$hs2002_code_downstream %in% potential_p1, ]

Hidden_net           <- as.data.frame(Hidden_net)
colnames(Hidden_net) <- c("col", "row")   # col = upstream/from,  row = downstream/to
Full_net             <- Hidden_net

Fish_network       <- cbind.data.frame("id" = seq_len(nrow(Full_net)), Full_net)
colnames(Full_net) <- c("Source", "Target")

write.xlsx(Full_net,
           file = paste0(Key, "Data/Full_network", product4d, ".xlsx"))

# =============================================================================
# SECTION 3 — Upstreamness scoring  (UNCHANGED)
# =============================================================================
# Upstream score = (# products this node outputs to) / (# products it takes as input)
# High value → more downstream; low value → more upstream / raw material

Dta  <- c()
Dta2 <- c()
for (y in seq_along(potential_p1)) {
  Dta1 <- length(Hidden_net$col[Hidden_net$row == potential_p1[y]]) /
    length(Hidden_net$row[Hidden_net$col == potential_p1[y]])
  Dta  <- c(Dta,  Dta1)
  Dta2 <- c(Dta2, potential_p1[y])
}

Full_dta           <- cbind.data.frame(Dta2, Dta)
colnames(Full_dta) <- c("Id", "Upstream2")
FDU                <- Full_dta

# Build per-product sub-chains and score each product locally within its chain
Full_list <- list()

for (t in seq_along(potential_p1)) {
  
  # kuj1 = all products that receive output FROM potential_p1[t]
  kuj1           <- Hidden_net$row[Hidden_net$col == potential_p1[t]]
  kuj            <- c(potential_p1[t], kuj1)
  
  # Restrict Hidden_net to nodes in this sub-chain
  Hidden_net_kuj <- Hidden_net[
    Hidden_net$col %in% kuj & Hidden_net$row %in% kuj, ]
  
  kuj2 <- kuj1[kuj1 != potential_p1[t]]
  
  # Guard: if kuj2 is empty (this node has no downstream connections),
  # create an empty data frame directly to avoid cbind.data.frame() crash.
  # This happens when a node in potential_p1 has no outgoing edges in Hidden_net.
  if (length(kuj2) == 0) {
    Full_list[[t]] <- data.frame(Id = character(0), Upstream1 = numeric(0))
    next   # skip to the next iteration of the outer loop
  }
  
  Dta  <- c()
  Dta2 <- c()
  for (y in seq_along(kuj2)) {
    Dta1 <- length(Hidden_net_kuj$col[Hidden_net_kuj$row == kuj[y]]) /
      length(Hidden_net_kuj$row[Hidden_net_kuj$col == kuj[y]])
    Dta  <- c(Dta,  Dta1)
    Dta2 <- c(Dta2, kuj[y])
  }
  
  KUJ_dta           <- cbind.data.frame(Dta2, Dta)
  colnames(KUJ_dta) <- c("Id", "Upstream1")
  Full_list[[t]]    <- KUJ_dta
}

# Merge local (Upstream1) and global (Upstream2) scores into each list element
for (t in seq_along(potential_p1)) {
  Full_list[[t]] <- merge(Full_list[[t]], Full_dta, by = "Id", all.x = TRUE)
}

# =============================================================================
# SECTION 4 — Rank by upstreamness and clean scores  (UNCHANGED)
# =============================================================================
hsnames$id <- hsnames$Code

for (t in seq_along(potential_p1)) {
  # Sort: most upstream first (descending Upstream1, then Upstream2)
  Full_list[[t]] <- Full_list[[t]] %>%
    arrange(desc(Upstream1), desc(Upstream2))
  
  # Replace Inf (node with no downstream inputs = purest upstream) with 100
  Full_list[[t]]$Upstream1[Full_list[[t]]$Upstream1 == Inf] <- 100
  Full_list[[t]]$Upstream2[Full_list[[t]]$Upstream2 == Inf] <- 100
  
  # Replace NaN (0/0 = isolated node) with 0
  Full_list[[t]]$Upstream1[is.nan(Full_list[[t]]$Upstream1)] <- 0
  Full_list[[t]]$Upstream2[is.nan(Full_list[[t]]$Upstream2)] <- 0
}

# =============================================================================
# SECTION 5 — Assemble igraph from all chains  (UNCHANGED)
# =============================================================================
# Collect all unique node IDs across every chain
all_nodes <- Full_list %>%
  map(~ .x %>% select(Id) %>% distinct()) %>%
  bind_rows() %>%
  distinct(Id)

# Build edges: each consecutive pair of nodes within a chain forms a directed edge
edges <- imap_dfr(Full_list, function(df, chain_id) {
  df <- df %>%
    select(Id) %>%
    distinct() %>%
    mutate(Id = as.character(Id))
  
  if (nrow(df) < 2) {
    return(tibble(from = character(), to = character(),
                  chain_id = integer(), step = integer()))
  }
  
  tibble(
    from     = df$Id[-nrow(df)],
    to       = df$Id[-1],
    chain_id = as.integer(chain_id),
    step     = seq_len(nrow(df) - 1)
  )
})

# Summarise edges: weight = how many chains share the same directed edge
edge_summary <- edges %>%
  group_by(from, to) %>%
  summarise(
    weight = n(),
    chains = paste(sort(unique(chain_id)), collapse = ","),
    .groups = "drop"
  )

# Build directed igraph; reverse edges so arrows point upstream → root
g        <- graph_from_data_frame(d = edge_summary, vertices = all_nodes,
                                  directed = TRUE)
gnetwork <- reverse_edges(g)

# Attach global upstreamness score (Upstream2) as a vertex attribute
vertex_names     <- get.vertex.attribute(gnetwork, "name")
upstream2_values <- setNames(FDU$Upstream2, FDU$Id)
gnetwork <- set.vertex.attribute(gnetwork, "Upstream2",
                                 value = upstream2_values[vertex_names])

# Attach all HS name columns as vertex attributes
for (col in setdiff(names(hsnames), "id")) {
  attr_values <- setNames(hsnames[[col]], hsnames$id)
  gnetwork    <- set.vertex.attribute(gnetwork, col,
                                      value = attr_values[vertex_names])
}

# =============================================================================
# SECTION 6 — Build the full HS reference graph for feasibility  (UNCHANGED)
# =============================================================================
# Hidd_network_full is the feasibility oracle: an edge is valid only if it
# exists in the original HS trade graph (after capital goods removal).
edges_hs <- graphh_clean %>%
  transmute(from = hs2002_code_upstream, to = hs2002_code_downstream)

Hidd_network <- graph_from_data_frame(edges_hs, directed = TRUE)

# Convert both graphs to plain data frames for set operations
edges_full <- as.data.frame(get.edgelist(gnetwork), stringsAsFactors = FALSE)
colnames(edges_full) <- c("from", "to")

Hidd_network_full <- as.data.frame(get.edgelist(Hidd_network), stringsAsFactors = FALSE)
colnames(Hidd_network_full) <- c("from", "to")

# =============================================================================
# SECTION 7 — Helper functions
# =============================================================================

# ── Helper: find nodes with no directed path to root ──────────────────────────
# Uses hop-count distances (weights = NA) to avoid NaN crashes from bridge edges.
# mode = "out" follows edge direction outward from each node toward root.
nodes_disconnected_from_root <- function(g, root) {
  root_idx <- which(V(g)$name == root)
  if (length(root_idx) == 0) return(character(0))
  
  dist_to_root <- distances(g, to = root_idx, mode = "out", weights = NA)
  
  disconnected <- rownames(dist_to_root)[is.infinite(dist_to_root[, 1])]
  disconnected <- disconnected[disconnected != root]
  return(disconnected)
}

# ── Helper: get immediate downstream neighbours of a node ─────────────────────
# mode = "out" follows edge direction: edges point upstream → root,
# so "out" neighbours are one step closer to root (downstream).
downstream_neighbours <- function(g, node_name) {
  node_idx <- which(V(g)$name == node_name)
  if (length(node_idx) == 0) return(character(0))
  nbrs <- neighbors(g, node_idx, mode = "out")
  V(g)$name[nbrs]
}

# ── Helper: remove non-feasible edges, always preserving bridge edges ──────────
# feasible_df   = (from, to) data frame of all HS-trade-valid edges
# rewired_edges = (from, to) data frame of bridge edges added by the algorithm;
#                 these are permanently exempt from removal
filter_to_feasible <- function(g, feasible_df, rewired_edges) {
  el           <- as.data.frame(get.edgelist(g), stringsAsFactors = FALSE)
  colnames(el) <- c("from", "to")
  el$edge_idx  <- seq_len(nrow(el))
  
  in_feasible <- semi_join(el, feasible_df,  by = c("from", "to"))$edge_idx
  in_rewired  <- if (nrow(rewired_edges) > 0) {
    semi_join(el, rewired_edges, by = c("from", "to"))$edge_idx
  } else {
    integer(0)
  }
  
  keep_idx   <- union(in_feasible, in_rewired)
  remove_idx <- setdiff(el$edge_idx, keep_idx)
  
  if (length(remove_idx) > 0) g <- delete_edges(g, remove_idx)
  return(g)
}

# ── Helper: annotate a (from, to) rewire data frame for the log ───────────────
# Attaches HS descriptions and flags whether the target is the root.
build_log_entry <- function(rewires_df, hsnames, product4d) {
  if (is.null(rewires_df) || nrow(rewires_df) == 0) return(NULL)
  rewires_df %>%
    mutate(
      from_description = hsnames$Description[match(from, hsnames$Code)],
      to_description   = hsnames$Description[match(to,   hsnames$Code)],
      rewired_to_root  = (to == product4d)
    ) %>%
    arrange(rewired_to_root, from)   # intermediate rewires first, root rewires last
}

# ── Core rewiring subroutine ───────────────────────────────────────────────────
# DESIGN PRINCIPLE: blind first-candidate approach using gnetwork_original.
#
# For each disconnected node:
#   1. Look up its downstream neighbours in gnetwork_original (the permanent
#      unfiltered reference — never loses edge memory across iterations)
#   2. Take the first candidate blindly — no reachability check
#   3. If that candidate is itself later disconnected, the next loop iteration
#      handles it automatically
#   4. Only falls back to root if the node has NO downstream neighbours at all
#
# Returns:
#   $graph       = updated graph with new bridge edges added
#   $new_rewires = (from, to) data frame of edges added in this call
rewire_disconnected <- function(g, disconnected_nodes, original_graph, product4d) {
  
  new_rewires_this_pass <- data.frame(from = character(), to = character(),
                                      stringsAsFactors = FALSE)
  
  for (orphan in disconnected_nodes) {
    
    # Always look up downstream neighbours in the ORIGINAL unfiltered graph.
    # This is the key design decision: gnetwork_original never loses its edge
    # memory no matter how many filtering passes have occurred.
    candidates <- downstream_neighbours(original_graph, orphan)
    
    if (length(candidates) == 0) {
      # Orphan has no downstream neighbours even in the original graph.
      # This means it was truly terminal — wire directly to root as last resort.
      target <- product4d
    } else {
      # Blind approach: take the first downstream candidate.
      # If this candidate is also disconnected, the next iteration rewires it.
      # The loop is the safety net — no reachability check needed.
      target <- candidates[1]
    }
    
    # Check the edge does not already exist to avoid duplicates
    existing_el           <- as.data.frame(get.edgelist(g), stringsAsFactors = FALSE)
    colnames(existing_el) <- c("from", "to")
    already_exists        <- any(existing_el$from == orphan & existing_el$to == target)
    
    if (!already_exists) {
      # Re-add vertices if they were dropped as degree-0 in a previous pass
      if (!orphan %in% V(g)$name) g <- add_vertices(g, 1, name = orphan)
      if (!target %in% V(g)$name) g <- add_vertices(g, 1, name = target)
      
      # Add bridge edge with weight = 1 (finite value avoids NaN in distances())
      g <- add_edges(
        g,
        c(which(V(g)$name == orphan), which(V(g)$name == target)),
        weight = 1
      )
      
      new_rewires_this_pass <- rbind(
        new_rewires_this_pass,
        data.frame(from = orphan, to = target, stringsAsFactors = FALSE)
      )
      cat("  Rewired:", orphan, "→", target, "\n")
    }
  }
  
  list(graph = g, new_rewires = new_rewires_this_pass)
}

# =============================================================================
# SECTION 8 — Initialise rewiring tracking objects
# =============================================================================

# gnetwork_original: permanent unfiltered reference — never modified.
# Used by rewire_disconnected() to look up downstream neighbours in every
# iteration, ensuring edge memory is never lost across filtering passes.
gnetwork_original <- gnetwork

# gnetwork_rewired: working copy that is mutated through the loop
gnetwork_rewired  <- gnetwork

# Flat running log of ALL bridge edges ever added (from, to).
# Passed to filter_to_feasible() to permanently protect bridge edges.
rewired_edges_log <- data.frame(from = character(), to = character(),
                                stringsAsFactors = FALSE)

# Named list: one annotated data frame per pass.
# Names: "pre_loop", "iteration_1", "iteration_2", ...
# Exported as a multi-sheet Excel workbook at the end.
rewiring_log_by_iteration <- list()

# =============================================================================
# SECTION 9 — PRE-LOOP: wire nodes already disconnected before any filtering
# =============================================================================
# These nodes never had a valid path to product4d from the very start —
# before any edge removal has occurred. We handle them first so the
# iterative loop only deals with nodes disconnected BY filtering.

cat("=== Pre-loop: checking for nodes disconnected before filtering ===\n")

initial_disconnected <- nodes_disconnected_from_root(gnetwork_rewired, product4d)
cat("Nodes disconnected before filtering:", length(initial_disconnected), "\n")

if (length(initial_disconnected) > 0) {
  
  result <- rewire_disconnected(
    g                  = gnetwork_rewired,
    disconnected_nodes = initial_disconnected,
    original_graph     = gnetwork_original,   # always the permanent reference
    product4d          = product4d
  )
  
  gnetwork_rewired  <- result$graph
  rewired_edges_log <- rbind(rewired_edges_log, result$new_rewires)
  
  rewiring_log_by_iteration[["pre_loop"]] <- build_log_entry(
    result$new_rewires, hsnames, product4d
  )
  
  cat("Pre-loop rewiring complete:", nrow(result$new_rewires), "edge(s) added.\n")
  
} else {
  cat("No pre-loop rewiring needed.\n")
  rewiring_log_by_iteration[["pre_loop"]] <- NULL
}

# =============================================================================
# SECTION 10 — ITERATIVE LOOP: rewire nodes disconnected by filtering
# =============================================================================
# Each iteration:
#   1. Filter edges against Hidd_network_full (bridge edges always preserved)
#   2. Identify nodes disconnected after filtering
#   3. Wire each orphan to its first downstream neighbour in gnetwork_original
#   4. Repeat until no disconnected nodes remain
#
# gnetwork_original is always the snapshot — never the evolving filtered copy.
# This is what makes the loop work: downstream neighbour memory is never lost.

max_iterations <- 50
iteration      <- 0

cat("\n=== Starting iterative rewiring ===\n")

repeat {
  iteration  <- iteration + 1
  iter_label <- paste0("iteration_", iteration)
  
  if (iteration > max_iterations) {
    warning("Rewiring did not converge within ", max_iterations,
            " iterations. Inspect manually.")
    break
  }
  
  # ── Step 1: filter non-feasible edges ───────────────────────────────────────
  # Bridge edges in rewired_edges_log are always kept regardless of feasibility.
  gnetwork_rewired <- filter_to_feasible(gnetwork_rewired,
                                         Hidd_network_full,
                                         rewired_edges_log)
  
  # ── Step 2: find nodes disconnected after this filtering pass ───────────────
  disconnected_nodes <- nodes_disconnected_from_root(gnetwork_rewired, product4d)
  
  cat("Iteration", iteration, "— disconnected nodes:", length(disconnected_nodes), "\n")
  
  # ── Step 3: stop if nothing left to rewire ──────────────────────────────────
  if (length(disconnected_nodes) == 0) {
    cat("All nodes connected. Rewiring complete after", iteration, "iteration(s).\n")
    rewiring_log_by_iteration[[iter_label]] <- data.frame(
      from = character(), to = character(),
      from_description = character(), to_description = character(),
      rewired_to_root  = logical()
    )
    break
  }
  
  # ── Step 4: rewire all disconnected nodes ───────────────────────────────────
  # Always uses gnetwork_original as the downstream neighbour reference.
  result <- rewire_disconnected(
    g                  = gnetwork_rewired,
    disconnected_nodes = disconnected_nodes,
    original_graph     = gnetwork_original,
    product4d          = product4d
  )
  
  gnetwork_rewired  <- result$graph
  rewired_edges_log <- rbind(rewired_edges_log, result$new_rewires)
  
  rewiring_log_by_iteration[[iter_label]] <- build_log_entry(
    result$new_rewires, hsnames, product4d
  )
  
  # ── Step 5: safety break if no progress ─────────────────────────────────────
  # If no new rewires were added, the remaining disconnected nodes cannot be
  # resolved (e.g. they have no downstream neighbours in gnetwork_original
  # other than already-tried targets). Stop to avoid an infinite loop.
  if (nrow(result$new_rewires) == 0) {
    cat("No new rewires in iteration", iteration, "— stopping early.\n")
    break
  }
}

# =============================================================================
# SECTION 11 — DIAGNOSTIC: rewiring summary
# =============================================================================

cat("\n=== Rewiring summary by iteration ===\n")

for (pass_name in names(rewiring_log_by_iteration)) {
  entry <- rewiring_log_by_iteration[[pass_name]]
  
  if (is.null(entry) || nrow(entry) == 0) {
    cat(pass_name, ": 0 rewires\n")
    next
  }
  
  n_total        <- nrow(entry)
  n_to_root      <- sum(entry$rewired_to_root)
  n_intermediate <- n_total - n_to_root
  
  cat(pass_name, ": total =", n_total,
      "| to intermediate =", n_intermediate,
      "| to root =", n_to_root, "\n")
}

# Final check: any nodes still disconnected after all rewiring?
final_disconnected <- nodes_disconnected_from_root(gnetwork_rewired, product4d)
cat("\nFinal disconnected nodes remaining:", length(final_disconnected), "\n")

if (length(final_disconnected) > 0) {
  print(data.frame(
    node        = final_disconnected,
    description = hsnames$Description[match(final_disconnected, hsnames$Code)]
  ))
}

# =============================================================================
# SECTION 12 — Post-rewiring cleanup
# =============================================================================

# Remove any remaining degree-0 vertices (should be none after full rewiring)
gnetwork_new <- delete_vertices(gnetwork_rewired,
                                which(degree(gnetwork_rewired) == 0))

# Re-attach HS metadata to all vertices.
# Needed because add_vertices() does not copy attributes automatically —
# vertices re-added during rewiring need their attributes refreshed here.
vertex_names_new <- get.vertex.attribute(gnetwork_new, "name")

gnetwork_new <- set.vertex.attribute(
  gnetwork_new, "Upstream2",
  value = upstream2_values[vertex_names_new]
)

for (col in setdiff(names(hsnames), "id")) {
  attr_values  <- setNames(hsnames[[col]], hsnames$id)
  gnetwork_new <- set.vertex.attribute(gnetwork_new, col,
                                       value = attr_values[vertex_names_new])
}

# =============================================================================
# SECTION 13 — Export
# =============================================================================

# ── Main network outputs ───────────────────────────────────────────────────────
write.xlsx(as.matrix(get.adjacency(gnetwork_new)),
           rowNames = TRUE,
           file = paste0(Key, "Code/gnetwork_filtered", product4d, ".xlsx"))

vertex_df <- as.data.frame(vertex_attr(gnetwork_new))
vdf       <- cbind.data.frame(id = vertex_df$name, vertex_df)

write.xlsx(vdf, rowNames = FALSE,
           file = paste0(Key, "Output/Vertex_Data", product4d, ".xlsx"))

# gnetwork_new is the final object used by all downstream scripts
saveRDS(gnetwork_new,
        paste0(Key, "Output/PN_links_", product4d, "Final_version.rds"))

# ── Rewiring log: one sheet per iteration ─────────────────────────────────────
# Each sheet: from, from_description, to, to_description, rewired_to_root
# An empty sheet means no rewires were needed in that pass.
rewiring_wb <- createWorkbook()

for (pass_name in names(rewiring_log_by_iteration)) {
  entry <- rewiring_log_by_iteration[[pass_name]]
  
  addWorksheet(rewiring_wb, sheetName = pass_name)
  
  if (!is.null(entry) && nrow(entry) > 0) {
    writeData(rewiring_wb, sheet = pass_name, x = entry)
  } else {
    writeData(rewiring_wb, sheet = pass_name,
              x = data.frame(note = "No rewires in this pass"))
  }
}

saveWorkbook(rewiring_wb, overwrite = TRUE,
             file = paste0(Key, "Output/Rewiring_log_by_iteration_",
                           product4d, ".xlsx"))

cat("\n=== Script complete. All outputs saved. ===\n")
