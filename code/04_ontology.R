### 04_ontology.R
###
### Goal: Create ontology
###
### Content: 
### 1. Ontology of all provisions
### 2. Ontology for segmental autonomy
###

### 1. Ontology of all provisions

psp_ont <- ontology_data[[1]]$Sheet1
psp_ont <- dplyr::as_tibble(psp_ont)

# Select relevant variables
psp_ont_prep <- 
  psp_ont %>%
  dplyr::select(
    child_0,
    child_1,
    provisions
  )

# Generate pathString as new column
psp_ont_prep$pathString <- 
  paste("Power Sharing Provision",
        psp_ont_prep$child_0,
        psp_ont_prep$child_1,
        psp_ont_prep$provisions,
        sep = "|"
  )

# Create list 
psp_tree <- data.tree::as.Node(psp_ont_prep, pathDelimiter = "|")
print(psp_tree, limit = 15)

# Visualizations

# Pepare list for visualization 
psp_list <- data.tree::ToListExplicit(psp_tree, unname = TRUE)

psp_ontology_vis <- 
  networkD3::diagonalNetwork(
    psp_list, 
    fontSize = 14,
    textColour = "black",
    opacity = 100,
    width = 1900,
    height = 1400
  )

# Visualize
psp_ontology_vis

# Save visualizations
networkD3::saveNetwork(psp_ontology_vis, file = paste0(here::here(), '/vis/psp_ontology_vis.html'))

webshot::webshot("file:///C:/Users/tbrai/Dropbox/github_private/SeniorThesis/vis/psp_ontology_vis.html", paste0(here::here(), "/paper/psp_ontology_vis.png"))

### 2. Ontology for segmental autonomy

reg_aut_ont <- ontology_data[[1]]$Sheet2
reg_aut_ont <- dplyr::as_tibble(reg_aut_ont)

# Do the same for regional autonomy only 
reg_aut_ont$pathString <- 
  paste(
    reg_aut_ont$provision,
    reg_aut_ont$concept,
    reg_aut_ont$citation,
    sep = "|"
  )

reg_aut_tree <- data.tree::as.Node(reg_aut_ont, pathDelimiter = "|")
print(reg_aut_tree, limit = 15)


# Do the same for regional autonomy
reg_aut_list <- data.tree::ToListExplicit(reg_aut_tree, unname = TRUE)

reg_aut_ontology_vis <- 
  networkD3::diagonalNetwork(
    reg_aut_list, 
    fontSize = 50,
    textColour = "black",
    opacity = 100,
    width = 1500,
    height = 1200
  )

reg_aut_ontology_vis

networkD3::saveNetwork(reg_aut_ontology_vis, file = paste0(here::here(), '/vis/reg_aut_ontology_vis.html'))

webshot::webshot("file:///C:/Users/tbrai/Dropbox/github_private/SeniorThesis/vis/reg_aut_ontology_vis.html", paste0(here::here(), "/paper/reg_aut_ontology_vis.png"))


test <- reg_aut_ont %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), ~stringr::str_to_title(.)),
                dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "ø", "o"))) %>%
  dplyr::rename_with(.cols = dplyr::everything(), .fn = ~ stringr::str_to_title(.)) %>%
  dplyr::select(-Pathstring)

xtable::print.xtable(
  xtable::xtable(
    test, 
    caption = "Regional Autonomy Across Conceptualizations"
    ), 
  table.placement = "!htbp",
  scalebox = 0.8,
  include.rownames = FALSE,
  file = paste0(here::here(), "/paper/reg_aut_concept_map.tex"))
