## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/networks.R
## Jack Gregory
## 2 March 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides functions related to the sf_networks package and network cleaning.

## It has the following dependencies:
##  - dplyr
##  - purrr
##  - sf
##  - sfnetworks
##  - tidygraph


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  02Mar2024 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  17Jul2017 Jack Gregory  New draft; ...
## ...


### START CODE ###


# round_enpoints ----------------------------------------------------------------------------------
## Round the endpoints of a linestring object.

## x = linestring

round_endpoints <- function(x) {
  
  ## Assertions
  stopifnot(any(class(x)=="XY"))
  
  ## Create endpoint index
  d <- dim(x)
  idx <- c(1, d[1])
  
  ## Round endpoints
  x[idx, 1:2] <- round(x[idx, 1:2], 1)
  return(x)
}


# build_network -----------------------------------------------------------------------------------
## Clean network.

## net = sfnetwork object

build_network <- function(net) {
  
  ## Assertions
  stopifnot(
    sfnetworks::is.sfnetwork(net)
  )
  
  ## Build pre-network by smoothing over original coordinates
  cat("\nSmooth pre-network ...")
  pre_net <- net |>
    # tidygraph::convert(to_spatial_subdivision, .clean=TRUE) |>
    tidygraph::convert(to_spatial_smooth, .clean=TRUE)
  
  ## Build uni-directed network
  cat("\nBuild uni-directed network ...")
  uni_net <- sf::st_as_sf(pre_net, "edges") |>
    sf::st_geometry() |>
    purrr::map(\(x) round_endpoints(x)) |>
    sf::st_sfc(crs = st_crs(pre_net)) |>
    sf::st_as_sf()
  sf::st_geometry(uni_net) <- "geometry"
  uni_net <- uni_net |>
    sfnetworks::as_sfnetwork() |>
    # tidygraph::convert(to_spatial_subdivision, .clean=TRUE) |>
    tidygraph::convert(to_spatial_smooth, .clean=TRUE) |>
    sfnetworks::activate("nodes") |>
    dplyr::mutate(ID = dplyr::row_number())
  
  ## Build bi-directed network
  cat("\nBuild bi-directed network ...")
  edges <- sf::st_as_sf(uni_net, "edges") |>
    dplyr::select(-from, -to)
  edges_rev <- sf::st_reverse(edges)
  bi_net <- rbind(edges, edges_rev) |>
    sfnetworks::as_sfnetwork() |>
    sfnetworks::activate("nodes") |>
    dplyr::mutate(ID = dplyr::row_number())
  
  cat("\nComplete\n\n")
  return(bi_net)
}


### END CODE ###

