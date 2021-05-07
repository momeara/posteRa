

#'Search for  substance exactly
#'
#' For additional optional parameters, see posetra_REST()
#'
#' @param smiles a SMILES string
#' @param api_key an API_KEY
#'
#' @export
search_exact <- function(
  smiles,
  ...) {
  smiles %>%
    purrr::map_dfr(function(substance_smiles){
      if(is.na(substance_smiles)){
        return(data.frame())
      } else {
        tibble::tibble(
          smiles = substance_smiles,
          alerts = postera_REST(
            end_point = "exact",
            post_data = list(smiles = substance_smiles),
            ...))
      }
    })
}

#'Search for substances with the query substance as a substructure
#'
#' For additional optional parameters, see posetra_REST()
#'
#' @param smiles a SMILES string
#' @param api_key an API_KEY
#'
#' @export
search_superstructure <- function(
  smiles,
  ...) {
  smiles %>%
    purrr::map_dfr(function(substance_smiles){
      if(is.na(substance_smiles)){
        return(data.frame())
      } else {
        tibble::tibble(
          smiles = substance_smiles,
          alerts = postera_REST(
            end_point = "superstructure",
            post_data = list(smiles = substance_smiles),
            ...))
      }
    })
}

#'Search for substances by smarts patterns
#'
#' For additional optional parameters, see posetra_REST()
#'
#' @param smiles a SMILES string
#' @param api_key an API_KEY
#'
#' @export
search_smarts <- function(
  smiles,
  ...) {
  smiles %>%
    purrr::map_dfr(function(substance_smiles){
      if(is.na(substance_smiles)){
        return(data.frame())
      } else {
        tibble::tibble(
          smiles = substance_smiles,
          alerts = postera_REST(
            end_point = "smarts",
            post_data = list(smiles = substance_smiles),
            ...))
      }
    })
}

#'Search for substances by synthetic accessibility using the fast-score
#'
#' For additional optional parameters, see posetra_REST()
#'
#' @param smiles a SMILES string
#' @param api_key an API_KEY
#'
#' @export
search_synthetic_accessibility_fast <- function(
  smiles,
  ...) {
  smiles %>%
    purrr::map_dfr(function(substance_smiles){
      if(is.na(substance_smiles)){
        return(data.frame())
      } else {
        tibble::tibble(
          smiles = substance_smiles,
          alerts = postera_REST(
            end_point = "synthetic-accessibility/fast-score",
            post_data = list(smiles = substance_smiles),
            ...))
      }
    })
}


#'Search for substances by synthetic accessibility by retrosynthesis
#'
#' For additional optional parameters, see posetra_REST()
#'
#' @param smiles a SMILES string
#' @param api_key an API_KEY
#'
#' @export
search_synthetic_accessibility_retrosynthesis <- function(
  smiles,
  ...) {
  smiles %>%
    purrr::map_dfr(function(substance_smiles){
      if(is.na(substance_smiles)){
        return(data.frame())
      } else {
        tibble::tibble(
          smiles = substance_smiles,
          alerts = postera_REST(
            end_point = "synthetic-accessibility/retrosynthesis",
            post_data = list(smiles = substance_smiles),
            ...))
      }
    })
}
