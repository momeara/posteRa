
#'Look up Medicinal Chemistry Alerts
#'
#' See the blog post: Manifold’s new alerts for nuisance compounds in cellular assays
#' https://discuss.postera.ai/t/manifolds-new-alerts-for-nuisance-compounds-in-cellular-assays/2853
#'
#' For additional optional parameters, see posetra_REST()
#'
#' @param smiles a SMILES string
#' @param api_key an API_KEY
#'
#' @returns tibble::tibble with columns ['smiles', 'alerts']
#'   where smiles is the input smiles and alerts is nested data.frame
#'
#'
#' @export
med_chem_alerts <- function(
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
            end_point = "med-chem-alerts",
            post_data = list(smiles = substance_smiles),
            ...))
      }
    })
}

