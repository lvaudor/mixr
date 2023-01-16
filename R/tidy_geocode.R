#' Returns geographical information as a tibble through the use of a geocoding API
#' @param df a tibble
#' @param location the variable to geocode
#' @param info the variables to inform
#' Information can be many other variables depending on the method you are using.
#' Type `help(geocode_google)` or `help(geocode_opencage)` (according to the method you wish to use) to see details.
#' @param method the API to send the request to. Can be "opencage" (default) or "google".
#' @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' mydf <-tibble::tibble(location=c("Lyon, France",
#'                                  "22 place du Général de Gaulle, Paris",
#'                                  "la Guillotière, Lyon",
#'                                  NA,
#'                                  "Europe",
#'                                  "Tucson, AZ",
#'                                  "Rio Grande do Sul",
#'                                  ""))
#' tidy_geocode(mydf,location,method="opencage")
tidy_geocode=function(df,
                      location,
                      info=c("lat","lng"),
                      method="opencage"){
  qlocation=rlang::enquo(location)
  stringslocations=df %>%
    dplyr::select(!!qlocation) %>%
    dplyr::pull(1)
  result=stringslocations %>%
    purrr::map_df(geocode_each,
                  info=info,
                  method=method)
  return(result)
}

