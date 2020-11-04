#' Ads geographical information to a tibble through the use of the the Google API
#' @param df a tibble
#' @param location the variable to geocode
#' @param info the variables to inform: by default, latlng. Information can be e.g. "locality","country","country_code","formatted_address". See https://developers.google.com/maps/documentation/geocoding/intro#Types for the list of all possibilities supported by the Google Map geocoding API.
#' @param method the API to send the request to. Can be "opencage" (default) or "google".
#' @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' df <-tibble(location=c("Lyon, France",
#'                        "22 place du Général de Gaulle, Paris",
#'                        "la Guillotière, Lyon",
#'                        "Europe",
#'                        "Tucson, AZ",
#'                        "Rio Grande do Sul"))
#' tidy_geocode(df,location,method="opencage",...)
tidy_geocode=function(df,
                      location,
                      info=c("latlng"),
                      method="opencage"){
  qlocation=rlang::enquo(location)
  stringslocations=dplyr::select(df,!!qlocation) %>%
    dplyr::pull(1) %>%
    dplyr::na_if("")
  result=stringslocations %>%
    purrr::map_df(geocode_each,..., method=method)
  result=dplyr::bind_cols(df,result)
  return(result)
}

