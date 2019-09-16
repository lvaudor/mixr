#' Ads geographical information to a tibble through the use of the the Google API
#' @param data a tibble
#' @param location the variable to geocode
#' @param info the variables to inform: by default, latlng. Information can be e.g. "locality","country","country_code","formatted_address". See https://developers.google.com/maps/documentation/geocoding/intro#Types for the list of all possibilities supported by the Google Map geocoding API.
#' @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' register_google(key="[your key]")
#' df <-tibble(loc=c("Lyon, France",
#'                   "22 place du Général de Gaulle, Paris",
#'                   "la Guillotière, Lyon",
#'                   "Europe",
#'                   "Tucson, AZ",
#'                   "Rio Grande do Sul"))
#' tidy_geocode(df,loc,info=c("latlng","country","locality"))
tidy_geocode=function(data,
                      location,
                      info=c("latlng")){
  qlocation=enquo(location)
  stringslocations=select(data,!!qlocation) %>%
    pull(1) %>%
    na_if("")
  result=stringslocations %>%
    map_df(geocode_each,info=info)
  result=bind_cols(data,result)
  return(result)
}

