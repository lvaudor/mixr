#' Geocodes one location through the Google API
#' @param stringlocation a location string
#' @param info the variables to inform: by default, c("lat","lng").
#' Information can be e.g. "locality","country","country_code","formatted_address".
#' It can also be any type supported by the Google Map geocoding API:
#' see [Address types](https://developers.google.com/maps/documentation/geocoding/intro#Types).
#' @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' geocode_google("Grande Rue de la Guillotière, Lyon", info=c("lat","lng","country_code","locality","administrative_area_level_1"))
geocode_google=function(stringlocation,info=c("lat","lng")){
  url_base <- "https://maps.googleapis.com/maps/api/geocode/json?"
  # API response
  url_query <- stringlocation %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" +","+") %>%
    URLencode(reserved = FALSE) %>% c(address = .)
  url_query <- c(url_query,
                 key = google_key)
  url_query <- url_query[!is.na(url_query)]
  url_query_inline <- stringr::str_c(names(url_query),
                                     url_query, sep = "=",
                                     collapse = "&")
  url <- stringr::str_c(url_base, url_query_inline)
  repcontent=httr::GET(url) %>%
    httr::content()
  if(repcontent$status=="ZERO_RESULTS"){
    result=make_empty_result(info)
  }
  if(repcontent$status!="ZERO_RESULTS"){
    repcontent=repcontent%>%
      .$results %>%
      .[[1]]
    if(any(info %in% c("lat","lng"))){
      mylat=repcontent$geometry$location$lat
      mylng=repcontent$geometry$location$lng
    }
    # address components
    address_components=repcontent %>%
      .$address_components %>%
      purrr::map_df(as_tibble) %>%
      dplyr::mutate(types=unlist(types))
    formatted_address=repcontent %>%
      .$formatted_address
    address_components=bind_rows(address_components,
                                 tibble::tibble(long_name=formatted_address,
                                                short_name=NA,
                                                types="formatted_address"),
                                 tibble::tibble(long_name=address_components %>%
                                                  filter(types=="country") %>%
                                                  pull(short_name),
                                                short_name=NA_character_,
                                                types="country_code")) %>%
      dplyr::select(name=long_name,
                    types=types)
    result=address_components %>%
      dplyr::right_join(tibble(types=info), by="types") %>%
      dplyr::select(types,name) %>%
      tibble::deframe() %>%
      dplyr::bind_rows()
    if(any(info=="lat")){result=mutate(result,lat=mylat)}
    if(any(info=="lng")){result=mutate(result,lng=mylng)}
    return(result)
  }
}
