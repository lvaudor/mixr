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
  if(stringlocation==""|is.na(stringlocation)){return(make_empty_result(info=info))}
  url_base <- "https://maps.googleapis.com/maps/api/geocode/json?"
  # API response
  url_query <- stringlocation %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" +","+") %>%
    URLencode(reserved = FALSE) %>%
    c(address = .) %>%
    c(.,key = Sys.getenv("GOOGLE_KEY")) %>%
    stringr::str_c(names(.),., sep = "=",collapse = "&") %>%
    stringr::str_c(url_base,.)
  repcontent=httr::GET(url_query) %>%
    httr::content()
  if(repcontent$status=="ZERO_RESULTS"){
    result=make_empty_result(info)
  }
  if(repcontent$status!="ZERO_RESULTS"){
    repcontent=repcontent$results[[1]]
    if(any(info %in% c("lat","lng"))){
      mylat=repcontent$geometry$location$lat
      mylng=repcontent$geometry$location$lng
    }
    address_components=repcontent$address_components %>%
      purrr::map_df(tibble::as_tibble) %>%
      dplyr::mutate(types=unlist(types))
    result=dplyr::bind_rows(
      address_components,
      tibble::tibble(long_name=repcontent$formatted_address,
                     short_name=NA,
                     types="formatted_address"),
      tibble::tibble(long_name=address_components %>%
                       dplyr::filter(types=="country") %>%
                       dplyr::pull(short_name),
                     short_name=NA_character_,
                     types="country_code")) %>%
      dplyr::select(name=long_name,
                    types=types) %>%
      dplyr::right_join(tibble::tibble(types=info), by="types") %>%
      dplyr::select(types,name) %>%
      tibble::deframe() %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(stringlocation=stringlocation) %>%
      dplyr::select(stringlocation,dplyr::everything())
    if(any(info=="lng")){
      result=result %>%
        dplyr::mutate(lng=mylng) %>%
        dplyr::select(stringlocation,lng,dplyr::everything())
    }
    if(any(info=="lat")){
      result=result %>%
        dplyr::mutate(lat=mylat) %>%
        dplyr::select(stringlocation,lat,dplyr::everything())
    }
    return(result)
  }
}
