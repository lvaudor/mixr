#' Geocodes one location through the Google API
#' @param stringlocation a location string
#' @param info the variables to inform: by default, latlng. Information can be e.g. "locality","country","country_code","formatted_address". See https://developers.google.com/maps/documentation/geocoding/intro#Types for the list of all possibilities supported by the Google Map geocoding API.
#' @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' register_google(key="[your key]")
#' geocode_each("la Guillotière, Lyon")
geocode_each=function(stringlocation,info=c("latlng")){
  make_empty_result=function(info){
    result=rep(NA,length(info))
    names(result)=info
    result=result %>%
      dplyr::bind_rows()
    if("latlng" %in% info){
      result=dplyr::bind_cols(tibble::tibble(lat=NA,lng=NA),
                              result) %>%
             dplyr::select(-latlng)
    }
    return(result)
  }
  url_base <- "https://maps.googleapis.com/maps/api/geocode/json?"
  if(is.na(stringlocation)){
    result=make_empty_result(info)
  }
  if(!is.na(stringlocation)){
      # API response
      url_query <- stringlocation %>%
        stringr::str_trim() %>%
        stringr::str_replace_all(" +","+") %>%
        URLencode(reserved = FALSE) %>% c(address = .)
      url_query <- c(url_query,
                     key = ggmap::google_key())
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
            .$results
          # latlng
          latlng=repcontent %>%
            .[[1]] %>%
            .$geometry %>%
            .$location %>%
            tibble::as_tibble()
          # address components
          address_components=repcontent %>%
            .[[1]] %>%
            .$address_components %>%
            purrr::map_df(as_tibble) %>%
            dplyr::mutate(types=unlist(types)) %>%
            dplyr::filter(types!="political")
          formatted_address=repcontent %>%
            .[[1]] %>%
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
          if("latlng" %in% info){
            result=dplyr::bind_cols(latlng,result) %>%
              dplyr::select(-latlng)
          }
      }
  }
  return(result)
}
