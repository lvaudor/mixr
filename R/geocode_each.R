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
      bind_rows()
    if("latlng" %in% info){
      result=bind_cols(tibble(lat=NA,lng=NA),
                      result) %>%
        select(-latlng)
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
        str_trim() %>%
        str_replace_all(" +","+") %>%
        URLencode(reserved = FALSE) %>% c(address = .)
      url_query <- c(url_query,
                     key = google_key())
      url_query <- url_query[!is.na(url_query)]
      url_query_inline <- str_c(names(url_query),
                                url_query, sep = "=",
                                collapse = "&")
      url <- str_c(url_base, url_query_inline)
      rep=httr::GET(url)
      if(is.null(rep$results)){
        result=make_empty_result(info)
      }
      if(!is.null(rep$results)){
          repcontent=rep %>%
            httr::content() %>%
            .$results
          # latlng
          latlng=repcontent %>%
            .[[1]] %>%
            .$geometry %>%
            .$location %>%
            as_tibble()
          # address components
          address_components=repcontent %>%
            .[[1]] %>%
            .$address_components %>%
            map_df(as_tibble) %>%
            mutate(types=unlist(types)) %>%
            filter(types!="political")
          formatted_address=repcontent %>%
            .[[1]] %>%
            .$formatted_address
          address_components=bind_rows(address_components,
                                       tibble(long_name=formatted_address,
                                              short_name=NA,
                                              types="formatted_address"),
                                       tibble(long_name=address_components %>% filter(types=="country") %>% pull(short_name),
                                              short_name=NA,
                                              types="country_code")) %>%
            select(name=long_name,
                   types=types)
          result=address_components %>%
            right_join(tibble(types=info), by="types") %>%
            select(types,name) %>%
            deframe() %>%
            bind_rows()
          if("latlng" %in% info){
            result=bind_cols(latlng,result) %>%
              select(-latlng)
          }
      }
  }
  return(result)
}
