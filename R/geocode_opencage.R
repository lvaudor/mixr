#' Geocodes one location through the OSM nominatim API
#' @param stringlocation a location string
#' @param info the variables to inform: by default, latlng.
#' Additional information can be e.g.
#'
#' - bounds.northeast.lat
#' - bounds.northeast.lng
#' - bounds.southwest.lat
#' - bounds.southwest.lng
#' - components.ISO_3166-1_alpha-2
#' - components.ISO_3166-1_alpha-3
#' - components._category
#' - components._type
#' - components.boundary
#' - components.continent
#' - components.country
#' - components.country_code
#' - components.state
#' - confidence
#' - formatted
#' - annotations.FIPS.county
#' - annotations.FIPS.state
#' - annotations.currency.disambiguate_symbol
#' - components.county
#' - components.state_code
#' - components.town
#' - query
#' - annotations.DMS.lat
#' - annotations.DMS.lng
#' - annotations.MGRS
#' - annotations.Maidenhead
#' - annotations.Mercator.x
#' - annotations.Mercator.y
#' - annotations.OSM.edit_url
#' - annotations.OSM.note_url
#' - annotations.OSM.url
#' - annotations.UN_M49.statistical_groupings
#' - annotations.callingcode
#' - annotations.currency.alternate_symbols
#' - annotations.currency.decimal_mark
#' - annotations.currency.html_entity
#' - annotations.currency.iso_code
#' - annotations.currency.iso_numeric
#' - annotations.currency.name
#' - annotations.currency.smallest_denomination
#' - annotations.currency.subunit
#' - annotations.currency.subunit_to_unit
#' - annotations.currency.symbol
#' - annotations.currency.symbol_first
#' - annotations.currency.thousands_separator
#' - annotations.flag
#' - annotations.geohash
#' - annotations.qibla
#' - annotations.roadinfo.drive_on
#' - annotations.roadinfo.speed_in
#' - annotations.sun.rise.apparent
#' - annotations.sun.rise.astronomical
#' - annotations.sun.rise.civil
#' - annotations.sun.rise.nautical
#' - annotations.sun.set.apparent
#' - annotations.sun.set.astronomical
#' - annotations.sun.set.civil
#' - annotations.sun.set.nautical
#' - annotations.timezone.name
#' - annotations.timezone.now_in_dst
#' - annotations.timezone.offset_sec
#' - annotations.timezone.offset_string
#' - annotations.timezone.short_name
#' - annotations.what3words.words
#' - annotations.wikidata
# @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' geocode_opencage("la Guillotière, Lyon",info=c("lat","lng","components.country_code"))
geocode_opencage=function(stringlocation,info=c("lat","lng")){
  if(stringlocation==""|is.na(stringlocation)){return(make_empty_result(info=info))}
  Sys.sleep(1)
  rep=opencage::opencage_forward(stringlocation,
                                 key=Sys.getenv("OPENCAGE_KEY"))
  result=rep$results %>%
    dplyr::select(lat=.data$geometry.lat,
                  lng=.data$geometry.lng,
                  dplyr::everything()) %>%
    dplyr::select(dplyr::any_of(info)) %>%
    dplyr::mutate(stringlocation=stringlocation) %>%
    dplyr::select(stringlocation,
                  dplyr::everything())
  info_not_provided=info[!(info %in% colnames(result))]
  if(length(info_not_provided)>0){
    complement=matrix(NA_character_,nrow=nrow(result),ncol=length(info_not_provided)) %>%
      tibble::as_tibble(.name_repair="minimal") %>%
      purrr::set_names(info_not_provided)
    result=dplyr::bind_cols(result,complement)
  }
  return(result)
}
