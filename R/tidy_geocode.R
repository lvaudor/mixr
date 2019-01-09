#' Returns a tibble that includes a variable describing locations, with added information corresponding to the Data Science Toolkit geocoding tool.
#' @param data a tibble
#' @param location the variable to geocode
#' @return tibble with additional columns lat, lng, country, locality, area
#' @export
#' @examples
#' df <-tibble(loc=c("Lyon, France",
#'                   "22 place du Général de Gaulle, Paris",
#'                   "la Guillotière, Lyon",
#'                   "Europe",
#'                   "Tucson, AZ",
#'                   "Rio Grande do Sul"))
#' tidy_geocode(df,loc)
tidy_geocode=function(data,location){
  qlocation=enquo(location)
  geocode_dsk_all=function(loc){
    result=ggmap::geocode(loc, source="dsk", output="all")
    return(result)}
  stringslocations=select(data,!!qlocation) %>%
    pull(1)
  locations= stringslocations%>%
    map(safely(geocode_dsk_all)) %>%
    map("result") %>%
    map("results")
  coords=locations %>%
    map(1) %>%
    map("geometry") %>%
    map("location") %>%
    map(unlist) %>%
    map(function(x){if(is.null(x)) x=c(lat=NA,lng=NA) else x=x}) %>%
    map(t) %>%
    map(as_tibble) %>%
    map(function(x){select(x,lat,lng)}) %>%
    map(function(x){if(is.null(x)) x=c(lat=NA,lng=NA) else x=x}) %>%
    bind_rows()
  comp=locations %>%
    map(1) %>%
    map("address_components") %>%
    map(function(x){map(x,safely(as.data.frame),stringsAsFactors=FALSE )}) %>%
    map(function(x) map(x,"result")) %>%
    map(function(x){do.call(rbind,x)})
  country=comp %>%
    map(safely(function(x){filter(x,types=="country")})) %>%
    map("result") %>%
    map("long_name") %>%
    map(function(x){if(is.null(x)) x=NA else x=x}) %>%
    unlist()
  locality=comp %>%
    map(safely(function(x){filter(x,types=="locality")}
    )) %>%
    map("result") %>%
    map("short_name") %>%
    map(function(x){if(is.null(x)) x=NA else x=x}) %>%
    unlist()
  area=comp %>%
    map(safely(function(x){filter(x,types=="administrative_area_level_1")}
    )) %>%
    map("result") %>%
    map("short_name") %>%
    map(function(x){if(is.null(x)) x=NA else x=x}) %>%
    unlist()
  coords=tibble(stringslocations,
                lat=coords$lat,
                lng=coords$lng,
                country,
                locality,
                area)
  colnames(coords)[1]=colnames(select(data,!!qlocation))
  data=left_join(data,coords,by=colnames(coords)[1])
  return(data)
}

