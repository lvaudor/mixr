#' Returns a tibble with specificities according to two crossed categories.
#' @param data a tibble
#' @param cat1 a factor corresponding to words or lemmas
#' @param cat2 a category
#' @param criterion one of "all" (default), "top_n" or "min_spec": should the information displayed be filtered by top specificities (top_n) or according to a minimum value of specificity (min_spec).
#' @param top_n in case criterion=='top_n', how many items by category should be kept (defaults to 50)
#' @param min_spec in case criterion=='min_spec', which is the minimum specificity for an item to be kept (defaults to 2)
#' @return tibble with additional columns cat1, cat2, spec
#' @export
#' @examples
#' df <-tibble(loc=c("Lyon, France",
#'                   "22 place du Général de Gaulle, Paris",
#'                   "la Guillotière, Lyon",
#'                   "Europe",
#'                   "Tucson, AZ"))
#' tidy_geocode(df,loc)
tidy_geocode=function(df,location){
  qlocation=enquo(location)
  geocode_dsk_all=function(loc){
    result=ggmap::geocode(loc, source="dsk", output="all")
    return(result)}
  stringslocations=select(df,!!qlocation) %>%
    pull(1)
  locations= stringslocations%>%
    map(safely(geocode_dsk_all)) %>%
    map("result") %>%
    map("results")
  print(locations)
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
  colnames(coords)[1]=colnames(select(df,!!qlocation))
  df=left_join(df,coords,by=colnames(coords)[1])
  return(df)
}

