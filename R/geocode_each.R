#' Geocodes one location through the Google API. See  [here](https://developers.google.com/maps/documentation/geocoding/intro#Types) for the list of all possibilities supported by the Google Map geocoding API.
#' @param stringlocation a location string
#' @param method the API to send the request to. Can be "opencage" (default) or "google".
#' @param info the variables to inform: by default, c("lat","lng").
#' Information can be many other variables depending on the method you are using. Type `help(geocode_google)` or `help(geocode_opencage)` to see details.
#' @return tibble with additional columns corresponding to the info required
#' @export
#' @examples
#' geocode_each("la Guillotière, Lyon")
geocode_each=function(stringlocation,method,info,...){
   if(is.na(stringlocation)){
    result=tibble(NA)
  }
  if(!is.na(stringlocation)){
    if(method=="google"){
      result=geocode_google(stringlocation,...)
    }
    if(method=="opencage"){
      result=geocode_opencage(stringlocation,...)
    }
  }
  return(result)
}
