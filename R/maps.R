#' Build leaflet map from among multiple available maps.
#'
#' Build a leaflet map from a shapefile in a context of multiple map sets and corresponding shapefiles.
#'
#' This function builds a leaflet map from a shapefile. There can be a list of shapefiles, and corresponding
#' lists of map set names and parameters for setting up each map such as centering and zooming defaults.
#'
#' Each \code{mapset} and \code{shp} may contain named subregions/polygons. Polygons have fill color, outline, and
#' change appearance on mouse hover and click (selection).
#'
#' A \code{default} map may be identified, and if so, it is assumed to be a simpler base map with only a single polygon.
#' Such a map set has one region, and therefore is fully colored as permanently "selected", with no need to click a polygon.
#'
#' @param shp a shapefile pertaining to a single \code{mapset}.
#' @param regions a named vector of region IDs pertaining to regions in \code{shp}.
#' @param ids literal names of regions in \code{shp} (may not match \code{regions} elements).
#' @param mapset character, name of a mapset in \code{mapsets}.
#' @param mapsets character, vector of all available map sets.
#' @param default name of a default map set, if used. Otherwise defaults to \code{default=""}, the empty string.
#' @param default.name label used in place of \code{default} string. Defaults to \code{default}.
#' @param all_loc_names a named list (names match \code{mapsets})
#' where each list element is a vector of regions specified to match the region IDs in \code{shp}.
#' @param xyz a named list (names match \code{mapsets})
#' where each list element is a vector of three values: longitude, latitude, and zoom level, for centering and zoom level setup.
#' Alternatively, a non-list vector of three values if all map sets will share a common centering and zoom level.
#' @param progress logical, add progress bar in app during map rendering. Helpful is rendering is slow.
#'
#' @return a leaflet map
#' @export
#'
#' @examples
#' #not run
build_mapset <- function(shp, regions, ids, mapset, mapsets, default="", default.name=default, all_loc_names, xyz, progress=F){
  if(progress){
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message="Generating map", value=0)
  }
  is_default <- input$mapset==default
  if(!is.list(xyz) && length(xyz)==3 && is.numeric(xyz)){
    xyz <- rep(as.list(xyz), length(mapsets))
    names(xyz) <- mapsets
  }
  if(!is.list(xyz) || length(xyz) != length(mapsets) || any(sapply(xyz, length) != 3))
    stop("Invalid xyz argument.")
  if(is_default){
    z.lab <- z.id <- z <- default.name
  } else {
    z <- as.character(shp[[ids]])
    idx <- match(z, all_loc_names[[mapset]])
    z.id <- as.character(regions[idx])
    z.lab <- names(regions[idx])
  }
  n <- 1 + length(z)
  x <- leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::setView(xyz[[mapset]][1], xyz[[mapset]][2], xyz[[mapset]][3])
  if(progress) prog$inc(1/n, detail="Basemap built")
  # Add background polygon region outlines after map is created
  if(!is_default){
    for(i in seq_along(z)){
      x <- x %>% leaflet::addPolygons(
        data=shp[shp[[ids]]==z[i],], stroke=TRUE, fillOpacity=0, weight=1,
        color="black", group="not_selected", layerId=z.id[i], label=z.lab[i],
        highlightOptions=leaflet::highlightOptions(opacity=1, weight=2, fillOpacity=0,
                                                   bringToFront=FALSE, sendToBack=FALSE))
      if(progress) prog$inc((i+1)/n, detail=paste("Adding polygon", i))
    }
  } else {
    x <- x %>% leaflet::addPolygons(
      data=shp, stroke=TRUE, opacity=1, fillOpacity=0.2, weight=2,
      group="not_selected", layerId=z.id[1], label=z.lab[1],
      highlightOptions=leaflet::highlightOptions(opacity=1, weight=2, fillOpacity=0.2,
                                                 bringToFront=FALSE, sendToBack=FALSE))
    if(progress) prog$inc((1+1)/n, detail=paste("Adding polygon", 1))
  }
  x
}
