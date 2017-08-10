#' Build leaflet map from among multiple available maps
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
#' Polygon border and fill colors and opacities are vectors of three values each.
#' These correspond to unselected, highlighted and selected polygons, respectively.
#' While \code{build_mapset} deals primarily with unselected or highlighted polygons and \code{update_mapset}
#' deals strictly with selected polygons, \code{build_mapset} still takes a third value because this is used
#' in the case of a \code{default} map set consisting of a single polygon (which is always colored and filled as a "selected" polygon).
#'
#' @param shp a shapefile pertaining to a single \code{mapset}.
#' @param all_regions a named vector of region IDs pertaining to regions in \code{shp}.
#' @param ids literal names of regions in \code{shp} (may not match \code{regions} elements).
#' @param mapset character, name of a mapset in \code{mapsets}.
#' @param mapsets character, vector of all available map sets.
#' @param default name of a default map set, if used. Otherwise defaults to \code{default=""}.
#' @param default.name label used in place of \code{default} string. Defaults to \code{default}.
#' @param all_locs_shapefile a named list (names match \code{mapsets})
#' where each list element is a vector of regions specified to match the region IDs in \code{shp}.
#' @param xyz a named list (names match \code{mapsets})
#' where each list element is a vector of three values: longitude, latitude, and zoom level, for centering and zoom level setup.
#' Alternatively, a non-list vector of three values if all map sets will share a common centering and zoom level.
#' @param color vector of three polygon border colors. See details.
#' @param fill vector of three polygon fill colors. See details.
#' @param color_opacity numeric vector of three polygon border color opacities. See details.
#' @param fill_opacity numeric vector of three polygon fill color opacities. See details.
#' @param progress logical, add progress bar in app during map rendering. Helpful is rendering is slow.
#'
#' @return a leaflet map
#' @export
#'
#' @examples
#' #not run
build_mapset <- function(shp, all_regions, ids, mapset, mapsets, default="",
                         default.name=default, all_locs_shapefile, xyz,
                         color=c("#000000", "#000000", "#3C8DBC"),
                         fill=c("#000000", "#3C8DBC", "#3C8DBC"),
                         color_opacity=c(0.5, 1, 1), fill_opacity=c(0, 0, 0.4), progress=F){
  if(progress){
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message="Generating map", value=0)
  }
  is_default <- mapset==default
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
    idx <- match(z, all_locs_shapefile[[mapset]])
    z.id <- as.character(all_regions[idx])
    z.lab <- names(all_regions[idx])
  }
  n <- 1 + length(z)
  x <- leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::setView(xyz[[mapset]][1], xyz[[mapset]][2], xyz[[mapset]][3])
  if(progress) prog$inc(1/n, detail="Basemap built")
  # Add background polygon region outlines after map is created
  if(!is_default){
    for(i in seq_along(z)){
      x <- x %>% leaflet::addPolygons(
        data=shp[shp[[ids]]==z[i], ], stroke=TRUE, weight=1,
        color=color[1], fillColor=fill[1], opacity=color_opacity[1], fillOpacity=fill_opacity[1],
        group="not_selected", layerId=z.id[i], label=z.lab[i],
        highlightOptions=leaflet::highlightOptions(
          weight=2, bringToFront=FALSE, sendToBack=FALSE,
          color=color[2], fillColor=fill[2], opacity=color_opacity[2], fillOpacity=fill_opacity[2]))
      if(progress) prog$inc((i+1)/n, detail=paste("Adding polygon", i))
    }
  } else {
    x <- x %>% leaflet::addPolygons(
      data=shp, stroke=TRUE, weight=2,
      color=color[3], fillColor=fill[3], opacity=color_opacity[3], fillOpacity=fill_opacity[3],
      group="not_selected", layerId=z.id[1], label=z.lab[1])
    if(progress) prog$inc((1+1)/n, detail=paste("Adding polygon", 1))
  }
  x
}

#' Update leaflet map in map set context
#'
#' Update an existing leaflet map in a context of multiple map sets and corresponding shapefiles.
#'
#' This function updates a leaflet map made using \code{build_mapset}. It is called from within Shiny observers. See examples.
#' The function is used twice in two contexts in an app in conjunction with a single original \code{build_mapset} call for a single
#' leaflet output instance.
#' Specifically, one call is within an observer that reacts to map polygon mouse clicks
#' and the other call is within an observer that reacts to changes to the \code{shiny::selectInput}
#' that is synced to the leaflet map polygon selections.
#'
#' The function takes a number of the same arguments passed to \code{build_mapset}.
#' Compared to \code{build_mapset}, color, fill and opacity is simplified to single values each
#' since this function only deals with the adding of selected polygons
#' to the map, not the adding of unselected polygons or polygon mouse hover options.
#'
#' Note that the \code{selectInput} observer context (\code{trigger="selectInput"}) requires all arguments to be supplied except for
#' \code{click_id} and {session}, which are needed only for \code{trigger="mapclick"}.
#' In the case of the latter, the only arguments needed (or used) are
#' \code{trigger}, \code{click_id}, \code{session} and \code{selected_regions}. All other arguments may be left out.
#' Map clicks are observed strictly to update the synchronized \code{selectInput}, not to remove or add polygons directly.
#' In turn, changes to the \code{selectInput}, whether by the user directly, or by updating on map click observation,
#' are responsible for updating map polygon layers.
#'
#' @param trigger character, \code{"selectInput"} or \code{"mapclick"}. Must used both in an app. See details.
#' @param shp a shapefile pertaining to a single \code{mapset}. See \code{build_mapset}.
#' @param mapset character, name of a mapset in \code{mapsets}. See \code{build_mapset}.
#' @param ids literal names of regions in \code{shp} (may not match \code{regions} elements).
#' @param selected_regions a subset of \code{all_regions}.
#' @param all_regions a named vector of region IDs pertaining to regions in \code{shp}.
#' @param all_locs a named list (names match \code{mapsets})
#' where each list element is a vector of regions.
#' @param all_locs_shapefile a named list (names match \code{mapsets})
#' where each list element is a vector of regions specified to match the region IDs in \code{shp}.
#' @param default name of a default map set, if used. Otherwise defaults to \code{default=""}.
#' @param click_id character, the leaflet map click id.
#' @param session the app session variable.
#' @param map character, name of leaflet map object.
#' @param color border color for selected polygons.
#' @param fill fill color for selected polygons.
#' @param color_opacity numeric, border color opacity.
#' @param fill_opacity numeric, fill color opacity.
#'
#' @return side effects only, called from within Shiny observers to update \code{selectInput} and leaflet output map.
#' @export
#'
#' @examples
#' \dontrun{
#' # Observe polygon selectInput.
#' # Add or remove selected polygons leaflet map polygons.
#' observeEvent(input$regions, {
#'   update_mapset("selectInput", shp, mapset, ids,
#'     input$regions, all_regions, all_locs)
#' }, ignoreNULL=FALSE)
#'
#' # Observe leaflet map shape click.
#' # Update region selectInput.
#' observeEvent(input$Map_shape_click, {
#'   update_mapset("mapclick", selected_regions=input$regions,
#'                 click_id=input$Map_shape_click$id, session=session)
#' })
#' }
update_mapset <- function(trigger, shp, mapset, ids, selected_regions, all_regions,
                          all_locs, all_locs_shapefile=all_locs, default="", click_id, session, map="Map",
                          color="#3C8DBC", fill=color, color_opacity=1, fill_opacity=0.4){
  if(!trigger %in% c("selectInput", "mapclick")) stop("trigger must be 'selectInput' or 'mapclick'")
  if(!is.null(selected_regions) && selected_regions[1]==default) return()
  if(trigger=="selectInput"){
    proxy <- leaflet::leafletProxy(map) # nolint
    z <- as.character(shp[[ids]])
    idx <- match(z, all_locs_shapefile[[mapset]])
    not_selected <- dplyr::setdiff(all_regions, selected_regions)
    if(length(not_selected))
      purrr::walk(not_selected, ~proxy %>% leaflet::removeShape(layerId=paste0("selected_", .x)))
    if(length(selected_regions)){
      shp_regions <- as.character(all_locs_shapefile[[mapset]][match(selected_regions, all_locs[[mapset]])]) # nolint
      z.lab <- names(selected_regions[idx]) # nolint
      purrr::walk(seq_along(selected_regions), ~proxy %>%
        leaflet::addPolygons(data=shp[shp[[ids]]==shp_regions[.x], ], stroke=TRUE, weight=1, label=z.lab[.x],
          color=color, fillColor=fill, opacity=color_opacity, fillOpacity=fill_opacity,
          group="selected", layerId=paste0("selected_", selected_regions[.x]),
          highlightOptions=leaflet::highlightOptions(weight=2, bringToFront=FALSE, sendToBack=FALSE,
            color=color, fillColor=fill, opacity=color_opacity, fillOpacity=fill_opacity)))
    }
  } else {
    if(missing(click_id) | missing(session))
      stop("Must provide click_id and session arguments when trigger='mapclick'.")
    if(!is.null(click_id)){
      p <- strsplit(click_id, "_")[[1]][2]
      if(is.na(p) && (is.null(selected_regions) || !click_id %in% selected_regions)){
        shiny::updateSelectInput(session, "regions", selected=c(selected_regions, click_id))
      } else if(!is.na(p) && p %in% selected_regions){
        shiny::updateSelectInput(session, "regions", selected=selected_regions[selected_regions!=p])
      }
    }
  }
}
