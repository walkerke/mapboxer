#' Add a fill layer to the map
#' @param source A Mapbox source. Uses the source from the \link{mapboxer} object if no source is supplied.
#' @param source_layer The layer to use from a vector tile source (not needed if the source is not vector).
#' @param filter A filter expression that is applied to the \code{source}.
#' @param fill_antialias (paint) Whether or not the fill should be antialiased.
#' @param fill_color (paint) The color of the filled part of this layer.
#'   This color can be specified as rgba with an alpha component and
#'   the color's opacity will not affect the opacity of the 1px stroke, if it is used.
#' @param fill_opacity (paint) The opacity of the entire fill layer.
#'   In contrast to the \code{fill_color}, this value will also affect the 1px stroke around the fill,
#'   if the stroke is used.
#' @param fill_outline_color (paint) The outline color of the fill.
#'   Matches the value of \code{fill_color} if unspecified.
#' @param fill_pattern (paint) Name of image in sprite to use for drawing image fills.
#' @param fill_sort_key (layout) Sorts features in ascending order based on this value.
#'   Features with a higher sort key will appear above features with a lower sort key.
#' @param fill_translate (paint) The geometry's offset.
#'   Values are [x, y] where negatives indicate left and up, respectively.
#' @param fill_translate_anchor (paint) Controls the frame of reference for \code{fill_translate}.
#'   One of "map", "viewport".
#' @param visibility (layout) Whether the layer should be displayed. One of "visible", "none".
#' @inheritParams add_popups
#' @param id The unique id of the layer.
#' @param minzoom The minimum zoom at which the layer appears (optional)
#' @param maxzoom The maximum zoom at which the layer appears (optional)
#' @seealso \url{https://docs.mapbox.com/mapbox-gl-js/style-spec/layers/#fill}
#' @example examples/api-reference/fill-layer.R
#' @export
add_fill_layer <- function(
                           map,
                           source = NULL,
                           source_layer = NULL,
                           filter = NULL,
                           fill_antialias = TRUE,
                           fill_color = NULL,
                           fill_opacity = NULL,
                           fill_outline_color = NULL,
                           fill_pattern = NULL,
                           fill_sort_key = NULL,
                           fill_translate = NULL,
                           fill_translate_anchor = NULL,
                           visibility = c("visible", "none"),
                           popup = NULL,
                           id = "fill-layer",
                           minzoom = NULL,
                           maxzoom = NULL) {
  paint <- list(
    "fill-antialias" = fill_antialias,
    "fill-color" = fill_color,
    "fill-opacity" = fill_opacity,
    "fill-outline-color" = fill_outline_color,
    "fill-pattern" = fill_pattern,
    "fill-translate" = fill_translate,
    "fill-translate-anchor" = fill_translate_anchor
  )
  layout <- list(
    "fill-sort-key" = fill_sort_key,
    "visibility" = match.arg(visibility)
  )
  style <- create_layer_style(id, "fill", source,
                              source_layer, filter, paint, layout,
                              minzoom, maxzoom)
  map %>%
    add_layer(style, popup)
}



#' Add a fill-extrusion layer to the map
#'
#' @param map The mapboxer map
#' @param source A Mapbox source. Uses the source from the \link{mapboxer} object if no source is supplied.
#' @param source_layer The layer to use from a vector tile source (not needed if the source is not vector).
#' @param filter A filter expression that is applied to the \code{source}.
#' @param fill_extrusion_base The base height of the layer; if \code{NULL}, will default to 0.
#' @param fill_extrusion_color (paint) The color of the filled part of this layer.
#'   This color can be specified as rgba with an alpha component and
#'   the color's opacity will not affect the opacity of the 1px stroke, if it is used.
#' @param fill_extrusion_height (paint) The height of the fill-extrusion layer.
#' @param fill_extrusion_opacity (paint) The opacity of the fill-extrusion layer.
#' @param fill_extrusion_pattern (paint) Name of image in sprite to use for drawing image fills.
#' @param fill_extrusion_translate (paint) The geometry's offset.
#'   Values are [x, y] where negatives indicate left and up, respectively.
#' @param fill_extrusion_translate_anchor (paint) Controls the frame of reference for \code{fill_translate}.
#'   One of "map", "viewport".
#' @param fill_extrusion_vertical_gradient (paint) If \code{TRUE} (the default), applies a vertical gradient to the sides of the layer.
#' @param visibility (layout) Whether the layer should be displayed.
#' @inheritParams add_popups
#' @param id The unique id of the layer.
#' @param minzoom The minimum zoom at which the layer appears (optional)
#' @param maxzoom The maximum zoom at which the layer appears (optional)
#' @seealso \url{https://docs.mapbox.com/mapbox-gl-js/style-spec/layers/#fill}
#' @example examples/api-reference/fill-layer.R
#' @export
add_fill_extrusion_layer <- function(
    map,
    source = NULL,
    source_layer = NULL,
    filter = NULL,
    fill_extrusion_base = NULL,
    fill_extrusion_color = NULL,
    fill_extrusion_height = NULL,
    fill_extrusion_opacity = NULL,
    fill_extrusion_pattern = NULL,
    fill_extrusion_translate = NULL,
    fill_extrusion_translate_anchor = NULL,
    fill_extrusion_vertical_gradient = TRUE,
    visibility = TRUE,
    popup = NULL,
    id = "fill-extrusion-layer",
    minzoom = NULL,
    maxzoom = NULL) {
  paint <- list(
    "fill-extrusion-base" = fill_extrusion_base,
    "fill-extrusion-color" = fill_extrusion_color,
    "fill-extrusion-height" = fill_extrusion_height,
    "fill-extrusion-opacity" = fill_extrusion_opacity,
    "fill-extrusion-pattern" = fill_extrusion_pattern,
    "fill-extrusion-translate" = fill_extrusion_translate,
    "fill-extrusion-translate-anchor" = fill_extrusion_translate_anchor,
    "fill-extrusion-vertical-gradient" = fill_extrusion_vertical_gradient
  )
  layout <- list(
    "visibility" = ifelse(visibility, "visible", "none")
  )
  style <- create_layer_style(id, "fill-extrusion", source,
                              source_layer, filter, paint, layout,
                              minzoom, maxzoom)
  map %>%
    add_layer(style, popup)
}
