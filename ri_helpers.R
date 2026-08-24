# ri_helpers.R ---------------------------------------------------------------
# A collection of functions to help with the QC pipeline
#
# Code generated with Claude Opus 4.8
# ----------------------------------------------------------------------------

library(sf)
library(dplyr)

.ri_cache <- new.env(parent = emptyenv())

ri_polygon <- function() {
  if (is.null(.ri_cache$poly)) {
    .ri_cache$poly <- tigris::states(cb = FALSE, year = 2023, progress_bar = FALSE) |>
      filter(STUSPS == "RI") |>
      st_transform(4326) |>
      st_geometry()
  }
  .ri_cache$poly
}

#' Is each coordinate inside Rhode Island?
#'
#' @param na_value what to return for missing/invalid coordinates. NA (default)
#'   keeps "never matched" distinct from "matched but wrong"; FALSE folds them
#'   together for use as a filter.
in_rhode_island <- function(lat, lon, method = c("polygon", "bbox"),
                            buffer_m = 0, na_value = NA) {
  method <- match.arg(method)
  stopifnot(length(lat) == length(lon))
  lat <- as.numeric(lat); lon <- as.numeric(lon)
  
  ok  <- !is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180
  out <- rep(na_value, length(lat))
  if (!any(ok)) return(out)
  
  if (method == "bbox") {
    out[ok] <- lat[ok] >= 41.09 & lat[ok] <= 42.02 &
      lon[ok] >= -71.91 & lon[ok] <= -71.08
    return(out)
  }
  
  poly <- ri_polygon()
  if (buffer_m > 0) {
    poly <- poly |> st_transform(3438) |> st_buffer(buffer_m / 0.3048006) |>
      st_transform(4326)
  }
  pts <- st_as_sf(data.frame(lon = lon[ok], lat = lat[ok]),
                  coords = c("lon", "lat"), crs = 4326)
  out[ok] <- lengths(st_intersects(pts, poly)) > 0
  out
}

#' Build Google Maps URLs from coordinates.
#'
#' @param lat,lon  numeric vectors of equal length
#' @param label    optional labels printed alongside each URL
#' @param type     "pin" for a dropped marker, "sat" for satellite imagery,
#'                 "pano" for Street View (most useful for eyeballing matches)
#' @param zoom     zoom level for "pin" and "sat"
#' @param open     if TRUE, open the URLs in a browser (capped at 10)
#' @return the URLs, invisibly
gmaps_url <- function(lat, lon, label = NULL, type = c("pin", "sat", "pano"),
                      zoom = 18, open = FALSE) {
  type <- match.arg(type)
  stopifnot(length(lat) == length(lon))
  
  bad <- is.na(lat) | is.na(lon) | abs(lat) > 90 | abs(lon) > 180
  if (any(bad)) warning(sum(bad), " coordinate(s) missing or out of range", call. = FALSE)
  
  coord <- sprintf("%.6f,%.6f", lat, lon)
  url <- switch(type,
                pin  = sprintf("https://www.google.com/maps/search/?api=1&query=%s&zoom=%d", coord, zoom),
                sat  = sprintf("https://www.google.com/maps/@%s,%dz/data=!3m1!1e3", coord, zoom),
                pano = sprintf("https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=%s", coord)
  )
  url[bad] <- NA_character_
  
  if (is.null(label)) label <- coord
  cat(paste0(format(label), "  ", url, collapse = "\n"), "\n")
  
  if (open) {
    ok <- which(!bad)
    if (length(ok) > 10) {
      message("Opening first 10 of ", length(ok), " URLs.")
      ok <- ok[1:10]
    }
    invisible(lapply(url[ok], utils::browseURL))
  }
  
  invisible(url)
}

#' Great-circle distance between coordinate pairs (haversine).
#'
#' Vectorised and recycling: pass scalars, equal-length vectors, or one scalar
#' against a vector. Missing or out-of-range inputs return NA.
#'
#' @param lat1,lon1,lat2,lon2 numeric, WGS84 decimal degrees
#' @param units "m" (default), "km", "ft", or "mi"
#' @return numeric vector of distances
geo_distance <- function(lat1, lon1, lat2, lon2, units = c("m", "km", "ft", "mi")) {
  units <- match.arg(units)
  
  n <- max(lengths(list(lat1, lon1, lat2, lon2)))
  lat1 <- rep_len(as.numeric(lat1), n); lon1 <- rep_len(as.numeric(lon1), n)
  lat2 <- rep_len(as.numeric(lat2), n); lon2 <- rep_len(as.numeric(lon2), n)
  
  ok <- !is.na(lat1) & !is.na(lon1) & !is.na(lat2) & !is.na(lon2) &
    abs(lat1) <= 90 & abs(lat2) <= 90 & abs(lon1) <= 180 & abs(lon2) <= 180
  
  out <- rep(NA_real_, n)
  if (!any(ok)) return(out)
  
  R  <- 6371008.8                      # IUGG mean Earth radius, metres
  p1 <- lat1[ok] * pi / 180
  p2 <- lat2[ok] * pi / 180
  dp <- p2 - p1
  dl <- (lon2[ok] - lon1[ok]) * pi / 180
  
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  out[ok] <- 2 * R * asin(pmin(1, sqrt(a)))   # pmin guards float overshoot
  
  out * switch(units, m = 1, km = 1 / 1000, ft = 1 / 0.3048, mi = 1 / 1609.344)
}
