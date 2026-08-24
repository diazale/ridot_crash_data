# ri_geocode.R ---------------------------------------------------------------
# Batch geocoding against Rhode Island's public E-911 ArcGIS locators.
#
#   E911_Sites_AddressLocator  - point-level address matches (no intersections)
#   E911_StreetRange_Locator   - address-range interpolation AND intersections
#
# No API key required. Results come back in WGS84 (EPSG:4326).
#
# Dependencies: httr2, jsonlite, dplyr, tibble
# 
# Code generated with Claude Opus 4.8
# ---------------------------------------------------------------------------

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)
library(sf)

RI_SITES <- "https://risegis.ri.gov/gpserver/rest/services/E911_Sites_AddressLocator/GeocodeServer"
RI_RANGE <- "https://risegis.ri.gov/gpserver/rest/services/E911_StreetRange_Locator/GeocodeServer"

source("ri_helpers.R")

# --- helpers ---------------------------------------------------------------

#' Does this string look like an intersection rather than a street address?
#'
#' The StreetRange locator accepts & @ | "and" "at" as intersection connectors.
#' We look for a connector that is NOT preceded by a leading house number.
is_intersection <- function(x) {
  x <- trimws(as.character(x))
  has_connector <- grepl("(\\s[&@|]\\s|\\s(and|at)\\s)", x, ignore.case = TRUE)
  starts_with_number <- grepl("^\\s*\\d", x)
  has_connector & !starts_with_number
}

#' Normalise an intersection string to the "A & B" form the locator prefers.
normalize_intersection <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s+(and|at)\\s+", " & ", x, ignore.case = TRUE)
  x <- gsub("\\s*[@|]\\s*", " & ", x)
  gsub("\\s+", " ", x)
}

#' Build the ArcGIS `addresses` payload for one chunk.
build_payload <- function(chunk) {
  records <- lapply(seq_len(nrow(chunk)), function(i) {
    attrs <- list(OBJECTID = as.integer(chunk$.oid[i]))
    for (fld in c("Street", "City", "State", "ZIP")) {
      v <- chunk[[fld]][i]
      if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) {
        attrs[[fld]] <- trimws(as.character(v))
      }
    }
    list(attributes = attrs)
  })
  toJSON(list(records = records), auto_unbox = TRUE)
}

#' Flatten the ArcGIS `locations` response into a tibble.
parse_locations <- function(payload) {
  locs <- payload$locations
  if (is.null(locs) || length(locs) == 0) {
    return(tibble(.oid = integer(), lon = double(), lat = double(),
                  score = double(), match_addr = character(),
                  addr_type = character(), status = character()))
  }
  
  bind_rows(lapply(locs, function(loc) {
    a <- loc$attributes
    pick <- function(nm, default = NA) {
      v <- a[[nm]]
      if (is.null(v) || length(v) == 0) default else v[[1]]
    }
    
    # Read the `location` geometry, which honors outSR — NOT attributes$X/Y,
    # which are the stored coordinate fields in the locator's native SR.
    x <- suppressWarnings(as.numeric(loc$location$x))
    y <- suppressWarnings(as.numeric(loc$location$y))
    if (length(x) == 0 || length(y) == 0) { x <- NA_real_; y <- NA_real_ }
    if (is.na(x) || is.na(y) || (x == 0 && y == 0)) { x <- NA_real_; y <- NA_real_ }
    
    # Guard: if anything still comes back projected, say so loudly rather
    # than letting State Plane values pass as degrees.
    if (!is.na(x) && abs(x) > 180) {
      stop("Got projected coordinates (", round(x), ", ", round(y),
           ") — outSR was not applied.", call. = FALSE)
    }
    
    tibble(
      .oid       = as.integer(pick("ResultID")),
      lon        = x,
      lat        = y,
      score      = suppressWarnings(as.numeric(pick("Score"))),
      match_addr = as.character(pick("Match_addr", NA_character_)),
      addr_type  = as.character(pick("Addr_type",  NA_character_)),
      status     = as.character(pick("Status",     NA_character_))
    )
  }))
}


# --- core: one locator, one chunk -----------------------------------------

geocode_chunk <- function(chunk, locator_url, timeout = 180, max_tries = 5) {
  resp <- request(paste0(locator_url, "/geocodeAddresses")) |>
    req_body_form(
      addresses = build_payload(chunk),
      outSR     = "4326",   # ask for WGS84 directly; native SR is RI State Plane ft
      f         = "json"
    ) |>
    req_timeout(timeout) |>
    req_retry(max_tries = max_tries, backoff = function(i) min(2^i, 60)) |>
    req_user_agent("ri-geocode-R/1.0") |>
    req_perform()
  
  payload <- resp_body_json(resp)
  
  # ArcGIS returns HTTP 200 with an error object on failure — check explicitly.
  if (!is.null(payload$error)) {
    stop("ArcGIS error ", payload$error$code, ": ",
         paste(unlist(payload$error$message), collapse = " "), call. = FALSE)
  }
  
  parse_locations(payload)
}


# --- core: one locator, full table ----------------------------------------

#' Geocode a data frame against a single RI locator.
#'
#' @param df         data frame with the input records
#' @param street     column name holding the street address or intersection
#' @param city,zip   optional column names
#' @param locator    RI_SITES or RI_RANGE
#' @param chunk_size rows per request. The service advertises a very large
#'   MaxBatchSize (400k for Sites, 100k for StreetRange) but has a 60-second
#'   load-balancer timeout, so keep requests small.
#' @param cache_dir  if set, each chunk's result is written to an .rds file and
#'   reused on re-run. Makes a 500k job resumable after a crash.
geocode_ri_one <- function(df,
                           street,
                           city       = NULL,
                           zip        = NULL,
                           locator    = RI_SITES,
                           chunk_size = 1000,
                           cache_dir  = NULL,
                           verbose    = TRUE) {
  
  stopifnot(is.data.frame(df), nrow(df) > 0)
  
  work <- tibble(
    .oid   = seq_len(nrow(df)),
    Street = as.character(df[[street]]),
    City   = if (!is.null(city)) as.character(df[[city]]) else NA_character_,
    State  = "RI",
    ZIP    = if (!is.null(zip))  as.character(df[[zip]])  else NA_character_
  )
  
  if (!is.null(cache_dir)) dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  chunks  <- split(work, ceiling(work$.oid / chunk_size))
  tag     <- if (identical(locator, RI_SITES)) "sites" else "range"
  results <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    cache_file <- if (!is.null(cache_dir)) {
      file.path(cache_dir, sprintf("%s_%05d.rds", tag, i))
    } else NULL
    
    if (!is.null(cache_file) && file.exists(cache_file)) {
      results[[i]] <- readRDS(cache_file)
      if (verbose) message(sprintf("[%s] chunk %d/%d (cached)", tag, i, length(chunks)))
      next
    }
    
    results[[i]] <- tryCatch(
      geocode_chunk(chunks[[i]], locator),
      error = function(e) {
        warning(sprintf("[%s] chunk %d failed: %s", tag, i, conditionMessage(e)),
                call. = FALSE)
        tibble(.oid = integer(), lon = double(), lat = double(), score = double(),
               match_addr = character(), addr_type = character(), status = character())
      }
    )
    
    if (!is.null(cache_file)) saveRDS(results[[i]], cache_file)
    if (verbose) message(sprintf("[%s] chunk %d/%d (%d rows)",
                                 tag, i, length(chunks), nrow(results[[i]])))
    Sys.sleep(0.1)  # be polite to a free public service
  }
  
  out <- bind_rows(results)
  tibble(.oid = work$.oid) |>
    left_join(out, by = ".oid") |>
    mutate(locator = tag)
}


# --- the cascade ----------------------------------------------------------

#' Geocode addresses and intersections against both RI locators.
#'
#' Strategy:
#'   1. Everything that looks like a street address -> Sites locator (rooftop).
#'   2. Everything that looks like an intersection  -> StreetRange locator.
#'   3. Anything from step 1 that failed or scored below `min_score` is retried
#'      on StreetRange, which will interpolate along the address range.
#'
#' Returns the original data frame with lon, lat, score, match_addr, addr_type,
#' locator, and an `input_type` column appended.
geocode_ri <- function(df,
                       street,
                       city       = NULL,
                       zip        = NULL,
                       min_score  = 85,
                       chunk_size = 1000,
                       cache_dir  = NULL,
                       verbose    = TRUE) {
  
  raw   <- as.character(df[[street]])
  isect <- is_intersection(raw)
  
  prepped <- df
  prepped[[street]] <- ifelse(isect, normalize_intersection(raw), trimws(raw))
  
  res <- tibble(
    .oid = seq_len(nrow(df)),
    input_type = ifelse(isect, "intersection", "address"),
    lon = NA_real_, lat = NA_real_, score = NA_real_,
    match_addr = NA_character_, addr_type = NA_character_,
    locator = NA_character_
  )
  
  fill_in <- function(res, idx, got) {
    m <- match(res$.oid[idx], got$.oid)
    for (col in c("lon", "lat", "score", "match_addr", "addr_type", "locator")) {
      res[[col]][idx] <- got[[col]][m]
    }
    res
  }
  
  # --- pass 1: addresses against the point-level Sites locator
  addr_idx <- which(!isect)
  if (length(addr_idx) > 0) {
    if (verbose) message(sprintf("Pass 1: %d addresses -> Sites locator", length(addr_idx)))
    got <- geocode_ri_one(prepped[addr_idx, , drop = FALSE], street, city, zip,
                          RI_SITES, chunk_size,
                          if (!is.null(cache_dir)) file.path(cache_dir, "pass1"),
                          verbose)
    got$.oid <- res$.oid[addr_idx]
    res <- fill_in(res, addr_idx, got)
  }
  
  # --- pass 2: intersections against the StreetRange locator
  int_idx <- which(isect)
  if (length(int_idx) > 0) {
    if (verbose) message(sprintf("Pass 2: %d intersections -> StreetRange locator",
                                 length(int_idx)))
    got <- geocode_ri_one(prepped[int_idx, , drop = FALSE], street, city, zip,
                          RI_RANGE, chunk_size,
                          if (!is.null(cache_dir)) file.path(cache_dir, "pass2"),
                          verbose)
    got$.oid <- res$.oid[int_idx]
    res <- fill_in(res, int_idx, got)
  }
  
  # --- pass 3: fall back to range interpolation for weak address matches
  weak <- which(!isect & (is.na(res$lon) | res$score < min_score))
  if (length(weak) > 0) {
    if (verbose) message(sprintf("Pass 3: %d weak matches -> StreetRange fallback",
                                 length(weak)))
    got <- geocode_ri_one(prepped[weak, , drop = FALSE], street, city, zip,
                          RI_RANGE, chunk_size,
                          if (!is.null(cache_dir)) file.path(cache_dir, "pass3"),
                          verbose)
    got$.oid <- res$.oid[weak]
    # Only overwrite where the fallback actually did better.
    better <- which(!is.na(got$lon) &
                      (is.na(res$score[weak]) | got$score > res$score[weak]))
    if (length(better) > 0) res <- fill_in(res, weak[better], got[better, ])
  }
  
  bind_cols(df, res |> select(-.oid))
}

# Convert coordinates
sp_to_wgs84 <- function(df, x = "X", y = "Y") {
  pts <- st_as_sf(df, coords = c(x, y), crs = 3438, remove = FALSE) |>
    st_transform(4326)
  coords <- st_coordinates(pts)
  df$lon <- coords[, 1]
  df$lat <- coords[, 2]
  df
}


# --- example --------------------------------------------------------------

if (interactive()) {
  #locations <- tibble(
  #  id   = 1:5,
  #  addr = c("150 South Main St",
  #           "Broad St & Thurbers Ave",
  #           "One Citizens Plaza",
  #           "Hope St at Rochambeau Ave",
  #           "593 Eddy St"),
  #  town = c("Providence", "Providence", "Providence", "Providence", "Providence")
  #)
  locations <- tibble(
    id = 1:nrow(cyc),
    addr = cyc$StreetOrHighway,
    town = cyc$CityOrTown
  )
  
  out <- geocode_ri(locations,
                    street    = "addr",
                    city      = "town",
                    cache_dir = "geocode_cache")
  
  print(out)
  
  # Match-rate diagnostics — always look at these before trusting the output.
  out |>
    group_by(input_type, locator, addr_type) |>
    summarise(n = n(), median_score = median(score, na.rm = TRUE), .groups = "drop") |>
    print()
  
  # Anything landing outside RI's bounding box is almost certainly a bad match.
  out |>
    filter(!is.na(lon)) |>
    filter(lon < -71.95 | lon > -71.10 | lat < 41.13 | lat > 42.03) |>
    print()
}
