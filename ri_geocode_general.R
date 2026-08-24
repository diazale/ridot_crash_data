# ri_geocode_general.R -------------------------------------------------------
#
# Self-contained geocoder for Rhode Island locations. No other project files
# required.
#
#   geocode_locations(df, city = "CityOrTown",
#                         street1 = "StreetOrHighway",
#                         street2 = "NearestIntersectionOfficerCoded")
#
# Returns `df` unchanged, plus `geocoded_lat` / `geocoded_lon` (WGS84) and a
# few diagnostic columns.
#
# Behaviour:
#   - Both street columns present and naming different streets -> geocodes
#     their intersection. House numbers are stripped first.
#   - street2 missing/NA, or naming the same street as street1 -> geocodes
#     street1 alone, as a point address if it carries a house number,
#     otherwise as a street segment.
#   - street1 already written as an intersection ("A & B", "A and B") is used
#     as-is and street2 is ignored.
#
# Backed by Rhode Island's public E-911 ArcGIS locators. No API key needed.
#
# Dependencies: httr2, jsonlite, dplyr, stringr, tibble
# 
# Code generated with Claude Opus 4.8
# ---------------------------------------------------------------------------

library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(tibble)

RI_SITES <- "https://risegis.ri.gov/gpserver/rest/services/E911_Sites_AddressLocator/GeocodeServer"
RI_RANGE <- "https://risegis.ri.gov/gpserver/rest/services/E911_StreetRange_Locator/GeocodeServer"


# --- text normalisation ----------------------------------------------------

clean_street <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\+", " ")     # undo URL-encoded input
  x <- str_squish(str_to_upper(x))
  x[x %in% c("", "NA", "N/A", "NULL", "UNKNOWN")] <- NA_character_
  x
}

# Requires whitespace after the digits, so "1ST ST" is not mistaken for one.
has_house_number <- function(x) !is.na(x) & str_detect(x, "^\\d+\\s+\\S")

strip_house_number <- function(x) {
  ifelse(has_house_number(x), str_squish(str_remove(x, "^\\d+\\s+")), x)
}

# "POST RD 02886" -> "POST RD"
strip_trailing_zip <- function(x) {
  ifelse(!is.na(x), str_squish(str_remove(x, "\\s+0[23]\\d{3}\\s*$")), x)
}

has_connector <- function(x) {
  !is.na(x) & str_detect(x, "(?i)(\\s[&@|]\\s|\\s(and|at)\\s)")
}

normalize_connectors <- function(x) {
  x <- str_replace_all(x, "(?i)\\s+(and|at)\\s+", " & ")
  x <- str_replace_all(x, "\\s*[@|]\\s*", " & ")
  str_squish(x)
}


# --- ArcGIS transport ------------------------------------------------------

build_payload <- function(street, city, oid) {
  records <- lapply(seq_along(street), function(i) {
    attrs <- list(OBJECTID = as.integer(oid[i]), Street = street[i], State = "RI")
    if (!is.na(city[i]) && nzchar(city[i])) attrs$City <- city[i]
    list(attributes = attrs)
  })
  toJSON(list(records = records), auto_unbox = TRUE)
}

parse_locations <- function(payload) {
  locs <- payload$locations
  empty <- tibble(.oid = integer(), lon = double(), lat = double(),
                  score = double(), match_addr = character(),
                  addr_type = character())
  if (is.null(locs) || length(locs) == 0) return(empty)
  
  bind_rows(lapply(locs, function(loc) {
    a <- loc$attributes
    pick <- function(nm, default = NA) {
      v <- a[[nm]]; if (is.null(v) || length(v) == 0) default else v[[1]]
    }
    # Read loc$location (honours outSR), NOT attributes$X/Y (native State Plane).
    x <- suppressWarnings(as.numeric(loc$location$x %||% NA))
    y <- suppressWarnings(as.numeric(loc$location$y %||% NA))
    if (length(x) == 0) x <- NA_real_
    if (length(y) == 0) y <- NA_real_
    if (is.na(x) || is.na(y) || (x == 0 && y == 0)) { x <- NA_real_; y <- NA_real_ }
    if (!is.na(x) && abs(x) > 180) {
      stop("Projected coordinates returned (", round(x), ", ", round(y),
           ") - outSR was not applied.", call. = FALSE)
    }
    tibble(
      .oid       = as.integer(pick("ResultID")),
      lon        = x,
      lat        = y,
      score      = suppressWarnings(as.numeric(pick("Score"))),
      match_addr = as.character(pick("Match_addr", NA_character_)),
      addr_type  = as.character(pick("Addr_type",  NA_character_))
    )
  }))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Geocode character vectors against one locator, with chunking and caching.
geocode_vec <- function(street, city, locator, chunk_size = 1000,
                        cache_dir = NULL, tag = "chunk", verbose = TRUE) {
  
  n   <- length(street)
  oid <- seq_len(n)
  out <- tibble(.oid = oid, lon = NA_real_, lat = NA_real_, score = NA_real_,
                match_addr = NA_character_, addr_type = NA_character_)
  if (n == 0) return(out)
  
  if (!is.null(cache_dir)) dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  groups <- split(oid, ceiling(oid / chunk_size))
  
  for (i in seq_along(groups)) {
    idx <- groups[[i]]
    cf  <- if (!is.null(cache_dir)) file.path(cache_dir, sprintf("%s_%05d.rds", tag, i))
    
    if (!is.null(cf) && file.exists(cf)) {
      got <- readRDS(cf)
    } else {
      got <- tryCatch({
        resp <- request(paste0(locator, "/geocodeAddresses")) |>
          req_body_form(
            addresses = build_payload(street[idx], city[idx], seq_along(idx)),
            outSR = "4326", f = "json"
          ) |>
          req_timeout(180) |>
          req_retry(max_tries = 5, backoff = function(k) min(2^k, 60)) |>
          req_user_agent("ri-geocode-R/2.0") |>
          req_perform()
        payload <- resp_body_json(resp)
        if (!is.null(payload$error)) {
          stop("ArcGIS error ", payload$error$code, ": ",
               paste(unlist(payload$error$message), collapse = " "), call. = FALSE)
        }
        parse_locations(payload)
      }, error = function(e) {
        warning(sprintf("[%s] chunk %d failed: %s", tag, i, conditionMessage(e)),
                call. = FALSE)
        tibble(.oid = integer(), lon = double(), lat = double(), score = double(),
               match_addr = character(), addr_type = character())
      })
      if (!is.null(cf)) saveRDS(got, cf)
    }
    
    if (nrow(got) > 0) {
      m <- match(seq_along(idx), got$.oid)
      for (col in c("lon", "lat", "score", "match_addr", "addr_type")) {
        out[[col]][idx] <- got[[col]][m]
      }
    }
    if (verbose) message(sprintf("  [%s] chunk %d/%d", tag, i, length(groups)))
    Sys.sleep(0.05)
  }
  out
}


# --- main entry point ------------------------------------------------------

#' Geocode a data frame of Rhode Island locations.
#'
#' @param df       a data frame
#' @param city     name of the city/town column
#' @param street1  name of the primary address-or-street column
#' @param street2  name of the secondary column, or NULL. May contain NAs.
#' @param min_score below this, a point-address match is retried as a street
#'   match and the better of the two kept
#' @param chunk_size rows per HTTP request
#' @param cache_dir directory for resumable chunk caching; NULL to disable
#' @param keep_diagnostics append score/match_addr/addr_type/method columns
#' @return `df` with `geocoded_lat` and `geocoded_lon` appended
geocode_locations <- function(df,
                              city,
                              street1,
                              street2          = NULL,
                              min_score        = 85,
                              chunk_size       = 1000,
                              cache_dir        = NULL,
                              keep_diagnostics = TRUE,
                              verbose          = TRUE) {
  
  stopifnot(is.data.frame(df))
  for (col in c(city, street1)) {
    if (!col %in% names(df)) stop("Column not found: ", col, call. = FALSE)
  }
  if (!is.null(street2) && !street2 %in% names(df)) {
    stop("Column not found: ", street2, call. = FALSE)
  }
  clash <- intersect(c("geocoded_lat", "geocoded_lon"), names(df))
  if (length(clash)) {
    warning("Overwriting existing column(s): ", paste(clash, collapse = ", "),
            call. = FALSE)
    df <- df[, setdiff(names(df), clash), drop = FALSE]
  }
  
  n   <- nrow(df)
  twn <- clean_street(df[[city]])
  s1  <- strip_trailing_zip(clean_street(df[[street1]]))
  s2  <- if (is.null(street2)) rep(NA_character_, n)
  else strip_trailing_zip(clean_street(df[[street2]]))
  
  s1_name <- strip_house_number(s1)
  s2_name <- strip_house_number(s2)
  
  # A cross street is usable only if present and naming a different street.
  usable2 <- !is.na(s2_name) & !is.na(s1_name) & s2_name != s1_name
  
  method <- case_when(
    is.na(s1)            ~ "missing",
    has_connector(s1)    ~ "preformed_intersection",
    usable2              ~ "intersection",
    has_house_number(s1) ~ "point_address",
    TRUE                 ~ "street_only"
  )
  
  query <- case_when(
    method == "missing"                ~ NA_character_,
    method == "preformed_intersection" ~ normalize_connectors(s1),
    method == "intersection"           ~ paste(s1_name, "&", s2_name),
    TRUE                               ~ s1
  )
  
  res <- tibble(lon = NA_real_, lat = NA_real_, score = NA_real_,
                match_addr = NA_character_, addr_type = NA_character_,
                locator = NA_character_, .rows = n)
  
  fill <- function(idx, got, tag) {
    for (col in c("lon", "lat", "score", "match_addr", "addr_type")) {
      res[[col]][idx] <<- got[[col]]
    }
    res$locator[idx] <<- tag
  }
  
  # Pass 1: point addresses -> Sites locator (address points)
  i_pt <- which(method == "point_address")
  if (length(i_pt)) {
    if (verbose) message(sprintf("Pass 1: %d point addresses -> Sites", length(i_pt)))
    fill(i_pt, geocode_vec(query[i_pt], twn[i_pt], RI_SITES, chunk_size,
                           cache_dir, "sites", verbose), "sites")
  }
  
  # Pass 2: intersections and bare streets -> StreetRange locator
  i_sr <- which(method %in% c("intersection", "preformed_intersection", "street_only"))
  if (length(i_sr)) {
    if (verbose) message(sprintf("Pass 2: %d intersections/streets -> StreetRange",
                                 length(i_sr)))
    fill(i_sr, geocode_vec(query[i_sr], twn[i_sr], RI_RANGE, chunk_size,
                           cache_dir, "range", verbose), "range")
  }
  
  # Pass 3: weak point-address matches retried as street matches
  weak <- i_pt[is.na(res$lon[i_pt]) | res$score[i_pt] < min_score]
  if (length(weak)) {
    if (verbose) message(sprintf("Pass 3: %d weak matches -> StreetRange fallback",
                                 length(weak)))
    before <- res[weak, ]
    got <- geocode_vec(query[weak], twn[weak], RI_RANGE, chunk_size,
                       cache_dir, "fallback", verbose)
    keep <- !is.na(got$lon) & (is.na(before$score) | got$score > before$score)
    if (any(keep)) fill(weak[keep], got[keep, ], "range_fallback")
  }
  
  df$geocoded_lat <- res$lat
  df$geocoded_lon <- res$lon
  
  if (keep_diagnostics) {
    df$geocoded_query     <- query
    df$geocoded_method    <- method
    df$geocoded_score     <- res$score
    df$geocoded_match     <- res$match_addr
    df$geocoded_addr_type <- res$addr_type
    df$geocoded_locator   <- res$locator
  }
  
  if (verbose) {
    message(sprintf("\nMatched %d of %d (%.1f%%)",
                    sum(!is.na(df$geocoded_lat)), n,
                    100 * mean(!is.na(df$geocoded_lat))))
  }
  df
}