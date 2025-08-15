#' @note
#' Version 0.4.2 (patched again):
#'  • FIX plot_ts not rendering when Gravity column is absent (make_ts_plot now optionalises it)
#'  • Restore Debug button (opens browser())
#'  • Keep high‑tide vs nearest Moon transit/antitransit lag plot

# install.packages(c(
#   "shiny","httr","jsonlite","dplyr","tidyr","lubridate","plotly",
#   "suncalc","ggplot2","DT"
# ))
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(suncalc)
library(ggplot2)
library(DT)
library(ggdark)  


theme_set(
  dark_theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.title = element_text(size = 20),
      axis.text  = element_text(size = 18),
      legend.title = element_text(size = 20),
      legend.text  = element_text(size = 18)
    )
)

LAT <- -34.193764
LON <-  18.435665
TZ  <- "Africa/Johannesburg"
# Default WorldTides key (overridden by env var if set)
DEFAULT_WORLDTIDES_KEY <- "d50d07dd-b330-4b8f-8c8f-021dbf7817ba"

# Where we cache the previous-cycle data (local file)
CACHE_PATH <- file.path(getwd(), "moon_tide_cache.rds")

# ----------------------------- Helpers ----------------------------------------

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Split a date range into <=7-day chunks (WorldTides limit)
split_into_chunks <- function(start_date, end_date, chunk_days = 7L) {
  starts <- seq(as.Date(start_date), as.Date(end_date), by = chunk_days)
  ends   <- pmin(as.Date(starts) + chunk_days - 1, as.Date(end_date))
  tibble(start = starts, end = ends)
}

fetch_worldtides_heights_chunk <- function(date_start, date_end, lat, lon, key, step_sec = 1800, tz = TZ) {
  q <- list(
    heights   = 1, date = format(as.Date(date_start), "%Y-%m-%d"),
    days      = as.integer(as.Date(date_end) - as.Date(date_start)) + 1L,
    lat = lat, lon = lon, step = step_sec, localtime = 1, key = key
  )
  resp <- httr::GET("https://www.worldtides.info/api/v3", query = q)
  httr::stop_for_status(resp)
  out <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  if (is.null(out$heights)) stop("No heights returned by WorldTides.", call. = FALSE)
  tibble(
    time_utc = as.POSIXct(out$heights$dt, origin = "1970-01-01", tz = "UTC"),
    time     = with_tz(time_utc, tz = tz),
    height_m = as.numeric(out$heights$height)
  ) %>% select(time, height_m)
}

fetch_worldtides_extremes_chunk <- function(date_start, date_end, lat, lon, key, tz = TZ) {
  q <- list(
    extremes  = 1, date = format(as.Date(date_start), "%Y-%m-%d"),
    days = as.integer(as.Date(date_end) - as.Date(date_start)) + 1L,
    lat = lat, lon = lon, localtime = 1, key = key
  )
  resp <- httr::GET("https://www.worldtides.info/api/v3", query = q)
  httr::stop_for_status(resp)
  out <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  if (is.null(out$extremes)) return(tibble())
  tibble(
    time_utc = as.POSIXct(out$extremes$dt, origin = "1970-01-01", tz = "UTC"),
    time     = with_tz(time_utc, tz = tz),
    type     = out$extremes$type,
    height_m = as.numeric(out$extremes$height)
  ) %>% transmute(date = as.Date(time, tz = tz), time, type = ifelse(type=="High","High","Low"), height_m)
}

fetch_worldtides_heights_range <- function(date_start, date_end, lat, lon, key, step_sec = 1800, tz = TZ) {
  chunks <- split_into_chunks(date_start, date_end, 7L)
  bind_rows(lapply(seq_len(nrow(chunks)), function(i) {
    fetch_worldtides_heights_chunk(chunks$start[i], chunks$end[i], lat, lon, key, step_sec, tz)
  })) %>% arrange(time)
}

fetch_worldtides_extremes_range <- function(date_start, date_end, lat, lon, key, tz = TZ) {
  chunks <- split_into_chunks(date_start, date_end, 7L)
  bind_rows(lapply(seq_len(nrow(chunks)), function(i) {
    fetch_worldtides_extremes_chunk(chunks$start[i], chunks$end[i], lat, lon, key, tz)
  })) %>% arrange(time)
}

# Estimate previous lunar cycle using daily-noon phase minima over last 60 days
estimate_previous_cycle <- function(lat, lon, tz = TZ, lookback_days = 60) {
  days <- seq.Date(Sys.Date() - lookback_days, Sys.Date(), by = "day")
  noon  <- as.POSIXct(paste0(days, " 12:00:00"), tz = tz)
  pha   <- suncalc::getMoonIllumination(noon)$phase  # 0=new, 0.5=full, ~1=new again
  cand_idx <- order(pha)[1:min(8, length(pha))]
  cand_days <- days[sort(cand_idx)]
  kept <- c()
  for (d in cand_days) {
    if (!length(kept) || min(abs(as.numeric(d - kept))) >= 20) kept <- c(kept, d)
  }
  kept <- sort(kept)
  if (length(kept) < 2) {
    end_d <- Sys.Date(); start_d <- end_d - 29
  } else {
    end_d   <- kept[length(kept)]
    start_d <- kept[length(kept) - 1]
  }
  list(start_date = start_d, end_date = end_d - 1)
}

# Derive extremes from a time series (fallback)
derive_extremes_from_ts <- function(ts_df) {
  y <- ts_df$height_m
  n <- length(y)
  if (n < 5) return(tibble())
  dy  <- diff(y); sgn <- sign(dy); turn <- diff(sgn)
  hi_idx <- which(turn < 0) + 1; lo_idx <- which(turn > 0) + 1
  idx <- c(hi_idx, lo_idx); types <- c(rep("High", length(hi_idx)), rep("Low", length(lo_idx)))
  ord <- order(ts_df$time[idx])
  tibble(
    date = as.Date(ts_df$time[idx][ord], tz = attr(ts_df$time, "tzone")),
    time = ts_df$time[idx][ord],
    type = types[ord],
    height_m = y[idx][ord]
  )
}

moon_altitude_deg <- function(times, lat, lon) {
  pos <- suncalc::getMoonPosition(date = times, lat = lat, lon = lon)
  as.numeric(pos$altitude) * 180/pi
}

sun_altitude_deg <- function(times, lat, lon) {
  pos <- suncalc::getSunlightPosition(date = times, lat = lat, lon = lon)
  as.numeric(pos$altitude) * 180/pi
}

# Phase markers (New, Full, Quarters) at local noon
compute_phase_marks <- function(tmin, tmax, tz = TZ) {
  days <- seq.Date(as.Date(tmin, tz = tz), as.Date(tmax, tz = tz), by = "day")
  noon <- as.POSIXct(paste0(days, " 12:00:00"), tz = tz)
  ph   <- suncalc::getMoonIllumination(noon)$phase
  tol <- 0.08
  lab <- rep(NA_character_, length(ph))
  lab[ ph < tol | (1 - ph) < tol ] <- "New Moon"
  lab[ abs(ph - 0.5) < tol ]       <- "Full Moon"
  lab[ abs(ph - 0.25) < tol ]      <- "First Quarter"
  lab[ abs(ph - 0.75) < tol ]      <- "Last Quarter"
  keep <- which(!is.na(lab))
  tibble(time = noon[keep], label = lab[keep])
}

# include Moon antitransit (min altitude) in events
compute_moon_events <- function(start_time, end_time, lat, lon, step_min = 1, tz = TZ) {
  t0 <- as.POSIXct(start_time, tz = tz) - hours(6)
  t1 <- as.POSIXct(end_time,   tz = tz) + hours(6)
  times <- seq(t0, t1, by = paste(step_min, "mins"))
  alt   <- moon_altitude_deg(times, lat, lon)
  i <- 2:(length(alt)-1)
  i_max <- i[alt[i] > alt[i-1] & alt[i] > alt[i+1]]
  i_min <- i[alt[i] < alt[i-1] & alt[i] < alt[i+1]]
  refine <- function(k) {
    if (k <= 1 || k >= length(alt)) return(times[k])
    y1 <- alt[k-1]; y2 <- alt[k]; y3 <- alt[k+1]
    denom <- (y1 - 2*y2 + y3); if (abs(denom) < 1e-9) return(times[k])
    delta <- 0.5*(y1 - y3)/denom
    times[k] + as.difftime(delta*step_min, units = "mins")
  }
  transits     <- vapply(i_max, refine, as.POSIXct(Sys.time(), tz = tz))
  antitransits <- vapply(i_min, refine, as.POSIXct(Sys.time(), tz = tz))
  sgn <- sign(alt); cross_idx <- which(sgn[-1] != sgn[-length(sgn)])
  interpolate_zero <- function(k) {
    t0 <- times[k]; t1 <- times[k+1]; a0 <- alt[k]; a1 <- alt[k+1]
    frac <- ifelse(a1 != a0, -a0/(a1 - a0), 0.5); t0 + frac*(t1 - t0)
  }
  zero_times <- vapply(cross_idx, interpolate_zero, as.POSIXct(Sys.time(), tz = tz))
  zero_types <- ifelse(alt[cross_idx] < 0 & alt[cross_idx+1] > 0, "Rise", "Set")
  tibble(
    event   = c(rep("Moon transit", length(transits)),
                rep("Moon antitransit", length(antitransits)),
                paste("Moon", zero_types)),
    time    = c(transits, antitransits, zero_times)
  ) %>% arrange(time) %>% dplyr::filter(time >= as.POSIXct(start_time, tz = tz), time <= as.POSIXct(end_time, tz = tz))
}

moon_phase_at <- function(times) {
  ill <- suncalc::getMoonIllumination(date = times)
  tibble(time = times, fraction = ill$fraction, phase = ill$phase)
}

nearest_time <- function(target, candidates) {
  if (length(candidates) == 0) return(NA)
  candidates[which.min(abs(difftime(candidates, target, units = "mins")))]
}

# ---------------- Dark theme + Plots ------------------------------------------

dark_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "#0b1020", color = NA),
      panel.background = element_rect(fill = "#0b1020", color = NA),
      panel.grid.major = element_line(color = "#26304a"),
      panel.grid.minor = element_line(color = "#1a2338"),
      axis.text        = element_text(color = "#d9e1ff"),
      axis.title       = element_text(color = "#d9e1ff"),
      plot.title       = element_text(color = "#ffffff", face = "bold"),
      legend.background= element_rect(fill = "#0b1020", color = NA),
      legend.key       = element_rect(fill = "#0b1020", color = NA),
      legend.text      = element_text(color = "#d9e1ff"),
      legend.title     = element_text(color = "#d9e1ff")
    )
}

make_ts_plot <- function(df, events_df, phase_marks, input_values) { 
  # build the list of series columns that actually exist
  cols <- c("time", "height_m", "moon_norm", "sun_norm")
  if ("lunisolar_force_norm" %in% names(df)) cols <- c(cols, "lunisolar_force_norm")
  
  df_long <- df %>%
    select(any_of(cols)) %>%
    pivot_longer(-time, names_to = "series", values_to = "z") %>%
    mutate(series = recode(series,
                           height_m = "Tide",
                           moon_norm = "Moon",
                           sun_norm = "Sun",
                           `lunisolar_force_norm` = "Gravity",
                           .default = "Other"))
  
  base <- ggplot(df_long %>% filter(series %in% input_values), aes(time, z, color = series)) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c("Tide" = "#2ca2f9", "Moon" = "#ffffff", "Sun" = "#ffd84d", "Gravity" = "red")) +
    scale_y_continuous(
      name = "Normalized / meters",
      sec.axis = sec_axis(~ 90 * ., name = "Altitude (°)", breaks = c(-90, -45, 0, 45, 90))
    ) +
    labs(x = NULL, title = "Tides", color = NULL) 
  
  # Moon events (transit & antitransit)
  if (!is.null(events_df) && nrow(events_df)) {
    base <- base +
      geom_vline(data = events_df %>% filter(event %in% c("Moon transit","Moon antitransit")),
                 aes(xintercept = time), linetype = "dotted", linewidth = 0.4, color = "#9aa6c1", inherit.aes = FALSE) +
      geom_vline(data = events_df %>% filter(grepl("Moon Rise|Moon Set", event)),
                 aes(xintercept = time), linetype = "dotdash", linewidth = 0.3, color = "#6e7ba0", inherit.aes = FALSE)
  }
  
  # Phase markers (New/Full/Quarters)
  if (!is.null(phase_marks) && nrow(phase_marks)) {
    pm <- phase_marks
    base <- base +
      geom_vline(data = pm, aes(xintercept = time), color = "#ffffff", linewidth = 0.4, linetype = "longdash", inherit.aes = FALSE) +
      geom_text(data = pm, aes(x = time, y = max(df_long$z, na.rm = TRUE), label = label),
                color = "#ffffff", size = 3, vjust = -0.5, angle = 90, show.legend = FALSE, inherit.aes = FALSE)
  }
  base + geom_hline(yintercept = 0, col = 'white', linewidth = 0.6) + coord_cartesian(ylim = c(-1.5,1.5))
}

# Plot of lag(minutes) between each HIGH tide and the nearest Moon
# transit (max alt) or antitransit (min alt)
make_high_vs_moonlag_plot <- function(lags_df) {
  dfp <- lags_df %>%
    filter(tide_type == "High", moon_event %in% c("Moon transit","Moon antitransit")) %>%
    mutate(label = ifelse(moon_event == "Moon transit", "Moon max alt", "Moon min alt"))
  
  ggplot(dfp, aes(x = tide_time, y = lag_min, color = label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#6e7ba0") +
    geom_point(size = 2) +
    scale_color_manual(values = c("Moon max alt" = "#ffffff", "Moon min alt" = "#ffd84d")) +
    labs(x = NULL, y = "Lag (minutes): High tide minus nearest moon max/min",
         title = "Lag of high tide vs nearest Moon transit/antitransit",
         color = NULL) +
    dark_theme()
}

make_scatter_plot <- function(df) {
  ggplot(df, aes(moon_z, tide_z)) +
    geom_point(alpha = 0.5, size = 1.8, color = "#2ca2f9") +
    geom_smooth(method = "loess", se = FALSE, color = "#ffd84d") +
    labs(x = "z(Moon altitude)", y = "z(Tide height)", title = "Scatter: Moon altitude vs Tide height (z)") +
    dark_theme()
}

make_spring_plot <- function(day_stats) {
  ggplot(day_stats, aes(date, tide_range_m)) +
    geom_line(linewidth = 0.7, color = "#2ca2f9") +
    geom_point(size = 1.8, color = "#2ca2f9") +
    geom_vline(data = day_stats %>% filter(is_new_or_full),
               aes(xintercept = as.numeric(date)), linetype = "dashed", color = "#ffffff") +
    labs(x = NULL, y = "Daily tide range (m)", title = "Daily tide range vs new/full Moon (dashed lines)") +
    dark_theme()
}

# Map altitude (degrees) to [-1,1] (kept from your version)
alt_to_norm <- function(alt_deg) {
  x <- as.numeric(alt_deg)
  x[!is.finite(x)] <- NA_real_
  x <- pmax(-90, pmin(90, x))
  x / 90
}


#' Download data from the past 5 days
#'
#' This function downloads data between today and 5 days ago.
#'
#' @param base_url Character. The base API or file URL to fetch data from.
#' @param date_param Character. Name of the query parameter for date filtering.
#' @param ... Additional arguments passed to the downloader (e.g., httr::GET).
#'
#' @return Data object from the download process.
#'
#' @note
#' Version 1.0.0 from
#' download_last5days.R
download_lastdays <- function(base_url, date_param = "date", days, ...) {
  
  start_date <- Sys.Date() - days
  end_date <- Sys.Date()
  
  # Construct URL with date range
  full_url <- paste0(
    base_url,
    "?", date_param, "_from=", start_date,
    "&", date_param, "_to=", end_date
  )
  
  message("Downloading data from: ", start_date, " to ", end_date)
  message("Request URL: ", full_url)
  
  # Example with read.csv (replace with your own download logic)
  tryCatch({
    data <- read.csv(full_url, ...)
    return(data)
  }, error = function(e) {
    warning("Download failed: ", e$message)
    return(NULL)
  })
}


join_caches <- function(cache1, cache2) {
  # Merge tides_ts
  tides_ts_combined <- dplyr::bind_rows(cache1$tides_ts, cache2$tides_ts) |>
    dplyr::distinct() |>            # remove duplicates
    dplyr::arrange(time)             # sort chronologically
  
  # Merge tides_ext
  tides_ext_combined <- dplyr::bind_rows(cache1$tides_ext, cache2$tides_ext) |>
    dplyr::distinct() |>
    dplyr::arrange(date, time)
  
  # Combine meta info (keep lat/lon/tz from first, update date range)
  new_meta <- cache1$meta
  new_meta$start_date <- min(cache1$meta$start_date, cache2$meta$start_date)
  new_meta$end_date   <- max(cache1$meta$end_date,   cache2$meta$end_date)
  new_meta$created    <- Sys.time()
  
  # Return combined cache
  list(
    meta      = new_meta,
    tides_ts  = tides_ts_combined,
    tides_ext = tides_ext_combined
  )
}

# Example usage:
# combined_cache <- join_caches(cashe_o, cashe_p)

build_analysis <- function(tides_ts, tides_ext, interval_min = 30) {
  moon_deg <- moon_altitude_deg(tides_ts$time, LAT, LON) 
  sun_deg  <- sun_altitude_deg(tides_ts$time, LAT, LON)
  
  df <- tides_ts %>%
    mutate(
      moon_alt_deg = moon_deg,
      sun_alt_deg  = sun_deg,
      tide_z       = zscore(height_m),
      moon_z       = zscore(moon_alt_deg),
      sun_z        = zscore(sun_alt_deg),
      diff_z       = tide_z - moon_z,
      moon_norm    = alt_to_norm(moon_alt_deg),
      sun_norm     = alt_to_norm(sun_alt_deg)
    )
  
  # Events with both transit and antitransit
  events <- compute_moon_events(min(df$time), max(df$time), LAT, LON, step_min = 1, tz = TZ)
  
  # Extremes (if not provided)
  if (is.null(tides_ext) || !nrow(tides_ext)) {
    tides_ext <- derive_extremes_from_ts(tides_ts)
  }
  
  # Lags: HIGH vs nearest of transit/antitransit; LOW vs nearest of rise/set (kept)
  lags_df <- tibble()
  if (nrow(tides_ext)) {
    highs <- tides_ext %>% filter(type == "High")
    lows  <- tides_ext %>% filter(type == "Low")
    
    tm_candidates <- events %>% filter(event %in% c("Moon transit","Moon antitransit"))
    rs_candidates <- events %>% filter(event %in% c("Moon Rise","Moon Set"))
    
    if (nrow(highs) && nrow(tm_candidates)) {
      match_t <- sapply(highs$time, nearest_time, candidates = tm_candidates$time)
      matched_type <- tm_candidates$event[match(match_t, tm_candidates$time)]
      lags_high <- tibble(
        date = as.Date(highs$time, tz = TZ),
        tide_type = "High",
        tide_time = highs$time,
        tide_height_m = highs$height_m,
        moon_event = matched_type,
        moon_time = as.POSIXct(match_t, origin = "1970-01-01", tz = TZ)
      ) %>% mutate(lag_min = as.numeric(difftime(tide_time, moon_time, units = "mins")))
      lags_df <- bind_rows(lags_df, lags_high)
    }
    
    if (nrow(lows) && nrow(rs_candidates)) {
      match_r <- sapply(lows$time, nearest_time, candidates = rs_candidates$time)
      matched_type <- rs_candidates$event[match(match_r, rs_candidates$time)]
      lags_low <- tibble(
        date = as.Date(lows$time, tz = TZ),
        tide_type = "Low",
        tide_time = lows$time,
        tide_height_m = lows$height_m,
        moon_event = matched_type,
        moon_time = as.POSIXct(match_r, origin = "1970-01-01", tz = TZ)
      ) %>% mutate(lag_min = as.numeric(difftime(tide_time, moon_time, units = "mins")))
      lags_df <- bind_rows(lags_df, lags_low)
    }
  }
  
  # Daily range & phase (unchanged)
  day_stats <- tides_ext %>%
    group_by(date) %>%
    summarise(
      max_high = if (any(type == "High")) max(height_m[type == "High"], na.rm = TRUE) else NA_real_,
      min_low  = if (any(type == "Low"))  min(height_m[type == "Low"],  na.rm = TRUE) else NA_real_,
      tide_range_m = max_high - min_low,
      .groups = "drop"
    ) %>% filter(!is.na(tide_range_m))
  
  spring_lag_days <- NA_real_; spring_peak_day <- NA; nearest_nf_day <- NA
  if (nrow(day_stats)) {
    noon_times <- as.POSIXct(paste0(day_stats$date, " 12:00:00"), tz = TZ)
    phase_df <- moon_phase_at(noon_times)
    day_stats <- day_stats %>%
      left_join(phase_df %>% transmute(date = as.Date(time), phase, fraction), by = "date") %>%
      mutate(is_new_or_full = (abs(phase - 0.0) < 0.08) | (abs(phase - 1.0) < 0.08) | (abs(phase - 0.5) < 0.08))
    if (nrow(day_stats) > 2) {
      i_peak <- which.max(day_stats$tide_range_m)
      spring_peak_day <- day_stats$date[i_peak]
      cand <- day_stats %>% filter(is_new_or_full) %>% pull(date)
      if (length(cand)) {
        nearest_nf_day <- cand[which.min(abs(as.numeric(difftime(cand, spring_peak_day, units = "days"))))]
        spring_lag_days <- as.numeric(difftime(spring_peak_day, nearest_nf_day, units = "days"))
      }
    }
  }
  
  # Phase marks
  phase_marks <- compute_phase_marks(min(df$time), max(df$time), tz = TZ)
  
  # Correlation summary
  step_min <- as.numeric(median(diff(df$time))) / 60
  best_lag_min <- NA_real_
  if (!is.na(step_min) && is.finite(step_min)) {
    ccf_res  <- stats::ccf(df$tide_z, df$moon_z, lag.max = floor(6*60/step_min),
                           plot = FALSE, na.action = na.omit)
    if (length(ccf_res$acf)) {
      best_i   <- which.max(abs(ccf_res$acf))
      best_lag_steps <- ccf_res$lag[best_i]
      best_lag_min   <- as.numeric(best_lag_steps) * step_min
    }
  }
  
  list(
    df = df,
    events = events,
    tides_ext = tides_ext,
    lags = lags_df %>% arrange(tide_time),
    day_stats = day_stats,
    phase_marks = phase_marks,
    spring_peak_day = spring_peak_day,
    nearest_nf_day  = nearest_nf_day,
    spring_lag_days = spring_lag_days,
    cor_now = suppressWarnings(cor(df$tide_z, df$moon_z, use = "complete.obs")),
    best_lag_min = best_lag_min
  )
}
