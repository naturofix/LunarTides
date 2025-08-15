#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  observeEvent(input$debug, { browser() })
  
  # --- cache management ---
  load_cache <- function() if (file.exists(CACHE_PATH)) readRDS(CACHE_PATH) else NULL
  save_cache <- function(obj) saveRDS(obj, CACHE_PATH)
  
  
  values = reactiveValues()
  
  # ----- build_analysis: creates lags for HIGH vs nearest (transit/antitransit) -----

  
  # Holds the active analysis (either cached or manual)
  analysis <- reactiveVal(NULL) 
  
  # On startup: if cache exists and use_cache=TRUE, load it
  observe({ 
    if (isTRUE(input$use_cache)) {
      cache <- load_cache() 
      values$cache = cache
      if (!is.null(cache)) {
        analysis(build_analysis(cache$tides_ts, cache$tides_ext))
        
        last_day <- max(cache$tides_ts$time, na.rm = TRUE)
        first_day <- last_day - lubridate::days(1)  # last 3 days
        updateDateRangeInput(session, "range",
                             start = first_day,
                             end   = last_day)
      }
    }
  })
  
  # Refresh to previous lunar cycle: download & save, then load
  observeEvent(input$refresh_cache_lunar, { 
    key <- if (nzchar(Sys.getenv("WORLDTIDES_KEY"))) Sys.getenv("WORLDTIDES_KEY") else DEFAULT_WORLDTIDES_KEY
    cyc <- estimate_previous_cycle(LAT, LON, TZ)
    start_date <- cyc$start_date; end_date <- cyc$end_date
    showNotification(paste0("Downloading previous lunar cycle: ", start_date, " to ", end_date, " …"), type = "message", duration = 5)
    tides_ts <- fetch_worldtides_heights_range(start_date, end_date, LAT, LON, key, step_sec = as.integer(30 * 60), tz = TZ)
    tides_ext <- fetch_worldtides_extremes_range(start_date, end_date, LAT, LON, key, tz = TZ)
    cache <- list(meta = list(lat = LAT, lon = LON, tz = TZ, start_date = start_date, end_date = end_date, created = Sys.time(), interval_min = 30), tides_ts = tides_ts, tides_ext = tides_ext)
    save_cache(cache)
    analysis(build_analysis(tides_ts, tides_ext, interval_min = 30))
    updateDateRangeInput(session, "range", start = start_date, end = end_date)
    showNotification("Cache updated.", type = "message", duration = 3)
  })
  
  observeEvent(input$refresh_cache, { 
    DAYS_BACK <- 5L  # change this if you want a different number of days
    
    key <- if (nzchar(Sys.getenv("WORLDTIDES_KEY"))) Sys.getenv("WORLDTIDES_KEY") else DEFAULT_WORLDTIDES_KEY
    
    # Compute date bounds in the app's timezone
    end_date   <- as.Date(Sys.time(), tz = TZ)
    start_date <- end_date - (DAYS_BACK - 1L)
    
    # Use full-day POSIXct bounds for API calls
    start_dt <- as.POSIXct(paste0(start_date, " 00:00:00"), tz = TZ)
    end_dt   <- as.POSIXct(paste0(end_date,   " 23:59:59"), tz = TZ)
    
    showNotification(
      sprintf("Downloading last %d days: %s to %s …", DAYS_BACK, start_date, end_date),
      type = "message", duration = 5
    )
    
    tides_ts  <- fetch_worldtides_heights_range(
      start_dt, end_dt, LAT, LON, key, step_sec = as.integer(30 * 60), tz = TZ
    )
    tides_ext <- fetch_worldtides_extremes_range(
      start_dt, end_dt, LAT, LON, key, tz = TZ
    )
    
    cashe_o = values$cache
    
    cache_n <- list(
      meta = list(
        lat = LAT, lon = LON, tz = TZ,
        start_date = start_dt, end_date = end_dt,
        created = Sys.time(), interval_min = 30
      ),
      tides_ts  = tides_ts,
      tides_ext = tides_ext
    )
    
    cache = join_caches(cashe_o,cache_n)

    save_cache(cache)
    
    analysis(build_analysis(tides_ts, tides_ext, interval_min = 30))
    
    # keep the UI range as Dates (not POSIXct)
    
    last_day <- max(cache$tides_ts$time, na.rm = TRUE)
    first_day <- last_day - lubridate::days(1)  # last 3 days
    
    updateDateRangeInput(session, "range", start = first_day, end = last_day)
    

    
    showNotification("Cache updated.", type = "message", duration = 3)
  })
  
  
  
  
  # Manual run (ignored when use_cache=TRUE)
  observeEvent(input$go, {
    if (isTRUE(input$use_cache)) {
      showNotification("Using cache. Untick 'Use cached…' to run manual inputs.", type = "warning", duration = 4)
      return()
    }
    shiny::validate(shiny::need(input$range[1] <= input$range[2], "Start date must be <= end date."))
    if (input$source == "worldtides") {
      shiny::validate(shiny::need(nzchar(input$wt_key), "Please provide your WorldTides API key."))
      tides_ts  <- fetch_worldtides_heights_range(input$range[1], input$range[2], LAT, LON, input$wt_key, step_sec = as.integer(input$interval * 60), tz = TZ)
      tides_ext <- fetch_worldtides_extremes_range(input$range[1], input$range[2], LAT, LON, input$wt_key, tz = TZ)
    } else {
      inFile <- input$csv
      shiny::validate(shiny::need(!is.null(inFile), "Upload a CSV with columns: time,height"))
      raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      shiny::validate(shiny::need(all(c("time", "height") %in% tolower(names(raw))), "CSV must have columns named time and height"))
      names(raw) <- tolower(names(raw))
      t <- suppressWarnings(lubridate::ymd_hms(raw$time, tz = TZ))
      if (any(is.na(t))) t <- suppressWarnings(as.POSIXct(raw$time, tz = TZ))
      shiny::validate(shiny::need(!any(is.na(t)), "Could not parse some timestamps; use ISO 8601."))
      tides_ts <- tibble(time = t, height_m = as.numeric(raw$height)) %>% arrange(time) %>% filter(!is.na(time), !is.na(height_m))
      times_seq <- seq(max(min(tides_ts$time), as.POSIXct(input$range[1], tz = TZ)), min(max(tides_ts$time), as.POSIXct(input$range[2] + 1, tz = TZ) - 1), by = paste0(input$interval, " mins"))
      tides_ts <- tibble(time = times_seq, height_m = approx(x = as.numeric(tides_ts$time), y = tides_ts$height_m, xout = as.numeric(times_seq), rule = 2)$y)
      tides_ext <- derive_extremes_from_ts(tides_ts)
    }
    analysis(build_analysis(tides_ts, tides_ext, interval_min = ifelse(input$source=="worldtides", input$interval, input$interval)))
  })
  
  # ---------------- Visible-window slider + cropping ---------------------------
  output$visible_slider <- renderUI({
    a <- analysis(); req(a)
    
    cache = values$cache
    last_day <- max(cache$tides_ts$time, na.rm = TRUE)
    first_day <- last_day - lubridate::days(1)  # last 3 days
    
    min_t =  last_day - lubridate::days(30)
    # first_time
    min_t <- min(a$df$time); max_t <- max(a$df$time)
    #default_end <- min(min_t + as.difftime(7, units = "days"), max_t)
    div(style = "width:100%;", sliderInput(
      inputId   = "vis_range",
      label     = "",
      min       = min_t,
      max       = max_t,
      value     = c(first_day, last_day),
      step      = 24*60*60,
      timeFormat = "%Y-%m-%d",
      dragRange = TRUE,
      width     = "100%"
    ))
  })
  
  crop_analysis <- function(a, t0, t1) {
    if (is.null(a)) return(NULL)
    keep_time <- a$df$time >= t0 & a$df$time <= t1
    df2 <- a$df[keep_time, , drop = FALSE]
    events2 <- a$events %>% dplyr::filter(time >= t0, time <= t1)
    tides_ext2 <- a$tides_ext %>% dplyr::filter(time >= t0, time <= t1)
    lags2 <- a$lags %>% dplyr::filter(tide_time >= t0, tide_time <= t1)
    day_stats2 <- a$day_stats %>% dplyr::filter(date >= as.Date(t0, tz = attr(a$df$time, "tzone")), date <= as.Date(t1, tz = attr(a$df$time, "tzone")))
    phase2 <- a$phase_marks %>% dplyr::filter(time >= t0, time <= t1)
    list(
      df = df2, events = events2, tides_ext = tides_ext2, lags = lags2, day_stats = day_stats2,
      phase_marks = phase2,
      spring_peak_day = a$spring_peak_day, nearest_nf_day  = a$nearest_nf_day, spring_lag_days = a$spring_lag_days,
      cor_now = suppressWarnings(cor(df2$tide_z, df2$moon_z, use = "complete.obs")), best_lag_min = a$best_lag_min
    )
  }
  
  lunar_tide_data = reactive({
    cache = values$cache
    
    vis_range = input$vis_range
    
    tides_ts = cache$tides_ts
    moon_deg <- moon_altitude_deg(tides_ts$time, LAT, LON) 
    sun_deg  <- sun_altitude_deg(tides_ts$time, LAT, LON)
    
    selected_tides_ts = tides_ts %>% 
      dplyr::filter(time >= vis_range[1]) %>% 
      dplyr::filter(time <= vis_range[2])
    
    df = selected_tides_ts %>% 
      mutate(moon_alt_deg = moon_altitude_deg(selected_tides_ts$time, LAT, LON) ) %>% 
      mutate(sun_alt_deg = sun_altitude_deg(selected_tides_ts$time, LAT, LON)) %>% 
      mutate(moon_norm    = alt_to_norm(moon_alt_deg),
              sun_norm  = alt_to_norm(sun_alt_deg))
    range(df$time)
    
    df
  })
  
  lunar_tide_plot = reactive({ 
    df = lunar_tide_data()
    cols <- c("time", "height_m", "moon_norm", "sun_norm")
    df_long <- df %>%
      select(any_of(cols)) %>%
      pivot_longer(-time, names_to = "series", values_to = "z") %>%
      mutate(series = recode(series,
                             height_m = "Tide",
                             moon_norm = "Moon",
                             sun_norm = "Sun",
                             `lunisolar_force_norm` = "Gravity",
                             .default = "Other"))
    max_time = max(df$time)
    range(df$time)
    last_point = df %>% 
      filter(time == max_time)
    
    
    
    (p = ggplot(df_long) + 
      geom_line(aes(x = time,y = z, col = series)) + 
      scale_color_manual(values = c("Tide" = "#2ca2f9", "Moon" = "#ffffff", "Sun" = "#ffd84d", "Gravity" = "red")) +
      scale_y_continuous(
        name = "Tide height (m)",
        sec.axis = sec_axis(~ 90 * ., name = "Altitude (°)", breaks = c(-90, -45, 0, 45, 90))
      ) +
        geom_hline(yintercept = c(-1, -0.5, 0, 0.5, 1), colour = "white", linetype = "dotted") +
        
      #geom_hline(yintercept = 0,col = 'white') +
      #geom_abline(intercept = 1,slope = -1, col = 'white',linetype = 'dashed', linewidth = 3) +
      
      labs(x = NULL, title = "Tides", color = NULL) 
  )
    
    # ggplot(df) + 
    #   geom_point(aes(x = time, y = height_m)) +
    #   geom_abline(intercept = 1,slope = -1, col = 'white',linetype = 'dashed', linewidth = 3)
      
      
    p
  })
  
  visible <- reactive({ 
    a <- analysis(); req(a)
    min_t <- min(a$df$time); max_t <- max(a$df$time)
    input_range <- input$vis_range
    if (is.null(input_range)) {
      t0 <- min_t; t1 <- min(min_t + as.difftime(7, units = "days"), max_t) 
    } else { 
      t0 <- as.POSIXct(input_range[1], tz = attr(a$df$time, "tzone")); t1 <- as.POSIXct(input_range[2], tz = attr(a$df$time, "tzone"))
    }
    crop_analysis(a, t0, t1)
  })
  
  # ----------------- Outputs (use visible()) ----------------------------------
  output$summary <- renderText({
    v <- visible(); req(v)
    paste0(
      "Pearson r (z(tide) vs z(moon)): ", sprintf("%0.2f", v$cor_now),
      ifelse(is.na(v$best_lag_min), "", paste0(" | Best lag (±6h): ", sprintf("%0.0f min", v$best_lag_min)))
    )
  })
  
  output$plotly_ts <- renderPlotly({ v <- visible(); req(v); ggplotly(make_ts_plot(v$df, v$events, v$phase_marks, c("Tide","Moon","Sun")), dynamicTicks = TRUE) %>% layout(plot_bgcolor = "#0b1020", paper_bgcolor = "#0b1020") })
  output$plot_ts <- renderPlot({
    # v <- visible()
    # df = v$df
    # events_df = v$events
    # phase_marks  = v$phase_marks
    # input_values = c("Tide","Moon","Sun")
    # make_ts_plot(v$df, v$events, v$phase_marks, c("Tide","Moon","Sun"))
    lunar_tide_plot()
    })
  
  
  
  output$plot_high_moonlag <- renderPlotly({
    v <- visible(); req(v)
    p <- make_high_vs_moonlag_plot(v$lags)
    ggplotly(p, dynamicTicks = TRUE) %>% layout(plot_bgcolor = "#0b1020", paper_bgcolor = "#0b1020")
  })
  
  output$plot_scatter <- renderPlotly({ v <- visible(); req(v); ggplotly(make_scatter_plot(v$df)) %>% layout(plot_bgcolor = "#0b1020", paper_bgcolor = "#0b1020") })
  
  output$data_table <- DT::renderDataTable({ 
    v <- visible(); req(v)
    v$df %>% dplyr::transmute(
      time_local = format(time, "%Y-%m-%d %H:%M:%S %Z"),
      tide_height_m = height_m,
      moon_altitude_deg = moon_alt_deg,
      sun_altitude_deg  = sun_alt_deg,
      tide_z, moon_z, sun_z, diff_z
    )
  }, options = list(pageLength = 25), rownames = FALSE)
  
  output$lags_table <- DT::renderDataTable({
    v <- visible(); req(v)
    if (!nrow(v$lags)) return(tibble(message = "No extremes/events in window."))
    v$lags %>% dplyr::transmute(
      date,
      tide_type,
      tide_time = format(tide_time, "%Y-%m-%d %H:%M:%S %Z"),
      tide_height_m = round(tide_height_m, 3),
      moon_event,
      moon_time = format(moon_time, "%Y-%m-%d %H:%M:%S %Z"),
      lag_minutes = round(lag_min)
    )
  }, options = list(pageLength = 25), rownames = FALSE)
  
  output$plot_spring <- renderPlotly({ v <- visible(); req(v, nrow(v$day_stats)); ggplotly(make_spring_plot(v$day_stats), dynamicTicks = TRUE) %>% layout(plot_bgcolor = "#0b1020", paper_bgcolor = "#0b1020") })
  
  output$spring_summary <- renderText({
    a <- analysis(); req(a)
    if (is.na(a$spring_lag_days)) {
      "Insufficient data to estimate spring lag (need > 2 days and extremes)."
    } else {
      paste0("Peak tide range day: ", a$spring_peak_day,
             " | Nearest new/full day: ", a$nearest_nf_day,
             " | Estimated \"age of the tide\": ", sprintf("%+0.1f days (positive = springs AFTER new/full).", a$spring_lag_days))
    }
  })
}
