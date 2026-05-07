# =============================================================================
# ! RUN_AFTER_EVERY_TRAINING.R
# Pulls ALL SRW training data from the 2026 season via the Catapult OpenField
# API (from March 4, 2026 onwards).  Saves results as:
#   Data/All_Data26.rds / Data/All_Data26.csv     (= All_Data for dashboard)
#   Data/Weekly_Data26.rds / Data/Weekly_Data26.csv (= Weekly_Data for dashboard)
#
# Rules:
#   INCLUDE  — activity name contains "srw" (case-insensitive)
#   EXCLUDE  — activity name also contains "extra" (case-insensitive)
#
# Outputs are in EXACTLY the same column format as All_Data.rds so they can
# slot straight into the dashboard.
#
# Two API calls are made per pull:
#   1. groupby = c("athlete","activity")        → Session summary rows (Period 0)
#   2. groupby = c("athlete","period","activity")→ Individual drill rows (Period 1+)
# This mirrors the CTR report format which includes both a Session row AND all
# drill period rows for each athlete.
# =============================================================================

library(catapultR)
library(dplyr)
library(lubridate)
library(hms)
library(stringr)

setwd("/Users/renee/! R/Waratahs_SrW/")

# ---- Season settings --------------------------------------------------------
SEASON_START <- as.Date("2026-03-02")   # Monday of Week 1

# Date ranges — update these when the season transitions
PRESEASON_START <- as.Date("2026-03-04")   # first training day
PRESEASON_END   <- as.Date("2026-05-03")   # last preseason day (update when known)
INSEASON_START  <- as.Date("2026-05-04")   # first inseason day (update when known)
INSEASON_END    <- as.Date("2026-08-02")   # end of season


# ---- API token --------------------------------------------------------------
catapult_token <- ofCloudCreateToken(
  sToken  = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEzMWY5NGIxOTg3ZGY4NzcxNTljOGQ2MTAzMTIzNDNjIn0.eyJhdWQiOiJhZDMxNzI5Yy05OWYwLTQyM2MtYThhOC0wYTExYjU3MWU2Y2EiLCJqdGkiOiJhZjFlNWEwMjVlZjc3MzhhYTEwOTNlMTQ0MGE0NWU0MzkzYWRjN2M3ZDkyM2EwOGE3MzJlMjgyOWY0YmJkODY0NjJkMjc0YjE4NGIxYzhmNCIsImlhdCI6MTc2MjU3NzEyMi40MjAwOTksIm5iZiI6MTc2MjU3NzEyMi40MjAxLCJleHAiOjQ5MTYxNzcxMjIuNDExMTg2LCJzdWIiOiI3YmY2OTJkMC05MDJjLTQ5ZDgtYjBmYy05NmZiMDgzZGI5YmEiLCJzY29wZXMiOlsiY29ubmVjdCIsInNlbnNvci1yZWFkLW9ubHkiXSwiaXNzIjoiaHR0cHM6Ly9iYWNrZW5kLWF1Lm9wZW5maWVsZC5jYXRhcHVsdHNwb3J0cy5jb20iLCJjb20uY2F0YXB1bHRzcG9ydHMiOnsib3BlbmZpZWxkIjp7ImN1c3RvbWVycyI6W3sicmVsYXRpb24iOiJhdXRoIiwiaWQiOjIxNDV9XX19fQ.R-ADcXp_aYVjOm6nMUF-wo1_daDcHVNGNU8uP4jEmByOVXSt507gMf7XL_KCXtj4PtAOc-KeSdjlRsx3SOV2SJRIno53KZBZdVK6B2u8c7tQ13E2UVNXlPDOpJ1XISQTaunslAPsTGsQPlz5Y8FBfnRngJirjaplH3wIPxhzKhVl21vhmBK8-ihe1QeFl9vvMsjls-8zPCnOzFQZgSDMYkpnyKb3lqUWuhYAD3m0IVMfniQ3Okx-ocM-wqitZCLfJoSgB2LzlXwX4KsZr99LMMNHvrUn2WwCmLu2fM4OdZY7nnX-hAp0ejV2i3ePM71EtZfTRN5HMTL6Bm8bOIHnM6rGtYGYqvhH1fVtc8UoPLyDHxW8zPkT-jlJtDGjPRumeF2ozxGz6qn1CUpoDYOEehKFLXgbd5Kiqizg0vCxbcqocCn__eRuv6x7uOLht8-OHqzbIGXTIXiBf0SCFsE3WpDva5ipOXOoKrstFiiQER0OSWg5Qlk-362CRurRVoqcE-uw_LtUB_8htG0IXg2D5wH7t32KE7PGni-dkBAm0y7EdjUqcW7Aha5LPkP1rSLQnMWbs2xrU4PNUhwlzVYZ3R-D8kBpdlDAbFXAOzt0f3ur55JmmNst_FvmAr3P2s2vs35Fshf-4SJxEL0dQFgMkPy9p6vgL3apym4au_KnT8k",
  sRegion = "APAC"
)

# ---- Pull activities (from March 4 to capture the full season) --------------
cat("Fetching activities from 4 Mar 2026 onwards...\n")

activities <- ofCloudGetActivities(
  catapult_token,
  from = as.numeric(as.POSIXct(as.Date("2026-03-04"), tz = "UTC"))
) %>%
  filter(grepl("srw",   name, ignore.case = TRUE)) %>%   # SRW only
  filter(!grepl("extra", name, ignore.case = TRUE))       # exclude extras

cat("Found", nrow(activities), "qualifying activities:\n")
print(head(activities[, intersect(c("id","name","start_time"), names(activities))], 50))

if (nrow(activities) == 0) stop("No activities found — check token, region, and date range.")

# ---- API parameters ---------------------------------------------------------
selected_params <- c(
  "date",
  "athlete_name",
  "activity_name",
  "period_name",
  # Duration & distance
  "total_duration",
  "total_distance",
  "meterage_per_minute",
  # Player load
  "player_load",
  "player_load_per_minute",
  # Velocity
  "max_vel",
  "percentage_max_velocity",
  "velocity_band1_total_distance",
  "velocity_band2_total_distance",
  "velocity_band3_total_distance",
  "velocity_band4_total_distance",
  "velocity_band5_total_distance",
  "velocity_band6_total_distance",
  "velocity_band7_total_distance",
  "velocity_band8_total_distance",
  # Gen2 acceleration effort counts (bands 1-3 = decel, 6-8 = accel)
  "gen2_acceleration_band1_total_effort_count",
  "gen2_acceleration_band2_total_effort_count",
  "gen2_acceleration_band3_total_effort_count",
  "gen2_acceleration_band6_total_effort_count",
  "gen2_acceleration_band7_total_effort_count",
  "gen2_acceleration_band8_total_effort_count",
  # Peak acceleration
  "max_effort_acceleration",
  # Metabolic power bands (4-7 = HMLD)
  "metabolic_power_band4_total_distance",
  "metabolic_power_band5_total_distance",
  "metabolic_power_band6_total_distance",
  "metabolic_power_band7_total_distance",
  # Extras
  "contactinvolvement_total_count",
  "acceleration_density",
  "total_acceleration_load"
)

api_filters <- list(name = "activity_id", comparison = "=", values = activities$id)

# ---- Pull 1: Session-level (one row per athlete per activity) ---------------
# This becomes Period Number = 0, Period Name = "Session"
cat("\nPull 1/2 — session totals (groupby athlete + activity)...\n")

stats_session <- ofCloudGetStatistics(
  catapult_token,
  params  = selected_params,
  groupby = c("athlete", "activity"),
  filters = api_filters
)
cat("Session rows returned:", nrow(stats_session), "\n")

# ---- Pull 2: Drill-level (one row per athlete per period per activity) ------
cat("Pull 2/2 — drill periods (groupby athlete + period + activity)...\n")

stats_drills <- ofCloudGetStatistics(
  catapult_token,
  params  = selected_params,
  groupby = c("athlete", "period", "activity"),
  filters = api_filters
)
cat("Drill rows returned:", nrow(stats_drills), "\n")

# ---- Helper functions -------------------------------------------------------

date_to_week <- function(d) {
  d  <- as.Date(d)
  wk <- floor(as.numeric(difftime(d, SEASON_START, units = "days")) / 7) + 1
  pmax(1L, as.integer(wk))
}

derive_type <- function(name) {
  # Types used by Catapult API sessions:
  #   training, intensive, extensive, rehabrun, clubgame, sevens
  # Types used in KEEP external data (set manually in xlsx):
  #   othergame = international / representative game (Wallaroos, tours, etc.)
  #   other     = anything else
  nm <- tolower(trimws(name))
  dplyr::case_when(
    grepl("clubgame|game|vs\\b",       nm) ~ "clubgame",
    grepl("rehabrun|rehab.run|rehab",  nm) ~ "rehabrun",
    grepl("intensive",                 nm) ~ "intensive",
    grepl("extensive",                 nm) ~ "extensive",
    grepl("moderate",                  nm) ~ "moderate",
    grepl("light",                     nm) ~ "light",
    grepl("sevens|7s",                 nm) ~ "sevens",
    TRUE                                   ~ "training"
  )
}

# Safe zero-for-NA helper (only for numeric columns that should be 0 not NA)
z <- function(x) { x[is.na(x)] <- 0; x }

# Core transform: raw stats_df -> All_Data column layout
transform_to_alldata <- function(df, period_number_col, period_name_override = NULL) {
  df %>%
    mutate(
      period_name_clean = if (!is.null(period_name_override)) {
        period_name_override
      } else {
        period_name %>%
          gsub("\\s*-\\s*", " - ", .) %>%
          stringr::str_to_title()
      },
      `Period Number` = as.integer(period_number_col),
      date_parsed     = dmy(date),
      # Derived metrics
      vhsd = z(velocity_band6_total_distance) +
        z(velocity_band7_total_distance) +
        z(velocity_band8_total_distance),
      hsd  = z(velocity_band4_total_distance) +
        z(velocity_band5_total_distance) +
        z(velocity_band6_total_distance) +
        z(velocity_band7_total_distance) +
        z(velocity_band8_total_distance),
      hmld = z(metabolic_power_band4_total_distance) +
        z(metabolic_power_band5_total_distance) +
        z(metabolic_power_band6_total_distance) +
        z(metabolic_power_band7_total_distance),
      acc_b13       = z(gen2_acceleration_band6_total_effort_count) +
        z(gen2_acceleration_band7_total_effort_count) +
        z(gen2_acceleration_band8_total_effort_count),
      dec_b13       = z(gen2_acceleration_band1_total_effort_count) +
        z(gen2_acceleration_band2_total_effort_count) +
        z(gen2_acceleration_band3_total_effort_count),
      acc_dec_total = acc_b13 + dec_b13,
      dur_min       = total_duration / 60,
      hsd_per_min   = ifelse(dur_min > 0, hsd / dur_min, NA_real_),
      acc_dec_per_min = ifelse(dur_min > 0, acc_dec_total / dur_min, NA_real_),
      dur_hms       = hms::as_hms(as.integer(round(total_duration))),
      # Metadata
      Date      = as.numeric(format(date_parsed, "%Y%m%d")),
      Day       = tolower(weekdays(date_parsed)),
      Week      = date_to_week(date_parsed),
      Type      = derive_type(activity_name),
      Preseason = dplyr::case_when(
        date_parsed >= PRESEASON_START & date_parsed <= PRESEASON_END ~ "yes",
        date_parsed >= INSEASON_START  & date_parsed <= INSEASON_END  ~ "no",
        TRUE ~ NA_character_
      ),
      External_Data = NA_character_
    ) %>%
    transmute(
      `Player Name`                                         = athlete_name,
      `Period Name`                                         = period_name_clean,
      `Period Number`                                       = `Period Number`,
      `Max Acceleration`                                    = max_effort_acceleration,
      `Max Deceleration`                                    = NA_real_,
      `Acceleration B1-3 Average Efforts (Session) (Gen 2)` = acc_b13,
      `Deceleration B1-3 Average Efforts (Session) (Gen 2)` = dec_b13,
      `Average Duration (Session)`                          = dur_hms,
      `Average Distance (Session)`                          = total_distance,
      `Average Player Load (Session)`                       = player_load,
      `Maximum Velocity`                                    = max_vel,
      `Max Vel (% Max)`                                     = percentage_max_velocity,
      `Meterage Per Minute`                                 = meterage_per_minute,
      `Player Load Per Minute`                              = player_load_per_minute,
      `Velocity Work/Rest Ratio`                            = NA_real_,
      `IMA Impacts Band 2 Average Count (Session)`          = NA_real_,
      `Velocity Band 1 Average Distance (Session)`          = velocity_band1_total_distance,
      `Velocity Band 2 Average Distance (Session)`          = velocity_band2_total_distance,
      `Velocity Band 3 Average Distance (Session)`          = velocity_band3_total_distance,
      `Velocity Band 4 Average Distance (Session)`          = velocity_band4_total_distance,
      `Velocity Band 5 Average Distance (Session)`          = velocity_band5_total_distance,
      `Velocity Band 6 Average Distance (Session)`          = vhsd,
      `Accel&Decel Efforts`                                 = acc_dec_total,
      `Accel&Decel Efforts Per Minute`                      = acc_dec_per_min,
      `Heart Rate Exertion`                                 = NA_real_,
      `Red Zone`                                            = NA_real_,
      `Energy`                                              = NA_real_,
      `High Metabolic Load Distance`                        = hmld,
      `High Speed Distance`                                 = hsd,
      `High Speed Distance Per Minute`                      = hsd_per_min,
      `High Speed Efforts`                                  = NA_real_,
      `Sprint Distance Per Minute`                          = NA_real_,
      `Sprint Efforts`                                      = NA_real_,
      # Tag columns — match All_Data types exactly:
      #   Athlete Tags             = numeric (CTR IDs, NA from API)
      #   Activity Tags            = character
      #   Game Tags                = logical (all NA in CTR too)
      #   Athlete Participation Tags = character
      #   Period Tags              = character
      `Athlete Tags`                                        = NA_real_,
      `Activity Tags`                                       = NA_character_,
      `Game Tags`                                           = NA,
      `Athlete Participation Tags`                          = NA_character_,
      `Period Tags`                                         = NA_character_,
      Preseason,
      Week,
      Type,
      Date          = as.numeric(Date),
      Day,
      External_Data
    ) %>%
    mutate(
      `Period Number`                  = as.numeric(`Period Number`),
      `Average Player Load (Session)`  = as.numeric(`Average Player Load (Session)`)
    )
}

# ---- Build Session rows (Period Number 0) -----------------------------------
cat("\nTransforming session rows...\n")

# Session pull has no period_name column — inject "Session" as the period name
if (!"period_name" %in% names(stats_session)) {
  stats_session$period_name <- "Session"
}

rows_session <- transform_to_alldata(
  stats_session,
  period_number_col    = 0L,
  period_name_override = "Session"
)

cat("Session rows built:", nrow(rows_session), "\n")

# ---- Build Drill rows (Period Number 1+) ------------------------------------
cat("Transforming drill rows...\n")

# Assign sequential period numbers within each activity per athlete
# (exclude any row the API labelled "Session" — that would duplicate the above)
stats_drills_clean <- stats_drills %>%
  filter(trimws(tolower(period_name)) != "session") %>%
  group_by(athlete_name, activity_name, date) %>%
  mutate(drill_period_number = row_number()) %>%
  ungroup()

rows_drills <- transform_to_alldata(
  stats_drills_clean,
  period_number_col = stats_drills_clean$drill_period_number
)

cat("Drill rows built:", nrow(rows_drills), "\n")

# ---- Combine & sort ---------------------------------------------------------
result <- bind_rows(rows_session, rows_drills) %>%
  arrange(desc(Date), `Period Number`)

cat("\n=== Final All_Data26 summary ===\n")
cat("Total rows:", nrow(result), "\n")
cat("Session rows (Period 0):", sum(result$`Period Number` == 0, na.rm = TRUE), "\n")
cat("Drill rows  (Period 1+):", sum(result$`Period Number` > 0,  na.rm = TRUE), "\n")
cat("Date range:", format(as.Date(as.character(min(result$Date, na.rm=TRUE)), "%Y%m%d"), "%d %b %Y"),
    "to", format(as.Date(as.character(max(result$Date, na.rm=TRUE)), "%Y%m%d"), "%d %b %Y"), "\n")
cat("Players:", length(unique(result$`Player Name`)), "\n")
cat("Activities per date:\n")
result %>%
  filter(`Period Number` == 0) %>%
  distinct(Date, Type, Week, Day) %>%
  arrange(Date) %>%
  head(60) %>%
  print()

# ---- Build Weekly_Data26 ----------------------------------------------------
weekly <- result %>%
  filter(`Period Number` == 0, trimws(tolower(`Period Name`)) == "session")

cat("\nWeekly (session-level) rows:", nrow(weekly), "\n")

# ---- Save -------------------------------------------------------------------
saveRDS(result, "Data/All_Data26.rds")
write.csv(result, "Data/All_Data26.csv", row.names = FALSE)

saveRDS(weekly, "Data/Weekly_Data26.rds")
write.csv(weekly, "Data/Weekly_Data26.csv", row.names = FALSE)

cat("\nSaved:\n")
cat("  Data/All_Data26.rds/.csv    —", nrow(result), "rows,", ncol(result), "cols\n")
cat("  Data/Weekly_Data26.rds/.csv —", nrow(weekly),  "rows\n")

# ---- Append KEEP-External Data.csv ------------------------------------------
# Any row added to Data/External/KEEP-External Data.csv is automatically
# merged into the RDS files every time this script runs.
# Duplicates (same Player Name + Date + Period Number) are silently skipped.
# Weekly_Data_Only = "yes"  → Weekly Data only
# Weekly_Data_Only = "no"   → All Data (all periods) + Weekly Data (Period 0)

suppressPackageStartupMessages(library(openxlsx))
KEEP_XLSX <- "Data/External/KEEP-External Data.xlsx"

if (file.exists(KEEP_XLSX)) {
  cat("\n--- Appending KEEP-External Data.xlsx ---\n")

  keep_df <- tryCatch({
    raw <- read.xlsx(KEEP_XLSX, sheet="Data Entry", colNames=FALSE, skipEmptyRows=FALSE)
    if (is.null(raw) || nrow(raw) <= 1) stop("empty")
    col_names <- as.character(unlist(raw[1, ]))
    df <- as.data.frame(raw[-1, ], stringsAsFactors=FALSE)
    colnames(df) <- col_names
    df
  }, error = function(e) { cat("  ERROR reading xlsx:", conditionMessage(e), "\n"); NULL })

  if (!is.null(keep_df) && nrow(keep_df) > 0) {
    keep_df <- keep_df[!is.na(keep_df[["Player Name"]]) & trimws(keep_df[["Player Name"]]) != "", , drop=FALSE]
  }

  if (!is.null(keep_df) && nrow(keep_df) > 0) {
    # Parse dates (store as numeric YYYYMMDD to match result/weekly)
    keep_df$Date <- suppressWarnings(as.integer(keep_df$Date))
    keep_df      <- keep_df[!is.na(keep_df$Date), ]

    date_parsed  <- as.Date(as.character(keep_df$Date), format = "%Y%m%d")
    keep_df$Date <- as.numeric(keep_df$Date)
    keep_df$Day  <- tolower(weekdays(date_parsed))
    keep_df$Week <- pmax(1L, as.integer(
      floor(as.numeric(difftime(date_parsed, SEASON_START, units = "days")) / 7) + 1L
    ))
    keep_df$Preseason <- dplyr::case_when(
      date_parsed >= PRESEASON_START & date_parsed <= PRESEASON_END ~ "yes",
      date_parsed >= INSEASON_START  & date_parsed <= INSEASON_END  ~ "no",
      TRUE ~ NA_character_
    )

    # Defaults
    if (!"Type"             %in% names(keep_df)) keep_df$Type             <- "training"
    if (!"Period Number"    %in% names(keep_df)) keep_df$`Period Number`  <- 0L
    if (!"Period Name"      %in% names(keep_df)) keep_df$`Period Name`    <- "Session"
    if (!"External_Data"    %in% names(keep_df)) keep_df$External_Data    <- NA_character_
    if (!"Weekly_Data_Only" %in% names(keep_df)) keep_df$Weekly_Data_Only <- "no"

    keep_df$Type             <- ifelse(is.na(keep_df$Type)    | trimws(keep_df$Type) == "",    "training", trimws(keep_df$Type))
    keep_df$`Period Number`  <- suppressWarnings(as.numeric(keep_df$`Period Number`))
    keep_df$`Period Number`[is.na(keep_df$`Period Number`)] <- 0
    keep_df$`Period Name`    <- ifelse(is.na(keep_df$`Period Name`) | trimws(keep_df$`Period Name`) == "", "Session", keep_df$`Period Name`)
    keep_df$Weekly_Data_Only <- tolower(trimws(ifelse(is.na(keep_df$Weekly_Data_Only), "no", keep_df$Weekly_Data_Only)))

    # Coerce every column to match the type it has in result
    # (xlsx colNames=FALSE returns everything as character — this fixes all mismatches)
    for (cn in names(keep_df)) {
      if (!cn %in% names(result)) next
      ref <- result[[cn]]
      if (inherits(ref, "hms") || inherits(ref, "difftime"))
        keep_df[[cn]] <- NA
      else if (is.numeric(ref) && !is.numeric(keep_df[[cn]]))
        keep_df[[cn]] <- suppressWarnings(as.numeric(keep_df[[cn]]))
      else if (is.logical(ref) && !is.logical(keep_df[[cn]]))
        keep_df[[cn]] <- suppressWarnings(as.logical(keep_df[[cn]]))
    }

    keep_for_all    <- keep_df[keep_df$Weekly_Data_Only == "no", ]
    keep_for_weekly <- keep_df[keep_df$Weekly_Data_Only == "yes" |
                                (keep_df$Weekly_Data_Only == "no" & keep_df$`Period Number` == 0), ]

    dup_key_k <- function(df) paste(df$`Player Name`, df$Date, df$`Period Number`, sep = "|")

    new_all    <- keep_for_all[!dup_key_k(keep_for_all)       %in% dup_key_k(result), ]
    new_weekly <- keep_for_weekly[!dup_key_k(keep_for_weekly) %in% dup_key_k(weekly), ]

    added_all <- added_weekly <- 0L
    if (nrow(new_all) > 0) {
      result    <- bind_rows(result, new_all[intersect(names(new_all), names(result))]) %>%
        arrange(desc(Date), `Period Number`)
      added_all <- nrow(new_all)
    }
    if (nrow(new_weekly) > 0) {
      weekly       <- bind_rows(weekly, new_weekly[intersect(names(new_weekly), names(weekly))]) %>%
        arrange(desc(Date), `Period Number`)
      added_weekly <- nrow(new_weekly)
    }

    if (added_all > 0 || added_weekly > 0) {
      saveRDS(result, "Data/All_Data26.rds");  write.csv(result, "Data/All_Data26.csv",  row.names = FALSE)
      saveRDS(weekly, "Data/Weekly_Data26.rds"); write.csv(weekly, "Data/Weekly_Data26.csv", row.names = FALSE)
      cat("  Added", added_all, "row(s) to All_Data,", added_weekly, "row(s) to Weekly_Data.\n")
      cat("  RDS files re-saved with KEEP data included.\n")
    } else {
      cat("  No new rows (all already present — skipped).\n")
    }
  } else {
    cat("  CSV empty or unreadable — skipped.\n")
  }
}

cat("\nDone!\n")
