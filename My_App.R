# ============================================================
#  NSW WARATAHS — Catapult Performance Dashboard
#  Author: Renée Sakura Steiner 
#  WhatsApp: +8180 5652 0929
#  Email: reneesakura@icloud.com
#  Last updated: 2026
# ============================================================
# install.packages(c("shiny","shinydashboard","dplyr","ggplot2",
#                    "lubridate","scales","tidyr","stringr","readr","DT"))

library(shiny); library(shinydashboard); library(dplyr); library(ggplot2)
library(lubridate); library(scales); library(tidyr); library(stringr)
library(readr); library(DT); library(plotly); library(purrr); library(openxlsx)

All_Data    <- readRDS("Data/All_Data26.rds")
Players_W   <- read.csv("Data/Players_W.csv",  check.names=FALSE, stringsAsFactors=FALSE, na.strings=c("","NA"))
Weekly_Data <- readRDS("Data/Weekly_Data26.rds")

All_Data$Date    <- as.Date(as.character(All_Data$Date),    format = "%Y%m%d")
Weekly_Data$Date <- as.Date(as.character(Weekly_Data$Date), format = "%Y%m%d")

# Remove sevens sessions — not part of the XVs program
All_Data    <- All_Data    %>% filter(tolower(trimws(Type)) != "sevens")
Weekly_Data <- Weekly_Data %>% filter(tolower(trimws(Type)) != "sevens")

All_Data <- All_Data %>%
  left_join(Players_W %>% select(`Name`, Position_Name, Position_Abrev, Forward_Back, Position_Number, Sevens),
            by = c("Player Name" = "Name"))

Weekly_Data <- Weekly_Data %>%
  left_join(Players_W %>% select(`Name`, Position_Name, Position_Abrev, Forward_Back, Position_Number, Sevens),
            by = c("Player Name" = "Name"))

# ---- KEEP-External Data.xlsx: auto-import on every app start ---------------
# Add any external player data to Data/External/KEEP-External Data.xlsx
# using the colour-coded template columns. The dashboard reads it automatically.
# Weekly_Data_Only = "yes"  → Weekly Data only
# Weekly_Data_Only = "no"   → All Data (all periods) + Weekly Data (Period 0 only)
local({
  keep_xlsx <- "Data/External/KEEP-External Data.xlsx"
  if (!file.exists(keep_xlsx)) return()

  SEASON_START    <- as.Date("2026-03-02")
  PRESEASON_START <- as.Date("2026-03-04")
  PRESEASON_END   <- as.Date("2026-06-07")
  INSEASON_START  <- as.Date("2026-06-08")
  INSEASON_END    <- as.Date("2026-08-02")

  # Read with colNames=FALSE to preserve column names with spaces exactly
  keep_df <- tryCatch({
    raw <- openxlsx::read.xlsx(keep_xlsx, sheet="Data Entry", colNames=FALSE, skipEmptyRows=FALSE)
    if (is.null(raw) || nrow(raw) <= 1) return(NULL)
    col_names <- as.character(unlist(raw[1, ]))
    df <- as.data.frame(raw[-1, ], stringsAsFactors=FALSE)
    colnames(df) <- col_names
    df
  }, error = function(e) NULL)
  if (is.null(keep_df) || nrow(keep_df) == 0) return()
  keep_df <- keep_df[!is.na(keep_df[["Player Name"]]) & trimws(keep_df[["Player Name"]]) != "", , drop=FALSE]
  if (nrow(keep_df) == 0) return()

  # Parse date → Date object (consistent with All_Data / Weekly_Data)
  keep_df$Date <- suppressWarnings(as.integer(keep_df$Date))
  keep_df      <- keep_df[!is.na(keep_df$Date), ]
  if (nrow(keep_df) == 0) return()

  date_parsed       <- as.Date(as.character(keep_df$Date), format = "%Y%m%d")
  keep_df$Date      <- date_parsed
  keep_df$Day       <- tolower(weekdays(date_parsed))
  keep_df$Week      <- pmax(1L, as.integer(
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

  # Coerce every column to match the type it has in All_Data
  # (xlsx colNames=FALSE returns everything as character — this fixes all mismatches)
  for (cn in names(keep_df)) {
    if (!cn %in% names(All_Data)) next
    ref <- All_Data[[cn]]
    if (inherits(ref, "hms") || inherits(ref, "difftime"))
      keep_df[[cn]] <- NA          # duration not meaningful for external data
    else if (is.numeric(ref) && !is.numeric(keep_df[[cn]]))
      keep_df[[cn]] <- suppressWarnings(as.numeric(keep_df[[cn]]))
    else if (is.logical(ref) && !is.logical(keep_df[[cn]]))
      keep_df[[cn]] <- suppressWarnings(as.logical(keep_df[[cn]]))
  }

  # Join position data
  keep_df <- keep_df %>%
    dplyr::left_join(
      Players_W %>% dplyr::select(Name, Position_Name, Position_Abrev,
                                   Forward_Back, Position_Number, Sevens),
      by = c("Player Name" = "Name")
    )

  # Deduplication key: Player Name + Date + Period Number
  dup_key <- function(df) paste(df$`Player Name`, format(df$Date, "%Y%m%d"), df$`Period Number`, sep = "|")

  keep_for_all    <- keep_df[keep_df$Weekly_Data_Only == "no", ]
  keep_for_weekly <- keep_df[keep_df$Weekly_Data_Only == "yes" |
                              (keep_df$Weekly_Data_Only == "no" & keep_df$`Period Number` == 0), ]

  new_all    <- keep_for_all[!dup_key(keep_for_all)       %in% dup_key(All_Data),    ]
  new_weekly <- keep_for_weekly[!dup_key(keep_for_weekly) %in% dup_key(Weekly_Data), ]

  if (nrow(new_all) > 0)
    All_Data <<- dplyr::bind_rows(All_Data, new_all[intersect(names(new_all), names(All_Data))]) %>%
      dplyr::arrange(desc(Date), `Player Name`)
  if (nrow(new_weekly) > 0)
    Weekly_Data <<- dplyr::bind_rows(Weekly_Data, new_weekly[intersect(names(new_weekly), names(Weekly_Data))]) %>%
      dplyr::arrange(desc(Date), `Player Name`)
})

PRED_DIR <- "predictions"

# ---- Max weekly progression rate (change this to update all predictions) ----
MAX_PROG_RATE <- 1.2

# ---- Catapult R API Setup ---------------------------------------------------
# (API token not used in dashboard — data loaded from .rds files)
# catapult_token_SRW <- ofCloudCreateToken(sToken = "...", sRegion = "APAC")



All_Data25 <- read.csv("Data/All_Data25.csv", check.names=FALSE, stringsAsFactors=FALSE, na.strings=c("","NA"))
All_Data25$Date <- as.Date(as.character(All_Data25$Date), format = "%Y%m%d")
All_Data25 <- All_Data25 %>%
  left_join(Players_W %>% select(`Name`, Position_Name, Position_Abrev, Forward_Back, Position_Number, Sevens),
            by = c("Player Name" = "Name"))


NAVY<-"#001F4E";SKY<-"#009FDF";WHITE<-"#FFFFFF";LIGHT_BG<-"#F0F4F8"
CARD_BG<-"#FFFFFF";ACCENT<-"#C8102E";GREY_MID<-"#8899AA"
HML_COL<-"High Metabolic Load Distance"

# ============================================================
#  CHANGE FORWARDS & BACKS COLOURS HERE
# ============================================================
FORWARD <- "#009FDF"   # colour used for Forwards throughout the dashboard
BACK    <- "#EE3377"   # colour used for Backs throughout the dashboard

colour_options <- c( '#E69F00','#56B4E9','#009E73','#F0E442','#0072B2','#D55E00','#CC79A7','#000000','#0077BB','#33BBEE','#EE7733','#CC3311',
                     '#EE3377','#88CCEE','#CC6677','#DDCC77','#117733','#332288')
# ============================================================

fwd_back_css <- paste0(
  ".kpi-fwd{color:", FORWARD, "!important;font-weight:700;}",
  ".kpi-bck{color:", BACK,    "!important;font-weight:700;}",
  ".player-stat.fwd .ps-value{color:", FORWARD, "!important;}",
  ".player-stat.bck .ps-value{color:", BACK,    "!important;}"
)

theme_waratahs <- function(base_size=12) {
  theme_minimal(base_size=base_size) +
    theme(plot.background=element_rect(fill=CARD_BG,colour=NA),
          panel.background=element_rect(fill=CARD_BG,colour=NA),
          panel.grid.major=element_line(colour="#E2EAF0",linewidth=0.4),
          panel.grid.minor=element_blank(),
          axis.text=element_text(colour=NAVY,size=rel(0.85)),
          axis.title=element_text(colour=NAVY,face="bold",size=rel(0.9)),
          plot.title=element_text(colour=NAVY,face="bold",size=rel(1.1)),
          plot.subtitle=element_text(colour=GREY_MID,size=rel(0.85)),
          legend.background=element_rect(fill=CARD_BG,colour=NA),
          legend.text=element_text(colour=NAVY),
          legend.title=element_text(colour=NAVY,face="bold"),
          strip.background=element_rect(fill=NAVY,colour=NA),
          strip.text=element_text(colour=WHITE,face="bold"))
}

custom_css <- "
  body,.content-wrapper,.main-footer{background-color:#F0F4F8!important;}
  .skin-blue .main-header .logo{background-color:#001F4E!important;color:#FFFFFF!important;font-weight:700;}
  .skin-blue .main-header .logo span{color:#FFFFFF!important;}
  .skin-blue .main-header .navbar{background-color:#001F4E!important;}
  .skin-blue .main-sidebar{background-color:#001F4E!important;}
  .skin-blue .sidebar-menu>li>a{color:#B0C8E0!important;font-weight:500;}
  .skin-blue .sidebar-menu>li.active>a,.skin-blue .sidebar-menu>li>a:hover{background-color:#009FDF!important;color:#FFFFFF!important;}
  .skin-blue .sidebar-menu>li.active>a{border-left:4px solid #FFFFFF!important;}
  .session-banner{background:linear-gradient(135deg,#001F4E 0%,#003580 60%,#009FDF 100%);border-radius:10px;padding:18px 24px;margin-bottom:18px;display:flex;flex-wrap:wrap;gap:16px;align-items:center;}
  .banner-item{display:flex;flex-direction:column;}
  .banner-label{color:#88BBDD;font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:0.12em;}
  .banner-value{color:#FFFFFF;font-size:18px;font-weight:800;margin-top:2px;}
  .banner-divider{width:1px;background:rgba(255,255,255,0.2);height:40px;margin:0 4px;}
  .type-badge{display:inline-block;padding:4px 14px;border-radius:20px;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:0.08em;margin-top:4px;}
  .badge-intensive{background-color:#C8102E;color:#fff;}
  .badge-extensive{background-color:#009FDF;color:#fff;}
  .badge-training{background-color:#009FDF;color:#fff;}
  .badge-clubgame{background-color:#00843D;color:#fff;}
  .badge-othergame{background-color:#7B2D8B;color:#fff;}
  .badge-extensive{background-color:#0077BB;color:#fff;}
  .badge-sevens{background-color:#E8A020;color:#fff;}
  .badge-rehabrun{background-color:#8899AA;color:#fff;}
  .badge-other{background-color:#556677;color:#fff;}
  .badge-preseason{background:rgba(255,255,255,0.15);color:#AACCEE;border:1px solid rgba(255,255,255,0.3);}
  .kpi-row{display:flex;flex-wrap:wrap;gap:14px;margin-bottom:18px;}
  .kpi-card{background:#FFFFFF;border-radius:10px;padding:16px 20px;flex:1 1 140px;min-width:130px;border-top:4px solid #009FDF;box-shadow:0 2px 8px rgba(0,31,78,0.08);}
  .kpi-card.accent-red{border-top-color:#C8102E;} .kpi-card.accent-navy{border-top-color:#001F4E;} .kpi-card.accent-green{border-top-color:#00843D;}
  .kpi-label{font-size:10px;font-weight:700;color:#8899AA;text-transform:uppercase;letter-spacing:0.1em;}
  .kpi-value{font-size:26px;font-weight:800;color:#001F4E;line-height:1.1;margin:6px 0 2px;}
  .kpi-unit{font-size:11px;color:#8899AA;font-weight:500;}
  .kpi-split{font-size:11px;margin-top:6px;}
  .kpi-fwd{color:#009FDF;font-weight:700;} .kpi-bck{color:#E8A020;font-weight:700;}
  .player-box{background:#FFFFFF;border-radius:10px;padding:14px 20px;border-left:5px solid #001F4E;box-shadow:0 2px 8px rgba(0,31,78,0.08);margin-bottom:18px;display:flex;align-items:center;gap:24px;flex-wrap:wrap;}
  .player-stat{display:flex;flex-direction:column;align-items:center;}
  .player-stat .ps-value{font-size:28px;font-weight:800;color:#001F4E;}
  .player-stat .ps-label{font-size:10px;font-weight:700;color:#8899AA;text-transform:uppercase;letter-spacing:0.1em;}
  .player-stat.fwd .ps-value{color:#009FDF;} .player-stat.bck .ps-value{color:#E8A020;}
  .player-box-divider{width:1px;background:#E2EAF0;height:50px;}
  .section-header{font-size:13px;font-weight:800;color:#001F4E;text-transform:uppercase;letter-spacing:0.1em;border-left:4px solid #009FDF;padding-left:10px;margin:22px 0 12px;}
  .chart-card{background:#FFFFFF;border-radius:10px;padding:18px 20px;box-shadow:0 2px 8px rgba(0,31,78,0.08);margin-bottom:18px;}
  .chart-card-title{font-size:12px;font-weight:800;color:#001F4E;text-transform:uppercase;letter-spacing:0.08em;margin-bottom:12px;border-bottom:1px solid #E2EAF0;padding-bottom:8px;}
  .totals-notice{background:#FFF8E1;border-left:4px solid #F9A825;border-radius:8px;padding:10px 16px;margin-bottom:14px;display:flex;align-items:center;gap:10px;cursor:pointer;}
  .totals-notice:hover{background:#FFF3CD;}
  .totals-notice-icon{font-size:16px;}
  .totals-notice-text{font-size:12px;color:#5D4037;font-weight:600;}
  .totals-notice-link{font-size:12px;color:#0072B2;font-weight:700;text-decoration:underline;white-space:nowrap;}
  .selectize-input{border:2px solid #009FDF!important;border-radius:6px!important;font-weight:600!important;color:#001F4E!important;}
  .selectize-dropdown{border:2px solid #009FDF!important;border-radius:6px!important;}
  .main-footer{background:#001F4E!important;color:#7799BB!important;font-size:11px;}
  .hml-tabs .nav-tabs{border-bottom:3px solid #009FDF;margin-bottom:18px;}
  .hml-tabs .nav-tabs>li>a{color:#001F4E;font-weight:700;font-size:12px;text-transform:uppercase;letter-spacing:0.07em;border-radius:6px 6px 0 0;border:none;padding:9px 20px;background:#E8F4FB;}
  .hml-tabs .nav-tabs>li.active>a,.hml-tabs .nav-tabs>li>a:hover{background:#009FDF!important;color:#fff!important;border:none!important;}
  .hml-tabs .tab-content{padding-top:4px;}
  .acc-box{border-radius:10px;padding:14px 18px;text-align:center;box-shadow:0 2px 8px rgba(0,31,78,0.10);margin-bottom:14px;}
  .acc-box .ab-label{font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:0.1em;opacity:0.75;margin-bottom:4px;}
  .acc-box .ab-metric{font-size:15px;font-weight:800;margin-bottom:2px;}
  .acc-box .ab-status{font-size:11px;font-weight:700;border-radius:10px;padding:2px 10px;display:inline-block;margin-top:4px;}
  .status-on{background:#2ECC71;color:#fff;} .status-over{background:#C8102E;color:#fff;} .status-under{background:#E8A020;color:#fff;}
  .acc-box-on{border-left:5px solid #2ECC71;background:#F0FFF5;} .acc-box-over{border-left:5px solid #C8102E;background:#FFF5F5;}
  .acc-box-under{border-left:5px solid #E8A020;background:#FFFBF0;} .acc-box-na{border-left:5px solid #aaa;background:#F8F8F8;}
  .peaks-banner{background:linear-gradient(135deg,#78350F 0%,#92400E 55%,#B45309 100%);border-radius:10px;padding:14px 20px 16px;margin-bottom:18px;position:relative;overflow:hidden;}
  .peaks-banner::before{content:'🎉';position:absolute;right:18px;top:10px;font-size:42px;opacity:0.25;line-height:1;}
  .peaks-banner-header{color:#FEF3C7;font-size:12px;font-weight:800;text-transform:uppercase;letter-spacing:0.12em;margin-bottom:12px;display:flex;align-items:center;gap:8px;}
  .peaks-banner-title{color:#FFFBEB;font-size:13px;font-weight:800;text-transform:uppercase;letter-spacing:0.1em;}
  .peaks-banner-cards{display:flex;flex-wrap:wrap;gap:10px;}
  .peak-card{background:rgba(255,255,255,0.14);border-radius:8px;padding:10px 16px;border:1px solid rgba(255,255,255,0.22);min-width:150px;}
  .peak-card-player{color:#FDE68A;font-size:10px;font-weight:800;text-transform:uppercase;letter-spacing:0.1em;margin-bottom:2px;}
  .peak-card-metric{color:rgba(255,255,255,0.80);font-size:11px;font-weight:600;margin-bottom:3px;}
  .peak-card-value{color:#FFFFFF;font-size:20px;font-weight:800;line-height:1.1;}
  .peak-card-prev{color:rgba(255,255,255,0.50);font-size:10px;margin-top:3px;}
"

fmt_num <- function(x, digits=0) {
  if (is.na(x)) return("---")
  formatC(round(x,digits), format="f", digits=digits, big.mark=",")}

type_badge_html <- function(type_val) {
  cls <- switch(tolower(trimws(as.character(type_val))),
    "intensive" = "badge-intensive",
    "extensive" = "badge-extensive",
    "training"  = "badge-training",
    "clubgame"  = "badge-clubgame",
    "sevens"    = "badge-sevens",
    "rehabrun"  = "badge-rehabrun",
    "badge-other"
  )
  sprintf('<span class="type-badge %s">%s</span>', cls, toupper(type_val))
}

week_to_num <- function(w) {
  # New pipeline stores weeks as integers; All_Data25 uses words — handle both
  n <- suppressWarnings(as.integer(w))
  if (all(!is.na(n))) return(n)
  lut <- c(one=1,two=2,three=3,four=4,five=5,six=6,seven=7,eight=8,nine=9,
           ten=10,eleven=11,twelve=12,thirteen=13,fourteen=14,fifteen=15,sixteen=16)
  w_low <- tolower(trimws(as.character(w)))
  ifelse(!is.na(n), n, ifelse(w_low %in% names(lut), lut[w_low], NA_integer_))
}

# ============================================================
#  WARATAH ANALYTICS — pre-computed datasets (from All_Data)
#  Matches the eight datasets from the original GPS Dashboard script:
#    1. Weekly Volume                    (weekly_volume_SRW)
#    2. Session Summary & Weekly Loads   (session_data)
#    3. Drill Summary                    (drill_data)
#    4. Match Report                     (match_data)
#    5. ACWR — Individual                (ACWR_SR)
#    6. Positional Acute & Chronic Loads (pos_EWMA_AcCh)
#    7. Session Predictor                (session_predictor)
#    8. Positional Session Predictor     (pos_drill_predictor)
#
#  Notes on unavailable columns (set to NA):
#    accel_load / accel_density  — total_acceleration_load not pulled by pipeline
#    %maxa                       — profile_max_accel not in All_Data
#    %maxv uses Max Vel (% Max) which IS available from the Catapult API
# ============================================================
local({

  # ---- Base: drill rows only (Period > 0 = individual periods) ---------------
  wara_base <<- All_Data %>%
    filter(`Period Number` > 0) %>%
    transmute(
      athlete_name  = `Player Name`,
      period_name   = `Period Name`,
      date          = Date,
      week_number   = week_to_num(Week),
      season        = as.integer(format(Date, "%Y")),
      position_name = coalesce(Position_Name, "Unknown"),
      group         = case_when(
        tolower(coalesce(Forward_Back, "")) == "forward" ~ "FWD",
        tolower(coalesce(Forward_Back, "")) == "back"    ~ "BKS",
        TRUE ~ "Other"
      ),
      pos_abrev     = coalesce(Position_Abrev, "?"),
      session       = str_to_title(coalesce(Type, "Training")),
      is_match      = (coalesce(Type, "") == "clubgame"),
      dur_sec       = suppressWarnings(as.numeric(`Average Duration (Session)`)),
      total_distance= coalesce(`Average Distance (Session)`, 0),
      hsr           = coalesce(`High Speed Distance`, 0),
      vhsr          = coalesce(`Velocity Band 6 Average Distance (Session)`, 0),
      HMLD          = coalesce(`High Metabolic Load Distance`, 0),
      max_vel       = coalesce(`Maximum Velocity`, 0),
      pct_max_vel   = coalesce(`Max Vel (% Max)`, 0),   # % of profile max velocity
      mpm           = coalesce(`Meterage Per Minute`, 0),
      accels        = coalesce(`Acceleration B1-3 Average Efforts (Session) (Gen 2)`, 0),
      max_accel     = coalesce(`Max Acceleration`, 0)
    ) %>%
    mutate(dur_sec = coalesce(dur_sec, 0), dur_min = dur_sec / 60)

  # EWMA constants (shared across datasets 5, 6)
  ck <<- 2 / (28 + 1)   # chronic: 28-day time constant
  ak <<- 2 / (7  + 1)   # acute:   7-day  time constant

  # Full date sequence for EWMA grids
  wara_min_dt <- min(wara_base$date, na.rm = TRUE)
  wara_dates  <- seq(wara_min_dt, Sys.Date(), by = "day")

  # ---- 1. Weekly Volume -------------------------------------------------------
  # Equivalent to weekly_volume_SRW in the original script
  wara_weekly_vol <<- wara_base %>%
    group_by(athlete_name, week_number, position_name, season) %>%
    summarise(
      `Total Distance` = round(sum(total_distance, na.rm = TRUE), 0),
      `Total HSR`      = round(sum(hsr,            na.rm = TRUE), 0),
      `Total VHSR`     = round(sum(vhsr,           na.rm = TRUE), 0),
      `Accel Load`     = NA_real_,   # total_acceleration_load not in pipeline output
      `HMLD`           = round(sum(HMLD,           na.rm = TRUE), 0),
      `Accel Count`    = round(sum(accels,          na.rm = TRUE), 0),
      `% of HSR`       = round((sum(hsr) / max(sum(total_distance), 0.01)) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(season, week_number, athlete_name)

  # ---- 2. Session Summary & Weekly Loads -------------------------------------
  # Equivalent to session_data in the original script.
  # tot_session = how many distinct sessions that athlete attended that week.
  tot_sess <- wara_base %>%
    group_by(athlete_name, date, week_number, season) %>%
    summarise(n = n_distinct(session), .groups = "drop") %>%
    group_by(athlete_name, week_number, season) %>%
    summarise(tot_session = sum(n), .groups = "drop")

  wara_session <<- wara_base %>%
    group_by(session, athlete_name, position_name, pos_abrev, date, group, week_number, season) %>%
    summarise(
      duration      = round(sum(dur_min,        na.rm = TRUE), 1),
      dist          = round(sum(total_distance, na.rm = TRUE), 0),
      hsr           = round(sum(hsr,            na.rm = TRUE), 0),
      vhsr          = round(sum(vhsr,           na.rm = TRUE), 0),
      hmld          = round(sum(HMLD,           na.rm = TRUE), 0),
      maxv          = round(max(max_vel,         na.rm = TRUE), 2),
      maxa          = round(max(max_accel,       na.rm = TRUE), 2),
      accels        = round(sum(accels,          na.rm = TRUE), 0),
      `%maxv`       = round(max(pct_max_vel,     na.rm = TRUE), 1),
      `%maxa`       = NA_real_,    # profile_max_accel not available
      accel_density = NA_real_,    # not in pipeline output
      .groups = "drop"
    ) %>%
    left_join(tot_sess, by = c("athlete_name", "week_number", "season")) %>%
    select(date, session, athlete_name, position_name, pos_abrev, duration,
           dist, hsr, vhsr, maxv, `%maxv`, maxa, `%maxa`,
           accels, hmld, group, week_number, accel_density, season, tot_session) %>%
    arrange(desc(date))

  # ---- 3. Drill Summary -------------------------------------------------------
  # Equivalent to drill_data in the original script.
  # Team average per named drill period across all athletes present.
  wara_drill <<- wara_base %>%
    group_by(session, period_name, date) %>%
    summarise(
      duration      = round(mean(dur_min,        na.rm = TRUE), 1),
      dist          = round(mean(total_distance, na.rm = TRUE), 0),
      hsr           = round(mean(hsr,            na.rm = TRUE), 0),
      vhsr          = round(mean(vhsr,           na.rm = TRUE), 0),
      hmld          = round(mean(HMLD,           na.rm = TRUE), 0),
      maxv          = round(mean(max_vel,         na.rm = TRUE), 2),
      maxa          = round(mean(max_accel,       na.rm = TRUE), 2),
      accels        = round(mean(accels,          na.rm = TRUE), 1),
      `%maxv`       = round(mean(pct_max_vel,     na.rm = TRUE), 1),
      `%maxa`       = NA_real_,
      accel_density = NA_real_,
      .groups = "drop"
    ) %>%
    select(date, session, period_name, duration, dist, hsr, vhsr,
           maxv, `%maxv`, maxa, `%maxa`, accels, hmld, accel_density) %>%
    arrange(desc(date))

  # ---- 4. Match Report --------------------------------------------------------
  # Equivalent to match_data in the original script.
  # Filtered to clubgame activities; session label used as Opposition proxy.
  wara_match <<- wara_base %>%
    filter(is_match) %>%
    group_by(Opposition = session, period_name, athlete_name,
             position_name, date, group) %>%
    summarise(
      duration      = round(sum(dur_min,        na.rm = TRUE), 1),
      dist          = round(sum(total_distance, na.rm = TRUE), 0),
      hsr           = round(sum(hsr,            na.rm = TRUE), 0),
      vhsr          = round(sum(vhsr,           na.rm = TRUE), 0),
      hmld          = round(sum(HMLD,           na.rm = TRUE), 0),
      maxv          = round(max(max_vel,         na.rm = TRUE), 2),
      maxa          = round(max(max_accel,       na.rm = TRUE), 2),
      accels        = round(sum(accels,          na.rm = TRUE), 0),
      `%maxv`       = round(max(pct_max_vel,     na.rm = TRUE), 1),
      `%maxa`       = NA_real_,
      accel_density = NA_real_,
      .groups = "drop"
    ) %>%
    select(date, Opposition, period_name, athlete_name, position_name,
           duration, dist, hsr, vhsr, maxv, `%maxv`, maxa, `%maxa`,
           accels, hmld, accel_density, group) %>%
    arrange(desc(date))

  # ---- 5. ACWR — Individual ---------------------------------------------------
  # Equivalent to ACWR_SR in the original script.
  # Outputs: acwr_dist/hsr/vhsr/accel_count/HMLD, days_since_95/90, all EWMA values.
  # %maxv (pct_max_vel = Max Vel % Max) IS available — used for days_since_95/90.
  wara_daily_ind <- wara_base %>%
    group_by(athlete_name, date) %>%
    summarise(
      dist        = sum(total_distance, na.rm = TRUE),
      hsr         = sum(hsr,            na.rm = TRUE),
      vhsr        = sum(vhsr,           na.rm = TRUE),
      HMLD        = sum(HMLD,           na.rm = TRUE),
      accel_count = sum(accels,         na.rm = TRUE),
      percentmaxv = max(pct_max_vel,    na.rm = TRUE),
      week_number = first(week_number),
      .groups = "drop"
    )

  # Supplement ACWR with external touring data (weekly totals -> single day entry).
  # Without this, touring players show zero load during tour weeks, inflating ACWR on return.
  if (exists("Weekly_Data") && any(!is.na(Weekly_Data$External_Data))) {
    ext_daily_acwr <- Weekly_Data %>%
      filter(!is.na(External_Data), trimws(as.character(External_Data)) != "") %>%
      transmute(
        athlete_name = `Player Name`,
        date         = Date,
        dist         = coalesce(suppressWarnings(as.numeric(`Average Distance (Session)`)), 0),
        hsr          = coalesce(suppressWarnings(as.numeric(`High Speed Distance`)), 0),
        vhsr         = coalesce(suppressWarnings(as.numeric(`Velocity Band 6 Average Distance (Session)`)), 0),
        HMLD         = coalesce(suppressWarnings(as.numeric(`High Metabolic Load Distance`)), 0),
        accel_count  = coalesce(suppressWarnings(as.numeric(`Acceleration B1-3 Average Efforts (Session) (Gen 2)`)), 0),
        percentmaxv  = 0,
        week_number  = 1L + floor(as.integer(difftime(date, wara_min_dt, units = "weeks")))
      )
    if (nrow(ext_daily_acwr) > 0) {
      wara_daily_ind <- bind_rows(wara_daily_ind, ext_daily_acwr) %>%
        group_by(athlete_name, date) %>%
        summarise(
          dist        = sum(dist,        na.rm = TRUE),
          hsr         = sum(hsr,         na.rm = TRUE),
          vhsr        = sum(vhsr,        na.rm = TRUE),
          HMLD        = sum(HMLD,        na.rm = TRUE),
          accel_count = sum(accel_count, na.rm = TRUE),
          percentmaxv = max(percentmaxv, na.rm = TRUE),
          week_number = first(week_number),
          .groups = "drop"
        )
    }
  }

  ath_names_w <- unique(wara_daily_ind$athlete_name)

  wara_grid_ind <<- expand.grid(
    athlete_name = ath_names_w, date = wara_dates, stringsAsFactors = FALSE
  ) %>%
    mutate(date = as.Date(date)) %>%
    left_join(wara_daily_ind, by = c("athlete_name", "date")) %>%
    replace(is.na(.), 0)

  # Compute EWMA loads then ACWR
  wara_acwr_raw <- wara_grid_ind %>%
    group_by(athlete_name) %>%
    arrange(date) %>%
    mutate(
      EWMA_chronic_dist      = accumulate(dist,        ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_hsr       = accumulate(hsr,         ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_vhsr      = accumulate(vhsr,        ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_HMLD      = accumulate(HMLD,        ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_accel     = accumulate(accel_count, ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_acute_dist        = accumulate(dist,        ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_hsr         = accumulate(hsr,         ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_vhsr        = accumulate(vhsr,        ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_HMLD        = accumulate(HMLD,        ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_accel       = accumulate(accel_count, ~.y*ak+((1-ak)*.x), .init=0)[-1]
    ) %>%
    mutate(
      acwr_dist        = round(EWMA_acute_dist  / ifelse(EWMA_chronic_dist==0,  NA, EWMA_chronic_dist),  2),
      acwr_hsr         = round(EWMA_acute_hsr   / ifelse(EWMA_chronic_hsr==0,   NA, EWMA_chronic_hsr),   2),
      acwr_vhsr        = round(EWMA_acute_vhsr  / ifelse(EWMA_chronic_vhsr==0,  NA, EWMA_chronic_vhsr),  2),
      acwr_HMLD        = round(EWMA_acute_HMLD  / ifelse(EWMA_chronic_HMLD==0,  NA, EWMA_chronic_HMLD),  2),
      acwr_accel_count = round(EWMA_acute_accel / ifelse(EWMA_chronic_accel==0, NA, EWMA_chronic_accel), 2)
    ) %>%
    ungroup()

  # days_since_95 / days_since_90: most recent date each athlete hit >= 95% / 90% max vel
  wara_acwr_raw <- wara_acwr_raw %>%
    arrange(athlete_name, date) %>%
    group_by(athlete_name) %>%
    mutate(
      last_95 = as.Date(sapply(seq_along(date), function(i) {
        pd <- date[1:i]; pv <- percentmaxv[1:i]
        if (any(pv > 94.999, na.rm = TRUE)) as.character(max(pd[pv > 94.999], na.rm = TRUE))
        else as.character(min(pd, na.rm = TRUE))
      })),
      days_since_95 = as.numeric(date - last_95),
      last_90 = as.Date(sapply(seq_along(date), function(i) {
        pd <- date[1:i]; pv <- percentmaxv[1:i]
        if (any(pv > 89.999, na.rm = TRUE)) as.character(max(pd[pv > 89.999], na.rm = TRUE))
        else as.character(min(pd, na.rm = TRUE))
      })),
      days_since_90 = as.numeric(date - last_90)
    ) %>%
    ungroup()

  # First training date per athlete — used to suppress ACWR for the first 28 days.
  # Chronic EWMA starts at 0 and takes ~28 days to become meaningful; showing a ratio
  # before then would be misleading (low chronic → artificially inflated ACWR).
  first_train_date <- wara_daily_ind %>%
    filter(dist > 0 | hsr > 0 | accel_count > 0) %>%
    group_by(athlete_name) %>%
    summarise(first_date = min(date, na.rm = TRUE), .groups = "drop")

  wara_acwr <<- wara_acwr_raw %>%
    filter(dist > 0 | hsr > 0) %>%
    left_join(first_train_date, by = "athlete_name") %>%
    filter(!is.na(first_date), date >= first_date + 28) %>%   # ≥28 days of prior history required
    mutate(
      week_number = 1L + floor(as.integer(difftime(date, wara_min_dt, units = "weeks")))
    ) %>%
    select(athlete_name, date, week_number,
           acwr_dist, acwr_hsr, acwr_vhsr, acwr_accel_count, acwr_HMLD,
           days_since_95, days_since_90,
           EWMA_chronic_dist, EWMA_chronic_hsr, EWMA_chronic_vhsr, EWMA_chronic_HMLD,
           EWMA_acute_dist,   EWMA_acute_hsr,   EWMA_acute_vhsr,   EWMA_acute_HMLD) %>%
    mutate(across(where(is.numeric), ~round(.x, 2))) %>%
    arrange(desc(date), athlete_name)

  # ---- 6. Positional Acute & Chronic Loads ------------------------------------
  # Equivalent to pos_EWMA_AcCh in the original script.
  # Daily average load per position → EWMA acute (7d) + chronic (28d).
  # Columns match original output exactly (no ACWR ratio — original doesn't compute it).
  wara_pos_daily <- wara_base %>%
    filter(position_name != "Unknown", nchar(trimws(position_name)) > 0) %>%
    group_by(date, position_name) %>%
    summarise(
      dist  = mean(total_distance, na.rm = TRUE),
      hsr   = mean(hsr,            na.rm = TRUE),
      vhsr  = mean(vhsr,           na.rm = TRUE),
      HMLD  = mean(HMLD,           na.rm = TRUE),
      .groups = "drop"
    )

  pos_names_w <- unique(wara_pos_daily$position_name)

  wara_grid_pos <- expand.grid(
    position_name = pos_names_w, date = wara_dates, stringsAsFactors = FALSE
  ) %>%
    mutate(date = as.Date(date)) %>%
    left_join(wara_pos_daily, by = c("position_name", "date")) %>%
    replace(is.na(.), 0)

  wara_pos_ewma <<- wara_grid_pos %>%
    group_by(position_name) %>%
    arrange(date) %>%
    mutate(
      EWMA_acute_dist   = accumulate(dist, ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_hsr    = accumulate(hsr,  ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_vhsr   = accumulate(vhsr, ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_acute_HMLD   = accumulate(HMLD, ~.y*ak+((1-ak)*.x), .init=0)[-1],
      EWMA_chronic_dist = accumulate(dist, ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_hsr  = accumulate(hsr,  ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_vhsr = accumulate(vhsr, ~.y*ck+((1-ck)*.x), .init=0)[-1],
      EWMA_chronic_HMLD = accumulate(HMLD, ~.y*ck+((1-ck)*.x), .init=0)[-1]
    ) %>%
    ungroup() %>%
    filter(dist > 0) %>%
    select(position_name, date,
           EWMA_acute_dist,   EWMA_acute_hsr,   EWMA_acute_vhsr,   EWMA_acute_HMLD,
           EWMA_chronic_dist, EWMA_chronic_hsr, EWMA_chronic_vhsr, EWMA_chronic_HMLD) %>%
    mutate(across(where(is.numeric), ~round(.x, 1))) %>%
    arrange(desc(date), position_name)

  # ---- 7. Session Predictor (squad-level) ------------------------------------
  # Equivalent to session_predictor in the original script.
  # Grouped by period_name only — gives per-minute intensity benchmark for each drill.
  # Use: planned_duration_min × rate = predicted load for that drill.
  wara_predictor <<- wara_base %>%
    filter(tolower(session) != "rehabrun") %>%
    group_by(period_name) %>%
    summarise(
      `dist/min`        = round(sum(total_distance, na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 1),
      `hsr/min`         = round(sum(hsr,            na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 2),
      `vhsr/min`        = round(sum(vhsr,           na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 2),
      `accel_load/min`  = NA_real_,   # total_acceleration_load not in pipeline output
      `hard_accels/min` = round(sum(accels,          na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 2),
      `accel_density`   = NA_real_,   # not in pipeline output
      `meters/min`      = round(mean(mpm, na.rm=TRUE), 1),
      .groups = "drop"
    ) %>%
    select(period_name, `dist/min`, `hsr/min`, `vhsr/min`,
           `accel_load/min`, `hard_accels/min`, `accel_density`, `meters/min`) %>%
    arrange(period_name)

  # ---- 8. Positional Session Predictor ----------------------------------------
  # Equivalent to pos_drill_predictor in the original script.
  # Same per-minute rates split by position — the key tool for next-week load planning.
  # For each drill + position: planned_duration_min × rate = predicted load per athlete.
  wara_pos_predictor <<- wara_base %>%
    filter(tolower(session) != "rehabrun",
           position_name != "Unknown", nchar(trimws(position_name)) > 0) %>%
    group_by(period_name, position_name) %>%
    summarise(
      `dist/min`        = round(sum(total_distance, na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 1),
      `hsr/min`         = round(sum(hsr,            na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 2),
      `vhsr/min`        = round(sum(vhsr,           na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 2),
      `accel_load/min`  = NA_real_,
      `hard_accels/min` = round(sum(accels,          na.rm=TRUE) / max(sum(dur_min, na.rm=TRUE), 0.01), 2),
      `accel_density`   = NA_real_,
      `meters/min`      = round(mean(mpm, na.rm=TRUE), 1),
      .groups = "drop"
    ) %>%
    select(period_name, position_name, `dist/min`, `hsr/min`, `vhsr/min`,
           `accel_load/min`, `hard_accels/min`, `accel_density`, `meters/min`) %>%
    arrange(period_name, position_name)

  # ---- DRILL PREDICTOR RATES --------------------------------------------------
  # Per-minute rates (avg + 90th-pct peak) used by the Drill Predictor page.
  # Sourced from wara_session (session-level, non-rehab, ≥5 min duration).
  dp_base <- wara_session %>%
    filter(duration >= 5, tolower(session) != "rehabrun", dist > 0) %>%
    mutate(
      hsr_pm   = hsr    / duration,
      vhsr_pm  = vhsr   / duration,
      accel_pm = accels / duration,
      mpm_sess = dist   / duration
    )

  # Squad-level median m/min → used to estimate drill work duration
  wara_dp_squad_mpm <<- median(dp_base$mpm_sess, na.rm = TRUE)

  POS_ABREV_ORDER <- c("FR","HK","SR","BR","IB","OB")

  # Per player: avg and 90th-pct peak rates per minute
  wara_dp_player <<- dp_base %>%
    group_by(athlete_name, position_name, pos_abrev, group) %>%
    summarise(
      n_sessions    = n(),
      avg_mpm       = mean(mpm_sess, na.rm = TRUE),
      avg_hsr_pm    = mean(hsr_pm,   na.rm = TRUE),
      avg_vhsr_pm   = mean(vhsr_pm,  na.rm = TRUE),
      avg_accel_pm  = mean(accel_pm, na.rm = TRUE),
      peak_hsr_pm   = quantile(hsr_pm,   0.90, na.rm = TRUE),
      peak_vhsr_pm  = quantile(vhsr_pm,  0.90, na.rm = TRUE),
      peak_accel_pm = quantile(accel_pm, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_sessions >= 3)   # need at least 3 sessions for a reliable rate

  # Per position (old — kept for backward compatibility)
  wara_dp_pos <<- dp_base %>%
    filter(position_name != "Unknown", nchar(trimws(position_name)) > 0) %>%
    group_by(position_name, group) %>%
    summarise(
      avg_mpm       = mean(mpm_sess, na.rm = TRUE),
      avg_hsr_pm    = mean(hsr_pm,   na.rm = TRUE),
      avg_vhsr_pm   = mean(vhsr_pm,  na.rm = TRUE),
      avg_accel_pm  = mean(accel_pm, na.rm = TRUE),
      peak_hsr_pm   = quantile(hsr_pm,   0.90, na.rm = TRUE),
      peak_vhsr_pm  = quantile(vhsr_pm,  0.90, na.rm = TRUE),
      peak_accel_pm = quantile(accel_pm, 0.90, na.rm = TRUE),
      .groups = "drop"
    )

  # Per position abbreviation (FR/HK/SR/BR/IB/OB) — used by Drill Predictor
  wara_dp_pos_abrev <<- dp_base %>%
    filter(pos_abrev %in% POS_ABREV_ORDER) %>%
    group_by(pos_abrev) %>%
    summarise(
      avg_mpm       = mean(mpm_sess, na.rm = TRUE),
      avg_hsr_pm    = mean(hsr_pm,   na.rm = TRUE),
      avg_vhsr_pm   = mean(vhsr_pm,  na.rm = TRUE),
      avg_accel_pm  = mean(accel_pm, na.rm = TRUE),
      peak_hsr_pm   = quantile(hsr_pm,   0.90, na.rm = TRUE),
      peak_vhsr_pm  = quantile(vhsr_pm,  0.90, na.rm = TRUE),
      peak_accel_pm = quantile(accel_pm, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(data.frame(pos_abrev=POS_ABREV_ORDER, stringsAsFactors=FALSE), by="pos_abrev") %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    arrange(match(pos_abrev, POS_ABREV_ORDER))

  # Per group (FWD / BKS)
  wara_dp_group <<- dp_base %>%
    filter(group %in% c("FWD", "BKS")) %>%
    group_by(group) %>%
    summarise(
      avg_mpm       = mean(mpm_sess, na.rm = TRUE),
      avg_hsr_pm    = mean(hsr_pm,   na.rm = TRUE),
      avg_vhsr_pm   = mean(vhsr_pm,  na.rm = TRUE),
      avg_accel_pm  = mean(accel_pm, na.rm = TRUE),
      peak_hsr_pm   = quantile(hsr_pm,   0.90, na.rm = TRUE),
      peak_vhsr_pm  = quantile(vhsr_pm,  0.90, na.rm = TRUE),
      peak_accel_pm = quantile(accel_pm, 0.90, na.rm = TRUE),
      .groups = "drop"
    )

})

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(skin = "blue",dashboardHeader(title = tags$span(style="display:flex;align-items:center;gap:8px;",
                                                                    tags$img(src="Waratahs_Logo.png", height="32px", style="vertical-align:middle;"),
                                                                    tags$span("Waratahs W", style="color:#FFFFFF;font-weight:700;font-size:18px;vertical-align:middle;"))),
                    dashboardSidebar(sidebarMenu(id="sidebar",
                                                 menuItem("By Day",       tabName="by_day",       icon=icon("calendar-day")),
                                                 menuItem("By Week",      tabName="by_week",      icon=icon("chart-line")),
                                                 menuItem("By Player",    tabName="by_player",    icon=icon("user")),
                                                 menuItem("HML Distance", tabName="hml",          icon=icon("fire")),
                                                 menuItem("Predictions",     tabName="predictions",  icon=icon("bullseye")),
                                                 menuItem("Drill Predictor", tabName="drill_pred",   icon=icon("drafting-compass")),
                                                 menuItem("Loads",           tabName="loads",        icon=icon("weight-hanging")),
                                                 menuItem("All Data",     tabName="all_data",     icon=icon("table")))),
                    dashboardBody(tags$head(tags$style(HTML(custom_css)), tags$style(HTML(fwd_back_css))),
                                  tabItems(
                                    tabItem(tabName="by_day",
                                            fluidRow(column(12, div(class="section-header","Session Selector"))),
                                            fluidRow(
                                              column(8, selectizeInput("selected_date",label=NULL,choices=NULL,width="100%",
                                                                       options=list(sortField=list(list(field="$order",direction="asc"))))),
                                              column(4, div(style="padding-top:4px;",
                                                            actionButton("refresh_btn","Refresh Data",
                                                                         style=paste0("background:",SKY,";color:white;border:none;border-radius:6px;font-weight:700;padding:7px 18px;"))))),
                                            uiOutput("peaks_banner"),
                                            uiOutput("session_banner"), uiOutput("player_box"),
                                            div(class="section-header","Team Session Averages"), uiOutput("kpi_cards"),
                                            div(class="section-header","Drill Breakdown - Metrics per Drill"), uiOutput("drill_charts_ui"),
                                            fluidRow(column(12, div(class="chart-card",
                                                                    div(class="chart-card-title","Forward vs Back Comparison - All Drills"),
                                                                    plotOutput("fwd_back_radar",height="320px")))),
                                            fluidRow(column(12, div(class="chart-card",
                                                                    div(class="chart-card-title","Individual Player Summary (Session)"),
                                                                    DTOutput("player_table")))),
                                            tags$br()
                                    ),
                                    
                                    tabItem(tabName="by_player",
                                            fluidRow(column(12, div(class="section-header","By Player"))),
                                            tabsetPanel(id="by_player_inner", type="tabs",
                                                        tabPanel("Player Profile", tags$br(),
                                                                 fluidRow(
                                                                   column(4, tags$label("Select Player:"), selectizeInput("sel_player",label=NULL,choices=NULL,width="100%",options=list(placeholder="Search or scroll for a player…",allowEmptyOption=FALSE))),
                                                                   column(4, tags$label("Show Sessions:"), selectInput("player_season_filter", label=NULL,
                                                                                                                       choices=c("Preseason Only"="preseason",
                                                                                                                                 "In-Season Only"="inseason",
                                                                                                                                 "Both"="both"),
                                                                                                                       selected="both", width="100%"))
                                                                 ),
                                                                 uiOutput("player_banner"),
                                                                 div(class="section-header","Weekly Load Trends"),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Average Distance per Week - by Session Type"),
                                                                                         plotOutput("player_dist_plot",height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","High Speed Distance per Week"),      plotOutput("player_hsd_plot",   height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Very High Speed Distance per Week"), plotOutput("player_vhsd_plot",  height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Meterage per Minute per Week"),      plotOutput("player_mpm_plot",   height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","HML Distance per Week"),              plotOutput("player_hml_plot",   height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Max Velocity per Session"),           plotOutput("player_maxvel_plot",height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Accel Counts per Week"),              plotOutput("player_accels_plot",height="260px")))),
                                                                 tags$br()
                                                        ), # end tabPanel("Player Profile")
                                                        
                                                        tabPanel("Players at a Glance", tags$br(),
                                                                 fluidRow(column(12,
                                                                                 div(class="chart-card",
                                                                                     div(class="chart-card-title", "All Players — Season Snapshot"),
                                                                                     tags$p(style="font-size:11px;color:#8899AA;margin-bottom:10px;",
                                                                                            HTML(paste0(
                                                                                              "<span style='color:#001F4E;font-weight:700;'>Dark navy</span> = '25 averages &nbsp;|&nbsp; ",
                                                                                              "<span style='color:#003A7A;font-weight:700;'>Medium navy</span> = '25 peaks &nbsp;|&nbsp; ",
                                                                                              "<span style='color:#007BB5;font-weight:700;'>Medium sky</span> = '26 averages &nbsp;|&nbsp; ",
                                                                                              "<span style='color:#009FDF;font-weight:700;'>Bright sky</span> = '26 peaks &nbsp;&nbsp;",
                                                                                              "'26 values update after each training upload. Click any column to sort."
                                                                                            ))),
                                                                                     DTOutput("players_glance_table")
                                                                                 )
                                                                 )),
                                                                 tags$br(),
                                                                 div(class="section-header","Group & Position Benchmarks"),
                                                                 fluidRow(column(12,
                                                                                 div(class="chart-card",
                                                                                     div(class="chart-card-title", "Forwards / Backs / Positions — Season Benchmarks"),
                                                                                     tags$p(style="font-size:11px;color:#8899AA;margin-bottom:10px;",
                                                                                            HTML(paste0(
                                                                                              "These are the benchmark values used in Predictions when a group/position goal type is selected. ",
                                                                                              "Peaks = highest individual peak in the group. Averages = mean of individual averages across the group. ",
                                                                                              "<span style='color:#001F4E;font-weight:700;'>Dark navy</span> = '25 avg &nbsp;|&nbsp; ",
                                                                                              "<span style='color:#003A7A;font-weight:700;'>Medium navy</span> = '25 peak &nbsp;|&nbsp; ",
                                                                                              "<span style='color:#007BB5;font-weight:700;'>Medium sky</span> = '26 avg &nbsp;|&nbsp; ",
                                                                                              "<span style='color:#009FDF;font-weight:700;'>Bright sky</span> = '26 peak"
                                                                                            ))),
                                                                                     DTOutput("group_summary_table")
                                                                                 )
                                                                 )),
                                                                 tags$br()
                                                        ), # end tabPanel("Players at a Glance")
                                                        
                                                        tabPanel("This Season", tags$br(),
                                                                 fluidRow(
                                                                   column(4, selectInput("ts_season", label="Show sessions:",
                                                                                         choices=c("Preseason Only"="preseason",
                                                                                                   "In-Season Only"="inseason",
                                                                                                   "Both"="both"),
                                                                                         selected="both", width="100%")),
                                                                   column(4, selectInput("ts_group", label="Filter by group:",
                                                                                         choices=c("All Players"="all","Forwards Only"="forward","Backs Only"="back",
                                                                                                   "Props"="pos_1","Hookers"="pos_2","Locks"="pos_3",
                                                                                                   "Backrow"="pos_4","Halfbacks"="pos_5","Outside Backs"="pos_6"),
                                                                                         selected="all", width="100%")),
                                                                   column(4, selectInput("ts_metric", label="Ranking metric:",
                                                                                         choices=c("Distance (m)"                 = "Average Distance (Session)",
                                                                                                   "Player Load"                  = "Average Player Load (Session)",
                                                                                                   "HML Distance (m)"             = "High Metabolic Load Distance",
                                                                                                   "High Speed Distance (m)"      = "High Speed Distance",
                                                                                                   "Very High Speed Distance (m)" = "Velocity Band 6 Average Distance (Session)",
                                                                                                   "Max Velocity (m/s)"           = "Maximum Velocity",
                                                                                                   "Metres per Minute"            = "Meterage Per Minute"),
                                                                                         width="100%"))
                                                                 ),
                                                                 uiOutput("ts_kpi_row"),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Player Rankings — sorted by selected metric above"),
                                                                                         plotOutput("ts_rank_plot", height="460px")))),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Full Player Metrics Table — click any column header to sort"),
                                                                                         DTOutput("ts_table")))),
                                                                 tags$br()
                                                        ) # end tabPanel("This Season")
                                                        
                                            ) # end tabsetPanel by_player_inner
                                    ),       # end tabItem by_player
                                    
                                    
                                    tabItem(tabName="by_week",
                                            fluidRow(column(12, div(class="section-header","By Week"))),
                                            tabsetPanel(id="by_week_inner", type="tabs",
                                                        
                                                        # ---- Individual Week ----
                                                        tabPanel("Individual Week", tags$br(),
                                                                 fluidRow(
                                                                   column(4, selectInput("indiv_week_sel", label="Select week:", choices=NULL, width="100%")),
                                                                   column(4, selectInput("indiv_week_by",  label="View by:",
                                                                                         choices=c("Forwards & Backs"="group","By Position"="position"),
                                                                                         width="100%")),
                                                                   conditionalPanel(
                                                                     condition="input.indiv_week_by == 'position'",
                                                                     column(4, selectizeInput("iw_pos_filter", label="Select positions:",
                                                                                              choices=NULL, multiple=TRUE, width="100%",
                                                                                              options=list(placeholder="Choose positions...", plugins=list("remove_button")))))
                                                                 ),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Average Distance by Session"),
                                                                                         plotOutput("indiv_dist_plot", height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Accel Count by Session"),              plotOutput("indiv_accel_plot",  height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","HML Distance by Session"),             plotOutput("indiv_hml_plot",    height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","High Speed Distance by Session"),      plotOutput("indiv_hsd_plot",    height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Very High Speed Distance by Session"), plotOutput("indiv_vhsd_plot",   height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Max Velocity by Session"),             plotOutput("indiv_maxvel_plot", height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Meterage per Minute by Session"),      plotOutput("indiv_mmin_plot",   height="260px")))),
                                                                 tags$br()
                                                        ),
                                                        # ---- Comparing Weeks - Average ----
                                                        tabPanel("Comparing Weeks - Average", tags$br(),
                                                                 fluidRow(
                                                                   column(4, selectInput("week_compare_by", label="View by:",
                                                                                         choices=c("Forwards & Backs"="group","By Position"="position"),
                                                                                         width="100%")),
                                                                   conditionalPanel(
                                                                     condition="input.week_compare_by == 'position'",
                                                                     column(8, selectizeInput("cw_avg_pos_filter", label="Select positions:",
                                                                                              choices=NULL, multiple=TRUE, width="100%",
                                                                                              options=list(placeholder="Choose positions...", plugins=list("remove_button")))))
                                                                 ),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Distance per Week"),
                                                                                         plotOutput("week_dist_plot", height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Player Load per Week"),              plotOutput("week_load_plot",   height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","HML Distance per Week"),             plotOutput("week_hml_plot",    height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","High Speed Distance per Week"),      plotOutput("week_hsd_plot",    height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Very High Speed Distance per Week"), plotOutput("week_vhsd_plot",   height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Max Velocity per Week"),             plotOutput("week_maxvel_plot", height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Meterage per Minute per Week"),      plotOutput("week_mmin_plot",   height="260px")))),
                                                                 tags$br()
                                                        ),
                                                        
                                                        # ---- Comparing Weeks - Totals ----
                                                        tabPanel("Comparing Weeks - Totals", tags$br(),
                                                                 fluidRow(column(12,
                                                                                 div(class="totals-notice", onclick="Shiny.setInputValue('show_week_sessions_modal', Math.random())",
                                                                                     span(class="totals-notice-icon", "\u26a0\ufe0f"),
                                                                                     span(class="totals-notice-text", "Please keep in mind that not all weeks have the same number of training days."),
                                                                                     span(class="totals-notice-link", "Click to see team training days per week \u2192"))
                                                                 )),
                                                                 fluidRow(
                                                                   column(4, selectInput("week_total_by", label="View by:",
                                                                                         choices=c("Forwards & Backs"="group","By Position"="position"),
                                                                                         width="100%")),
                                                                   conditionalPanel(
                                                                     condition="input.week_total_by == 'position'",
                                                                     column(8, selectizeInput("cw_tot_pos_filter", label="Select positions:",
                                                                                              choices=NULL, multiple=TRUE, width="100%",
                                                                                              options=list(placeholder="Choose positions...", plugins=list("remove_button")))))
                                                                 ),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Total Distance per Week"),
                                                                                         plotOutput("week_tot_dist_plot", height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total Player Load per Week"),              plotOutput("week_tot_load_plot",   height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total HML Distance per Week"),             plotOutput("week_tot_hml_plot",    height="260px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total High Speed Distance per Week"),      plotOutput("week_tot_hsd_plot",    height="260px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total Very High Speed Distance per Week"), plotOutput("week_tot_vhsd_plot",   height="260px")))),
                                                                 tags$br()
                                                        ),
                                                        
                                                        # ---- Comparing Athletes - Averages ----
                                                        tabPanel("Comparing Athletes - Averages", tags$br(),
                                                                 fluidRow(
                                                                   column(4, selectInput("ath_avg_filter", label="Filter players:",
                                                                                         choices=NULL, width="100%")),
                                                                   column(8, selectizeInput("ath_avg_custom", label="Select players:",
                                                                                            choices=NULL, multiple=TRUE, width="100%",
                                                                                            options=list(placeholder="Choose players...", plugins=list("remove_button"))))
                                                                 ),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Distance per Week"),
                                                                                         plotlyOutput("ath_avg_dist_plot", height="320px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Player Load per Week"),
                                                                                 plotlyOutput("ath_avg_load_plot", height="280px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","HML Distance per Week"),
                                                                                 plotlyOutput("ath_avg_hml_plot", height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","High Speed Distance per Week"),
                                                                                 plotlyOutput("ath_avg_hsd_plot", height="280px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Very High Speed Distance per Week"),
                                                                                 plotlyOutput("ath_avg_vhsd_plot", height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Max Velocity per Week"),
                                                                                 plotlyOutput("ath_avg_maxvel_plot", height="280px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Meterage per Minute per Week"),
                                                                                 plotlyOutput("ath_avg_mmin_plot", height="280px")))),
                                                                 tags$br()
                                                        ),
                                                        
                                                        # ---- Comparing Athletes - Totals ----
                                                        tabPanel("Comparing Athletes - Totals", tags$br(),
                                                                 fluidRow(column(12,
                                                                                 div(class="totals-notice", onclick="Shiny.setInputValue('show_ath_sessions_modal', Math.random())",
                                                                                     span(class="totals-notice-icon", "\u26a0\ufe0f"),
                                                                                     span(class="totals-notice-text", "Please keep in mind that not all athletes attended the same number of trainings each week."),
                                                                                     span(class="totals-notice-link", "Click to see training attendance per athlete \u2192"))
                                                                 )),
                                                                 fluidRow(
                                                                   column(4, selectInput("ath_tot_filter", label="Filter players:",
                                                                                         choices=NULL, width="100%")),
                                                                   column(8, selectizeInput("ath_tot_custom", label="Select players:",
                                                                                            choices=NULL, multiple=TRUE, width="100%",
                                                                                            options=list(placeholder="Choose players...", plugins=list("remove_button"))))
                                                                 ),
                                                                 fluidRow(column(12, div(class="chart-card",
                                                                                         div(class="chart-card-title","Total Distance per Week"),
                                                                                         plotlyOutput("ath_tot_dist_plot", height="320px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total Player Load per Week"),
                                                                                 plotlyOutput("ath_tot_load_plot", height="280px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total HML Distance per Week"),
                                                                                 plotlyOutput("ath_tot_hml_plot", height="280px")))),
                                                                 fluidRow(
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total High Speed Distance per Week"),
                                                                                 plotlyOutput("ath_tot_hsd_plot", height="280px"))),
                                                                   column(6, div(class="chart-card", div(class="chart-card-title","Total Very High Speed Distance per Week"),
                                                                                 plotlyOutput("ath_tot_vhsd_plot", height="280px")))),
                                                                 tags$br()
                                                        )
                                            )
                                    ),
                                    
                                    tabItem(tabName="hml",
                                            fluidRow(column(12, div(class="section-header","High Metabolic Load Distance"))),
                                            uiOutput("hml_kpi_row"),
                                            div(class="hml-tabs",
                                                tabsetPanel(id="hml_inner_tabs",type="tabs",
                                                            tabPanel("Session", tags$br(),
                                                                     fluidRow(column(12, div(class="chart-card",
                                                                                             div(class="chart-card-title","Team Average HML per Session - Forwards vs Backs"),
                                                                                             plotOutput("hml_trend_plot",height="300px")))),
                                                                     fluidRow(column(12, div(class="chart-card",
                                                                                             div(class="chart-card-title","Average HML per Week - Forwards vs Backs"),
                                                                                             plotOutput("hml_week_plot",height="300px")))),
                                                                     fluidRow(column(12, div(class="chart-card",
                                                                                             fluidRow(
                                                                                               column(6, div(class="chart-card-title","Preseason Average Breakdown")),
                                                                                               column(6, selectInput("hml_breakdown_view", label=NULL,
                                                                                                                     choices=c("By Group"="group","By Position"="position"), width="100%"))),
                                                                                             uiOutput("hml_breakdown_plot_ui")))),),
                                                            tabPanel("Players", tags$br(),
                                                                     fluidRow(column(12, div(class="chart-card",
                                                                                             div(class="chart-card-title","Season Average HML per Player - ranked high to low"),
                                                                                             plotOutput("hml_player_bar_plot",height="520px")))),
                                                                     fluidRow(column(12, div(class="chart-card",
                                                                                             fluidRow(
                                                                                               column(6, div(class="chart-card-title","Individual Sessions - each dot = one session")),
                                                                                               column(4, selectInput("hml_dot_season", label=NULL,
                                                                                                                     choices=c("Preseason Only"="preseason",
                                                                                                                               "In-Season Only"="inseason",
                                                                                                                               "Both"="both"),
                                                                                                                     selected="both", width="100%"))
                                                                                             ),
                                                                                             plotOutput("hml_player_dot_plot",height="520px"))))),
                                                            tabPanel("Drills", tags$br(),
                                                                     fluidRow(column(12, div(class="chart-card",
                                                                                             div(class="chart-card-title","HML by Drill — Averages & % Contribution"),
                                                                                             DTOutput("hml_drill_table")))))
                                                )
                                            ),
                                            tags$br()
                                    ),
                                    
                                    
                                    
                                    tabItem(tabName="predictions",
                                            div(class="hml-tabs",
                                                tabsetPanel(id="pred_inner_tabs", type="tabs",
                                                            
                                                            # ---- Predictions & Goals ----
                                                            tabPanel("Predictions & Goals", tags$br(),
                                                                     div(class="chart-card", style="margin-bottom:18px;padding:16px 20px;",
                                                                         div(class="chart-card-title", "Prediction Settings"),
                                                                         fluidRow(
                                                                           column(3, dateInput("pred_start_date", "Start date:", value=Sys.Date(), width="100%")),
                                                                           column(3, dateInput("pred_goal_date",  "Goal date:", value=as.Date("2026-05-04"), width="100%")),
                                                                           column(3, selectInput("pred_goal_type", "Goal type:",
                                                                                                 choices=c(
                                                                                                   "'25 Individual Averages"    = "25_avg",
                                                                                                   "'25 Individual Peaks"       = "25_peaks",
                                                                                                   "'25 Group Averages (Fwd/Back)" = "25_group_avg",
                                                                                                   "'25 Group Peaks (Fwd/Back)"   = "25_group_peak",
                                                                                                   "'25 Position Averages"      = "25_pos_avg",
                                                                                                   "'25 Position Peaks"         = "25_pos_peak",
                                                                                                   "'26 Individual Peaks"       = "26_peaks",
                                                                                                   "'26 Group Peaks (Fwd/Back)" = "26_group_peak",
                                                                                                   "'26 Position Peaks"         = "26_pos_peak"
                                                                                                 ), selected="25_peaks", width="100%")),
                                                                           column(3, numericInput("pred_max_prog", "Max progression/week:", value=1.2, min=1.0, max=3.0, step=0.05, width="100%"))
                                                                         ),
                                                                         div(style="font-size:11px;color:#8899AA;margin-top:8px;",
                                                                             "Predictions never decrease. ",
                                                                             tags$span(style="margin-left:10px;",
                                                                                       tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#FFE0B2;border:1px solid #BF360C;"), " Adjusted (capped at max rate)  ",
                                                                                       tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#C8E6C9;border:1px solid #1B5E20;"), " On Track  ",
                                                                                       tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#BBDEFB;border:1px solid #0D47A1;"), " Already Exceeded"))
                                                                     ),
                                                                     fluidRow(
                                                                       column(5, selectizeInput("pred_search", label="Search player (jumps to top):",
                                                                                                choices=NULL, selected=NULL, multiple=FALSE, width="100%",
                                                                                                options=list(placeholder="Type a name...", allowEmptyOption=TRUE))),
                                                                       column(7, uiOutput("pred_settings_label"))
                                                                     ),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         uiOutput("pred_goals_table_title"),
                                                                                         div(style="font-size:11px;color:#8899AA;margin-bottom:10px;",
                                                                                             "Predicted value for each player at the goal date. Score = % of known metrics on track or exceeded."),
                                                                                         DTOutput("pred_goals_table")
                                                                                     )
                                                                     )),
                                                                     tags$br(),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         div(class="chart-card-title", "Predicted Values by Week"),
                                                                                         div(style="font-size:11px;color:#8899AA;margin-bottom:12px;",
                                                                                             "Select a predicted week checkpoint to see values for every player."),
                                                                                         fluidRow(
                                                                                           column(4, uiOutput("pred_week_select_ui"))
                                                                                         ),
                                                                                         tags$br(),
                                                                                         DTOutput("pred_week_table")
                                                                                     )
                                                                     )),
                                                                     uiOutput("pred_goals_detail")
                                                            ),
                                                            
                                                            # ---- Recovering Athletes ----
                                                            tabPanel("Recovering Athletes", tags$br(),
                                                                     fluidRow(
                                                                       column(4, selectInput("recover_athlete", label="Select Athlete:",
                                                                                             choices=c("Leilani Nathan"), width="100%"))
                                                                     ),
                                                                     tags$br(),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         div(class="chart-card-title", "Season Comparison \u2014 Current vs Last Season"),
                                                                                         tags$div(style="display:flex;gap:18px;align-items:center;margin-bottom:14px;flex-wrap:wrap;",
                                                                                                  tags$span(style="display:inline-flex;align-items:center;gap:6px;font-size:12px;font-weight:600;color:#001F4E;",
                                                                                                            tags$span(style="display:inline-block;width:14px;height:14px;border-radius:3px;background:#BBDEFB;border:1px solid #0D47A1;"),
                                                                                                            "Current max exceeds last season max"),
                                                                                                  tags$span(style="display:inline-flex;align-items:center;gap:6px;font-size:12px;font-weight:600;color:#001F4E;",
                                                                                                            tags$span(style="display:inline-block;width:14px;height:14px;border-radius:3px;background:#C8E6C9;border:1px solid #1B5E20;"),
                                                                                                            "Current max exceeds last season average"),
                                                                                                  tags$span(style="display:inline-flex;align-items:center;gap:6px;font-size:12px;font-weight:600;color:#001F4E;",
                                                                                                            tags$span(style="display:inline-block;width:14px;height:14px;border-radius:3px;background:#FFCDD2;border:1px solid #B71C1C;"),
                                                                                                            "Current max below last season average")
                                                                                         ),
                                                                                         DTOutput("recover_table")
                                                                                     )
                                                                     )),
                                                                     tags$br(),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         div(class="chart-card-title", "6-Week Prediction Trajectory to May 25"),
                                                                                         tags$p(style="font-size:11px;color:#8899AA;margin-bottom:14px;",
                                                                                                paste0("Weekly targets to last season\u2019s max by May 25. Orange = goal adjusted (", MAX_PROG_RATE, "\u00d7/wk cap).")),
                                                                                         DTOutput("recover_pred_table")
                                                                                     )
                                                                     ))
                                                            ),
                                                            
                                                            # ---- On Track? ----
                                                            tabPanel("On Track?", tags$br(),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card", style="margin-bottom:14px;padding:14px 18px;",
                                                                                         div(class="chart-card-title","Goal Settings"),
                                                                                         fluidRow(
                                                                                           column(6, selectInput("ontrack_goal_type", "Compare players against:",
                                                                                                                 choices=c(
                                                                                                                   "'25 Individual Averages"    = "25_avg",
                                                                                                                   "'25 Individual Peaks"       = "25_peaks",
                                                                                                                   "'25 Group Averages (Fwd/Back)" = "25_group_avg",
                                                                                                                   "'25 Group Peaks (Fwd/Back)"   = "25_group_peak",
                                                                                                                   "'25 Position Averages"      = "25_pos_avg",
                                                                                                                   "'25 Position Peaks"         = "25_pos_peak",
                                                                                                                   "'26 Individual Peaks"       = "26_peaks",
                                                                                                                   "'26 Group Peaks (Fwd/Back)" = "26_group_peak",
                                                                                                                   "'26 Position Peaks"         = "26_pos_peak"
                                                                                                                 ), selected="25_peaks", width="100%"))
# ============================================================
#  Renée Sakura Steiner
# ============================================================
                                                                                           
                                                                                         )))),
                                                                     uiOutput("on_track_boxes"),
                                                                     tags$br(),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         div(class="chart-card-title", "Goal Progress Ranking"),
                                                                                         fluidRow(
                                                                                           column(4, selectInput("ranking_view",
                                                                                                                 label = "Compare using:",
                                                                                                                 choices = c(
                                                                                                                   "Latest Training Session" = "latest",
                                                                                                                   "Latest Week Average"     = "latest_week",
                                                                                                                   "Season Average"          = "season_avg",
                                                                                                                   "Season Best Session"     = "season_max"
                                                                                                                 ),
                                                                                                                 selected = "latest", width = "100%")),
                                                                                           column(8, uiOutput("ranking_date_label"))
                                                                                         ),
                                                                                         tags$p(style="font-size:11px;color:#8899AA;margin-bottom:10px;",
                                                                                                HTML(paste0(
                                                                                                  '<span style="color:#1E40AF;">&#9679;</span> Exceeded last season max &nbsp; ',
                                                                                                  '<span style="color:#166534;">&#9679;</span> Above last season avg &nbsp; ',
                                                                                                  '<span style="color:#B91C1C;">&#9679;</span> Below last season avg &nbsp; ',
                                                                                                  '<span style="color:#9CA3AF;">&#9679;</span> No data &nbsp;&nbsp; ',
                                                                                                  '* = goal set to position average (no 25-season data)'
                                                                                                ))
                                                                                         ),
                                                                                         DTOutput("on_track_ranking")
                                                                                     )
                                                                     )),
                                                                     tags$br(),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         div(class="chart-card-title", "Weekly Progress Alerts"),
                                                                                         tags$p(style="font-size:11px;color:#8899AA;margin-bottom:8px;",
                                                                                                HTML(paste0(
                                                                                                  '<span style="background:#FEF3C7;color:#92400E;padding:1px 6px;border-radius:3px;font-size:11px;font-weight:600;">Too Fast</span> &gt;30% rise week-over-week &nbsp;&nbsp; ',
                                                                                                  '<span style="background:#FEE2E2;color:#991B1B;padding:1px 6px;border-radius:3px;font-size:11px;font-weight:600;">Decrease</span> &gt;20% drop week-over-week'
                                                                                                ))
                                                                                         ),
                                                                                         uiOutput("alerts_week_info"),
                                                                                         uiOutput("alerts_watch_box"),
                                                                                         DTOutput("alerts_table")
                                                                                     )
                                                                     ))
                                                            ),
                                                            
                                                            # ---- Progression Comparison ----
                                                            tabPanel("Progression Comparison", tags$br(),
                                                                     fluidRow(
                                                                       column(4, selectizeInput("prog25_player", label="Select player:",
                                                                                                choices=NULL, selected=NULL, multiple=FALSE, width="100%",
                                                                                                options=list(placeholder="Search name...", allowEmptyOption=TRUE))),
                                                                       column(4, selectInput("prog25_metric", label="Metric:",
                                                                                             choices=c("Average Distance","Average Player Load","HML Distance",
                                                                                                       "High Speed Distance","Very High Speed Distance",
                                                                                                       "Maximum Velocity","Max Acceleration","Acceleration Efforts"),
                                                                                             width="100%"))
                                                                     ),
                                                                     fluidRow(column(12,
                                                                                     div(class="chart-card",
                                                                                         div(class="chart-card-title", "2025 vs 2026 Season Comparison — By Week"),
                                                                                         tags$p(style="font-size:11px;color:#8899AA;margin-bottom:10px;",
                                                                                                "Solid line = 2025 season. Dashed line = 2026 season. Axes aligned by week-in-season."),
                                                                                         plotlyOutput("prog25_plot", height="480px")
                                                                                     )
                                                                     ))
                                                            ),

                                                )
                                            ),
                                            tags$br()
                                    ),

                                    # ---- Drill Predictor ----
                                    tabItem(tabName="drill_pred",
                                      fluidRow(column(12, div(class="section-header","Drill Load Predictor"))),
                                      # ---- Inputs ----
                                      div(class="chart-card",
                                        # Row 1: sets | reps | rpe | rest/rep | rest/set
                                        fluidRow(
                                          column(2, numericInput("dp_sets", label=tags$span(style="font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;","Sets"),
                                                                 value=3, min=1, step=1, width="100%")),
                                          column(2, numericInput("dp_reps", label=tags$span(style="font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;","Reps"),
                                                                 value=5, min=1, step=1, width="100%")),
                                          column(2, numericInput("dp_rpe",  label=tags$span(style="font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;",
                                                                                            "RPE (optional, 1–10)"),
                                                                 value=NA, min=1, max=10, step=1, width="100%")),
                                          column(1, numericInput("dp_rest_rep", label=tags$span(style="font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;",
                                                                                                "Rest/Rep (sec)"),
                                                                 value=NA, min=0, step=5, width="100%")),
                                          column(1, numericInput("dp_rest_set", label=tags$span(style="font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;",
                                                                                                "Rest/Set (sec)"),
                                                                 value=NA, min=0, step=5, width="100%"))
                                        ),
                                        # Row 2: distance mode
                                        fluidRow(
                                          column(4, selectInput("dp_dist_mode",
                                                                label=tags$span(style="font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;","Distance Mode"),
                                                                choices=c("By Position"="position","By Group (Fwd/Back)"="group","Whole Team"="team"),
                                                                selected="position", width="100%"))
                                        ),
                                        # Row 3: dynamic distance inputs
                                        uiOutput("dp_dist_inputs_ui"),
                                        tags$p(style="font-size:10px;color:#8899AA;margin-top:8px;margin-bottom:0;",
                                               HTML(paste0(
                                                 "<b>Total Distance</b> = Distance × Reps × Sets &nbsp;|&nbsp; ",
                                                 "<b>Total Rest</b> = (Rest/Rep × Reps × Sets) + (Rest/Set × Sets) &nbsp;|&nbsp; ",
                                                 "<b>No RPE</b> = historical average rates &nbsp;|&nbsp; ",
                                                 "<b>RPE 1</b> = 10% of peak &nbsp;|&nbsp; ",
                                                 "<b>RPE 5</b> ≈ average &nbsp;|&nbsp; ",
                                                 "<b>RPE 10</b> = peak (90th percentile)"
                                               )))
                                      ),
                                      # ---- Summary banner (reactive) ----
                                      uiOutput("dp_banner"),
                                      tags$br(),
                                      # ---- Results tabs ----
                                      div(class="hml-tabs",
                                        tabsetPanel(id="dp_tabs", type="tabs",
                                          tabPanel("By Position",
                                            tags$br(),
                                            div(class="chart-card", DTOutput("dp_pos_tbl"))
                                          ),
                                          tabPanel("By Player",
                                            tags$br(),
                                            div(class="chart-card", DTOutput("dp_player_tbl"))
                                          ),
                                          tabPanel("By Group",
                                            tags$br(),
                                            div(class="chart-card", DTOutput("dp_group_tbl"))
                                          )
                                        )
                                      ),
                                      tags$br()
                                    ),

                                    # ---- Loads ----
                                    tabItem(tabName="loads",
                                      fluidRow(column(12, div(class="section-header","Training Loads"))),
                                      div(class="hml-tabs",
                                        tabsetPanel(id="loads_inner_tabs", type="tabs",

                                          # ---- Individual ACWR tab ----
                                          tabPanel("Individual ACWR", tags$br(),
                                            fluidRow(
                                              column(3,
                                                selectInput("acwr_week_sel", label="Week:",
                                                            choices=NULL, selected=NULL, width="100%")
                                              ),
                                              column(9, uiOutput("acwr_alert_banner"))
                                            ),
                                            fluidRow(column(12,
                                              div(class="chart-card", style="margin-bottom:12px;padding:12px 20px;",
                                                radioButtons("acwr_method", label=NULL,
                                                  choices=c("EWMA — full history (standard)"="ewma",
                                                            "EWMA — 7-day window (rolling)"="windowed"),
                                                  selected="ewma", inline=TRUE),
                                                tags$p(style="font-size:11px;color:#8899AA;margin-bottom:0;",
                                                  HTML(paste0(
                                                    "<b>Full history</b>: EWMA decay considers all sessions ever (older sessions contribute less but are never fully excluded). ",
                                                    "<b>7-day window</b>: same EWMA calculation but with a hard cutoff — ",
                                                    "only the past 7 calendar days count toward acute load, past 28 days toward chronic. ",
                                                    "Sessions outside those windows have zero weight."
                                                  )))
                                              )
                                            )),
                                            tags$br(),
                                            fluidRow(column(12,
                                              div(class="chart-card",
                                                tags$p(style="font-size:11px;color:#8899AA;margin-bottom:8px;",
                                                  HTML(paste0(
                                                    '<span style="background:#DCFCE7;color:#166534;padding:1px 8px;border-radius:3px;font-weight:600;">0.8–1.3</span> Optimal &nbsp;&nbsp; ',
                                                    '<span style="background:#FEF3C7;color:#92400E;padding:1px 8px;border-radius:3px;font-weight:600;">1.3–1.5</span> Caution &nbsp;&nbsp; ',
                                                    '<span style="background:#FEE2E2;color:#991B1B;padding:1px 8px;border-radius:3px;font-weight:600;">&gt;1.5</span> High Risk &nbsp;&nbsp; ',
                                                    '<span style="background:#EFF6FF;color:#1E40AF;padding:1px 8px;border-radius:3px;font-weight:600;">&lt;0.8</span> Under-loaded'
                                                  ))
                                                ),
                                                DTOutput("acwr_week_table")
                                              )
                                            ))
                                          ),

                                          # ---- Waratah Analytics tab ----
                                          tabPanel("Waratah Analytics", tags$br(),
                                            fluidRow(
                                              column(4,
                                                selectInput("wara_view", label="View:",
                                                            choices=c(
                                                              "Weekly Volume"                    = "weekly_vol",
                                                              "Session Summary & Weekly Loads"   = "session_summary",
                                                              "Drill Summary"                    = "drill_summary",
                                                              "Match Report"                     = "match_report",
                                                              "Positional Acute & Chronic Loads" = "pos_ewma",
                                                              "Session Predictor (Squad)"        = "session_predictor",
                                                              "Session Predictor (By Position)"  = "pos_predictor"
                                                            ), width="100%")
                                              ),
                                              column(8, uiOutput("wara_desc"))
                                            ),
                                            uiOutput("wara_session_filters"),
                                            fluidRow(column(12,
                                              div(class="chart-card",
                                                DTOutput("wara_table")
                                              )
                                            ))
                                          )

                                        )
                                      ),
                                      tags$br()
                                    ),

                                    # ---- All Data ----
                                    tabItem(tabName="all_data",
                                            fluidRow(column(12, div(class="section-header","All Training Data — Raw View"))),
                                            div(class="hml-tabs",
                                              tabsetPanel(id="all_data_tabs", type="tabs",
                                                tabPanel("Session Data",
                                                  tags$br(),
                                                  fluidRow(column(12,
                                                    div(class="chart-card",
                                                        tags$p(style="font-size:11px;color:#8899AA;margin-bottom:12px;",
                                                               HTML(paste0(
                                                                 "Complete session and drill data sorted by date (newest first) then period number. ",
                                                                 "Use the column filters below each header to narrow down. ",
                                                                 "Period 0 = full session row."
                                                               ))),
                                                        DTOutput("all_data_table")
                                                    )
                                                  ))
                                                ),
                                                tabPanel("Weekly Data",
                                                  tags$br(),
                                                  fluidRow(column(12,
                                                    div(class="chart-card",
                                                        tags$p(style="font-size:11px;color:#8899AA;margin-bottom:12px;",
                                                               HTML(paste0(
                                                                 "Weekly aggregate totals per player, including external players on international duty ",
                                                                 "(highlighted in orange). Use the column filters to narrow down by player, week, or location."
                                                               ))),
                                                        DTOutput("weekly_data_table")
                                                    )
                                                  ))
                                                )
                                              )
                                            ),
                                            tags$br()
                                    )
                                    
                                  )
                    )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {
  
  all_data    <- reactive({ All_Data })
  players_w   <- reactive({ Players_W })
  weekly_data <- reactive({ Weekly_Data })
  
  # ---- External player marker (disabled \u2014 multiple external players now) ----
  EXTERNAL_PLAYERS <- character(0)
  ext_mark <- function(nms) nms
  
  # BY DAY ===================================================
  observe({
    df <- all_data()
    sessions <- df %>% filter(`Period Number`==0) %>%
      select(Date,Day,Type) %>% distinct() %>% arrange(desc(Date))
    if (nrow(sessions) == 0) return()
    choices <- setNames(as.character(sessions$Date),
                        paste0(format(sessions$Date,"%d %b %Y")," - ",
                               str_to_title(sessions$Day)," (",str_to_title(sessions$Type),")"))
    updateSelectInput(session,"selected_date",choices=choices,selected=choices[1])
  })
  
  selected_df    <- reactive({ req(input$selected_date); all_data() %>% filter(Date==as.Date(input$selected_date)) })
  session_info   <- reactive({ selected_df() %>% filter(`Period Number`==0) %>% slice(1) %>% select(Date,Day,Type,Week,Preseason) })
  session_rows   <- reactive({ selected_df() %>% filter(`Period Number`==0) })
  drill_rows     <- reactive({ selected_df() %>% filter(`Period Number`!=0) })
  session_with_pos <- reactive({ session_rows() })
  drill_with_pos   <- reactive({ drill_rows() })
  
  # ---- New Personal Bests banner (only on the most recent session) ----
  output$peaks_banner <- renderUI({
    req(input$selected_date)
    sel_date   <- as.Date(input$selected_date)
    latest_dt  <- max(all_data()$Date, na.rm = TRUE)
    if (is.na(sel_date) || sel_date != latest_dt) return(NULL)

    # curr: max per player for the most recent session (Period 0, sel_date)
    curr <- all_data() %>% filter(`Period Number` == 0, Date == sel_date)
    if (nrow(curr) == 0) return(NULL)

    # hist: all Period 0 sessions this season EXCLUDING the current date
    hist <- all_data() %>% filter(`Period Number` == 0, Date < sel_date)
    req(nrow(hist) > 0)

    peak_metrics <- list(
      list(col="Maximum Velocity",                                          label="Max Velocity",    unit="m/s",  digits=2, icon="⚡"),
      list(col="Max Acceleration",                                          label="Max Acceleration",unit="m/s²", digits=2, icon="🚀"),
      list(col="Acceleration B1-3 Average Efforts (Session) (Gen 2)",      label="Accel Count",     unit="",     digits=0, icon="💥")
    )

    new_peaks <- do.call(rbind, lapply(peak_metrics, function(m) {
      col <- m$col
      if (!col %in% names(curr) || !col %in% names(hist)) return(NULL)
      # historical best per player across all prior sessions
      hist_max <- hist %>%
        group_by(`Player Name`) %>%
        summarise(hist_best = max(suppressWarnings(as.numeric(.data[[col]])), na.rm=TRUE), .groups="drop") %>%
        filter(is.finite(hist_best))
      # current session: take max per player (handles duplicate Period 0 rows)
      curr_vals <- curr %>%
        group_by(`Player Name`) %>%
        summarise(cur_val = max(suppressWarnings(as.numeric(.data[[col]])), na.rm=TRUE), .groups="drop") %>%
        filter(is.finite(cur_val), cur_val > 0)
      joined <- curr_vals %>%
        inner_join(hist_max, by="Player Name") %>%
        filter(cur_val > hist_best)
      if (nrow(joined) == 0) return(NULL)
      joined %>% mutate(metric=m$label, unit=m$unit, digits=m$digits, icon=m$icon,
                        player=`Player Name`)
    }))

    if (is.null(new_peaks) || nrow(new_peaks) == 0) return(NULL)

    # Sort: velocity first, then by metric label, then player
    metric_order <- c("Max Velocity","Max Acceleration","Accel Count")
    new_peaks <- new_peaks %>%
      mutate(metric_rank = match(metric, metric_order)) %>%
      arrange(metric_rank, player)

    peak_cards <- lapply(seq_len(nrow(new_peaks)), function(i) {
      pk       <- new_peaks[i, ]
      val_str  <- fmt_num(pk$cur_val,  pk$digits)
      prev_str <- fmt_num(pk$hist_best, pk$digits)
      unit_str <- if (nzchar(trimws(pk$unit))) paste0(" ", pk$unit) else ""
      div(class="peak-card",
        div(class="peak-card-player",  paste0(pk$icon, " ", pk$player)),
        div(class="peak-card-metric",  pk$metric),
        div(class="peak-card-value",   paste0(val_str, unit_str)),
        div(class="peak-card-prev",    paste0("prev PB: ", prev_str, unit_str))
      )
    })

    n_peaks  <- nrow(new_peaks)
    n_players <- n_distinct(new_peaks$player)
    subtitle  <- paste0(n_peaks, " new PB", if(n_peaks!=1)"s" else "",
                        " across ", n_players, " player", if(n_players!=1)"s" else "")

    div(class="peaks-banner",
      div(class="peaks-banner-header",
        span("🎉", style="font-size:18px;"),
        div(
          div(class="peaks-banner-title", "New Personal Bests!"),
          div(style="color:rgba(255,255,255,0.65);font-size:11px;font-weight:600;margin-top:2px;", subtitle)
        )
      ),
      div(class="peaks-banner-cards", peak_cards)
    )
  })

  output$session_banner <- renderUI({
    info <- session_info(); req(nrow(info)>0)
    pre_html <- if (tolower(info$Preseason)=="yes") '<span class="type-badge badge-preseason">PRE-SEASON</span>' else ""
    div(class="session-banner",
        div(class="banner-item", div(class="banner-label","Date"),    div(class="banner-value",format(info$Date,"%d %B %Y"))),
        div(class="banner-divider"),
        div(class="banner-item", div(class="banner-label","Day"),     div(class="banner-value",str_to_title(info$Day))),
        div(class="banner-divider"),
        div(class="banner-item", div(class="banner-label","Week"),    div(class="banner-value",str_to_title(info$Week))),
        div(class="banner-divider"),
        div(class="banner-item", div(class="banner-label","Session Type"), HTML(type_badge_html(info$Type))),
        div(class="banner-divider"),
        div(class="banner-item", div(class="banner-label","Phase"),
            HTML(if (pre_html!="") pre_html else '<span class="type-badge badge-other">IN-SEASON</span>'))
    )
  })
  
  output$player_box <- renderUI({
    df <- session_with_pos()
    n_total <- n_distinct(df$`Player Name`)
    n_fwd   <- df %>% filter(tolower(Forward_Back)=="forward") %>% pull(`Player Name`) %>% n_distinct()
    n_bck   <- df %>% filter(tolower(Forward_Back)=="back")    %>% pull(`Player Name`) %>% n_distinct()
    div(class="player-box",
        div(class="player-stat",     div(class="ps-value",n_total), div(class="ps-label","Players Present")),
        div(class="player-box-divider"),
        div(class="player-stat fwd", div(class="ps-value",n_fwd),   div(class="ps-label","Forwards")),
        div(class="player-box-divider"),
        div(class="player-stat bck", div(class="ps-value",n_bck),   div(class="ps-label","Backs")))
  })
  
  kpi_data <- reactive({
    df <- session_with_pos()
    vhsd_col <- "Velocity Band 6 Average Distance (Session)"; dur_col <- "Average Duration (Session)"
    if (vhsd_col %in% names(df) && dur_col %in% names(df)) {
      dur_mins <- as.numeric(df[[dur_col]],units="secs")/60
      df$vhsd_per_min_kpi <- ifelse(dur_mins>0, df[[vhsd_col]]/dur_mins, NA_real_)
    } else { df$vhsd_per_min_kpi <- NA_real_ }
    hml_col <- "High Metabolic Load Distance"
    if (hml_col %in% names(df) && dur_col %in% names(df)) {
      dur_mins <- as.numeric(df[[dur_col]],units="secs")/60
      df$hml_per_min_kpi <- ifelse(dur_mins>0, df[[hml_col]]/dur_mins, NA_real_)
    } else { df$hml_per_min_kpi <- NA_real_ }
    metrics <- c("Average Distance (Session)","Meterage Per Minute","High Speed Distance",
                 "High Speed Distance Per Minute","Velocity Band 6 Average Distance (Session)","vhsd_per_min_kpi",
                 "High Metabolic Load Distance","hml_per_min_kpi",
                 "Acceleration B1-3 Average Efforts (Session) (Gen 2)")
    labels  <- c("Total Distance","Dist / Min","High Speed Dist","HSD / Min","Very High Speed Dist","VHSD / Min","HML Distance","HML / Min","Accel Count")
    units   <- c("m","m/min","m","m/min","m","m/min","m","m/min","efforts")
    accents <- c("","","accent-red","accent-red","accent-navy","accent-navy","accent-green","accent-green","")
    purrr::pmap(list(metrics,labels,units,accents), function(col,lbl,unit,acc) {
      if (!col %in% names(df)) return(NULL)
      list(label=lbl, value=mean(df[[col]],na.rm=TRUE), unit=unit,
           fwd=df%>%filter(tolower(Forward_Back)=="forward")%>%pull(col)%>%mean(na.rm=TRUE),
           bck=df%>%filter(tolower(Forward_Back)=="back")%>%pull(col)%>%mean(na.rm=TRUE), accent=acc)
    })
  })
  
  output$kpi_cards <- renderUI({
    kd <- kpi_data()
    cards <- lapply(kd, function(k) {
      if (is.null(k)) return(NULL)
      digs <- if (grepl("/",k$unit)) 1 else 0
      div(class=paste("kpi-card",k$accent),
          div(class="kpi-label",k$label), div(class="kpi-value",fmt_num(k$value,digs)),
          div(class="kpi-unit",k$unit),
          div(class="kpi-split",
              span(class="kpi-fwd",paste0("FWD ",fmt_num(k$fwd,digs))),
              span(style="color:#ccc;margin:0 4px;","."),
              span(class="kpi-bck",paste0("BCK ",fmt_num(k$bck,digs)))))
    })
    div(class="kpi-row",tagList(cards))
  })
  
  DRILL_METRICS <- list(
    list(col="Average Distance (Session)",                                  label="Total Dist (m)", digits=0),
    list(col="Meterage Per Minute",                                         label="Dist/Min",       digits=1),
    list(col="High Speed Distance",                                         label="HSD (m)",        digits=0),
    list(col="High Speed Distance Per Minute",                              label="HSD/Min",        digits=2),
    list(col="Velocity Band 6 Average Distance (Session)",                  label="VHSD (m)",       digits=0),
    list(col="vhsd_per_min",                                                label="VHSD/Min",       digits=2),
    list(col="High Metabolic Load Distance",                                label="HML (m)",        digits=0),
    list(col="hml_per_min",                                                 label="HML/Min",        digits=2),
    list(col="Acceleration B1-3 Average Efforts (Session) (Gen 2)",         label="Accels",         digits=0))
  
  drill_long <- reactive({
    df <- drill_with_pos(); req(nrow(df)>0)
    vhsd_col <- "Velocity Band 6 Average Distance (Session)"; dur_col <- "Average Duration (Session)"
    if (vhsd_col %in% names(df) && dur_col %in% names(df)) {
      dur_mins <- as.numeric(df[[dur_col]],units="secs")/60
      df$vhsd_per_min <- ifelse(dur_mins>0, df[[vhsd_col]]/dur_mins, NA_real_)
    } else { df$vhsd_per_min <- NA_real_ }
    hml_col <- "High Metabolic Load Distance"
    if (hml_col %in% names(df) && dur_col %in% names(df)) {
      dur_mins <- as.numeric(df[[dur_col]],units="secs")/60
      df$hml_per_min <- ifelse(dur_mins>0, df[[hml_col]]/dur_mins, NA_real_)
    } else { df$hml_per_min <- NA_real_ }
    metrics_present <- Filter(function(m) m$col %in% names(df), DRILL_METRICS)
    req(length(metrics_present)>0)
    df %>%
      mutate(Position=case_when(tolower(Forward_Back)=="forward"~"Forward",tolower(Forward_Back)=="back"~"Back",TRUE~"Unknown")) %>%
      group_by(`Period Name`,Position) %>%
      summarise(across(all_of(sapply(metrics_present,`[[`,"col")),~mean(.x,na.rm=TRUE)),.groups="drop") %>%
      pivot_longer(cols=all_of(sapply(metrics_present,`[[`,"col")),names_to="col",values_to="value") %>%
      left_join(bind_rows(lapply(metrics_present,as.data.frame)),by="col") %>%
      mutate(label=factor(label,levels=sapply(metrics_present,`[[`,"label")))
  })
  
  output$drill_charts_ui <- renderUI({
    dl <- drill_long(); req(nrow(dl)>0)
    drills <- unique(dl$`Period Name`)
    rows <- lapply(seq(1,length(drills),by=2), function(i) {
      drill_pair <- drills[i:min(i+1,length(drills))]
      cols <- lapply(drill_pair, function(d) {
        plot_id <- paste0("drill_plot_",gsub("[^A-Za-z0-9]","_",d))
        column(6, div(class="chart-card", div(class="chart-card-title",d), plotOutput(plot_id,height="240px")))
      })
      fluidRow(do.call(tagList,cols))
    })
    tagList(rows)
  })
  
  observe({
    dl <- drill_long(); req(nrow(dl)>0)
    drills <- unique(dl$`Period Name`)
    lapply(drills, function(d) {
      plot_id <- paste0("drill_plot_",gsub("[^A-Za-z0-9]","_",d))
      output[[plot_id]] <- renderPlot({
        drill_data <- dl %>% filter(`Period Name`==d); req(nrow(drill_data)>0)
        drill_data <- drill_data %>%
          group_by(label) %>% mutate(norm=value/max(value,na.rm=TRUE)) %>% ungroup() %>%
          filter(Position %in% c("Forward","Back"))
        ggplot(drill_data,aes(x=label,y=norm,fill=Position)) +
          geom_col(position="dodge",width=0.65,alpha=0.90) +
          geom_text(aes(label=ifelse(digits==0,round(value,0),round(value,as.integer(digits)))),
                    position=position_dodge(width=0.65),vjust=-0.45,size=3.0,fontface="bold",colour=NAVY) +
          scale_fill_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
          scale_y_continuous(labels=percent,limits=c(0,1.22),expand=expansion(mult=c(0,0))) +
          labs(x=NULL,y="Relative (raw value labelled)") +
          theme_waratahs(base_size=11) +
          theme(legend.position="top",legend.key.size=unit(0.5,"lines"),
                axis.text.x=element_text(size=9,face="bold",colour=NAVY),
                axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                panel.grid.major.y=element_blank(),plot.margin=margin(8,12,4,12))
      }, bg=CARD_BG)
    })
  })
  
  output$fwd_back_radar <- renderPlot({
    df <- session_with_pos(); req(nrow(df)>0)
    dur_col <- "Average Duration (Session)"
    vhsd_col <- "Velocity Band 6 Average Distance (Session)"
    if (vhsd_col %in% names(df) && dur_col %in% names(df)) {
      dur_mins <- as.numeric(df[[dur_col]],units="secs")/60
      df$vhsd_per_min_radar <- ifelse(dur_mins>0, df[[vhsd_col]]/dur_mins, NA_real_)
    } else { df$vhsd_per_min_radar <- NA_real_ }
    hml_col <- "High Metabolic Load Distance"
    if (hml_col %in% names(df) && dur_col %in% names(df)) {
      dur_mins <- as.numeric(df[[dur_col]],units="secs")/60
      df$hml_per_min_radar <- ifelse(dur_mins>0, df[[hml_col]]/dur_mins, NA_real_)
    } else { df$hml_per_min_radar <- NA_real_ }
    metrics <- c("Average Distance (Session)","Meterage Per Minute","High Speed Distance","High Speed Distance Per Minute",
                 "Velocity Band 6 Average Distance (Session)","vhsd_per_min_radar","High Metabolic Load Distance","hml_per_min_radar")
    labels  <- c("Total Dist","Dist/Min","HSD","HSD/Min","VHSD","VHSD/Min","HML Dist","HML/Min")
    metric_map <- setNames(labels,metrics); present <- intersect(metrics,names(df))
    plot_df <- df %>%
      filter(tolower(Forward_Back) %in% c("forward","back")) %>%
      mutate(Position=str_to_title(Forward_Back)) %>%
      group_by(Position) %>%
      summarise(across(all_of(present),~mean(.x,na.rm=TRUE)),.groups="drop") %>%
      pivot_longer(cols=all_of(present),names_to="Metric",values_to="Value") %>%
      mutate(Metric=metric_map[Metric]) %>%
      group_by(Metric) %>% mutate(norm_val=Value/max(Value,na.rm=TRUE)) %>% ungroup()
    ggplot(plot_df,aes(x=Metric,y=norm_val,fill=Position)) +
      geom_col(position="dodge",width=0.6,alpha=0.90) +
      geom_text(aes(label=round(Value,1)),position=position_dodge(width=0.6),
                vjust=-0.5,size=3.2,colour=NAVY,fontface="bold") +
      scale_fill_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
      scale_y_continuous(labels=percent,limits=c(0,1.12)) +
      labs(x=NULL,y="Relative to Group High") +
      theme_waratahs() + theme(legend.position="top",axis.text.x=element_text(size=10,face="bold"))
  }, bg=CARD_BG)
  
  output$player_table <- renderDT({
    df <- session_with_pos(); req(nrow(df)>0)
    cols_show <- c("Player Name","Forward_Back","Average Distance (Session)","Meterage Per Minute",
                   "High Metabolic Load Distance","High Speed Distance","High Speed Distance Per Minute",
                   "Velocity Band 6 Average Distance (Session)","Player Load Per Minute","Maximum Velocity")
    cols_present <- intersect(cols_show,names(df))
    tbl <- df %>%
      mutate(`Player Name` = ext_mark(`Player Name`)) %>%
      select(all_of(cols_present)) %>% arrange(`Player Name`) %>%
      rename_with(~gsub(" \\(Session\\)","",.x)) %>%
      rename("Player"=`Player Name`,"Group"=Forward_Back,"Dist (m)"=`Average Distance`,
             "m/min"=`Meterage Per Minute`,"HML (m)"=`High Metabolic Load Distance`,
             "HSD (m)"=`High Speed Distance`,
             "HSD/min"=`High Speed Distance Per Minute`,"VHSD (m)"=`Velocity Band 6 Average Distance`,
             "PL/min"=`Player Load Per Minute`,"Max Vel (m/s)"=`Maximum Velocity`) %>%
      mutate(across(where(is.numeric),~round(.x,1)))
    datatable(tbl,rownames=FALSE,
              options=list(pageLength=-1,dom="ft",scrollX=TRUE,
                           columnDefs=list(list(className="dt-center",targets="_all"))),
              class="compact stripe hover") %>%
      formatStyle("Group",
                  backgroundColor=styleEqual(c("forward","back","Forward","Back"),c("#DFF0FB","#FFF3DC","#DFF0FB","#FFF3DC")),
                  fontWeight="bold") %>%
      formatStyle(names(tbl)[3:ncol(tbl)],color=NAVY,fontWeight="600")
  })
  
  # BY PLAYER ================================================
  observe({
    players_sorted <- sort(unique(all_data()$`Player Name`))
    # Load choices but leave the box visually empty (selected="") so the user
    # can start typing immediately without having to clear the field first.
    # sel_player_eff() below falls back to players_sorted[1] so graphs still load.
    updateSelectizeInput(session,"sel_player",choices=players_sorted,selected=character(0))
  })

  # Resolves the effective player: uses the typed/selected value, or the
  # alphabetically first player when the box is still empty on first load.
  sel_player_eff <- reactive({
    sel <- input$sel_player
    if (isTruthy(sel)) return(sel)
    players_sorted <- sort(unique(all_data()$`Player Name`))
    if (length(players_sorted) == 0) return(NULL)
    players_sorted[1]
  })

  player_sessions <- reactive({
    p <- sel_player_eff(); req(!is.null(p))
    df <- all_data() %>% filter(`Player Name`==p, `Period Number`==0) %>% arrange(Date)
    sf <- if (!is.null(input$player_season_filter)) input$player_season_filter else "both"
    if (sf == "preseason")  df <- df %>% filter(tolower(trimws(Preseason)) == "yes")
    if (sf == "inseason")   df <- df %>% filter(tolower(trimws(Preseason)) != "yes")
    # sf == "both": no filter applied
    df
  })
  
  output$player_banner <- renderUI({
    p <- sel_player_eff(); req(!is.null(p))
    pw <- players_w(); df <- all_data() %>% filter(`Player Name`==p,`Period Number`==0)
    pr <- pw %>% filter(Name==p)
    pos_name  <- if (nrow(pr)>0 && "Position_Name" %in% names(pr)) str_to_title(pr$Position_Name[1]) else "---"
    fwd_back  <- if (nrow(pr)>0) str_to_title(pr$Forward_Back[1]) else "---"
    is_sevens <- if (nrow(pr)>0 && "Sevens" %in% names(pr)) tolower(trimws(pr$Sevens[1]))=="yes" else FALSE
    vel_col <- "Maximum Velocity"; peak_vel <- NA; peak_vel_date <- NA
    if (vel_col %in% names(df) && nrow(df)>0) {
      idx <- which.max(df[[vel_col]])
      if (length(idx)) { peak_vel <- round(df[[vel_col]][idx],2); peak_vel_date <- format(df$Date[idx],"%d %b %Y") }
    }
    acc_col <- "Max Acceleration"; peak_acc <- NA; peak_acc_date <- NA
    if (acc_col %in% names(df) && nrow(df)>0) {
      idx <- which.max(df[[acc_col]])
      if (length(idx)) { peak_acc <- round(df[[acc_col]][idx],2); peak_acc_date <- format(df$Date[idx],"%d %b %Y") }
    }
    div(class="session-banner",style="margin-bottom:18px;",
        div(class="banner-item",
            div(class="banner-label","Player"), div(class="banner-value", p),
            if (is_sevens) div(style="font-size:10px;font-weight:700;margin-top:4px;background:#E8A020;color:#fff;border-radius:10px;padding:2px 8px;display:inline-block;","SEVENS")),
        div(class="banner-divider"),
        div(class="banner-item", div(class="banner-label","Position"), div(class="banner-value",pos_name)),
        div(class="banner-divider"),
        div(class="banner-item", div(class="banner-label","Group"), div(class="banner-value",fwd_back)),
        div(class="banner-divider"),
        div(class="banner-item",
            div(class="banner-label","Peak Velocity"),
            div(class="banner-value", if (!is.na(peak_vel)) paste0(peak_vel," m/s") else "---"),
            if (!is.na(peak_vel_date)) div(style="font-size:10px;color:#88BBDD;margin-top:2px;",peak_vel_date)),
        div(class="banner-divider"),
        div(class="banner-item",
            div(class="banner-label","Peak Acceleration"),
            div(class="banner-value", if (!is.na(peak_acc)) paste0(peak_acc," m/s2") else "---"),
            if (!is.na(peak_acc_date)) div(style="font-size:10px;color:#88BBDD;margin-top:2px;",peak_acc_date))
    )
  })
  
  player_weekly <- reactive({
    df <- player_sessions(); req(nrow(df)>0)
    df %>%
      mutate(week_num = week_to_num(Week)) %>%
      arrange(Date) %>%
      mutate(
        day_label = format(Date, "%a\n%d %b"),   # e.g. "Mon\n05 Apr"
        x_pos     = row_number()                 # numeric position for band calc
      )
  })

  # Returns ggplot layers that shade alternating weeks and label each week above the bars.
  # Works on a CONTINUOUS numeric x-axis (x = x_pos). All coordinates are numeric so
  # annotate() and geom_rect() never conflict with a discrete scale.
  player_week_bands <- function(df) {
    wb <- df %>%
      filter(!is.na(week_num)) %>%
      group_by(week_num) %>%
      summarise(
        xmin    = min(x_pos) - 0.5,
        xmax    = max(x_pos) + 0.5,
        x_mid   = mean(x_pos),
        wk_txt  = paste0("Wk ", first(week_num)),
        .groups = "drop"
      ) %>%
      arrange(week_num) %>%
      mutate(shade = row_number() %% 2 == 0)

    shaded <- wb %>% filter(shade)
    layers <- list()

    # alternating light-blue band for even weeks
    if (nrow(shaded) > 0)
      layers <- c(layers, list(
        annotate("rect",
                 xmin = shaded$xmin, xmax = shaded$xmax,
                 ymin = -Inf,        ymax = Inf,
                 fill = "#EBF2F8",   alpha = 0.55)
      ))

    # thin dashed divider between weeks
    dividers <- head(wb$xmax, -1)
    if (length(dividers) > 0)
      layers <- c(layers, list(
        geom_vline(xintercept = dividers,
                   colour = "#AABBCC", linewidth = 0.35, linetype = "dashed")
      ))

    # week label sitting just inside the top of the plot
    layers <- c(layers, list(
      annotate("text", x = wb$x_mid, y = Inf,
               label = wb$wk_txt, vjust = 1.6, size = 2.6,
               colour = "#7799BB", fontface = "bold")
    ))

    layers
  }

  # Shared x-axis scale for all By Player plots (continuous, date labels on breaks)
  player_x_scale <- function(df)
    scale_x_continuous(breaks = df$x_pos, labels = df$day_label,
                       expand = expansion(add = 0.6))

  player_theme <- function() list(
    theme_waratahs(base_size = 11),
    theme(axis.text.x  = element_text(size = 8, angle = 0, hjust = 0.5, lineheight = 0.9),
          plot.margin  = margin(8, 16, 8, 8))
  )
  type_colours <- c(
    "intensive" = ACCENT,      # red   — high intensity sessions
    "training"  = SKY,         # blue  — standard training (was "extensive")
    "extensive" = SKY,         # blue  — keep for any legacy data
    "clubgame"  = "#00843D",   # green — matches
    "sevens"    = "#E8A020",   # amber — sevens sessions
    "rehabrun"  = GREY_MID,    # grey  — rehab runs
    "other"     = GREY_MID     # grey  — fallback
  )
  
  output$player_dist_plot <- renderPlot({
    df <- player_weekly(); req("Average Distance (Session)" %in% names(df))
    ggplot(df,aes(x=x_pos,y=`Average Distance (Session)`,fill=tolower(Type))) +
      player_week_bands(df) +
      geom_col(width=0.65,alpha=0.90) +
      geom_text(aes(label=round(`Average Distance (Session)`,0)),vjust=-0.4,size=3,fontface="bold",colour=NAVY) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="Distance (m)") + player_theme()
  }, bg=CARD_BG)

  output$player_load_plot <- renderPlot({
    df <- player_weekly(); req("Average Player Load (Session)" %in% names(df))
    ggplot(df,aes(x=x_pos,y=`Average Player Load (Session)`,group=1)) +
      player_week_bands(df) +
      geom_line(colour=SKY,linewidth=1.2) +
      geom_point(aes(fill=tolower(Type)),shape=21,size=3.5,colour=WHITE,stroke=1.2) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(expand=expansion(mult=c(0.05,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="Player Load (AU)") + player_theme()
  }, bg=CARD_BG)

  output$player_hml_plot <- renderPlot({
    df <- player_weekly(); req("High Metabolic Load Distance" %in% names(df))
    ggplot(df,aes(x=x_pos,y=`High Metabolic Load Distance`,group=1)) +
      player_week_bands(df) +
      geom_line(colour=ACCENT,linewidth=1.2) +
      geom_point(aes(fill=tolower(Type)),shape=21,size=3.5,colour=WHITE,stroke=1.2) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0.05,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="HML Distance (m)") + player_theme()
  }, bg=CARD_BG)

  output$player_hsd_plot <- renderPlot({
    df <- player_weekly(); req("High Speed Distance" %in% names(df))
    ggplot(df,aes(x=x_pos,y=`High Speed Distance`,group=1)) +
      player_week_bands(df) +
      geom_area(fill=SKY,alpha=0.18) + geom_line(colour=SKY,linewidth=1.2) +
      geom_point(aes(fill=tolower(Type)),shape=21,size=3.5,colour=WHITE,stroke=1.2) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0.05,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="HSD (m)") + player_theme()
  }, bg=CARD_BG)

  output$player_vhsd_plot <- renderPlot({
    df <- player_weekly(); req("Velocity Band 6 Average Distance (Session)" %in% names(df))
    ggplot(df,aes(x=x_pos,y=`Velocity Band 6 Average Distance (Session)`,group=1)) +
      player_week_bands(df) +
      geom_area(fill=ACCENT,alpha=0.15) + geom_line(colour=ACCENT,linewidth=1.2) +
      geom_point(aes(fill=tolower(Type)),shape=21,size=3.5,colour=WHITE,stroke=1.2) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0.05,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="VHSD (m)") + player_theme()
  }, bg=CARD_BG)

  output$player_maxvel_plot <- renderPlot({
    df <- player_weekly(); req("Maximum Velocity" %in% names(df))
    pb <- max(df$`Maximum Velocity`,na.rm=TRUE)
    ggplot(df,aes(x=x_pos,y=`Maximum Velocity`)) +
      player_week_bands(df) +
      geom_hline(yintercept=pb,linetype="dashed",colour=ACCENT,linewidth=0.6,alpha=0.7) +
      annotate("text",x=max(df$x_pos),y=pb,label=paste0("PB: ",round(pb,2)," m/s"),hjust=1.05,vjust=-0.4,size=3,colour=ACCENT,fontface="bold") +
      geom_col(aes(fill=tolower(Type)),width=0.6,alpha=0.88) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="Max Velocity (m/s)") + player_theme()
  }, bg=CARD_BG)

  output$player_mpm_plot <- renderPlot({
    df <- player_weekly(); req("Meterage Per Minute" %in% names(df))
    ggplot(df,aes(x=x_pos,y=`Meterage Per Minute`,group=1)) +
      player_week_bands(df) +
      geom_line(colour=NAVY,linewidth=1.2) +
      geom_point(aes(fill=tolower(Type)),shape=21,size=3.5,colour=WHITE,stroke=1.2) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(expand=expansion(mult=c(0.05,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="m/min") + player_theme()
  }, bg=CARD_BG)

  output$player_accels_plot <- renderPlot({
    df <- player_weekly()
    col <- "Acceleration B1-3 Average Efforts (Session) (Gen 2)"
    req(col %in% names(df))
    peak_accels <- max(df[[col]], na.rm=TRUE)
    ggplot(df,aes(x=x_pos,y=.data[[col]])) +
      player_week_bands(df) +
      geom_hline(yintercept=peak_accels,linetype="dashed",colour=ACCENT,linewidth=0.6,alpha=0.7) +
      annotate("text",x=max(df$x_pos),y=peak_accels,
               label=paste0("PB: ",round(peak_accels,0)),
               hjust=1.05,vjust=-0.4,size=3,colour=ACCENT,fontface="bold") +
      geom_col(aes(fill=tolower(Type)),width=0.6,alpha=0.88) +
      scale_fill_manual(values=type_colours,name="Session Type",labels=str_to_title) +
      scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
      player_x_scale(df) +
      labs(x=NULL,y="Accel Efforts (B1-3)") + player_theme()
  }, bg=CARD_BG)

  # ---- Players at a Glance table ----
  output$players_glance_table <- renderDT({
    pw <- Players_W

    # Compute '26 season averages and peaks live from All_Data (Period 0 sessions)
    s26 <- all_data() %>%
      filter(`Period Number`==0) %>%
      group_by(`Player Name`) %>%
      summarise(
        a26_dist = mean(`Average Distance (Session)`,                 na.rm=TRUE),
        a26_load = mean(`Average Player Load (Session)`,              na.rm=TRUE),
        a26_hml  = mean(`High Metabolic Load Distance`,               na.rm=TRUE),
        a26_hsd  = mean(`High Speed Distance`,                        na.rm=TRUE),
        a26_vel  = mean(`Maximum Velocity`,                           na.rm=TRUE),
        a26_acc  = mean(`Max Acceleration`,                           na.rm=TRUE),
        p26_dist = max(`Average Distance (Session)`,                  na.rm=TRUE),
        p26_load = max(`Average Player Load (Session)`,               na.rm=TRUE),
        p26_hml  = max(`High Metabolic Load Distance`,                na.rm=TRUE),
        p26_hsd  = max(`High Speed Distance`,                         na.rm=TRUE),
        p26_vel  = max(`Maximum Velocity`,                            na.rm=TRUE),
        p26_acc  = max(`Max Acceleration`,                            na.rm=TRUE),
        .groups="drop"
      ) %>%
      # replace -Inf (all-NA max) with NA
      mutate(across(starts_with("p26_"), ~ifelse(is.infinite(.x), NA_real_, .x)))

    tbl <- pw %>%
      filter(!is.na(Name)) %>%
      left_join(s26, by=c("Name"="Player Name")) %>%
      transmute(
        Player   = Name,
        Pos      = Position_Abrev,
        Group    = str_to_title(Forward_Back),
        # --- '25 Averages (dark navy #001F4E) ---
        a25_dist = round(as.numeric(`25avgaverage_distance`),    0),
        a25_load = round(as.numeric(`25avgaverage_player_load`), 1),
        a25_hml  = round(as.numeric(`25avghmld`),                0),
        a25_hsd  = round(as.numeric(`25avghighspeed_distance`),  0),
        a25_vel  = round(as.numeric(`25avgmaximum_velocity`),    2),
        a25_acc  = round(as.numeric(`25avg_max_acceleration`),   2),
        # --- '25 Peaks (medium navy #003A7A) ---
        p25_dist = round(as.numeric(`25average_distance`),       0),
        p25_load = round(as.numeric(`25average_player_load`),    1),
        p25_hml  = round(as.numeric(`25hmld`),                   0),
        p25_hsd  = round(as.numeric(`25highspeed_distance`),     0),
        p25_vel  = round(as.numeric(`25maximum_velocity`),       2),
        p25_acc  = round(as.numeric(`25max_acceleration`),       2),
        # --- '26 Averages (medium sky #007BB5) — live from All_Data ---
        a26_dist = round(a26_dist, 0),
        a26_load = round(a26_load, 1),
        a26_hml  = round(a26_hml,  0),
        a26_hsd  = round(a26_hsd,  0),
        a26_vel  = round(a26_vel,  2),
        a26_acc  = round(a26_acc,  2),
        # --- '26 Peaks (bright sky #009FDF) — live from All_Data ---
        p26_dist = round(p26_dist, 0),
        p26_load = round(p26_load, 1),
        p26_hml  = round(p26_hml,  0),
        p26_hsd  = round(p26_hsd,  0),
        p26_vel  = round(p26_vel,  2),
        p26_acc  = round(p26_acc,  2)
      ) %>%
      arrange(Group, Player)
    
    # Custom grouped header — no year labels repeated per column
    metric_th <- function(label) tags$th(style="text-align:center;font-size:11px;padding:4px 6px;", label)
    group_th  <- function(label, n, bg) tags$th(colspan=n,
                                                style=paste0("text-align:center;background:",bg,";color:#fff;padding:6px 8px;",
                                                             "font-size:11px;font-weight:700;letter-spacing:0.06em;border-left:2px solid #fff;"), label)
    
    sketch <- htmltools::withTags(table(
      class="display",
      tags$thead(
        tags$tr(
          tags$th(rowspan=2, style="vertical-align:middle;padding:6px 8px;", "Player"),
          tags$th(rowspan=2, style="vertical-align:middle;padding:6px 4px;", "Pos"),
          tags$th(rowspan=2, style="vertical-align:middle;padding:6px 8px;border-right:2px solid #ddd;", "Group"),
          group_th("'25 Last Season — Averages", 6L, "#001F4E"),
          group_th("'25 Last Season — Peaks",    6L, "#003A7A"),
          group_th("'26 This Season — Averages", 6L, "#007BB5"),
          group_th("'26 This Season — Peaks",    6L, "#009FDF")
        ),
        tags$tr(
          lapply(rep(c("Dist", "Load", "HML", "HSD", "Vel", "Acc"), 4L), metric_th)
        )
      )
    ))
    
    datatable(
      tbl, rownames=FALSE, container=sketch, selection="none",
      options=list(
        pageLength=-1, dom="ft", scrollX=TRUE, ordering=TRUE,
        columnDefs=list(
          list(className="dt-center", targets=seq(2L, ncol(tbl)-1L)),
          list(width="130px", targets=0L),
          list(width="40px",  targets=1L)
        )
      ),
      class="compact stripe hover"
    ) %>%
      formatStyle("Group",
                  backgroundColor=styleEqual(c("Forward","Back","Forward","Back"),
                                             c("#DFF0FB","#FFF3DC","#DFF0FB","#FFF3DC")),
                  fontWeight="bold") %>%
      formatStyle("Player", fontWeight="700", textAlign="left") %>%
      # '25 avg  cols 4-9  (1-based)
      formatStyle(names(tbl)[4:9],   color="#001F4E", fontWeight="600") %>%
      # '25 peak cols 10-15
      formatStyle(names(tbl)[10:15], color="#003A7A", fontWeight="600") %>%
      # '26 avg  cols 16-21
      formatStyle(names(tbl)[16:21], color="#007BB5", fontWeight="600") %>%
      # '26 peak cols 22-27
      formatStyle(names(tbl)[22:27], color="#009FDF", fontWeight="600")
  })
  
  # ---- Group & Position Benchmarks table ----
  output$group_summary_table <- renderDT({
    pw <- Players_W %>% filter(!is.na(Name))

    # '26 season averages AND peaks per player — computed live from All_Data (Period 0)
    s26_player <- all_data() %>%
      filter(`Period Number`==0) %>%
      group_by(`Player Name`) %>%
      summarise(
        a26_dist = mean(`Average Distance (Session)`,  na.rm=TRUE),
        a26_load = mean(`Average Player Load (Session)`, na.rm=TRUE),
        a26_hml  = mean(`High Metabolic Load Distance`,  na.rm=TRUE),
        a26_hsd  = mean(`High Speed Distance`,           na.rm=TRUE),
        a26_vel  = mean(`Maximum Velocity`,              na.rm=TRUE),
        a26_acc  = mean(`Max Acceleration`,              na.rm=TRUE),
        p26_dist = max(`Average Distance (Session)`,   na.rm=TRUE),
        p26_load = max(`Average Player Load (Session)`, na.rm=TRUE),
        p26_hml  = max(`High Metabolic Load Distance`,  na.rm=TRUE),
        p26_hsd  = max(`High Speed Distance`,           na.rm=TRUE),
        p26_vel  = max(`Maximum Velocity`,              na.rm=TRUE),
        p26_acc  = max(`Max Acceleration`,              na.rm=TRUE),
        .groups="drop"
      ) %>%
      mutate(across(starts_with("p26_"), ~ifelse(is.infinite(.x), NA_real_, .x)))

    pw_aug <- pw %>% left_join(s26_player, by=c("Name"="Player Name"))

    # Column name vectors (same order: dist, load, hml, hsd, vel, acc)
    m25a <- c("25avgaverage_distance","25avgaverage_player_load","25avghmld",
              "25avghighspeed_distance","25avgmaximum_velocity","25avg_max_acceleration")
    m25p <- c("25average_distance","25average_player_load","25hmld",
              "25highspeed_distance","25maximum_velocity","25max_acceleration")
    m26a <- c("a26_dist","a26_load","a26_hml","a26_hsd","a26_vel","a26_acc")
    m26p <- c("p26_dist","p26_load","p26_hml","p26_hsd","p26_vel","p26_acc")
    rnd  <- c(0L, 1L, 0L, 0L, 2L, 2L)

    safe_mean <- function(v) { v <- suppressWarnings(as.numeric(v)); v <- v[!is.na(v)&!is.infinite(v)]; if(length(v)==0) NA_real_ else mean(v) }
    safe_max  <- function(v) { v <- suppressWarnings(as.numeric(v)); v <- v[!is.na(v)&!is.infinite(v)]; if(length(v)==0) NA_real_ else max(v)  }

    # NOTE: column must be named "Group" to match formatStyle below
    make_row <- function(df, label) {
      if (nrow(df)==0) return(NULL)
      n   <- nrow(df)
      out <- data.frame(Group=paste0(label, "  (n=", n, ")"), stringsAsFactors=FALSE)
      for (i in seq_along(m25a)) out[[paste0("a25_",i)]] <- round(safe_mean(df[[m25a[i]]]), rnd[i])
      for (i in seq_along(m25p)) out[[paste0("p25_",i)]] <- round(safe_max( df[[m25p[i]]]), rnd[i])
      for (i in seq_along(m26a)) out[[paste0("a26_",i)]] <- round(safe_mean(df[[m26a[i]]]), rnd[i])
      for (i in seq_along(m26p)) out[[paste0("p26_",i)]] <- round(safe_max( df[[m26p[i]]]), rnd[i])
      out
    }
    
    # ---- Build rows ----
    # Forwards / Backs: ALL players in that group (includes unspecified positions)
    rows <- list(
      make_row(pw_aug %>% filter(tolower(trimws(Forward_Back))=="forward"), "Forwards"),
      make_row(pw_aug %>% filter(tolower(trimws(Forward_Back))=="back"),    "Backs")
    )
    
    # Individual position rows (separate, as labelled in Players_W)
    # Players with dual positions (e.g. halfback/fullback = 10/15) appear in both rows
    pos_named <- list(
      list(label="Prop",      pos="prop"),
      list(label="Hooker",    pos="hooker"),
      list(label="Lock",      pos="lock"),
      list(label="Backrow",   pos="backrow"),
      list(label="Halfback",  pos="halfback"),
      list(label="10/FB",     pos="10/FB"),
      list(label="Wing",      pos="wing"),
      list(label="Centre",    pos="centre")
    )
    for (p in pos_named) {
      sub <- pw_aug %>% filter(tolower(trimws(Position_Name)) == tolower(p$pos))
      if (nrow(sub) > 0)
        rows <- c(rows, list(make_row(sub, p$label)))
    }
    
    tbl    <- do.call(rbind, Filter(Negate(is.null), rows))
    n_rows <- nrow(tbl)
    # Hidden helper column: "group" for Forwards/Backs rows, "position" for all others
    tbl$RowType <- c("group", "group", rep("position", max(0L, n_rows - 2L)))
    vis_cols <- names(tbl)[seq_len(ncol(tbl) - 1L)]  # all columns except RowType
    rt_idx   <- ncol(tbl) - 1L                        # 0-based index of RowType (last col)
    
    # Custom grouped header
    metric_th <- function(label) tags$th(style="text-align:center;font-size:11px;padding:4px 6px;", label)
    group_th  <- function(label, n, bg) tags$th(colspan=n,
                                                style=paste0("text-align:center;background:",bg,";color:#fff;padding:6px 8px;",
                                                             "font-size:11px;font-weight:700;letter-spacing:0.06em;border-left:2px solid #fff;"), label)
    
    sketch <- htmltools::withTags(table(
      class="display",
      tags$thead(
        tags$tr(
          tags$th(rowspan=2, style="vertical-align:middle;padding:6px 8px;border-right:2px solid #ddd;", "Group / Position"),
          group_th("'25 Last Season — Averages", 6L, "#001F4E"),
          group_th("'25 Last Season — Peaks",    6L, "#003A7A"),
          group_th("'26 This Season — Averages", 6L, "#007BB5"),
          group_th("'26 This Season — Peaks",    6L, "#009FDF")
        ),
        tags$tr(lapply(rep(c("Dist","Load","HML","HSD","Vel","Acc"), 4L), metric_th))
      )
    ))
    
    datatable(
      tbl, rownames=FALSE, container=sketch, selection="none",
      options=list(
        pageLength=-1, dom="t", scrollX=TRUE, ordering=FALSE,
        columnDefs=list(
          list(className="dt-center", targets=seq(1L, ncol(tbl) - 2L)),
          list(width="160px", targets=0L),
          list(visible=FALSE, targets=rt_idx)   # hide RowType column
        )
      ),
      class="compact hover"
    ) %>%
      # Row-level highlight: Forwards/Backs rows get light-blue background
      formatStyle(vis_cols,
                  valueColumns="RowType",
                  backgroundColor=styleEqual(c("group","position"), c("#C8E6FF","white")),
                  fontWeight=styleEqual(c("group","position"), c("800","600"))) %>%
      formatStyle("Group", textAlign="left", color=NAVY) %>%
      formatStyle(names(tbl)[2:7],   color="#001F4E") %>%
      formatStyle(names(tbl)[8:13],  color="#003A7A") %>%
      formatStyle(names(tbl)[14:19], color="#007BB5") %>%
      formatStyle(names(tbl)[20:25], color="#009FDF")
  })
  
  # THIS SEASON tab ==========================================
  ts_filtered <- reactive({
    req(input$ts_season, input$ts_group)
    base <- all_data() %>%
      filter(`Period Number`==0) %>%
      filter(!is.na(Position_Number))
    
    avg_base <- function(b) {
      b %>%
        group_by(`Player Name`, Forward_Back, Position_Name, Position_Abrev, Position_Number) %>%
        summarise(
          `Average Distance (Session)`                 = mean(`Average Distance (Session)`,                 na.rm=TRUE),
          `Average Player Load (Session)`              = mean(`Average Player Load (Session)`,              na.rm=TRUE),
          `High Metabolic Load Distance`               = mean(`High Metabolic Load Distance`,               na.rm=TRUE),
          `High Speed Distance`                        = mean(`High Speed Distance`,                        na.rm=TRUE),
          `Velocity Band 6 Average Distance (Session)` = mean(`Velocity Band 6 Average Distance (Session)`, na.rm=TRUE),
          `Maximum Velocity`                           = mean(`Maximum Velocity`,                           na.rm=TRUE),
          `Meterage Per Minute`                        = mean(`Meterage Per Minute`,                        na.rm=TRUE),
          `Max Acceleration`                           = mean(`Max Acceleration`,                           na.rm=TRUE),
          Sessions = n(), .groups="drop")
    }
    
    df <- if (input$ts_season == "preseason") {
      avg_base(base %>% filter(tolower(trimws(Preseason)) == "yes"))
    } else if (input$ts_season == "inseason") {
      avg_base(base %>% filter(tolower(trimws(Preseason)) != "yes"))
    } else {
      avg_base(base)
    }
    
    if (input$ts_group == "forward") {
      df <- df %>% filter(tolower(Forward_Back) == "forward")
    } else if (input$ts_group == "back") {
      df <- df %>% filter(tolower(Forward_Back) == "back")
    } else if (startsWith(input$ts_group, "pos_")) {
      pos_num <- as.integer(sub("pos_", "", input$ts_group))
      df <- df %>% filter(Position_Number == pos_num)
    }
    
    df %>% mutate(Group = case_when(
      tolower(Forward_Back)=="forward" ~ "Forward",
      tolower(Forward_Back)=="back"    ~ "Back",
      TRUE ~ Forward_Back))
  })
  
  output$ts_kpi_row <- renderUI({
    df <- ts_filtered(); req(nrow(df)>0)
    mk <- function(label, val, unit, accent="")
      div(class=paste("kpi-card", accent),
          div(class="kpi-label", label),
          div(class="kpi-value", fmt_num(val, 0)),
          div(class="kpi-unit",  unit))
    fwd <- df %>% filter(Group=="Forward")
    bck <- df %>% filter(Group=="Back")
    div(class="kpi-row",
        mk("Players",           nrow(df),                                                              "total"),
        mk("Avg Distance",      mean(df$`Average Distance (Session)`,   na.rm=TRUE),                   "m"),
        mk("Avg HML",           mean(df$`High Metabolic Load Distance`, na.rm=TRUE),                   "m", "accent-red"),
        mk("Avg HSD",           mean(df$`High Speed Distance`,          na.rm=TRUE),                   "m"),
        mk("Fwd Avg Distance",  mean(fwd$`Average Distance (Session)`,  na.rm=TRUE),                   "m"),
        mk("Back Avg Distance", mean(bck$`Average Distance (Session)`,  na.rm=TRUE),                   "m", "accent-navy"))
  })
  
  output$ts_rank_plot <- renderPlot({
    df <- ts_filtered(); req(nrow(df)>0)
    metric <- input$ts_metric; req(metric %in% names(df))
    plot_df <- df %>%
      select(Player=`Player Name`, Group, Value=all_of(metric)) %>%
      filter(!is.na(Value)) %>%
      arrange(Value) %>%
      mutate(Player=factor(Player, levels=Player))
    team_avg <- mean(plot_df$Value, na.rm=TRUE)
    metric_labels <- c(
      "Average Distance (Session)"                 = "Distance (m)",
      "Average Player Load (Session)"              = "Player Load",
      "High Metabolic Load Distance"               = "HML Distance (m)",
      "High Speed Distance"                        = "High Speed Distance (m)",
      "Velocity Band 6 Average Distance (Session)" = "Very High Speed Distance (m)",
      "Maximum Velocity"                           = "Max Velocity (m/s)",
      "Meterage Per Minute"                        = "Metres per Minute"
    )
    y_label <- if (metric %in% names(metric_labels)) metric_labels[metric] else metric
    ggplot(plot_df, aes(x=Player, y=Value, fill=Group)) +
      geom_col(width=0.7, alpha=0.90) +
      geom_hline(yintercept=team_avg, linetype="dashed", colour=NAVY, linewidth=0.7, alpha=0.6) +
      annotate("text", x=Inf, y=team_avg, label=paste0("Team avg: ",round(team_avg,1)),
               hjust=1.05, vjust=-0.5, size=3.2, colour=NAVY, fontface="bold") +
      geom_text(aes(label=round(Value,1)), hjust=-0.15, size=3, fontface="bold", colour=NAVY) +
      coord_flip() +
      scale_fill_manual(values=group_colours, name=NULL) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0, 0.18))) +
      labs(x=NULL, y=y_label) +
      theme_waratahs(base_size=11) +
      theme(legend.position="top", axis.text.y=element_text(size=10, face="bold"))
  }, bg=CARD_BG)
  
  output$ts_table <- renderDT({
    df <- ts_filtered(); req(nrow(df)>0)
    tbl <- df %>%
      arrange(Position_Number) %>%
      select(Player=`Player Name`, Position=Position_Abrev, Group,
             `Dist (m)`=`Average Distance (Session)`,
             `Load`=`Average Player Load (Session)`,
             `HML (m)`=`High Metabolic Load Distance`,
             `HSD (m)`=`High Speed Distance`,
             `VHSD (m)`=`Velocity Band 6 Average Distance (Session)`,
             `Max Vel`=`Maximum Velocity`,
             `m/min`=`Meterage Per Minute`,
             `Max Acc`=`Max Acceleration`) %>%
      mutate(across(where(is.numeric), ~round(.x, 1)))
    datatable(tbl, rownames=FALSE, filter="top",
              options=list(pageLength=-1, dom="ti", scrollX=TRUE,
                           columnDefs=list(list(className="dt-center", targets="_all"))),
              class="compact stripe hover") %>%
      formatStyle("Group",
                  backgroundColor=styleEqual(c("Forward","Back"), c("#DFF0FB","#FFF3DC")),
                  fontWeight="bold") %>%
      formatStyle("Dist (m)",
                  background=styleColorBar(tbl$`Dist (m)`, SKY),
                  backgroundSize="98% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") %>%
      formatStyle("HML (m)",
                  background=styleColorBar(tbl$`HML (m)`, ACCENT),
                  backgroundSize="98% 70%", backgroundRepeat="no-repeat", backgroundPosition="center") %>%
      formatStyle("Max Vel",
                  background=styleColorBar(tbl$`Max Vel`, "#009FDF44"),
                  backgroundSize="98% 70%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })
  
  # HML ======================================================
  hml_session <- reactive({
    weekly_data() %>%
      filter(tolower(trimws(`Period Name`)) != "week total") %>%
      mutate(Group=case_when(tolower(Forward_Back)=="forward"~"Forward",tolower(Forward_Back)=="back"~"Back",TRUE~"Unknown"),
             week_num=week_to_num(Week),
             week_label=paste0(ifelse(tolower(trimws(Preseason))=="yes","PS Wk ","Wk "),week_num,"\n",str_to_title(Day)))
  })
  
  hml_drills <- reactive({
    all_data() %>% filter(`Period Number`!=0) %>%
      mutate(Group=case_when(tolower(Forward_Back)=="forward"~"Forward",tolower(Forward_Back)=="back"~"Back",TRUE~"Unknown"))
  })
  
  output$hml_kpi_row <- renderUI({
    df <- hml_session(); req(HML_COL %in% names(df),nrow(df)>0)
    team_avg <- mean(df$`High Metabolic Load Distance`,na.rm=TRUE)
    fwd_avg  <- mean(df$`High Metabolic Load Distance`[df$Group=="Forward"],na.rm=TRUE)
    bck_avg  <- mean(df$`High Metabolic Load Distance`[df$Group=="Back"],   na.rm=TRUE)
    team_max <- max(df$`High Metabolic Load Distance`,na.rm=TRUE)
    mk <- function(label,value,unit,accent="") div(class=paste("kpi-card",accent),div(class="kpi-label",label),div(class="kpi-value",fmt_num(value,0)),div(class="kpi-unit",unit))
    div(class="kpi-row", mk("Team Avg HML/Session",team_avg,"m"), mk("Forward Avg",fwd_avg,"m","accent-red"), mk("Back Avg",bck_avg,"m"), mk("Session Peak",team_max,"m","accent-navy"))
  })
  
  output$hml_trend_plot <- renderPlot({
    df <- hml_session(); req(HML_COL %in% names(df),nrow(df)>0)
    # One row per Date (averaged across players within group)
    trend_base <- df %>%
      filter(Group %in% c("Forward","Back"), !is.na(`High Metabolic Load Distance`)) %>%
      group_by(Date, week_num, Group) %>%
      summarise(avg_hml=mean(`High Metabolic Load Distance`,na.rm=TRUE),.groups="drop") %>%
      filter(!is.na(avg_hml)) %>% arrange(Date)
    # Assign a numeric x position per unique date (for continuous axis + week bands)
    date_pos <- trend_base %>%
      distinct(Date, week_num) %>%
      arrange(Date) %>%
      mutate(x_pos = row_number(),
             day_label = format(Date, "%a\n%d %b"))
    trend <- trend_base %>% left_join(date_pos, by=c("Date","week_num"))
    ggplot(trend,aes(x=x_pos,y=avg_hml,colour=Group,group=Group)) +
      player_week_bands(date_pos) +
      geom_line(linewidth=1.1,alpha=0.8) + geom_point(size=3,aes(shape=Group)) +
      scale_colour_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
      scale_shape_manual(values=c("Forward"=16,"Back"=17),name=NULL) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0.05,0.12))) +
      scale_x_continuous(breaks=date_pos$x_pos, labels=date_pos$day_label,
                         expand=expansion(add=0.6)) +
      labs(x=NULL,y="Avg HML Distance (m)") + theme_waratahs(base_size=11) +
      theme(axis.text.x=element_text(size=8,angle=0,hjust=0.5),legend.position="top")
  }, bg=CARD_BG)

  output$hml_week_plot <- renderPlot({
    df <- hml_session(); req(HML_COL %in% names(df),nrow(df)>0)
    week_df <- df %>%
      filter(Group %in% c("Forward","Back"), !is.na(`High Metabolic Load Distance`)) %>%
      group_by(week_num, Group) %>%
      summarise(avg_hml=mean(`High Metabolic Load Distance`,na.rm=TRUE),.groups="drop") %>%
      arrange(week_num)
    # Assign x_pos per unique week (for continuous axis + week bands)
    wk_pos <- week_df %>%
      distinct(week_num) %>%
      arrange(week_num) %>%
      mutate(x_pos    = row_number(),
             day_label = paste0("Wk ", week_num))
    week_df <- week_df %>% left_join(wk_pos, by="week_num")
    ggplot(week_df,aes(x=x_pos,y=avg_hml,colour=Group,group=Group)) +
      player_week_bands(wk_pos) +
      geom_line(linewidth=1.2,alpha=0.85) + geom_point(size=3.5,aes(shape=Group)) +
      geom_text(aes(label=round(avg_hml,0)),vjust=-1,size=3.2,fontface="bold") +
      scale_colour_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
      scale_shape_manual(values=c("Forward"=16,"Back"=17),name=NULL) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0.05,0.15))) +
      scale_x_continuous(breaks=wk_pos$x_pos, labels=wk_pos$day_label,
                         expand=expansion(add=0.6)) +
      labs(x=NULL,y="Avg HML Distance (m)") + theme_waratahs(base_size=11) +
      theme(legend.position="top")
  }, bg=CARD_BG)
  
  output$hml_breakdown_plot_ui <- renderUI({
    h <- if (input$hml_breakdown_view=="position") "380px" else "260px"
    plotOutput("hml_breakdown_plot", height=h)
  })
  
  output$hml_breakdown_plot <- renderPlot({
    df <- hml_session(); req(nrow(df)>0, HML_COL %in% names(df))
    if (input$hml_breakdown_view=="position") {
      pos <- df %>% filter(!is.na(Position_Name),Group %in% c("Forward","Back")) %>%
        group_by(Position_Name,Group) %>%
        summarise(avg_hml=mean(`High Metabolic Load Distance`,na.rm=TRUE),.groups="drop") %>%
        arrange(desc(avg_hml)) %>% mutate(Position_Name=factor(Position_Name,levels=rev(unique(Position_Name))))
      ggplot(pos,aes(x=Position_Name,y=avg_hml,fill=Group)) +
        geom_col(width=0.65,alpha=0.90) +
        geom_text(aes(label=round(avg_hml,0)),hjust=-0.15,fontface="bold",colour=NAVY,size=3.2) +
        coord_flip() + scale_fill_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
        scale_y_continuous(labels=comma,expand=expansion(mult=c(0,0.18))) +
        labs(x=NULL,y="Avg HML Distance (m)") + theme_waratahs() + theme(legend.position="top")
    } else {
      grp_df <- df %>% filter(Group %in% c("Forward","Back"), !is.na(`High Metabolic Load Distance`)) %>%
        group_by(Group) %>%
        summarise(avg=mean(`High Metabolic Load Distance`,na.rm=TRUE),
                  q1=quantile(`High Metabolic Load Distance`,0.25,na.rm=TRUE),
                  q3=quantile(`High Metabolic Load Distance`,0.75,na.rm=TRUE), .groups="drop")
      ggplot(grp_df, aes(x=Group, y=avg, fill=Group)) +
        geom_col(width=0.5, alpha=0.90) +
        geom_linerange(aes(ymin=q1, ymax=q3), colour="grey50", linewidth=8, alpha=0.5) +
        # average label above bar
        geom_text(aes(label=paste0("Avg: ",round(avg,0),"m")),
                  vjust=-0.5, size=3.8, fontface="bold", colour=NAVY) +
        # Q3 label at top of grey band, nudged right
        geom_text(aes(y=q3, label=paste0("Q3: ",round(q3,0),"m")),
                  hjust=-0.15, size=3, colour="grey35", fontface="bold") +
        # Q1 label at bottom of grey band, nudged right
        geom_text(aes(y=q1, label=paste0("Q1: ",round(q1,0),"m")),
                  hjust=-0.15, size=3, colour="grey35", fontface="bold") +
        scale_fill_manual(values=c("Forward"=FORWARD,"Back"=BACK), guide="none") +
        scale_y_continuous(labels=comma, expand=expansion(mult=c(0,0.15))) +
        labs(x=NULL, y="Avg HML Distance (m)") +
        theme_waratahs()
    }
  }, bg=CARD_BG)
  
  # Shared drill summary reactive
  # Drill metrics table: one row per drill, columns = Fwd avg, Back avg, team avg, % contribution
  output$hml_drill_table <- renderDT({
    df <- hml_drills(); req(HML_COL %in% names(df), nrow(df) > 0)
    
    # Per-group averages
    grp <- df %>%
      filter(Group %in% c("Forward","Back"), !is.na(`High Metabolic Load Distance`)) %>%
      group_by(`Period Name`, Group) %>%
      summarise(avg_hml = mean(`High Metabolic Load Distance`, na.rm=TRUE), .groups="drop") %>%
      tidyr::pivot_wider(names_from=Group, values_from=avg_hml,
                         names_prefix="avg_")
    
    # Team average & % contribution
    team <- df %>%
      filter(!is.na(`High Metabolic Load Distance`)) %>%
      group_by(`Period Name`) %>%
      summarise(Team_Avg = mean(`High Metabolic Load Distance`, na.rm=TRUE), .groups="drop")
    
    tbl <- left_join(grp, team, by="Period Name") %>%
      mutate(`% of Total HML` = round(Team_Avg / sum(Team_Avg, na.rm=TRUE) * 100, 1),
             avg_Forward = round(avg_Forward, 1),
             avg_Back    = round(avg_Back,    1),
             Team_Avg    = round(Team_Avg,    1)) %>%
      arrange(desc(Team_Avg)) %>%
      rename(Drill = `Period Name`,
             `Fwd Avg (m)` = avg_Forward,
             `Back Avg (m)` = avg_Back,
             `Team Avg (m)` = Team_Avg)
    
    datatable(tbl, rownames=FALSE, options=list(
      pageLength=25, dom="ft",
      columnDefs=list(list(className="dt-center", targets=1:4))),
      class="stripe hover compact") %>%
      formatStyle("% of Total HML",
                  background=styleColorBar(c(0, max(tbl$`% of Total HML`, na.rm=TRUE)), "#009FDF33"),
                  backgroundSize="100% 80%", backgroundRepeat="no-repeat",
                  backgroundPosition="center") %>%
      formatStyle("Team Avg (m)",  fontWeight="bold") %>%
      formatStyle("Fwd Avg (m)",   color=FORWARD, fontWeight="bold") %>%
      formatStyle("Back Avg (m)",  color=BACK,    fontWeight="bold")
  })
  
  output$hml_player_dot_plot <- renderPlot({
    df <- hml_session(); req(HML_COL %in% names(df),nrow(df)>0)
    # Apply season filter
    sf <- if (!is.null(input$hml_dot_season)) input$hml_dot_season else "both"
    if (sf == "preseason") df <- df %>% filter(tolower(trimws(Preseason)) == "yes")
    if (sf == "inseason")  df <- df %>% filter(tolower(trimws(Preseason)) != "yes")
    # sf == "both": no filter applied
    req(nrow(df) > 0)
    player_avgs <- df %>% filter(Group %in% c("Forward","Back"),!is.na(`High Metabolic Load Distance`)) %>%
      group_by(`Player Name`,Group) %>%
      summarise(avg_hml=mean(`High Metabolic Load Distance`,na.rm=TRUE),.groups="drop") %>%
      arrange(Group,desc(avg_hml))
    op <- player_avgs$`Player Name`
    plot_df <- df %>% filter(`Player Name` %in% op,!is.na(`High Metabolic Load Distance`)) %>%
      mutate(`Player Name`=factor(`Player Name`,levels=rev(op)))
    ggplot(plot_df,aes(x=`Player Name`,y=`High Metabolic Load Distance`,colour=Group)) +
      geom_segment(data=player_avgs%>%mutate(`Player Name`=factor(`Player Name`,levels=rev(op))),
                   aes(x=`Player Name`,xend=`Player Name`,y=0,yend=avg_hml),colour="#E2EAF0",linewidth=3,inherit.aes=FALSE) +
      geom_jitter(size=2.5,alpha=0.80,width=0.18) +
      geom_point(data=player_avgs%>%mutate(`Player Name`=factor(`Player Name`,levels=rev(op))),
                 aes(x=`Player Name`,y=avg_hml),shape=18,size=4.5,colour=NAVY,inherit.aes=FALSE) +
      coord_flip() + scale_colour_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0,0.08))) +
      labs(x=NULL,y="HML Distance (m)",subtitle="Each dot = one session   diamond = season average") +
      theme_waratahs(base_size=10) + theme(legend.position="top",axis.text.y=element_text(size=9))
  }, bg=CARD_BG)
  
  output$hml_player_bar_plot <- renderPlot({
    df <- hml_session(); req(HML_COL %in% names(df),nrow(df)>0)
    ranked <- df %>% filter(Group %in% c("Forward","Back"),!is.na(`High Metabolic Load Distance`)) %>%
      group_by(`Player Name`,Group,Position_Name) %>%
      summarise(avg_hml=mean(`High Metabolic Load Distance`,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(avg_hml)) %>% mutate(`Player Name`=factor(`Player Name`,levels=rev(`Player Name`)))
    ggplot(ranked,aes(x=`Player Name`,y=avg_hml,fill=Group)) +
      geom_col(width=0.65,alpha=0.90) +
      geom_text(aes(label=paste0(round(avg_hml,0),"m")),hjust=-0.1,size=3,fontface="bold",colour=NAVY) +
      geom_text(aes(label=paste0("(",Position_Name,")")),y=2,hjust=0,size=2.6,colour=WHITE,alpha=0.85) +
      coord_flip() + scale_fill_manual(values=c("Forward"=FORWARD,"Back"=BACK),name=NULL) +
      scale_y_continuous(labels=comma,expand=expansion(mult=c(0,0.18))) +
      labs(x=NULL,y="Season Avg HML Distance (m)") +
      theme_waratahs(base_size=10) + theme(legend.position="top",axis.text.y=element_text(size=9))
  }, bg=CARD_BG)
  
  
  # BY WEEK ===================================================
  group_colours <- c("Forward"=FORWARD, "Back"=BACK)
  
  # Shared base: one row per player per session, with group/position/week info
  week_base <- reactive({
    weekly_data() %>%
      filter(tolower(trimws(`Period Name`)) != "week total") %>%
      mutate(
        Group      = case_when(tolower(Forward_Back)=="forward"~"Forward",
                               tolower(Forward_Back)=="back"~"Back", TRUE~NA_character_),
        week_num   = week_to_num(Week),
        week_label = ifelse(tolower(trimws(Preseason)) == "yes",
                            paste0("PS Wk ", week_num),
                            paste0("Wk ", week_num)),
        day_num    = match(tolower(Day), c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")),
        day_label  = str_to_title(Day),
        Position_Name = str_to_title(Position_Name)
      )
  })
  
  # Populate Individual Week dropdown from available weeks in data
  observe({
    df <- week_base(); req(nrow(df)>0)
    wks <- df %>% distinct(Week, week_num) %>% arrange(week_num) %>%
      mutate(label=paste0("Preseason Week ", week_num))
    updateSelectInput(session, "indiv_week_sel",
                      choices=setNames(wks$Week, wks$label), selected=wks$Week[1])
  })
  
  # Helper: which column to colour/group by
  week_col_var <- function(view_by) {
    switch(view_by, "group"="Group", "position"="Position_Name", "player"="Player Name")
  }
  
  # Aggregate helper: group base data by chosen variable + x variable
  week_agg <- function(df, grp_vars) {
    df %>%
      group_by(across(all_of(grp_vars))) %>%
      summarise(
        Avg_Distance = mean(`Average Distance (Session)`,                                  na.rm=TRUE),
        Avg_Load     = mean(`Average Player Load (Session)`,                             na.rm=TRUE),
        Avg_HML      = mean(`High Metabolic Load Distance`,                              na.rm=TRUE),
        Avg_HSD      = mean(`High Speed Distance`,                                       na.rm=TRUE),
        Avg_VHSD     = mean(`Velocity Band 6 Average Distance (Session)`,                na.rm=TRUE),
        Avg_MaxVel   = mean(`Maximum Velocity`,                                          na.rm=TRUE),
        Avg_MMin     = mean(`Meterage Per Minute`,                                       na.rm=TRUE),
        Avg_Accels   = mean(`Acceleration B1-3 Average Efforts (Session) (Gen 2)`,       na.rm=TRUE),
        .groups="drop")
  }
  
  # Populate position selectizes on data load
  observe({
    df <- week_base(); req(nrow(df) > 0)
    positions <- sort(unique(df$Position_Name[!is.na(df$Position_Name)]))
    updateSelectizeInput(session, "cw_avg_pos_filter", choices=positions, selected=positions, server=TRUE)
    updateSelectizeInput(session, "cw_tot_pos_filter", choices=positions, selected=positions, server=TRUE)
  })
  
  # Re-populate all positions when user switches to "By Position"
  observeEvent(input$week_compare_by, {
    if (input$week_compare_by == "position") {
      df <- week_base(); req(nrow(df) > 0)
      positions <- sort(unique(df$Position_Name[!is.na(df$Position_Name)]))
      updateSelectizeInput(session, "cw_avg_pos_filter", choices=positions, selected=positions, server=TRUE)
    }
  })
  observeEvent(input$week_total_by, {
    if (input$week_total_by == "position") {
      df <- week_base(); req(nrow(df) > 0)
      positions <- sort(unique(df$Position_Name[!is.na(df$Position_Name)]))
      updateSelectizeInput(session, "cw_tot_pos_filter", choices=positions, selected=positions, server=TRUE)
    }
  })
  
  # Comparing Weeks: one point per week per group/position/player
  by_week_data <- reactive({
    df <- week_base(); req(nrow(df)>0, input$week_compare_by)
    if (input$week_compare_by == "position") {
      pos_sel <- input$cw_avg_pos_filter; req(length(pos_sel) > 0)
      df <- df %>% filter(Position_Name %in% pos_sel)
    }
    col_var  <- week_col_var(input$week_compare_by)
    grp_cols <- unique(c("week_num","week_label", col_var))
    week_agg(df %>% filter(!is.na(.data[[col_var]])), grp_cols) %>% arrange(week_num)
  })
  
  # Comparing Weeks - Totals: sum of daily averages per week per group/position
  week_total_data <- reactive({
    df <- week_base(); req(nrow(df)>0, input$week_total_by)
    if (input$week_total_by == "position") {
      pos_sel <- input$cw_tot_pos_filter; req(length(pos_sel) > 0)
      df <- df %>% filter(Position_Name %in% pos_sel)
    }
    col_var      <- week_col_var(input$week_total_by)
    grp_day_cols <- unique(c("week_num","week_label","day_num","day_label", col_var))
    daily_avgs   <- week_agg(df %>% filter(!is.na(.data[[col_var]])), grp_day_cols)
    daily_avgs %>%
      group_by(across(all_of(unique(c("week_num","week_label", col_var))))) %>%
      summarise(
        Avg_Distance = sum(Avg_Distance, na.rm=TRUE),
        Avg_Load     = sum(Avg_Load,     na.rm=TRUE),
        Avg_HML      = sum(Avg_HML,      na.rm=TRUE),
        Avg_HSD      = sum(Avg_HSD,      na.rm=TRUE),
        Avg_VHSD     = sum(Avg_VHSD,     na.rm=TRUE),
        Avg_MaxVel   = max(Avg_MaxVel,   na.rm=TRUE),
        Avg_MMin     = mean(Avg_MMin,    na.rm=TRUE),
        Avg_Accels   = sum(Avg_Accels,   na.rm=TRUE),
        .groups="drop") %>%
      arrange(week_num)
  })

  # Individual Week: one point per session-day per group/position/player
  # Populate individual week position selectize on data load
  observe({
    df <- week_base(); req(nrow(df) > 0)
    positions <- sort(unique(df$Position_Name[!is.na(df$Position_Name)]))
    updateSelectizeInput(session, "iw_pos_filter", choices=positions, selected=positions, server=TRUE)
  })
  
  # Re-populate when user switches to "By Position"
  observeEvent(input$indiv_week_by, {
    if (input$indiv_week_by == "position") {
      df <- week_base(); req(nrow(df) > 0)
      positions <- sort(unique(df$Position_Name[!is.na(df$Position_Name)]))
      updateSelectizeInput(session, "iw_pos_filter", choices=positions, selected=positions, server=TRUE)
    }
  })
  
  indiv_week_data <- reactive({
    df <- week_base(); req(nrow(df)>0, input$indiv_week_sel, input$indiv_week_by)
    if (input$indiv_week_by == "position") {
      pos_sel <- input$iw_pos_filter; req(length(pos_sel) > 0)
      df <- df %>% filter(Position_Name %in% pos_sel)
    }
    col_var  <- week_col_var(input$indiv_week_by)
    grp_cols <- unique(c("day_num","day_label", col_var))
    week_agg(
      df %>% filter(Week == input$indiv_week_sel, !is.na(.data[[col_var]])),
      grp_cols) %>% arrange(day_num)
  })
  
  # Generic line plot — works for both comparing weeks and individual week
  week_line_plot <- function(df, col, y_label, colour_var, view_by,
                             x_var="week_label", x_order_var="week_num") {
    req(nrow(df)>0)
    p <- ggplot(df, aes(x=reorder(.data[[x_var]], .data[[x_order_var]]),
                        y=.data[[col]],
                        colour=.data[[colour_var]],
                        group=.data[[colour_var]])) +
      geom_line(linewidth=1.2, alpha=0.85) +
      geom_point(size=3) +
      geom_text(aes(label=round(.data[[col]], 1)), vjust=-0.9, size=2.8,
                fontface="bold", show.legend=FALSE) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0.05, 0.18))) +
      labs(x=NULL, y=y_label) +
      theme_waratahs(base_size=11) +
      theme(legend.position="top", axis.text.x=element_text(size=9, face="bold"))
    if (view_by=="group") {
      p + scale_colour_manual(values=group_colours, name=NULL)
    } else if (view_by=="position") {
      p + scale_colour_brewer(palette="Set1", name=NULL) +
        guides(colour=guide_legend(nrow=2))
    } else {
      p + scale_colour_discrete(name=NULL) +
        guides(colour=guide_legend(nrow=3, override.aes=list(linewidth=2)))
    }
  }
  
  # Comparing Weeks plots
  cw_plot <- function(col, y_label) {
    df <- by_week_data(); req(nrow(df)>0)
    vb <- input$week_compare_by; req(vb)
    week_line_plot(df, col, y_label, week_col_var(vb), vb)
  }
  output$week_dist_plot   <- renderPlot({ cw_plot("Avg_Distance","Distance (m)")        }, bg=CARD_BG)
  output$week_load_plot   <- renderPlot({ cw_plot("Avg_Load",    "Player Load (AU)")    }, bg=CARD_BG)
  output$week_hml_plot    <- renderPlot({ cw_plot("Avg_HML",     "HML Distance (m)")    }, bg=CARD_BG)
  output$week_hsd_plot    <- renderPlot({ cw_plot("Avg_HSD",     "HSD (m)")             }, bg=CARD_BG)
  output$week_vhsd_plot   <- renderPlot({ cw_plot("Avg_VHSD",    "VHSD (m)")            }, bg=CARD_BG)
  output$week_maxvel_plot <- renderPlot({ cw_plot("Avg_MaxVel",  "Max Velocity (m/s)")  }, bg=CARD_BG)
  output$week_mmin_plot   <- renderPlot({ cw_plot("Avg_MMin",    "m/min")               }, bg=CARD_BG)
  
  # ---- Training days per week modal ----
  observeEvent(input$show_week_sessions_modal, {
    df <- week_base(); req(nrow(df) > 0)
    
    # Find dates that had more than 10 athletes (= team training days)
    team_days <- All_Data %>%
      filter(`Period Number` == 0, trimws(tolower(`Period Name`)) == "session") %>%
      group_by(Date) %>%
      summarise(n_athletes = n_distinct(`Player Name`), .groups = "drop") %>%
      filter(n_athletes > 10) %>%
      pull(Date)
    
    tbl <- df %>%
      filter(!is.na(Date), Date %in% team_days) %>%
      mutate(week_num = week_to_num(Week)) %>%
      group_by(week_num, Week) %>%
      summarise(
        `Team Training Days` = n_distinct(Date),
        Days                 = paste(sort(unique(str_to_title(Day))), collapse=", "),
        .groups="drop") %>%
      arrange(week_num) %>%
      mutate(Week = paste0("Preseason Week ", str_to_title(Week))) %>%
      select(Week, `Team Training Days`, Days)
    
    output$week_sessions_table <- DT::renderDT({
      DT::datatable(tbl, rownames=FALSE,
                    options=list(dom="t", ordering=FALSE, pageLength=20),
                    colnames=c("Week","Team Training Days","Days Included"))
    })
    
    showModal(modalDialog(
      title     = "Team Training Days Per Week (>10 athletes)",
      size      = "m",
      easyClose = TRUE,
      footer    = modalButton("Close"),
      DT::DTOutput("week_sessions_table")
    ))
  })
  
  # ---- Athlete attendance per week modal ----
  observeEvent(input$show_ath_sessions_modal, {
    df <- week_base(); req(nrow(df) > 0)
    
    # How many sessions each player attended each week
    att <- df %>%
      filter(!is.na(Date)) %>%
      mutate(week_num = week_to_num(Week),
             week_label = paste0("PS Wk ", week_num)) %>%
      group_by(`Player Name`, week_num, week_label) %>%
      summarise(Sessions = n_distinct(Date), .groups="drop") %>%
      arrange(week_num)
    
    # Max possible sessions per week
    max_per_week <- df %>%
      filter(!is.na(Date)) %>%
      mutate(week_num = week_to_num(Week)) %>%
      group_by(week_num) %>%
      summarise(Max = n_distinct(Date), .groups="drop")
    
    att <- att %>% left_join(max_per_week, by="week_num") %>%
      mutate(label = paste0(Sessions, "/", Max))
    
    p <- ggplot(att, aes(x=reorder(week_label, week_num),
                         y=`Player Name`,
                         fill=Sessions/Max)) +
      geom_tile(colour="white", linewidth=0.5) +
      geom_text(aes(label=label), size=3, fontface="bold", colour="white") +
      scale_fill_gradient(low="#C8E6FA", high="#001F4E", limits=c(0,1),
                          labels=scales::percent, name="Attendance") +
      labs(x=NULL, y=NULL, title="Sessions Attended per Player per Week") +
      theme_minimal(base_size=11) +
      theme(axis.text.y=element_text(size=9),
            axis.text.x=element_text(size=10, face="bold"),
            panel.grid=element_blank(),
            plot.title=element_text(face="bold", size=12, colour="#001F4E"),
            legend.position="right")
    
    n_players <- n_distinct(att$`Player Name`)
    plot_h    <- max(300, n_players * 28)
    
    showModal(modalDialog(
      title     = "Athlete Training Attendance by Week",
      size      = "l",
      easyClose = TRUE,
      footer    = modalButton("Close"),
      plotOutput("ath_attendance_plot", height=paste0(plot_h, "px"))
    ))
    output$ath_attendance_plot <- renderPlot({ p }, bg="white")
  })
  
  # Comparing Weeks - Totals plots
  ct_plot <- function(col, y_label) {
    df <- week_total_data(); req(nrow(df)>0)
    vb <- input$week_total_by; req(vb)
    week_line_plot(df, col, y_label, week_col_var(vb), vb)
  }
  output$week_tot_dist_plot   <- renderPlot({ ct_plot("Avg_Distance","Total Distance (m)")        }, bg=CARD_BG)
  output$week_tot_load_plot   <- renderPlot({ ct_plot("Avg_Load",    "Total Player Load (AU)")    }, bg=CARD_BG)
  output$week_tot_hml_plot    <- renderPlot({ ct_plot("Avg_HML",     "Total HML Distance (m)")    }, bg=CARD_BG)
  output$week_tot_hsd_plot    <- renderPlot({ ct_plot("Avg_HSD",     "Total HSD (m)")             }, bg=CARD_BG)
  output$week_tot_vhsd_plot   <- renderPlot({ ct_plot("Avg_VHSD",    "Total VHSD (m)")            }, bg=CARD_BG)
  output$week_tot_maxvel_plot <- renderPlot({ ct_plot("Avg_MaxVel",  "Peak Max Velocity (m/s)")   }, bg=CARD_BG)
  output$week_tot_mmin_plot   <- renderPlot({ ct_plot("Avg_MMin",    "Avg m/min")                 }, bg=CARD_BG)
  
  # Individual Week plots
  iw_plot <- function(col, y_label) {
    df <- indiv_week_data(); req(nrow(df)>0)
    vb <- input$indiv_week_by; req(vb)
    week_line_plot(df, col, y_label, week_col_var(vb), vb, "day_label", "day_num")
  }
  output$indiv_dist_plot   <- renderPlot({ iw_plot("Avg_Distance","Distance (m)")       }, bg=CARD_BG)
  output$indiv_accel_plot  <- renderPlot({ iw_plot("Avg_Accels",  "Accel Count")        }, bg=CARD_BG)
  output$indiv_hml_plot    <- renderPlot({ iw_plot("Avg_HML",     "HML Distance (m)")   }, bg=CARD_BG)
  output$indiv_hsd_plot    <- renderPlot({ iw_plot("Avg_HSD",     "HSD (m)")            }, bg=CARD_BG)
  output$indiv_vhsd_plot   <- renderPlot({ iw_plot("Avg_VHSD",    "VHSD (m)")           }, bg=CARD_BG)
  output$indiv_maxvel_plot <- renderPlot({ iw_plot("Avg_MaxVel",  "Max Velocity (m/s)") }, bg=CARD_BG)
  output$indiv_mmin_plot   <- renderPlot({ iw_plot("Avg_MMin",    "m/min")              }, bg=CARD_BG)
  
  # ---- Comparing Athletes ----
  
  # Populate filter dropdowns and initialise selectize with all players on load
  observe({
    df <- week_base(); req(nrow(df) > 0)
    positions    <- sort(unique(df$Position_Name[!is.na(df$Position_Name)]))
    filt_choices <- c("Select...","All Players", "Backs", "Forwards", positions)
    all_players  <- sort(unique(df$`Player Name`))
    updateSelectInput(session,    "ath_avg_filter", choices=filt_choices, selected="Select...")
    updateSelectInput(session,    "ath_tot_filter", choices=filt_choices, selected="Select...")
    updateSelectizeInput(session, "ath_avg_custom", choices=all_players, selected=character(0), server=TRUE)
    updateSelectizeInput(session, "ath_tot_custom", choices=all_players, selected=character(0), server=TRUE)
  })
  
  # Helper: get the player list for a given filter value
  players_for_filter <- function(filter_val) {
    if (filter_val == "Select...") return(character(0))
    df <- week_base(); req(nrow(df) > 0)
    if      (filter_val == "All Players") sort(unique(df$`Player Name`))
    else if (filter_val == "Backs")       sort(unique(df$`Player Name`[df$Group == "Back"]))
    else if (filter_val == "Forwards")    sort(unique(df$`Player Name`[df$Group == "Forward"]))
    else sort(unique(df$`Player Name`[!is.na(df$Position_Name) & df$Position_Name == filter_val]))
  }
  
  # Auto-populate selectize whenever the filter dropdown changes
  observeEvent(input$ath_avg_filter, {
    req(input$ath_avg_filter)
    updateSelectizeInput(session, "ath_avg_custom", selected=players_for_filter(input$ath_avg_filter))
  })
  observeEvent(input$ath_tot_filter, {
    req(input$ath_tot_filter)
    updateSelectizeInput(session, "ath_tot_custom", selected=players_for_filter(input$ath_tot_filter))
  })
  
  # Athlete averages: filter to whatever is currently in the selectize
  ath_avg_data <- reactive({
    df <- week_base(); req(nrow(df) > 0)
    players <- input$ath_avg_custom
    req(length(players) > 0)
    df <- df %>% filter(`Player Name` %in% players)
    req(nrow(df) > 0)
    week_agg(df, c("week_num", "week_label", "Player Name")) %>% arrange(week_num)
  })
  
  # Athlete totals: filter to whatever is currently in the selectize
  ath_tot_data <- reactive({
    df <- week_base(); req(nrow(df) > 0)
    players <- input$ath_tot_custom
    req(length(players) > 0)
    df <- df %>% filter(`Player Name` %in% players)
    req(nrow(df) > 0)
    daily <- week_agg(df, c("week_num", "week_label", "day_num", "day_label", "Player Name"))
    daily %>%
      group_by(week_num, week_label, `Player Name`) %>%
      summarise(
        Avg_Distance = sum(Avg_Distance, na.rm=TRUE),
        Avg_Load     = sum(Avg_Load,     na.rm=TRUE),
        Avg_HML      = sum(Avg_HML,      na.rm=TRUE),
        Avg_HSD      = sum(Avg_HSD,      na.rm=TRUE),
        Avg_VHSD     = sum(Avg_VHSD,     na.rm=TRUE),
        Avg_MaxVel   = max(Avg_MaxVel,   na.rm=TRUE),
        Avg_MMin     = mean(Avg_MMin,    na.rm=TRUE),
        Avg_Accels   = sum(Avg_Accels,   na.rm=TRUE),
        .groups="drop") %>%
      arrange(week_num)
  })

  # One line per player, coloured by player name, with hover tooltip
  ath_line_plot <- function(df, col, y_label) {
    validate(need(nrow(df) > 0,
                  "Select at least one athlete above to see the comparison."))
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x=reorder(week_label, week_num),
                        y=.data[[col]],
                        colour=`Player Name`,
                        group=`Player Name`,
                        text=paste0("<b>", `Player Name`, "</b><br>",
                                    week_label, "<br>",
                                    y_label, ": ", round(.data[[col]], 1)))) +
      geom_line(linewidth=1, alpha=0.85) +
      geom_point(size=2.5) +
      scale_y_continuous(labels=comma, expand=expansion(mult=c(0.05, 0.15))) +
      labs(x=NULL, y=y_label) +
      theme_waratahs(base_size=11) +
      theme(legend.position="right", axis.text.x=element_text(size=9, face="bold")) +
      guides(colour=guide_legend(ncol=1, title=NULL, override.aes=list(linewidth=2)))
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="v", x=1.02, y=0.5)) %>%
      config(displayModeBar=FALSE)
  }
  
  # Comparing Athletes - Averages plots
  empty_plotly_msg <- function(msg) {
    plot_ly() %>%
      layout(
        xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
        paper_bgcolor=CARD_BG, plot_bgcolor=CARD_BG,
        annotations=list(list(text=msg, xref="paper", yref="paper",
                              x=0.5, y=0.5, showarrow=FALSE,
                              font=list(color=GREY_MID, size=14)))
      ) %>% config(displayModeBar=FALSE)
  }
  
  aa_plot <- function(col, y_label) {
    players <- input$ath_avg_custom
    if (is.null(players) || length(players) == 0)
      return(empty_plotly_msg("Select at least one athlete above to see the comparison."))
    df <- ath_avg_data(); req(nrow(df)>0); ath_line_plot(df, col, y_label)
  }
  output$ath_avg_dist_plot   <- renderPlotly({ aa_plot("Avg_Distance", "Distance (m)")       })
  output$ath_avg_load_plot   <- renderPlotly({ aa_plot("Avg_Load",     "Player Load (AU)")   })
  output$ath_avg_hml_plot    <- renderPlotly({ aa_plot("Avg_HML",      "HML Distance (m)")   })
  output$ath_avg_hsd_plot    <- renderPlotly({ aa_plot("Avg_HSD",      "HSD (m)")            })
  output$ath_avg_vhsd_plot   <- renderPlotly({ aa_plot("Avg_VHSD",     "VHSD (m)")           })
  output$ath_avg_maxvel_plot <- renderPlotly({ aa_plot("Avg_MaxVel",   "Max Velocity (m/s)") })
  output$ath_avg_mmin_plot   <- renderPlotly({ aa_plot("Avg_MMin",     "m/min")              })
  
  # Comparing Athletes - Totals plots
  at_plot <- function(col, y_label) {
    players <- input$ath_tot_custom
    if (is.null(players) || length(players) == 0)
      return(empty_plotly_msg("Select at least one athlete above to see the comparison."))
    df <- ath_tot_data(); req(nrow(df)>0); ath_line_plot(df, col, y_label)
  }
  output$ath_tot_dist_plot   <- renderPlotly({ at_plot("Avg_Distance", "Total Distance (m)")       })
  output$ath_tot_load_plot   <- renderPlotly({ at_plot("Avg_Load",     "Total Player Load (AU)")   })
  output$ath_tot_hml_plot    <- renderPlotly({ at_plot("Avg_HML",      "Total HML Distance (m)")   })
  output$ath_tot_hsd_plot    <- renderPlotly({ at_plot("Avg_HSD",      "Total HSD (m)")            })
  output$ath_tot_vhsd_plot   <- renderPlotly({ at_plot("Avg_VHSD",     "Total VHSD (m)")           })
  output$ath_tot_maxvel_plot <- renderPlotly({ at_plot("Avg_MaxVel",   "Peak Max Velocity (m/s)")  })
  output$ath_tot_mmin_plot   <- renderPlotly({ at_plot("Avg_MMin",     "Avg m/min")                })
  
  # PREDICTIONS — Recovering Athletes ========================
  recover_metrics <- data.frame(
    Metric    = c("Max Acceleration","Acceleration Efforts","Distance",
                  "Player Load","Maximum Velocity",
                  "High Speed Distance","Very High Speed Distance","HML Distance"),
    col_max25 = c("25max_acceleration","25acceleration_efforts","25average_distance",
                  "25average_player_load","25maximum_velocity",
                  "25highspeed_distance","25veryhighspeed_distance","25hmld"),
    col_avg25 = c("25avg_max_acceleration","25avgacceleration_efforts","25avgaverage_distance",
                  "25avgaverage_player_load","25avgmaximum_velocity",
                  "25avghighspeed_distance","25avgveryhighspeed_distance","25avghmld"),
    col_max26 = c("26max_acceleration","26acceleration_efforts","26average_distance",
                  "26average_player_load","26maximum_velocity",
                  "26highspeed_distance","26veryhighspeed_distance","26hmld"),
    col_alldata = c("Max Acceleration",
                    "Acceleration B1-3 Average Efforts (Session) (Gen 2)",
                    "Average Distance (Session)",
                    "Average Player Load (Session)",
                    "Maximum Velocity",
                    "High Speed Distance",
                    "Velocity Band 6 Average Distance (Session)",
                    "High Metabolic Load Distance"),
    Units     = c("m/s\u00b2","efforts","m","AU","m/s","m","m","m"),
    stringsAsFactors = FALSE
  )
  
  
  # Position-average fallback goals for players with no 25-season data
  pos_avg_goals <- {
    pw <- Players_W
    lapply(c("forward","back"), function(pos) {
      sub <- pw[!is.na(pw$Forward_Back) & tolower(trimws(pw$Forward_Back))==pos, ]
      setNames(
        sapply(recover_metrics$col_max25, function(col) mean(as.numeric(sub[[col]]), na.rm=TRUE)),
        recover_metrics$col_max25
      )
    }) %>% setNames(c("forward","back"))
  }
  
  # Helper: compute group-level goal (Forwards/Backs avg or peak) from Players_W
  group_goal <- function(fb, col, fn = max) {
    sub <- Players_W[!is.na(Players_W$Forward_Back) & tolower(trimws(Players_W$Forward_Back)) == fb, ]
    if (nrow(sub) == 0) return(NA_real_)
    vals <- suppressWarnings(as.numeric(sub[[col]]))
    vals <- vals[!is.na(vals) & !is.infinite(vals)]
    if (length(vals) == 0) NA_real_ else fn(vals)
  }
  
  # Helper: compute position-level goal (hooker, wing, etc. avg or peak) from Players_W
  pos_goal <- function(pos, col, fn = max) {
    sub <- Players_W[!is.na(Players_W$Position_Name) & tolower(trimws(Players_W$Position_Name)) == tolower(pos), ]
    if (nrow(sub) == 0) return(NA_real_)
    vals <- suppressWarnings(as.numeric(sub[[col]]))
    vals <- vals[!is.na(vals) & !is.infinite(vals)]
    if (length(vals) == 0) NA_real_ else fn(vals)
  }
  
  # Resolve goal for a player row + metric, given a goal_type string
  # goal_type: "25_avg","25_peaks","25_group_avg","25_group_peak","25_pos_avg","25_pos_peak",
  #            "26_peaks","26_group_peak","26_pos_peak"
  get_goal_for_type <- function(pr, metric_idx, goal_type = "25_peaks") {
    if (nrow(pr) == 0) return(list(goal=NA_real_, is_pos_avg=FALSE))
    fb  <- tolower(trimws(as.character(pr$Forward_Back)))
    fb  <- if (length(fb)==1  && !is.na(fb)  && nchar(fb)>0)  fb  else NA_character_
    pos <- tolower(trimws(as.character(pr$Position_Name)))
    pos <- if (length(pos)==1 && !is.na(pos) && nchar(pos)>0) pos else NA_character_
    
    goal <- switch(goal_type,
                   "25_avg"        = suppressWarnings(as.numeric(pr[[recover_metrics$col_avg25[metric_idx]]])),
                   "25_peaks"      = suppressWarnings(as.numeric(pr[[recover_metrics$col_max25[metric_idx]]])),
                   "26_peaks"      = suppressWarnings(as.numeric(pr[[recover_metrics$col_max26[metric_idx]]])),
                   "25_group_avg"  = if (!is.na(fb))  group_goal(fb,  recover_metrics$col_avg25[metric_idx], mean) else NA_real_,
                   "25_group_peak" = if (!is.na(fb))  group_goal(fb,  recover_metrics$col_max25[metric_idx], max)  else NA_real_,
                   "26_group_peak" = if (!is.na(fb))  group_goal(fb,  recover_metrics$col_max26[metric_idx], max)  else NA_real_,
                   "25_pos_avg"    = if (!is.na(pos)) pos_goal(pos,   recover_metrics$col_avg25[metric_idx], mean) else NA_real_,
                   "25_pos_peak"   = if (!is.na(pos)) pos_goal(pos,   recover_metrics$col_max25[metric_idx], max)  else NA_real_,
                   "26_pos_peak"   = if (!is.na(pos)) pos_goal(pos,   recover_metrics$col_max26[metric_idx], max)  else NA_real_,
                   suppressWarnings(as.numeric(pr[[recover_metrics$col_max25[metric_idx]]]))
    )
    is_pa <- goal_type %in% c("25_group_avg","25_group_peak","26_group_peak",
                              "25_pos_avg",  "25_pos_peak",  "26_pos_peak")
    
    # Fallback to position average if still NA (only for individual goal types)
    if (is.na(goal) && !is_pa && !is.na(fb) && fb %in% names(pos_avg_goals)) {
      g <- pos_avg_goals[[fb]][recover_metrics$col_max25[metric_idx]]
      if (!is.na(g)) { goal <- g; is_pa <- TRUE }
    }
    list(goal = if (!is.null(goal) && !is.na(goal) && !is.nan(goal) && !is.infinite(goal)) goal else NA_real_,
         is_pos_avg = is_pa)
  }
  
  # Backwards-compatible wrapper used by recovering athletes (always uses '25 peaks)
  get_goal_with_fallback <- function(pr, metric_idx) get_goal_for_type(pr, metric_idx, "25_peaks")
  
  output$recover_table <- renderDT({
    req(input$recover_athlete)
    player_row <- Players_W[Players_W$Name == input$recover_athlete, , drop=FALSE]
    req(nrow(player_row) > 0)
    
    get_val <- function(col) {
      if (col %in% names(player_row)) as.numeric(player_row[[col]]) else NA_real_
    }
    
    # Compute current season max live from All_Data
    player_sessions26 <- All_Data %>%
      filter(`Player Name` == input$recover_athlete,
             `Period Number` == 0,
             trimws(tolower(`Period Name`)) == "session")
    
    tbl <- recover_metrics
    tbl$`Last Season Max` <- sapply(tbl$col_max25, get_val)
    tbl$`Last Season Avg` <- sapply(tbl$col_avg25, get_val)
    tbl$`Current Max` <- sapply(tbl$col_alldata, function(col) {
      if (nrow(player_sessions26) == 0 || !col %in% names(player_sessions26)) return(NA_real_)
      v <- as.numeric(player_sessions26[[col]])
      if (all(is.na(v))) NA_real_ else max(v, na.rm=TRUE)
    })
    
    tbl$Status <- dplyr::case_when(
      is.na(tbl$`Current Max`)                                                          ~ "no_data",
      !is.na(tbl$`Last Season Max`) & tbl$`Current Max` > tbl$`Last Season Max`        ~ "above_max",
      !is.na(tbl$`Last Season Avg`) & tbl$`Current Max` > tbl$`Last Season Avg`        ~ "above_avg",
      TRUE                                                                               ~ "below_avg"
    )
    
    display <- tbl %>%
      dplyr::mutate(
        `Last Season Max` = round(`Last Season Max`, 2),
        `Last Season Avg` = round(`Last Season Avg`, 2),
        `Current Max`     = round(`Current Max`,     2)
      ) %>%
      dplyr::select(Metric, Units, `Last Season Max`, `Last Season Avg`, `Current Max`, Status)
    
    datatable(
      display,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength = -1, dom = "t", scrollX = FALSE,
        columnDefs = list(
          list(visible=FALSE, targets=5),
          list(className="dt-center", targets=c(1,2,3,4))
        )
      ),
      class = "compact stripe"
    ) %>%
      formatStyle(
        "Current Max",
        valueColumns  = "Status",
        backgroundColor = styleEqual(
          c("above_max","above_avg","below_avg","no_data"),
          c("#BBDEFB",  "#C8E6C9",  "#FFCDD2",  "#F5F5F5")
        ),
        color = styleEqual(
          c("above_max","above_avg","below_avg","no_data"),
          c("#0D47A1",  "#1B5E20",  "#B71C1C",  "#9E9E9E")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(c("Metric","Units","Last Season Max","Last Season Avg"), color="#001F4E") %>%
      formatStyle("Metric", fontWeight="700")
  })
  
  output$recover_pred_table <- renderDT({
    req(input$recover_athlete)
    player_row <- Players_W[Players_W$Name == input$recover_athlete, , drop=FALSE]
    req(nrow(player_row) > 0)
    
    wk_dates     <- as.Date(c("2026-04-20","2026-04-27","2026-05-04","2026-05-11","2026-05-18","2026-05-25"))
    n_weeks_rec  <- 6L
    weeks        <- seq_len(n_weeks_rec)
    wk_cols      <- c(format(wk_dates[1:5], "%b %d"), "Goal May 25")
    
    # Most recent session from All_Data for live current values
    latest_session <- All_Data %>%
      filter(`Player Name` == input$recover_athlete,
             `Period Number` == 0,
             trimws(tolower(`Period Name`)) == "session") %>%
      arrange(desc(Date)) %>%
      slice(1)
    
    rows <- lapply(seq_len(nrow(recover_metrics)), function(i) {
      # Current: most recent session value; fallback to Players_W season max
      if (nrow(latest_session) > 0 && recover_metrics$col_alldata[i] %in% names(latest_session)) {
        cur <- as.numeric(latest_session[[recover_metrics$col_alldata[i]]])
      } else {
        cur <- as.numeric(player_row[[recover_metrics$col_max26[i]]])
      }
      goal <- as.numeric(player_row[[recover_metrics$col_max25[i]]])
      
      cur_display <- if (is.na(cur)) NA_real_ else round(cur, 2)
      if (is.na(cur) || is.na(goal)) {
        vals   <- rep(NA_real_, n_weeks_rec)
        status <- "no_data"
        goal_display <- goal
      } else {
        cur_pred <- if (cur <= 0) 2 / MAX_PROG_RATE else cur  # seed zeros: week 1 = 2.0m
        max_reachable <- cur_pred * (MAX_PROG_RATE ^ n_weeks_rec)
        adjusted      <- goal > max_reachable
        if (adjusted) {
          goal_display <- round(max_reachable, 2)
          vals         <- round(cur_pred * (MAX_PROG_RATE ^ weeks), 2)
          status       <- "adjusted"
        } else if (cur_pred >= goal) {
          goal_display <- round(goal, 2)
          vals         <- round(rep(cur_pred, n_weeks_rec), 2)
          status       <- "exceeded"
        } else {
          goal_display <- round(goal, 2)
          vals         <- round(cur_pred + (goal - cur_pred) * (weeks / n_weeks_rec), 2)
          status       <- "on_track"
        }
      }
      
      # Replace the last value with the (possibly adjusted) goal
      vals[n_weeks_rec] <- goal_display
      
      row_df <- data.frame(
        Metric          = recover_metrics$Metric[i],
        Units           = recover_metrics$Units[i],
        Current         = if (is.na(cur_display)) NA_real_ else round(cur_display, 2),
        stringsAsFactors = FALSE
      )
      wk_df <- setNames(as.data.frame(t(vals)), wk_cols)
      meta_df <- data.frame(
        last_season_max = if (is.na(goal)) NA_real_ else round(goal, 2),
        status          = status,
        stringsAsFactors = FALSE
      )
      cbind(row_df, wk_df, meta_df)
    })
    
    tbl <- do.call(rbind, rows)
    
    # Column indices (0-based for JS / DT)
    # cols: 0=Metric,1=Units,2=Current,3-7=wk dates,8=Goal May 25,9=last_season_max,10=status
    goal_idx   <- 8L
    status_idx <- 10L
    
    row_cb <- JS(sprintf(
      "function(row, data, index) {
         var st = data[%d];
         if (st === 'adjusted') {
           $('td', row).css('background-color', '#FFF8F0');
           $('td:eq(%d)', row).css({'background-color':'#FFE0B2','color':'#BF360C','font-weight':'bold'});
         } else if (st === 'exceeded') {
           $('td:eq(%d)', row).css({'background-color':'#BBDEFB','color':'#0D47A1','font-weight':'bold'});
         } else if (st === 'on_track') {
           $('td:eq(%d)', row).css({'background-color':'#C8E6C9','color':'#1B5E20','font-weight':'bold'});
         }
       }",
      status_idx, goal_idx, goal_idx, goal_idx
    ))
    
    datatable(
      tbl,
      rownames  = FALSE,
      selection = "none",
      colnames  = c("Metric", "Units", "Current (Now)",
                    format(wk_dates[1:5], "%b %d"), "Goal (May 25)",
                    "Last Season Max", "Status"),
      options   = list(
        pageLength = -1, dom = "t", scrollX = TRUE,
        rowCallback = row_cb,
        columnDefs  = list(
          list(visible = FALSE, targets = as.list(c(status_idx))),
          list(className = "dt-center", targets = as.list(seq(1L, status_idx-1L)))
        )
      ),
      class = "compact stripe"
    ) %>%
      formatStyle("Metric",  fontWeight = "700", color = NAVY) %>%
      formatStyle("Current", fontWeight = "600", color = NAVY)
  })
  
  
  # PREDICTIONS — shared helpers =====================================
  GOAL_DATE_3WK <- as.Date("2026-05-04")
  GOAL_DATE_5WK <- as.Date("2026-05-25")
  
  # Reactive helpers that read the new prediction settings inputs
  pred_n_weeks <- reactive({
    sd <- input$pred_start_date; gd <- input$pred_goal_date
    if (is.null(sd) || is.null(gd) || is.na(sd) || is.na(gd) || gd <= sd) return(3L)
    max(1L, as.integer(ceiling(as.numeric(gd - sd) / 7)))
  })
  pred_goal_date <- reactive({
    gd <- input$pred_goal_date
    if (is.null(gd) || is.na(gd)) GOAL_DATE_3WK else gd
  })
  pred_start_date <- reactive({
    sd <- input$pred_start_date
    if (is.null(sd) || is.na(sd)) Sys.Date() else sd
  })
  pred_goal_type_val <- reactive({
    gt <- input$pred_goal_type
    if (is.null(gt) || !nchar(gt)) "25_peaks" else gt
  })
  pred_max_prog_val <- reactive({
    v <- input$pred_max_prog
    if (is.null(v) || is.na(v) || v < 1) 1.2 else v
  })
  
  metric_alldata_col <- c(
    "Max Acceleration"       = "Max Acceleration",
    "Acceleration Efforts"   = "Acceleration B1-3 Average Efforts (Session) (Gen 2)",
    "Average Distance"       = "Average Distance (Session)",
    "Average Player Load"    = "Average Player Load (Session)",
    "Maximum Velocity"       = "Maximum Velocity",
    "High Speed Distance"    = "High Speed Distance",
    "Very High Speed Distance" = "Velocity Band 6 Average Distance (Session)",
    "HML Distance"           = "High Metabolic Load Distance"
  )
  
  # Build prediction values for one metric row
  pred_vals_fn <- function(cur, goal, n_weeks, max_rate = MAX_PROG_RATE) {
    weeks <- seq_len(n_weeks)
    if (is.na(cur))
      return(list(vals=rep(NA_real_, n_weeks), status="no_data", goal_display=NA_real_))
    if (cur <= 0) cur <- 2 / max_rate  # seed zeros: week 1 predicts ~2m
    if (is.na(goal) || cur >= goal) {
      return(list(vals=rep(round(cur,2), n_weeks),
                  status=if(!is.na(goal) && cur>=goal) "exceeded" else "no_goal",
                  goal_display=if(!is.na(goal)) round(goal,2) else round(cur,2)))
    }
    max_reach <- cur * (max_rate^n_weeks)
    if (goal > max_reach) {
      g  <- round(max_reach, 2)
      v  <- round(pmax(cur*(max_rate^weeks), cur), 2)
      st <- "adjusted"
    } else {
      g  <- round(goal, 2)
      v  <- round(pmax(cur + (goal-cur)*(weeks/n_weeks), cur), 2)
      st <- "on_track"
    }
    v[n_weeks] <- g
    list(vals=v, status=st, goal_display=g)
  }
  
  # Populate player dropdowns
  observe({
    pw <- Players_W
    pl <- sort(pw$Name[!is.na(pw[["26max_acceleration"]]) | !is.na(pw[["26maximum_velocity"]])])
    updateSelectizeInput(session, "pred_search",  choices=c("" , pl), server=TRUE)
    updateSelectizeInput(session, "prog25_player", choices=c("", sort(pw$Name)), server=TRUE)
  })
  
  # ---- Predictions & Goals \u2014 label outputs ----
  output$pred_settings_label <- renderUI({
    gd  <- pred_goal_date()
    gt  <- pred_goal_type_val()
    mpr <- pred_max_prog_val()
    nwk <- pred_n_weeks()
    gt_lbl <- switch(gt,
                     "25_avg"       = "'25 Indiv. Avg",
                     "25_peaks"     = "'25 Indiv. Peak",
                     "25_group_avg" = "'25 Group Avg (Fwd/Back)",
                     "25_group_peak"= "'25 Group Peak (Fwd/Back)",
                     "25_pos_avg"   = "'25 Position Avg",
                     "25_pos_peak"  = "'25 Position Peak",
                     "26_peaks"     = "'26 Indiv. Peak",
                     "26_group_peak"= "'26 Group Peak (Fwd/Back)",
                     "26_pos_peak"  = "'26 Position Peak",
                     gt)
    div(style="padding-top:26px;font-size:11px;color:#8899AA;",
        paste0(nwk, "-week trajectory \u2014 Goal: ", format(gd, "%b %d"),
               " \u2014 Goal type: ", gt_lbl, " \u2014 Max rate: ", mpr, "\u00d7/wk"))
  })
  
  output$pred_goals_table_title <- renderUI({
    gd <- pred_goal_date()
    div(class="chart-card-title", paste0("Predicted Values at ", format(gd, "%b %d"), " (Goal Date)"))
  })
  
  output$pred_week_select_ui <- renderUI({
    nwk <- pred_n_weeks(); sd <- pred_start_date(); gd <- pred_goal_date()
    total_days <- as.numeric(gd - sd)
    dates <- sd + round(seq_len(nwk) / nwk * total_days)
    wk_labels <- c(
      if (nwk > 1) paste0("Wk ", seq_len(nwk-1), ": ", format(dates[seq_len(nwk-1)], "%b %d")) else character(0),
      paste0("Goal: ", format(gd, "%b %d"))
    )
    choices <- setNames(as.character(seq_len(nwk)), wk_labels)
    selectInput("pred_week_select", label="Select checkpoint:", choices=choices,
                selected=as.character(nwk), width="100%")
  })
  
  # Compact pivot: one row per player, one column per metric goal value
  cell_html_pred <- function(goal_val, status) {
    bg <- switch(status, exceeded="#DBEAFE", on_track="#DCFCE7", adjusted="#FEF3C7",
                 no_data="#F5F5F5", no_goal="#F5F5F5", "#F5F5F5")
    fg <- switch(status, exceeded="#1E40AF", on_track="#166534", adjusted="#92400E",
                 no_data="#9CA3AF", no_goal="#9CA3AF", "#6B7280")
    lbl <- if(is.na(goal_val) || status %in% c("no_data","no_goal")) "&mdash;" else as.character(round(goal_val,1))
    sprintf('<span style="background:%s;color:%s;padding:2px 6px;border-radius:3px;font-size:11px;font-weight:700;">%s</span>',
            bg, fg, lbl)
  }
  
  output$pred_goals_table <- renderDT({
    sel   <- if (!is.null(input$pred_search) && nchar(input$pred_search)>0) input$pred_search else ""
    pw    <- Players_W
    nwk   <- pred_n_weeks()
    gt    <- pred_goal_type_val()
    mpr   <- pred_max_prog_val()
    pl_list <- pw$Name[!is.na(pw[["26max_acceleration"]]) | !is.na(pw[["26maximum_velocity"]])]
    
    rows_pg <- lapply(pl_list, function(pname) {
      pr <- pw[pw$Name==pname,,drop=FALSE]
      score_pts <- 0L; known_n <- 0L
      metric_cells <- character(nrow(recover_metrics))
      for (i in seq_len(nrow(recover_metrics))) {
        cur   <- as.numeric(pr[[recover_metrics$col_max26[i]]])
        ginfo <- get_goal_for_type(pr, i, gt)
        pv    <- pred_vals_fn(cur, ginfo$goal, nwk, mpr)
        if (!pv$status %in% c("no_data","no_goal")) known_n <- known_n+1L
        if (pv$status == "exceeded")  score_pts <- score_pts+2L
        else if (pv$status == "on_track") score_pts <- score_pts+1L
        metric_cells[i] <- cell_html_pred(pv$vals[nwk], pv$status)
      }
      score_pct <- if(known_n>0L) round(score_pts/(2L*known_n)*100L) else 0L
      r <- data.frame(Player=pname, Score=score_pct, stringsAsFactors=FALSE)
      for(i in seq_len(nrow(recover_metrics))) r[[recover_metrics$Metric[i]]] <- metric_cells[i]
      r
    })
    
    tbl_pg <- do.call(rbind, rows_pg)
    if (sel != "" && sel %in% tbl_pg$Player) {
      tbl_pg <- rbind(tbl_pg[tbl_pg$Player==sel,,drop=FALSE],
                      tbl_pg[tbl_pg$Player!=sel,,drop=FALSE][order(-tbl_pg$Score[tbl_pg$Player!=sel]),,drop=FALSE])
    } else {
      tbl_pg <- tbl_pg[order(-tbl_pg$Score),,drop=FALSE]
    }
    tbl_pg$Score <- paste0(tbl_pg$Score, "%")
    
    datatable(tbl_pg, rownames=FALSE, escape=FALSE, selection="none",
              options=list(pageLength=-1, dom="t", scrollX=TRUE, ordering=FALSE,
                           columnDefs=list(list(className="dt-center", targets=as.list(seq(1L, ncol(tbl_pg)-1L))))),
              class="compact stripe") %>%
      formatStyle("Player", fontWeight="700", color=NAVY) %>%
      formatStyle("Score",  fontWeight="700")
  })
  
  output$pred_goals_detail <- renderUI({
    req(input$pred_search); sel <- input$pred_search; req(nchar(sel)>0)
    nwk <- pred_n_weeks(); mpr <- pred_max_prog_val(); gd <- pred_goal_date()
    tagList(tags$br(),
            div(class="chart-card",
                div(class="chart-card-title", paste0("Predicted Trajectory \u2014 ", sel)),
                tags$p(style="font-size:11px;color:#8899AA;margin-bottom:8px;",
                       paste0(nwk, "-week path to ", format(gd,"%b %d"), ". ",
                              "Orange = goal adjusted (", mpr, "\u00d7/wk cap). Blue = already exceeded.")),
                DTOutput("pred_goals_traj"),
                tags$br(),
                div(style="font-size:11px;font-weight:700;color:#001F4E;margin:10px 0 6px;",
                    "Actual vs Predicted \u2014 select a metric:"),
                fluidRow(
                  column(4, selectInput("pred_vs_metric", label=NULL,
                                        choices=setNames(recover_metrics$col_alldata, recover_metrics$Metric),
                                        width="100%"))
                ),
                plotlyOutput("pred_vs_actual_plot", height="320px")
            ))
  })
  
  output$pred_goals_traj <- renderDT({
    req(input$pred_search); sel <- input$pred_search; req(nchar(sel)>0)
    player_row <- Players_W[Players_W$Name==sel,,drop=FALSE]; req(nrow(player_row)>0)
    nwk <- pred_n_weeks(); sd <- pred_start_date(); gd <- pred_goal_date()
    gt  <- pred_goal_type_val(); mpr <- pred_max_prog_val()
    total_days <- as.numeric(gd - sd)
    dates <- sd + round(seq_len(nwk) / nwk * total_days)
    wk_cols <- c(
      if (nwk > 1) format(dates[seq_len(nwk-1)], "%b %d") else character(0),
      paste0("Goal (", format(gd, "%b %d"), ")")
    )
    rows_traj <- lapply(seq_len(nrow(recover_metrics)), function(i) {
      cur   <- as.numeric(player_row[[recover_metrics$col_max26[i]]])
      ginfo <- get_goal_for_type(player_row, i, gt)
      pv    <- pred_vals_fn(cur, ginfo$goal, nwk, mpr)
      row_df <- data.frame(Metric=recover_metrics$Metric[i], Units=recover_metrics$Units[i],
                           Current=if(is.na(cur)) NA_real_ else round(cur,2), stringsAsFactors=FALSE)
      wdf  <- setNames(as.data.frame(t(pv$vals)), wk_cols)
      meta <- data.frame(Goal=if(is.na(pv$goal_display)) NA_real_ else pv$goal_display,
                         Status=pv$status, stringsAsFactors=FALSE)
      cbind(row_df, wdf, meta)
    })
    tbl3 <- do.call(rbind, rows_traj)
    n_cols <- ncol(tbl3)
    status_col_0 <- n_cols - 1L  # 0-based
    goal_col_0   <- nwk + 2L     # 0-based index of last predicted week (= goal col)
    row_cb3 <- JS(sprintf(
      "function(row,data,index){
         var st=data[%d]; var gi=%d;
         if(st==='adjusted'){$('td:eq('+gi+')',row).css({'background-color':'#FFE0B2','color':'#BF360C','font-weight':'bold'});}
         else if(st==='exceeded'){$('td:eq('+gi+')',row).css({'background-color':'#BBDEFB','color':'#0D47A1','font-weight':'bold'});}
         else if(st==='on_track'){$('td:eq('+gi+')',row).css({'background-color':'#C8E6C9','color':'#1B5E20','font-weight':'bold'});}
       }", status_col_0, goal_col_0))
    datatable(tbl3, rownames=FALSE, selection="none",
              options=list(pageLength=-1, dom="t", scrollX=TRUE, rowCallback=row_cb3,
                           columnDefs=list(list(visible=FALSE, targets=status_col_0),
                                           list(className="dt-center", targets=as.list(seq_len(n_cols-1L))))),
              class="compact stripe") %>%
      formatStyle("Metric",  fontWeight="700", color=NAVY) %>%
      formatStyle("Current", fontWeight="600", color=NAVY)
  })
  
  # Actual vs Predicted plot for searched player
  output$pred_vs_actual_plot <- renderPlotly({
    req(input$pred_search, input$pred_vs_metric)
    sel     <- input$pred_search; req(nchar(sel)>0)
    col_nm  <- input$pred_vs_metric
    sd      <- pred_start_date(); gd <- pred_goal_date()
    nwk     <- pred_n_weeks()
    gt      <- pred_goal_type_val(); mpr <- pred_max_prog_val()
    player_row <- Players_W[Players_W$Name==sel,,drop=FALSE]; req(nrow(player_row)>0)
    met_idx    <- which(recover_metrics$col_alldata == col_nm)
    req(length(met_idx) > 0)
    cur   <- as.numeric(player_row[[recover_metrics$col_max26[met_idx[1]]]])
    ginfo <- get_goal_for_type(player_row, met_idx[1], gt)
    pv    <- pred_vals_fn(cur, ginfo$goal, nwk, mpr)
    total_days <- as.numeric(gd - sd)
    pred_dates <- sd + round(seq_len(nwk) / nwk * total_days)
    pred_df <- data.frame(Date=c(sd, pred_dates), Value=c(if(is.na(cur)) 0 else cur, pv$vals),
                          Type="Predicted")
    # Actual sessions for this player
    actual_df <- All_Data %>%
      filter(`Player Name` == sel, `Period Number` == 0,
             trimws(tolower(`Period Name`)) == "session",
             Date >= sd, !is.na(.data[[col_nm]])) %>%
      mutate(Value = as.numeric(.data[[col_nm]]), Type = "Actual") %>%
      select(Date, Value, Type)
    p <- plot_ly()
    p <- p %>% add_trace(data=pred_df, x=~Date, y=~Value, type="scatter", mode="lines+markers",
                         line=list(color=SKY, width=2, dash="dash"),
                         marker=list(color=SKY, size=6),
                         name="Predicted")
    if (nrow(actual_df) > 0) {
      p <- p %>% add_trace(data=actual_df, x=~Date, y=~Value, type="scatter", mode="markers",
                           marker=list(color=ACCENT, size=9, symbol="circle"),
                           name="Actual")
    }
    p %>% layout(
      title=list(text=paste0("<b>",sel,"</b> \u2014 ", recover_metrics$Metric[met_idx[1]]),
                 font=list(color=NAVY, size=13)),
      xaxis=list(title=NULL, showgrid=FALSE),
      yaxis=list(title=recover_metrics$Units[met_idx[1]], showgrid=TRUE, gridcolor="#E2EAF0"),
      paper_bgcolor=CARD_BG, plot_bgcolor=CARD_BG,
      legend=list(orientation="h", x=0, y=-0.2)
    ) %>% config(displayModeBar=FALSE)
  })
  
  # ---- On Track? summary boxes ----
  # ---- Ranking data reactive: computes current values from All_Data ----
  ranking_data <- reactive({
    view <- input$ranking_view
    if (is.null(view)) view <- "latest"
    
    sessions <- All_Data %>%
      filter(`Period Number` == 0, trimws(tolower(`Period Name`)) == "session")
    
    am <- recover_metrics$col_alldata  # All_Data column names
    
    agg_fn <- function(dat, fn) {
      result <- dat %>%
        group_by(`Player Name`) %>%
        summarise(across(any_of(am), ~{
          v <- suppressWarnings(as.numeric(.x))
          fn(v[!is.na(v)])
        }), .groups = "drop")
      # Fill in any columns that didn't exist in the data
      for (col in am) {
        if (!col %in% names(result)) result[[col]] <- NA_real_
      }
      result
    }
    
    if (view == "latest") {
      latest_date <- max(sessions$Date, na.rm = TRUE)
      dat   <- sessions %>% filter(Date == latest_date)
      label <- paste0("Latest session: ", format(latest_date, "%d %b %Y"))
      cur_vals <- agg_fn(dat, function(v) if (length(v)==0) NA_real_ else mean(v))
      
    } else if (view == "latest_week") {
      wk_s <- sessions %>% mutate(wn = week_to_num(Week)) %>% filter(!is.na(wn))
      latest_wk <- max(wk_s$wn, na.rm = TRUE)
      dat   <- wk_s %>% filter(wn == latest_wk)
      wk_label <- unique(dat$Week)[1]
      label <- paste0("Week ", latest_wk, " (", stringr::str_to_title(wk_label), "): session average")
      cur_vals <- agg_fn(dat, function(v) if (length(v)==0) NA_real_ else mean(v))
      
    } else if (view == "season_avg") {
      label    <- paste0("2026 season average (", nrow(sessions %>% distinct(Date)), " sessions)")
      cur_vals <- agg_fn(sessions, function(v) if (length(v)==0) NA_real_ else mean(v))
      
    } else {  # season_max
      n_dates <- nrow(sessions %>% distinct(Date))
      label    <- paste0("2026 season best session (", n_dates, " sessions)")
      cur_vals <- agg_fn(sessions, function(v) if (length(v)==0) NA_real_ else max(v))
    }
    
    list(cur_vals = cur_vals, label = label)
  })
  
  output$ranking_date_label <- renderUI({
    rd <- ranking_data()
    div(style = "padding-top:28px;font-size:11px;color:#8899AA;font-style:italic;",
        rd$label)
  })
  
  player_status_summary <- reactive({
    pw  <- Players_W
    rd  <- ranking_data()
    cv  <- rd$cur_vals  # Player Name + col_alldata columns
    
    players <- sort(unique(cv$`Player Name`))
    lapply(players, function(pname) {
      pr      <- pw[pw$Name == pname, , drop=FALSE]
      cv_row  <- cv[cv$`Player Name` == pname, , drop=FALSE]
      ot_gt <- if (!is.null(input$ontrack_goal_type)) input$ontrack_goal_type else "25_peaks"
      counts <- sapply(seq_len(nrow(recover_metrics)), function(i) {
        cur  <- if (nrow(cv_row)>0) suppressWarnings(as.numeric(cv_row[[recover_metrics$col_alldata[i]]])) else NA_real_
        ginfo <- get_goal_for_type(pr, i, ot_gt)
        mmax  <- ginfo$goal
        mavg  <- if (nrow(pr)>0) as.numeric(pr[[recover_metrics$col_avg25[i]]]) else NA_real_
        if (is.na(cur) || is.na(mmax)) return("no_data")
        if (cur > mmax) "exceeded"
        else if (!is.na(mavg) && cur > mavg) "above_avg"
        else "below_avg"
      })
      known <- counts[counts != "no_data"]
      n     <- length(known)
      if (n < 3) return(data.frame(Player=pname, Category="Insufficient Data", stringsAsFactors=FALSE))
      exc   <- sum(known=="exceeded")
      blw   <- sum(known=="below_avg")
      cat   <- if (exc/n >= 0.6)                              "Way Ahead"
      else if (blw/n >= 0.6)                        "Way Behind"
      else if ((exc+sum(known=="above_avg"))/n >= 0.5) "On Track"
      else                                           "Mixed"
      data.frame(Player=pname, Category=cat, stringsAsFactors=FALSE)
    }) %>% do.call(rbind, .)
  })
  
  # ---- Predictions & Goals – per-week detail table ────────────────────────────
  output$pred_week_table <- renderDT({
    nwk    <- pred_n_weeks()
    gt     <- pred_goal_type_val()
    mpr    <- pred_max_prog_val()
    wk_idx <- as.integer(input$pred_week_select)
    if (is.na(wk_idx) || wk_idx < 1L || wk_idx > nwk) wk_idx <- nwk
    pw     <- Players_W
    pl_list <- sort(pw$Name[!is.na(pw[["26max_acceleration"]]) | !is.na(pw[["26maximum_velocity"]])])
    
    rows_wk <- lapply(pl_list, function(pname) {
      pr <- pw[pw$Name==pname,,drop=FALSE]
      r  <- data.frame(Player=pname, stringsAsFactors=FALSE)
      for (i in seq_len(nrow(recover_metrics))) {
        cur   <- as.numeric(pr[[recover_metrics$col_max26[i]]])
        ginfo <- get_goal_for_type(pr, i, gt)
        pv    <- pred_vals_fn(cur, ginfo$goal, nwk, mpr)
        val   <- if (is.na(pv$vals[wk_idx])) NA_real_ else round(pv$vals[wk_idx], 1)
        r[[paste0(recover_metrics$Metric[i], "_val")]]  <- val
        r[[paste0(recover_metrics$Metric[i], "_st")]]   <- pv$status
      }
      r
    })
    tbl_wk <- do.call(rbind, rows_wk)
    
    # Build display: Player + 8 metric columns (HTML coloured cells)
    disp <- data.frame(Player=tbl_wk$Player, stringsAsFactors=FALSE)
    for (i in seq_len(nrow(recover_metrics))) {
      m    <- recover_metrics$Metric[i]
      vals <- tbl_wk[[paste0(m,"_val")]]
      sts  <- tbl_wk[[paste0(m,"_st")]]
      disp[[m]] <- mapply(cell_html_pred, vals, sts, USE.NAMES=FALSE)
    }
    
    datatable(disp, rownames=FALSE, escape=FALSE, selection="none",
              options=list(pageLength=-1, dom="t", scrollX=TRUE, ordering=FALSE,
                           columnDefs=list(list(className="dt-center",
                                                targets=as.list(seq(1L, ncol(disp)-1L))))),
              class="compact stripe") %>%
      formatStyle("Player", fontWeight="700", color=NAVY)
  })
  
  output$on_track_boxes <- renderUI({
    df  <- player_status_summary()
    mk  <- function(cat, bg, fg, icon_char) {
      pl  <- df$Player[df$Category==cat]
      div(style=paste0("background:",bg,";border-radius:10px;padding:16px 20px;flex:1 1 200px;",
                       "min-width:180px;box-shadow:0 2px 8px rgba(0,31,78,0.10);"),
          div(style=paste0("font-size:12px;font-weight:800;color:",fg,";text-transform:uppercase;",
                           "letter-spacing:0.08em;margin-bottom:10px;"),
              icon_char, " ", cat, " (", length(pl), ")"),
          if (length(pl)==0)
            div(style=paste0("font-size:11px;color:",fg,";opacity:0.7;"),"—")
          else
            tagList(lapply(sort(pl), function(p)
              div(style=paste0("font-size:12px;color:",fg,";font-weight:600;",
                               "padding:2px 0;border-bottom:1px solid rgba(0,0,0,0.06);"), ext_mark(p))))
      )
    }
    div(style="display:flex;gap:16px;flex-wrap:wrap;margin-bottom:18px;",
        mk("Way Ahead",  "#EBF5FB", "#0D47A1", "\u2b06"),
        mk("On Track",   "#EBF9EE", "#1B5E20", "\u2714"),
        mk("Mixed",      "#FFFDE7", "#7B5800", "\u21C4"),
        mk("Way Behind", "#FFF3F0", "#B71C1C", "\u2b07")
    )
  })
  
  # ---- On Track? goal progress ranking ----
  output$on_track_ranking <- renderDT({
    pw  <- Players_W
    rd  <- ranking_data()
    cv  <- rd$cur_vals
    players_rk <- sort(unique(cv$`Player Name`))
    
    cell_rk <- function(cur_val, st) {
      bg <- switch(st, exceeded="#DBEAFE", above_avg="#DCFCE7", below_avg="#FEE2E2", "#F3F4F6")
      fg <- switch(st, exceeded="#1E40AF", above_avg="#166534", below_avg="#991B1B", "#9CA3AF")
      lbl <- if(st=="no_data"||is.na(cur_val)) "—"
      else if(cur_val>=100) formatC(round(cur_val,0), format="f", digits=0, big.mark=",")
      else as.character(round(cur_val,1))
      sprintf('<div style="background:%s;color:%s;padding:2px 4px;border-radius:3px;font-size:10px;font-weight:600;text-align:center;">%s</div>', bg, fg, lbl)
    }
    
    ot_gt_rk <- if (!is.null(input$ontrack_goal_type)) input$ontrack_goal_type else "25_peaks"
    rows_rk <- lapply(players_rk, function(pname) {
      pr     <- pw[pw$Name == pname, , drop=FALSE]
      cv_row <- cv[cv$`Player Name` == pname, , drop=FALSE]
      score_pts <- 0L; known_n <- 0L; has_pa <- FALSE
      metric_statuses <- character(nrow(recover_metrics))
      metric_vals     <- numeric(nrow(recover_metrics))
      for (i in seq_len(nrow(recover_metrics))) {
        cur   <- if (nrow(cv_row)>0) suppressWarnings(as.numeric(cv_row[[recover_metrics$col_alldata[i]]])) else NA_real_
        mavg  <- if (nrow(pr)>0) as.numeric(pr[[recover_metrics$col_avg25[i]]]) else NA_real_
        ginfo <- get_goal_for_type(pr, i, ot_gt_rk)
        mmax  <- ginfo$goal
        if (ginfo$is_pos_avg) has_pa <- TRUE
        metric_vals[i] <- if (is.na(cur)) NA_real_ else cur
        if (is.na(cur) || is.na(mmax)) {
          metric_statuses[i] <- "no_data"
        } else {
          known_n <- known_n + 1L
          if (cur > mmax)                     { score_pts <- score_pts+2L; metric_statuses[i] <- "exceeded"  }
          else if (!is.na(mavg)&&cur > mavg)  { score_pts <- score_pts+1L; metric_statuses[i] <- "above_avg" }
          else                                {                             metric_statuses[i] <- "below_avg" }
        }
      }
      score_pct  <- if(known_n>0L) round(score_pts/(2L*known_n)*100L) else 0L
      player_lbl <- ext_mark(if(has_pa) paste0(pname," *") else pname)
      r <- data.frame(Player=player_lbl, Score=score_pct, stringsAsFactors=FALSE)
      for(i in seq_len(nrow(recover_metrics))) r[[recover_metrics$Metric[i]]] <- cell_rk(metric_vals[i], metric_statuses[i])
      r
    })
    
    tbl_rk <- do.call(rbind, rows_rk)
    tbl_rk <- tbl_rk[order(-tbl_rk$Score),,drop=FALSE]
    tbl_rk$Rank <- seq_len(nrow(tbl_rk))
    tbl_rk <- tbl_rk[, c("Rank","Player","Score", recover_metrics$Metric), drop=FALSE]
    tbl_rk$Score <- paste0(tbl_rk$Score, "%")
    
    datatable(tbl_rk, rownames=FALSE, escape=FALSE, selection="none",
              options=list(pageLength=-1, dom="t", scrollX=TRUE, ordering=FALSE,
                           columnDefs=list(
                             list(width="40px", targets=0L),
                             list(className="dt-center", targets=as.list(c(0L, seq(2L, ncol(tbl_rk)-1L))))
                           )),
              class="compact stripe") %>%
      formatStyle("Player", fontWeight="700", color=NAVY, textAlign="left") %>%
      formatStyle("Score",  fontWeight="700")
  })
  
  # ---- Progression Comparison (25 vs 26) ----
  output$prog25_plot <- renderPlotly({
    req(input$prog25_metric)
    sel_player <- if(!is.null(input$prog25_player) && nchar(input$prog25_player)>0) input$prog25_player else ""
    metric_name <- input$prog25_metric
    col_name    <- unname(metric_alldata_col[metric_name])
    req(!is.na(col_name), col_name %in% names(All_Data25))
    if (sel_player == "") {
      return(empty_plotly_msg("Select at least one athlete above to see the comparison."))
    }
    
    # 25 season for this player
    s25 <- All_Data25 %>%
      filter(`Period Number`==0, `Period Name`=="Session", `Player Name`==sel_player, !is.na(.data[[col_name]])) %>%
      select(Date, Value=all_of(col_name), Week) %>%
      mutate(WeekNum=week_to_num(Week)) %>%
      filter(!is.na(WeekNum)) %>%
      arrange(WeekNum) %>%
      group_by(WeekNum) %>% summarise(Value=mean(Value,na.rm=TRUE), .groups="drop")
    
    # 26 season for this player
    s26 <- All_Data %>%
      filter(`Period Number`==0, `Player Name`==sel_player, !is.na(.data[[col_name]])) %>%
      select(Date, Value=all_of(col_name), Week) %>%
      mutate(WeekNum=week_to_num(Week)) %>%
      filter(!is.na(WeekNum)) %>%
      arrange(WeekNum) %>%
      group_by(WeekNum) %>% summarise(Value=mean(Value,na.rm=TRUE), .groups="drop")
    
    p <- plot_ly()
    if (nrow(s25)>0)
      p <- p %>% add_trace(x=s25$WeekNum, y=round(s25$Value,2), type="scatter", mode="lines+markers",
                           line=list(color=NAVY, width=2.5), marker=list(color=NAVY, size=7),
                           name="2025 Season", text=paste0("2025 | Week ",s25$WeekNum,"<br>",round(s25$Value,2)), hoverinfo="text")
    if (nrow(s26)>0)
      p <- p %>% add_trace(x=s26$WeekNum, y=round(s26$Value,2), type="scatter", mode="lines+markers",
                           line=list(color=SKY, width=2.5, dash="dash"), marker=list(color=SKY, size=7, symbol="diamond"),
                           name="2026 Season", text=paste0("2026 | Week ",s26$WeekNum,"<br>",round(s26$Value,2)), hoverinfo="text")
    
    # current season max horizontal line
    rm_row <- Players_W[Players_W$Name==sel_player,,drop=FALSE]
    i_met  <- which(recover_metrics$Metric==metric_name)
    if (length(i_met)>0) {
      cur_max <- as.numeric(rm_row[[recover_metrics$col_max26[i_met]]])
      if (!is.na(cur_max) && cur_max>0) {
        all_weeks <- c(s25$WeekNum, s26$WeekNum)
        if (length(all_weeks)>0)
          p <- p %>% add_lines(x=c(min(all_weeks), max(all_weeks)),
                               y=c(cur_max, cur_max),
                               line=list(color=ACCENT, width=1.5, dash="dash"),
                               name=paste0("Current Max (",round(cur_max,2),")"))
      }
    }
    p %>% layout(
      title=list(text=paste0("<b>",sel_player,"</b> \u2014 ",metric_name), font=list(color=NAVY,size=14)),
      xaxis=list(title="Week in Season", showgrid=FALSE, dtick=1),
      yaxis=list(title=metric_name, showgrid=TRUE, gridcolor="#E2EAF0"),
      paper_bgcolor=CARD_BG, plot_bgcolor=CARD_BG,
      legend=list(orientation="h", x=0, y=-0.15)
    ) %>% config(displayModeBar=FALSE)
  })
  
  
  
  # ---- On Track? – Weekly Progress Alerts ─────────────────────────────────
  alert_metrics_map <- c(
    "Distance"    = "Average Distance (Session)",
    "HML"         = "High Metabolic Load Distance",
    "HighSpeed"   = "High Speed Distance",
    "Load"        = "Average Player Load (Session)",
    "MaxVel"      = "Maximum Velocity",
    "MaxAccel"    = "Max Acceleration"
  )
  
  output$alerts_week_info <- renderUI({
    wd <- Weekly_Data %>% filter(trimws(tolower(`Period Name`)) != "week total") %>%
      mutate(week_num = week_to_num(Week)) %>% filter(!is.na(week_num))
    rw <- sort(unique(wd$week_num), decreasing=TRUE)
    if (length(rw) < 2) return(tags$p(style="color:#8899AA;font-size:11px;", "Need at least 2 weeks of data."))
    tags$p(style="font-size:11px;color:#8899AA;margin-bottom:10px;",
           paste0("Comparing Week ", rw[2], " (prev) vs Week ", rw[1], " (current)."))
  })
  
  output$alerts_watch_box <- renderUI({
    wd <- Weekly_Data %>%
      filter(trimws(tolower(`Period Name`)) != "week total") %>%
      mutate(week_num = week_to_num(Week)) %>% filter(!is.na(week_num))
    rw <- sort(unique(wd$week_num), decreasing=TRUE)
    if (length(rw) < 2) return(NULL)
    wk_curr <- rw[1]; wk_prev <- rw[2]
    am_cols <- unname(alert_metrics_map)
    
    avg_fn <- function(wk) {
      wd %>% filter(week_num == wk) %>%
        group_by(`Player Name`) %>%
        summarise(across(all_of(am_cols), ~mean(.x, na.rm=TRUE)), .groups="drop")
    }
    curr <- avg_fn(wk_curr); prev <- avg_fn(wk_prev)
    jn   <- inner_join(curr, prev, by="Player Name", suffix=c("_c","_p"))
    
    player_scores <- do.call(rbind, lapply(seq_len(nrow(jn)), function(ri) {
      pname <- jn$`Player Name`[ri]
      fast_n <- 0L; fast_sum <- 0; drop_n <- 0L; drop_sum <- 0
      for (nm in names(alert_metrics_map)) {
        col <- alert_metrics_map[nm]
        vc <- jn[[paste0(col,"_c")]][ri]; vp <- jn[[paste0(col,"_p")]][ri]
        if (is.na(vc)||is.na(vp)||vp<=0) next
        pct <- (vc-vp)/vp*100
        if (pct > 30)  { fast_n <- fast_n+1L; fast_sum <- fast_sum+pct }
        if (pct < -20) { drop_n <- drop_n+1L; drop_sum <- drop_sum+abs(pct) }
      }
      data.frame(Player=pname, fast_n=fast_n, fast_sum=fast_sum,
                 drop_n=drop_n, drop_sum=drop_sum, stringsAsFactors=FALSE)
    }))
    
    fast_pl <- player_scores[player_scores$fast_n > 0, ]
    fast_pl  <- fast_pl[order(-fast_pl$fast_n, -fast_pl$fast_sum), ]
    drop_pl  <- player_scores[player_scores$drop_n > 0, ]
    drop_pl  <- drop_pl[order(-drop_pl$drop_n, -drop_pl$drop_sum), ]
    
    if (nrow(fast_pl) == 0 && nrow(drop_pl) == 0) return(NULL)
    
    make_pill <- function(pname, n_met, bg, fg) {
      lbl <- if (n_met > 1) paste0(pname, " (", n_met, " metrics)") else pname
      tags$span(style=paste0("display:inline-block;margin:2px 3px;padding:3px 9px;border-radius:10px;",
                             "font-size:11px;font-weight:600;background:", bg, ";color:", fg, ";"), lbl)
    }
    
    boxes <- tagList()
    if (nrow(fast_pl) > 0) {
      boxes <- tagList(boxes,
                       div(style="flex:1 1 220px;min-width:180px;background:#FFFBEB;border-radius:10px;padding:12px 16px;box-shadow:0 2px 8px rgba(0,31,78,0.08);",
                           div(style="font-size:11px;font-weight:800;color:#92400E;text-transform:uppercase;letter-spacing:0.08em;margin-bottom:8px;",
                               "\u26a0 Too Fast \u2014 Watch"),
                           div(style="display:flex;flex-wrap:wrap;",
                               lapply(seq_len(nrow(fast_pl)), function(i)
                                 make_pill(ext_mark(fast_pl$Player[i]), fast_pl$fast_n[i], "#FEF3C7", "#92400E")))
                       ))
    }
    if (nrow(drop_pl) > 0) {
      boxes <- tagList(boxes,
                       div(style="flex:1 1 220px;min-width:180px;background:#FFF5F5;border-radius:10px;padding:12px 16px;box-shadow:0 2px 8px rgba(0,31,78,0.08);",
                           div(style="font-size:11px;font-weight:800;color:#991B1B;text-transform:uppercase;letter-spacing:0.08em;margin-bottom:8px;",
                               "\u2b07 Dropping \u2014 Watch"),
                           div(style="display:flex;flex-wrap:wrap;",
                               lapply(seq_len(nrow(drop_pl)), function(i)
                                 make_pill(ext_mark(drop_pl$Player[i]), drop_pl$drop_n[i], "#FEE2E2", "#991B1B")))
                       ))
    }
    div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:14px;", boxes)
  })
  
  output$alerts_table <- renderDT({
    wd <- Weekly_Data %>%
      filter(trimws(tolower(`Period Name`)) != "week total") %>%
      mutate(week_num = week_to_num(Week)) %>% filter(!is.na(week_num))
    rw <- sort(unique(wd$week_num), decreasing=TRUE)
    if (length(rw) < 2) {
      return(datatable(data.frame(Message="Need at least 2 weeks of data to compute alerts."),
                       rownames=FALSE, options=list(dom="t"), class="compact"))
    }
    wk_curr <- rw[1]; wk_prev <- rw[2]
    am_cols <- unname(alert_metrics_map)
    
    avg_fn <- function(wk) {
      wd %>% filter(week_num == wk) %>%
        group_by(`Player Name`) %>%
        summarise(across(all_of(am_cols), ~mean(.x, na.rm=TRUE)), .groups="drop")
    }
    curr <- avg_fn(wk_curr); prev <- avg_fn(wk_prev)
    jn   <- inner_join(curr, prev, by="Player Name", suffix=c("_c","_p"))
    
    flags <- do.call(rbind, lapply(seq_len(nrow(jn)), function(ri) {
      pname <- jn$`Player Name`[ri]
      do.call(rbind, lapply(names(alert_metrics_map), function(nm) {
        col <- alert_metrics_map[nm]
        vc  <- jn[[paste0(col,"_c")]][ri]; vp <- jn[[paste0(col,"_p")]][ri]
        if(is.na(vc)||is.na(vp)||vp<=0) return(NULL)
        pct <- (vc-vp)/vp*100
        if(abs(pct) < 20) return(NULL)
        flag_type <- if(pct > 30) "Too Fast" else if(pct < -20) "Decrease" else return(NULL)
        data.frame(Player=ext_mark(pname), Metric=nm,
                   `Prev Week`=round(vp,1), `This Week`=round(vc,1),
                   `Change %`=paste0(if(pct>0) "+" else "", round(pct,0), "%"),
                   Flag=flag_type, stringsAsFactors=FALSE, check.names=FALSE)
      }))
    }))
    
    if (is.null(flags) || nrow(flags)==0) {
      return(datatable(data.frame(Message="No alerts \u2014 all metrics within normal ranges this week."),
                       rownames=FALSE, options=list(dom="t"), class="compact"))
    }
    flags <- flags[order(flags$Flag, flags$Player), ]
    nc_f <- ncol(flags)   # 6 cols: Player Metric Prev This Change% Flag
    flag_idx <- nc_f - 1L # 0-based index of Flag column
    cb_alert <- JS(sprintf(
      "function(row,data,index){
         var fl=data[%d];
         if(fl==='Too Fast'){$('td',row).css('background-color','#FFFBEB'); $('td:eq(%d)',row).css({'background-color':'#FEF3C7','color':'#92400E','font-weight':'bold'});}
         else if(fl==='Decrease'){$('td',row).css('background-color','#FFF5F5'); $('td:eq(%d)',row).css({'background-color':'#FEE2E2','color':'#991B1B','font-weight':'bold'});}
       }", flag_idx, flag_idx, flag_idx))
    datatable(flags, rownames=FALSE, selection="none",
              options=list(pageLength=-1, dom="t", scrollX=TRUE, rowCallback=cb_alert,
                           columnDefs=list(list(visible=FALSE, targets=flag_idx),
                                           list(className="dt-center", targets=as.list(seq(1L, nc_f-1L))))),
              class="compact stripe") %>%
      formatStyle("Player", fontWeight="700", color=NAVY)
  })
  
  # ALL DATA =================================================
  output$all_data_table <- renderDT({
    # Select and rename display columns
    keep_cols <- c(
      "Date", "Week", "Day", "Type", "Preseason",
      "Player Name", "Position_Abrev", "Forward_Back",
      "Period Number", "Period Name",
      "Average Distance (Session)",
      "Average Player Load (Session)",
      "High Metabolic Load Distance",
      "High Speed Distance",
      "Velocity Band 6 Average Distance (Session)",
      "Maximum Velocity",
      "Meterage Per Minute",
      "Max Acceleration",
      "Acceleration B1-3 Average Efforts (Session) (Gen 2)",
      "External_Data"
    )
    existing <- keep_cols[keep_cols %in% names(All_Data)]
    
    df <- All_Data %>%
      select(all_of(existing)) %>%
      arrange(desc(Date), `Period Number`) %>%
      mutate(
        `Player Name` = ext_mark(`Player Name`),
        Date          = format(Date, "%d %b %Y"),
        Week          = str_to_title(Week),
        Day           = str_to_title(Day),
        Type          = str_to_title(Type),
        across(where(is.numeric), ~round(.x, 1))
      )
    
    # Friendly column names
    rename_map <- c(
      "Position_Abrev"                                       = "Pos",
      "Forward_Back"                                         = "F/B",
      "Average Distance (Session)"                           = "Dist (m)",
      "Average Player Load (Session)"                        = "Load",
      "High Metabolic Load Distance"                         = "HML (m)",
      "High Speed Distance"                                  = "HSD (m)",
      "Velocity Band 6 Average Distance (Session)"           = "VHSD (m)",
      "Maximum Velocity"                                     = "Max Vel",
      "Meterage Per Minute"                                  = "m/min",
      "Max Acceleration"                                     = "Max Acc",
      "Acceleration B1-3 Average Efforts (Session) (Gen 2)"  = "Acc Efforts",
      "External_Data"                                        = "External"
    )
    for (old in names(rename_map)) {
      if (old %in% names(df)) names(df)[names(df)==old] <- rename_map[old]
    }
    
    datatable(
      df, rownames=FALSE, filter="top",
      options=list(
        pageLength=-1,
        lengthMenu=list(c(50, 100, 250, -1), c("50", "100", "250", "All")),
        dom="lftip",      # l=length chooser, f=search, t=table, i=info, p=pagination
        scrollX=TRUE,
        scrollY="620px",
        scroller=FALSE,
        order=list(),
        columnDefs=list(
          list(className="dt-center", targets=seq(1L, ncol(df)-1L)),
          list(width="90px",  targets=0L),
          list(width="130px", targets=5L)
        )
      ),
      class="compact stripe hover"
    ) %>%
      formatStyle("Player Name", fontWeight="700", color=NAVY, textAlign="left") %>%
      formatStyle("Period Number",
                  fontWeight=styleEqual(0, "bold"),
                  backgroundColor=styleEqual(0, "#F0F8FF")) %>%
      formatStyle("Preseason",
                  backgroundColor=styleEqual(c("yes","no","Yes","No"),
                                             c("#EBF9EE","#FFF8E1","#EBF9EE","#FFF8E1")))
  })


  # WEEKLY DATA TABLE =========================================
  # Aggregates sessions by player+week so that external tour totals
  # (single weekly-total row) compare fairly with regular squad sessions (multiple rows summed).
  output$weekly_data_table <- renderDT({
    req(nrow(Weekly_Data) > 0)

    wk <- Weekly_Data %>%
      filter(`Period Number` == 0) %>%
      mutate(is_ext = !is.na(External_Data) & trimws(as.character(External_Data)) != "") %>%
      group_by(`Player Name`, Week, Position_Abrev, Forward_Back, Preseason) %>%
      summarise(
        Date           = max(Date, na.rm = TRUE),
        Sessions       = sum(!is_ext),
        `Dist (m)`     = round(sum(coalesce(`Average Distance (Session)`, 0), na.rm = TRUE), 0),
        `HSD (m)`      = round(sum(coalesce(`High Speed Distance`, 0),         na.rm = TRUE), 0),
        `VHSD (m)`     = round(sum(coalesce(`Velocity Band 6 Average Distance (Session)`, 0), na.rm = TRUE), 0),
        `HML (m)`      = round(sum(coalesce(`High Metabolic Load Distance`, 0), na.rm = TRUE), 0),
        `Max Vel`      = round(max(suppressWarnings(as.numeric(`Maximum Velocity`)), na.rm = TRUE), 2),
        `Acc Efforts`  = round(sum(coalesce(`Acceleration B1-3 Average Efforts (Session) (Gen 2)`, 0), na.rm = TRUE), 0),
        Location       = paste(na.omit(External_Data), collapse = "/"),
        .groups = "drop"
      ) %>%
      mutate(
        `Max Vel`  = ifelse(is.finite(`Max Vel`), `Max Vel`, NA_real_),
        `HML (m)`  = ifelse(`HML (m)` == 0, NA_real_, `HML (m)`),
        Week       = as.character(Week),
        Date       = format(Date, "%d %b %Y"),
        Preseason  = coalesce(Preseason, ""),
        Location   = ifelse(
          trimws(Location) != "",
          paste0('<span style="background:#FEF3C7;color:#92400E;padding:2px 8px;border-radius:10px;',
                 'font-size:10px;font-weight:700;white-space:nowrap;">', trimws(Location), '</span>'),
          ""
        )
      ) %>%
      rename(Player = `Player Name`, Pos = Position_Abrev, `F/B` = Forward_Back) %>%
      arrange(desc(Week), Player)

    datatable(
      wk, rownames = FALSE, filter = "top", escape = FALSE,
      options = list(
        pageLength = 50,
        lengthMenu = list(c(50, 100, 250, -1), c("50", "100", "250", "All")),
        dom = "lftip",
        scrollX = TRUE,
        scrollY = "620px",
        order = list(),
        columnDefs = list(
          list(className = "dt-center", targets = seq(1L, ncol(wk) - 1L)),
          list(width = "130px", targets = 0L)
        )
      ),
      class = "compact stripe hover"
    ) %>%
      formatStyle("Player", fontWeight = "700", color = NAVY, textAlign = "left") %>%
      formatStyle("Preseason",
                  backgroundColor = styleEqual(c("yes","no","Yes","No"),
                                               c("#EBF9EE","#FFF8E1","#EBF9EE","#FFF8E1")))
  })


  # ============================================================
  #  DRILL PREDICTOR — server outputs
  # ============================================================

  # Helper: linear RPE scale where RPE/10 × peak gives the adjusted rate.
  # RPE 1 = 10% of peak, RPE 5 ≈ average (~50% of peak), RPE 10 = peak.
  # No RPE supplied → return historical average rate unchanged.
  dp_rate <- function(avg, peak, rpe) {
    if (is.null(rpe) || is.na(rpe)) return(avg)
    rpe <- min(max(rpe, 1), 10)
    (rpe / 10) * peak
  }

  # Position abbreviation order constant (also used in server)
  POS_ABREV_ORDER <- c("FR","HK","SR","BR","IB","OB")

  # Dynamic distance inputs UI (rendered based on mode)
  output$dp_dist_inputs_ui <- renderUI({
    mode <- if (isTruthy(input$dp_dist_mode)) input$dp_dist_mode else "position"
    lbl_style <- "font-size:11px;font-weight:700;color:#001F4E;text-transform:uppercase;"

    if (mode == "team") {
      fluidRow(
        column(3,
          numericInput("dp_dist_team",
                       label=tags$span(style=lbl_style, "Distance per Rep (m)"),
                       value=20, min=0, step=1, width="100%"))
      )
    } else if (mode == "group") {
      fluidRow(
        column(3, numericInput("dp_dist_FWD",
                               label=tags$span(style=lbl_style, "FWD — Distance/Rep (m)"),
                               value=20, min=0, step=1, width="100%")),
        column(3, numericInput("dp_dist_BKS",
                               label=tags$span(style=lbl_style, "BKS — Distance/Rep (m)"),
                               value=20, min=0, step=1, width="100%"))
      )
    } else {  # position — 6 fixed abbreviation inputs
      fluidRow(
        column(2, numericInput("dp_dist_FR", label=tags$span(style=lbl_style, "FR (m)"),  value=20, min=0, step=1, width="100%")),
        column(2, numericInput("dp_dist_HK", label=tags$span(style=lbl_style, "HK (m)"),  value=20, min=0, step=1, width="100%")),
        column(2, numericInput("dp_dist_SR", label=tags$span(style=lbl_style, "SR (m)"),  value=20, min=0, step=1, width="100%")),
        column(2, numericInput("dp_dist_BR", label=tags$span(style=lbl_style, "BR (m)"),  value=20, min=0, step=1, width="100%")),
        column(2, numericInput("dp_dist_IB", label=tags$span(style=lbl_style, "IB (m)"),  value=20, min=0, step=1, width="100%")),
        column(2, numericInput("dp_dist_OB", label=tags$span(style=lbl_style, "OB (m)"),  value=20, min=0, step=1, width="100%"))
      )
    }
  })

  # Core reactive: parse inputs, compute duration and rest
  dp_computed <- reactive({
    sets <- max(as.integer(if (isTruthy(input$dp_sets)) input$dp_sets else 3L), 1L)
    reps <- max(as.integer(if (isTruthy(input$dp_reps)) input$dp_reps else 5L), 1L)
    rpe  <- suppressWarnings(as.numeric(input$dp_rpe))
    rpe  <- if (is.null(rpe) || length(rpe)==0 || is.na(rpe) || rpe < 1 || rpe > 10) NA_real_ else rpe
    rest_rep_sec <- if (isTruthy(input$dp_rest_rep)) max(as.numeric(input$dp_rest_rep), 0) else 0
    rest_set_sec <- if (isTruthy(input$dp_rest_set)) max(as.numeric(input$dp_rest_set), 0) else 0
    rest_dur_min <- ((rest_rep_sec * reps * sets) + (rest_set_sec * sets)) / 60

    mode <- if (isTruthy(input$dp_dist_mode)) input$dp_dist_mode else "position"

    if (mode == "team") {
      dist       <- max(as.numeric(if (isTruthy(input$dp_dist_team)) input$dp_dist_team else 20), 0)
      total_dist <- dist * reps * sets
      dist_map   <- NULL   # not used in team mode
      # Squad-level work duration
      work_dur_min  <- total_dist / max(wara_dp_squad_mpm, 1)
      total_dur_min <- work_dur_min + rest_dur_min

    } else if (mode == "group") {
      dist_FWD <- max(as.numeric(if (isTruthy(input$dp_dist_FWD)) input$dp_dist_FWD else 20), 0)
      dist_BKS <- max(as.numeric(if (isTruthy(input$dp_dist_BKS)) input$dp_dist_BKS else 20), 0)
      dist_map <- c(FWD = dist_FWD * reps * sets,
                    BKS = dist_BKS * reps * sets)
      total_dist   <- mean(dist_map)   # squad average for shared rest/time
      # Per-group work duration using group avg mpm
      grp_mpm <- setNames(wara_dp_group$avg_mpm, wara_dp_group$group)
      work_dur_map <- sapply(names(dist_map), function(g)
        dist_map[g] / max(grp_mpm[g], 1, na.rm=TRUE))
      work_dur_min  <- mean(work_dur_map)  # representative for banner
      total_dur_min <- work_dur_min + rest_dur_min

    } else {  # position — 6 fixed abbreviation inputs
      inp_ids <- c(FR="dp_dist_FR", HK="dp_dist_HK", SR="dp_dist_SR",
                   BR="dp_dist_BR", IB="dp_dist_IB", OB="dp_dist_OB")
      dist_map <- sapply(POS_ABREV_ORDER, function(pa) {
        d <- suppressWarnings(as.numeric(input[[inp_ids[pa]]]))
        max(if (is.na(d) || length(d) == 0) 20 else d, 0) * reps * sets
      })
      names(dist_map) <- POS_ABREV_ORDER
      total_dist <- mean(dist_map)
      # Per-position work duration using pos_abrev avg_mpm
      pos_abrev_mpm <- setNames(wara_dp_pos_abrev$avg_mpm, wara_dp_pos_abrev$pos_abrev)
      work_dur_map <- sapply(POS_ABREV_ORDER, function(pa)
        dist_map[pa] / max(pos_abrev_mpm[pa], 1, na.rm=TRUE))
      work_dur_min  <- mean(work_dur_map)
      total_dur_min <- work_dur_min + rest_dur_min
    }

    list(mode=mode, total_dist=total_dist, dist_map=dist_map,
         work_dur_min=work_dur_min, rest_dur_min=rest_dur_min,
         total_dur_min=total_dur_min, rpe=rpe, sets=sets, reps=reps)
  })

  # Summary banner — two rows: row1 = distances, row2 = time/RPE
  output$dp_banner <- renderUI({
    p       <- dp_computed()
    rpe_txt <- if (is.na(p$rpe)) "None (avg rates)" else paste0(p$rpe, " / 10")

    # Build per-group/position distance items (row 1)
    dist_items <- if (p$mode == "team" || is.null(p$dist_map)) {
      tagList(
        div(class="banner-item",
            div(class="banner-label","Total Distance"),
            div(class="banner-value", paste0(formatC(p$total_dist, format="f", digits=0, big.mark=",")," m")))
      )
    } else if (p$mode == "group") {
      items <- lapply(names(p$dist_map), function(nm) {
        tagList(
          div(class="banner-item",
              div(class="banner-label", paste0(nm, " Distance")),
              div(class="banner-value", paste0(formatC(p$dist_map[[nm]], format="f", digits=0, big.mark=",")," m"))),
          div(class="banner-divider")
        )
      })
      do.call(tagList, items)
    } else {
      # position mode — 6 items in POS_ABREV_ORDER
      items <- lapply(POS_ABREV_ORDER, function(pa) {
        val <- if (pa %in% names(p$dist_map)) p$dist_map[[pa]] else 0
        tagList(
          div(class="banner-item",
              div(class="banner-label", paste0(pa, " Dist")),
              div(class="banner-value", paste0(formatC(val, format="f", digits=0, big.mark=",")," m"))),
          div(class="banner-divider")
        )
      })
      do.call(tagList, items)
    }

    div(
      div(class="session-banner", style="margin-bottom:4px;", dist_items),
      div(class="session-banner", style="margin-bottom:6px;",
        div(class="banner-item",
            div(class="banner-label","Work Time"),
            div(class="banner-value", paste0(round(p$work_dur_min, 1)," min"))),
        div(class="banner-divider"),
        div(class="banner-item",
            div(class="banner-label","Rest Time"),
            div(class="banner-value", paste0(round(p$rest_dur_min, 1)," min"))),
        div(class="banner-divider"),
        div(class="banner-item",
            div(class="banner-label","Total Drill Time"),
            div(class="banner-value", paste0(round(p$total_dur_min, 1)," min"))),
        div(class="banner-divider"),
        div(class="banner-item",
            div(class="banner-label","RPE"),
            div(class="banner-value", rpe_txt))
      )
    )
  })

  # Shared DT styling helper
  dp_datatable <- function(df, player_col=NULL) {
    dt <- datatable(df, rownames=FALSE, class="compact stripe hover",
      options=list(pageLength=40, scrollX=TRUE, scrollY="560px", dom="ftp",
                   columnDefs=list(list(className="dt-center", targets=seq_len(ncol(df))-1L))))
    if (!is.null(player_col) && player_col %in% names(df))
      dt <- dt %>% formatStyle(player_col, fontWeight="700", color=NAVY, textAlign="left")
    if ("F/B" %in% names(df))
      dt <- dt %>% formatStyle("F/B", fontWeight="bold",
                                color=styleEqual(c("FWD","BKS","Other"), c(FORWARD, BACK, GREY_MID)))
    hsr_max  <- max(as.numeric(gsub(",","",df[["Pred HSR (m)"]],  fixed=TRUE)), na.rm=TRUE)
    vhsr_max <- max(as.numeric(gsub(",","",df[["Pred VHSR (m)"]], fixed=TRUE)), na.rm=TRUE)
    dt %>%
      formatStyle("Pred HSR (m)",
                  background=styleColorBar(c(0, hsr_max  * 1.1), SKY),
                  backgroundSize="98% 60%", backgroundRepeat="no-repeat",
                  backgroundPosition="center") %>%
      formatStyle("Pred VHSR (m)",
                  background=styleColorBar(c(0, vhsr_max * 1.1), ACCENT),
                  backgroundSize="98% 60%", backgroundRepeat="no-repeat",
                  backgroundPosition="center")
  }

  # Helper: compute per-row work_dur_min given dist_map, avg_mpm column, row key column
  # By Player
  output$dp_player_tbl <- renderDT({
    p <- dp_computed()
    df <- wara_dp_player %>%
      mutate(
        .wdm = if (p$mode == "team") {
          rep(p$work_dur_min, n())
        } else if (p$mode == "group") {
          mapply(function(g, mpm) {
            d <- if (g %in% names(p$dist_map)) p$dist_map[[g]] else mean(p$dist_map)
            d / max(mpm, 1, na.rm=TRUE)
          }, group, avg_mpm)
        } else {
          # position mode: look up by pos_abrev
          mapply(function(pa, mpm) {
            d <- if (pa %in% names(p$dist_map)) p$dist_map[[pa]] else mean(p$dist_map)
            d / max(mpm, 1, na.rm=TRUE)
          }, pos_abrev, avg_mpm)
        },
        `Total Dist (m)` = if (p$mode == "team") {
          rep(p$total_dist, n())
        } else if (p$mode == "group") {
          ifelse(group %in% names(p$dist_map), p$dist_map[group], mean(p$dist_map))
        } else {
          # position mode: look up by pos_abrev
          ifelse(pos_abrev %in% names(p$dist_map), p$dist_map[pos_abrev], mean(p$dist_map))
        },
        `Pred HSR (m)`  = round(dp_rate(avg_hsr_pm,   peak_hsr_pm,   p$rpe) * .wdm, 0),
        `Pred VHSR (m)` = round(dp_rate(avg_vhsr_pm,  peak_vhsr_pm,  p$rpe) * .wdm, 0),
        `Pred Accels`   = round(dp_rate(avg_accel_pm,  peak_accel_pm, p$rpe) * .wdm, 0)
      ) %>%
      rename(Player=athlete_name, Position=position_name, `F/B`=group) %>%
      mutate(Position=str_to_title(Position),
             `Total Dist (m)` = formatC(`Total Dist (m)`, format="f", digits=0, big.mark=",")) %>%
      select(Player, Position, `F/B`, `Total Dist (m)`, `Pred HSR (m)`, `Pred VHSR (m)`, `Pred Accels`) %>%
      arrange(`F/B`, Position, Player)
    dp_datatable(df, player_col="Player")
  })

  # By Position (uses pos_abrev table: FR/HK/SR/BR/IB/OB)
  output$dp_pos_tbl <- renderDT({
    p <- dp_computed()
    df <- wara_dp_pos_abrev %>%
      mutate(
        .wdm = if (p$mode == "position") {
          mapply(function(pa, mpm) {
            d <- if (pa %in% names(p$dist_map)) p$dist_map[[pa]] else mean(p$dist_map)
            d / max(mpm, 1, na.rm=TRUE)
          }, pos_abrev, avg_mpm)
        } else {
          rep(p$work_dur_min, n())
        },
        `Total Dist (m)` = if (p$mode == "position") {
          ifelse(pos_abrev %in% names(p$dist_map), p$dist_map[pos_abrev], mean(p$dist_map))
        } else {
          rep(p$total_dist, n())
        },
        `Pred HSR (m)`  = round(dp_rate(avg_hsr_pm,   peak_hsr_pm,   p$rpe) * .wdm, 0),
        `Pred VHSR (m)` = round(dp_rate(avg_vhsr_pm,  peak_vhsr_pm,  p$rpe) * .wdm, 0),
        `Pred Accels`   = round(dp_rate(avg_accel_pm,  peak_accel_pm, p$rpe) * .wdm, 0)
      ) %>%
      rename(Position=pos_abrev) %>%
      mutate(`Total Dist (m)` = formatC(`Total Dist (m)`, format="f", digits=0, big.mark=",")) %>%
      select(Position, `Total Dist (m)`, `Pred HSR (m)`, `Pred VHSR (m)`, `Pred Accels`) %>%
      arrange(match(Position, POS_ABREV_ORDER))
    dp_datatable(df)
  })

  # By Group
  output$dp_group_tbl <- renderDT({
    p <- dp_computed()
    df <- wara_dp_group %>%
      mutate(
        .wdm = if (p$mode == "group") {
          mapply(function(g, mpm) {
            d <- if (g %in% names(p$dist_map)) p$dist_map[[g]] else mean(p$dist_map)
            d / max(mpm, 1, na.rm=TRUE)
          }, group, avg_mpm)
        } else {
          rep(p$work_dur_min, n())
        },
        `Total Dist (m)` = if (p$mode == "group") {
          ifelse(group %in% names(p$dist_map), p$dist_map[group], mean(p$dist_map))
        } else {
          rep(p$total_dist, n())
        },
        `Pred HSR (m)`  = round(dp_rate(avg_hsr_pm,   peak_hsr_pm,   p$rpe) * .wdm, 0),
        `Pred VHSR (m)` = round(dp_rate(avg_vhsr_pm,  peak_vhsr_pm,  p$rpe) * .wdm, 0),
        `Pred Accels`   = round(dp_rate(avg_accel_pm,  peak_accel_pm, p$rpe) * .wdm, 0)
      ) %>%
      rename(`F/B`=group) %>%
      mutate(`Total Dist (m)` = formatC(`Total Dist (m)`, format="f", digits=0, big.mark=",")) %>%
      select(`F/B`, `Total Dist (m)`, `Pred HSR (m)`, `Pred VHSR (m)`, `Pred Accels`) %>%
      arrange(`F/B`)
    dp_datatable(df)
  })


  # ============================================================
  #  WARATAH ANALYTICS — server outputs
  # ============================================================

  # Rolling sum helper (no external package required)
  # Windowed EWMA: applies EWMA to a hard n-day window only.
  # Sessions outside the window (older than n days) get zero weight.
  ewma_window <- function(x, n, lambda) {
    len <- length(x)
    out <- numeric(len)
    for (i in seq_len(len)) {
      start  <- max(1L, i - n + 1L)
      window <- x[start:i]
      # Apply EWMA left-to-right within window; oldest value is seed (unweighted)
      out[i] <- Reduce(function(acc, v) v * lambda + (1 - lambda) * acc, window)
    }
    out
  }

  # Windowed EWMA ACWR reactive — same lambda constants, but hard 7/28-day cutoffs
  acwr_windowed <- reactive({
    req(isTruthy(input$acwr_method) && input$acwr_method == "windowed")
    wara_min_date <- min(wara_grid_ind$date, na.rm = TRUE)
    wara_grid_ind %>%
      group_by(athlete_name) %>%
      arrange(date) %>%
      mutate(
        a_dist  = ewma_window(dist,        7L,  ak),
        c_dist  = ewma_window(dist,        28L, ck),
        a_hsr   = ewma_window(hsr,         7L,  ak),
        c_hsr   = ewma_window(hsr,         28L, ck),
        a_vhsr  = ewma_window(vhsr,        7L,  ak),
        c_vhsr  = ewma_window(vhsr,        28L, ck),
        a_accel = ewma_window(accel_count, 7L,  ak),
        c_accel = ewma_window(accel_count, 28L, ck),
        acwr_dist        = round(a_dist  / ifelse(c_dist  == 0, NA, c_dist),  2),
        acwr_hsr         = round(a_hsr   / ifelse(c_hsr   == 0, NA, c_hsr),   2),
        acwr_vhsr        = round(a_vhsr  / ifelse(c_vhsr  == 0, NA, c_vhsr),  2),
        acwr_accel_count = round(a_accel / ifelse(c_accel == 0, NA, c_accel), 2),
        acwr_HMLD        = NA_real_,
        days_since_95    = NA_real_,
        days_since_90    = NA_real_,
        week_number      = 1L + floor(as.integer(difftime(date, wara_min_date, units="weeks")))
      ) %>%
      ungroup() %>%
      filter(dist > 0 | hsr > 0) %>%
      select(athlete_name, date, week_number,
             acwr_dist, acwr_hsr, acwr_vhsr, acwr_accel_count, acwr_HMLD,
             days_since_95, days_since_90) %>%
      arrange(desc(date), athlete_name)
  })

  # Unified reactive: returns appropriate ACWR dataset based on method selection
  acwr_data <- reactive({
    method <- if (isTruthy(input$acwr_method)) input$acwr_method else "ewma"
    if (method == "windowed") acwr_windowed() else wara_acwr
  })

  # Populate ACWR week selector with available weeks, default to most recent
  observe({
    df <- acwr_data()
    req(nrow(df) > 0)
    weeks <- sort(unique(df$week_number), decreasing=TRUE)
    week_choices <- setNames(as.character(weeks), paste("Week", weeks))
    updateSelectInput(session, "acwr_week_sel",
                      choices=week_choices, selected=week_choices[1])
  })

  # Alert banner — athletes with any ACWR > 1.5 in selected week
  output$acwr_alert_banner <- renderUI({
    req(input$acwr_week_sel)
    sel_week <- as.integer(input$acwr_week_sel)

    df <- acwr_data() %>%
      filter(week_number == sel_week) %>%
      group_by(athlete_name) %>%
      slice_max(date, n=1, with_ties=FALSE) %>%
      ungroup() %>%
      mutate(max_acwr = pmax(acwr_dist, acwr_hsr, acwr_vhsr, acwr_accel_count, na.rm=TRUE)) %>%
      filter(!is.na(max_acwr), max_acwr > 1.5) %>%
      arrange(desc(max_acwr))

    if (nrow(df) == 0) return(NULL)

    badges <- lapply(seq_len(nrow(df)), function(i) {
      tags$span(
        style="display:inline-block;background:#FEE2E2;color:#991B1B;border:1px solid #FECACA;padding:3px 10px;border-radius:4px;font-size:12px;font-weight:600;margin:2px;",
        sprintf("%s (%.2f)", df$athlete_name[i], df$max_acwr[i])
      )
    })

    div(
      style="background:#FEF2F2;border:1px solid #FECACA;border-radius:6px;padding:8px 12px;",
      tags$span(style="font-weight:700;color:#991B1B;font-size:12px;margin-right:8px;",
                "⚠ HIGH RISK:"),
      tags$span(badges)
    )
  })

  # ACWR weekly table with colour coding
  output$acwr_week_table <- renderDT({
    req(input$acwr_week_sel)
    sel_week <- as.integer(input$acwr_week_sel)

    # Helper: render one ACWR value as a coloured HTML pill (same style as On Track table)
    cell_acwr <- function(val) {
      if (is.na(val)) {
        return('<div style="background:#F3F4F6;color:#9CA3AF;padding:2px 4px;border-radius:3px;font-size:11px;font-weight:600;text-align:center;">—</div>')
      }
      if      (val > 1.5) { bg <- "#FEE2E2"; fg <- "#991B1B" }   # red    — high risk
      else if (val > 1.3) { bg <- "#FEF3C7"; fg <- "#92400E" }   # amber  — caution
      else if (val >= 0.8){ bg <- "#DCFCE7"; fg <- "#166534" }   # green  — optimal
      else                 { bg <- "#EFF6FF"; fg <- "#1E40AF" }   # blue   — under-loaded
      sprintf(
        '<div style="background:%s;color:%s;padding:2px 4px;border-radius:3px;font-size:11px;font-weight:600;text-align:center;">%.2f</div>',
        bg, fg, val
      )
    }

    raw <- acwr_data() %>%
      filter(week_number == sel_week) %>%
      group_by(athlete_name) %>%
      slice_max(date, n=1, with_ties=FALSE) %>%
      ungroup() %>%
      arrange(athlete_name)

    tbl <- data.frame(
      Athlete      = raw$athlete_name,
      Date         = format(raw$date, "%d %b %Y"),
      `ACWR Dist`  = sapply(raw$acwr_dist,        cell_acwr),
      `ACWR HSR`   = sapply(raw$acwr_hsr,         cell_acwr),
      `ACWR VHSR`  = sapply(raw$acwr_vhsr,        cell_acwr),
      `ACWR Accels`= sapply(raw$acwr_accel_count, cell_acwr),
      stringsAsFactors = FALSE,
      check.names  = FALSE
    )

    datatable(
      tbl,
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      options   = list(
        pageLength = 30,
        dom        = "t",
        ordering   = FALSE,
        columnDefs = list(
          list(className="dt-left",   targets=0L),
          list(className="dt-center", targets=as.list(seq_len(ncol(tbl)-1L)))
        )
      ),
      class = "compact stripe"
    ) %>%
      formatStyle("Athlete", fontWeight="700", color=NAVY)
  })

  # Conditional filters for Session Summary view
  output$wara_session_filters <- renderUI({
    req(input$wara_view == "session_summary")

    weeks <- sort(unique(wara_session$week_number), decreasing=TRUE)
    week_choices <- setNames(as.character(weeks), paste("Week", weeks))

    sessions <- sort(unique(wara_session$session))

    # Default: most recent week's sessions
    latest_week <- weeks[1]
    default_sessions <- wara_session %>%
      filter(week_number == latest_week) %>%
      pull(session) %>% unique() %>% sort()

    fluidRow(
      column(3,
        selectInput("sess_week_sel", label="Week:",
                    choices=week_choices, selected=week_choices[1], width="100%")
      ),
      column(4,
        selectInput("sess_session_sel", label="Session:",
                    choices=default_sessions,
                    selected=if(length(default_sessions)>0) default_sessions[length(default_sessions)] else NULL,
                    width="100%")
      )
    )
  })

  # Update session choices when week changes
  observe({
    req(input$wara_view == "session_summary", input$sess_week_sel)
    sel_week <- as.integer(input$sess_week_sel)
    sessions <- wara_session %>%
      filter(week_number == sel_week) %>%
      pull(session) %>% unique() %>% sort()
    updateSelectInput(session, "sess_session_sel",
                      choices=sessions,
                      selected=if(length(sessions)>0) sessions[length(sessions)] else NULL)
  })

  output$wara_desc <- renderUI({
    desc <- switch(input$wara_view,
      weekly_vol        = "Total weekly volume per athlete per position. Columns: Total Distance, HSR, VHSR, HMLD, Accel Count, % of total distance that is HSR.",
      session_summary   = "Per-athlete session totals — all drill periods for that session summed. Includes %maxv (% of profile max velocity). tot_session = how many sessions that athlete attended in that week.",
      drill_summary     = "Team average per named drill period. Each row = one drill on one date, averaged across all athletes present.",
      match_report      = "Club Game activities only. Per-athlete per-period match output.",
      acwr              = "Individual EWMA Acute:Chronic Workload Ratio. Acute = 7-day EWMA (recent stress), Chronic = 28-day EWMA (fitness base). Includes days_since_95 and days_since_90 (days since athlete last hit 95%/90% of max velocity). Training days only.",
      pos_ewma          = "Positional Acute & Chronic Loads. Average daily load per position → EWMA acute (7-day) and chronic (28-day). No ACWR ratio — matches original script output exactly.",
      session_predictor = "Per-minute intensity benchmarks grouped by drill/period name (squad-level). Use: planned duration (min) × rate = predicted load. E.g. 20 min Scrimmage × dist/min = expected distance.",
      pos_predictor     = "Per-minute intensity benchmarks by drill AND position. Key planning tool: forwards and backs experience very different loads in the same drill. Planned duration × rate = predicted load per athlete in that position.",
      ""
    )
    tags$p(style="font-size:12px;color:#8899AA;margin:0 0 4px 0;", desc)
  })

  output$wara_table <- renderDT({
    df <- switch(input$wara_view,
      weekly_vol        = wara_weekly_vol,
      session_summary   = {
        req(input$sess_week_sel, input$sess_session_sel)
        wara_session %>%
          filter(week_number == as.integer(input$sess_week_sel),
                 session == input$sess_session_sel)
      },
      drill_summary     = wara_drill,
      match_report      = wara_match,
      acwr              = wara_acwr,
      pos_ewma          = wara_pos_ewma,
      session_predictor = wara_predictor,
      pos_predictor     = wara_pos_predictor,
      data.frame(Message="Select a view from the dropdown above.")
    )

    if (nrow(df) == 0) {
      df <- data.frame(Message = "No data available for this view yet.")
    }

    datatable(
      df,
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        pageLength = 25,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = list(
          list(extend="csv",   text="CSV",   filename="waratah_analytics"),
          list(extend="excel", text="Excel", filename="waratah_analytics")
        ),
        columnDefs = list(list(className="dt-center", targets="_all"))
      ),
      class = "compact stripe hover"
    )
  })


}

# ============================================================
#  RUN
# ============================================================
shinyApp(ui = ui, server = server)

