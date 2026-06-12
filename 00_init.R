# *****************************************************************************
#' 00_init.R
#' config file for report
#'
#' Author: R. Vizcarra - Eclipse Geomatics Ltd.
# *****************************************************************************

# install.packages(c("googleAnalyticsR", "dplyr", "ggplot2", "scales",
#                    "tidyr", "gridExtra", "grid", "jsonlite"))

library(googleAnalyticsR)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(grid)
library(jsonlite)

# auth
# ga_auth()

# *****************************************************************************
# constants
# *****************************************************************************

PROPERTY_ID   <- 486592111
wp_hosts      <- c("sipexchangebc.com")
ckan_hosts    <- c("resources.sipexchangebc.com", "www.resources.sipexchangebc.com")
bot_hosts     <- c("staging-resources.sipexchangebc.com", "104.36.148.162", "104.36.148.138")
bot_countries <- c("Singapore", "China")
bot_patterns  <- c("bot", "spider", "crawler", "googlebot", "bingbot",
                   "semrush", "ahrefsbot", "mj12bot", "yandex", "baidu")

# *****************************************************************************
# sip colours for pdf
# *****************************************************************************

col_dark    <- "#002a26"  
col_gold    <- "#e8aa00"  
col_lime    <- "#a7ce09"  
col_card_bg <- "#e8f5e0"  
col_white   <- "#ffffff"
col_black   <- "#000000"

# *****************************************************************************
#' pdf helper
#' there were some issues w the packages on mac quartz() works 
#' test if report has issues running on windows
# *****************************************************************************

pdf <- function(file, width = 7, height = 7, ...) {
  if (capabilities("aqua")) {
    quartz(file = file, type = "pdf", width = width, height = height)
  } else {
    grDevices::pdf(file = file, width = width, height = height, ...)
  }
}

# *****************************************************************************
# functions
# *****************************************************************************

section_title <- function(title, subtitle = NULL, description = NULL) {
  grid.newpage()
  grid.rect(gp = gpar(fill = col_dark, col = NA))
  title_y <- ifelse(!is.null(description), 0.65, ifelse(!is.null(subtitle), 0.55, 0.5))
  grid.text(title, x = 0.5, y = title_y,
            gp = gpar(col = col_white, fontsize = 22, fontface = "bold"))
  if (!is.null(subtitle))
    grid.text(subtitle, x = 0.5, y = title_y - 0.13,
              gp = gpar(col = col_lime, fontsize = 13))
  if (!is.null(description)) {
    desc_lines <- strwrap(description, width = 85)
    for (j in seq_along(desc_lines))
      grid.text(desc_lines[j], x = 0.5, y = 0.30 - (j - 1) * 0.07,
                gp = gpar(col = col_lime, fontsize = 11), just = "centre")
  }
}

make_table <- function(df, title, subtitle = NULL, description = NULL, footnote = NULL) {
  section_title(title, subtitle, description)
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))
  n_rows  <- nrow(df)
  cex_val <- ifelse(n_rows > 25, 0.45, ifelse(n_rows > 15, 0.55, 0.65))
  tbl <- tableGrob(df, rows = NULL,
                   theme = ttheme_default(
                     core    = list(fg_params = list(cex = cex_val)),
                     colhead = list(fg_params = list(cex = cex_val + 0.1, fontface = "bold"))
                   )
  )
  page_h  <- if (!is.null(footnote)) 0.88 else 0.95
  tbl_h   <- convertHeight(sum(tbl$heights), "in", valueOnly = TRUE)
  tbl_w   <- convertWidth(sum(tbl$widths),   "in", valueOnly = TRUE)
  avail_h <- convertHeight(unit(page_h, "npc"), "in", valueOnly = TRUE)
  avail_w <- convertWidth(unit(0.95, "npc"),    "in", valueOnly = TRUE)
  sf <- min(avail_h / tbl_h, avail_w / tbl_w)
  if (sf < 1) tbl <- editGrob(tbl, gp = gpar(cex = sf))
  grid.draw(tbl)
  if (!is.null(footnote))
    grid.text(footnote, x = 0.5, y = 0.02,
              gp = gpar(fontsize = 8, col = "grey50", fontface = "italic"))
}

cover_page <- function(title, subtitle, label) {
  grid.newpage()
  grid.rect(gp = gpar(fill = col_dark, col = NA))
  grid.text(title,    x = 0.5, y = 0.62, gp = gpar(col = col_white, fontsize = 28, fontface = "bold"))
  grid.text(subtitle, x = 0.5, y = 0.50, gp = gpar(col = col_white, fontsize = 18))
  grid.text(label,    x = 0.5, y = 0.42, gp = gpar(col = col_lime,  fontsize = 14))
  grid.text(paste("Generated:", Sys.Date()), x = 0.5, y = 0.32,
            gp = gpar(col = col_lime, fontsize = 12))
}