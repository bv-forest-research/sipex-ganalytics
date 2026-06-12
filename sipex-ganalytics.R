# *****************************************************************************
#' SIPex - Monthly Google Analytics Report
#' Property ID: 486592111
#'
#' Author: R. Vizcarra - Eclipse Geomatics Ltd.
# *****************************************************************************

# *****************************************************************************
# install and load packages -------------
# install.packages(c("googleAnalyticsR", "dplyr", "lubridate", "ggplot2",
#                    "scales", "tidyr", "gridExtra", "grid", "jsonlite"))

library(googleAnalyticsR)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(grid)
library(jsonlite)

# *****************************************************************************
# authenticate -------------
ga_auth()

# *****************************************************************************
# configuration -------------

PROPERTY_ID <- 486592111

# --- monthly snapshot (sections 1-7) ---
date_start <- "2026-04-01"
date_end   <- "2026-04-30"

# --- trend period (sections 8-10) - any range, reported monthly ---
# examples:
#   last 12 months: trend_start <- "2025-05-01", trend_end <- "2026-04-30"
#   last 6 months:  trend_start <- "2025-11-01", trend_end <- "2026-04-30"
#   full year 2025: trend_start <- "2025-01-01", trend_end <- "2025-12-31"
trend_start <- "2025-05-01"
trend_end   <- "2026-04-30"
trend_label <- paste(format(as.Date(trend_start), "%b %Y"), "to",
                     format(as.Date(trend_end), "%b %Y"))

# set to TRUE to report US & Canada only, FALSE for all users
usca_only <- FALSE

# number of rows in top-N tables
TOP_N <- 20

# known hostnames
wp_hosts   <- c("sipexchangebc.com")
ckan_hosts <- c("resources.sipexchangebc.com", "www.resources.sipexchangebc.com")
bot_hosts  <- c("staging-resources.sipexchangebc.com", "104.36.148.162", "104.36.148.138")
bot_countries <- c("Singapore", "China")
bot_patterns  <- c("bot", "spider", "crawler", "googlebot", "bingbot",
                   "semrush", "ahrefsbot", "mj12bot", "yandex", "baidu")

geo_label <- if (usca_only) "US & Canada" else "All Regions"


# =============================================================================
# PART 1 — MONTHLY SNAPSHOT PULLS
# =============================================================================

## traffic overview -----
df_traffic <- ga_data(
  PROPERTY_ID,
  metrics    = c("sessions", "activeUsers", "newUsers", "screenPageViews",
                 "averageSessionDuration", "engagementRate"),
  dimensions = c("date"),
  date_range = c(date_start, date_end)
)

## traffic by channel -----
df_channels <- ga_data(
  PROPERTY_ID,
  metrics    = c("sessions", "activeUsers"),
  dimensions = c("sessionDefaultChannelGroup", "country"),
  date_range = c(date_start, date_end)
)

## traffic by source/medium -----
df_sources <- ga_data(
  PROPERTY_ID,
  metrics    = c("sessions", "activeUsers"),
  dimensions = c("sessionSourceMedium", "country"),
  date_range = c(date_start, date_end),
  limit      = 200
)

## top pages -----
df_pages <- ga_data(
  PROPERTY_ID,
  metrics    = c("screenPageViews", "activeUsers", "averageSessionDuration"),
  dimensions = c("pageTitle", "pagePath", "hostName", "country"),
  date_range = c(date_start, date_end),
  limit      = 500
)

## geographic - city level -----
df_geo <- ga_data(
  PROPERTY_ID,
  metrics    = c("activeUsers", "sessions"),
  dimensions = c("country", "region", "city"),
  date_range = c(date_start, date_end),
  limit      = 100
)

## devices -----
df_devices <- ga_data(
  PROPERTY_ID,
  metrics    = c("activeUsers", "sessions"),
  dimensions = c("deviceCategory", "country"),
  date_range = c(date_start, date_end)
)

## site search terms -----
df_search <- ga_data(
  PROPERTY_ID,
  metrics    = c("eventCount", "activeUsers"),
  dimensions = c("searchTerm", "country"),
  date_range = c(date_start, date_end),
  limit      = 200
)

## file downloads -----
df_downloads <- ga_data(
  PROPERTY_ID,
  metrics    = c("eventCount"),
  dimensions = c("eventName", "linkText", "linkUrl", "country"),
  date_range = c(date_start, date_end),
  limit      = 500
)

## outbound link clicks -----
df_outbound <- ga_data(
  PROPERTY_ID,
  metrics    = c("eventCount"),
  dimensions = c("eventName", "linkText", "linkUrl", "country"),
  date_range = c(date_start, date_end),
  limit      = 500
)

## tag page views -----
df_tags_ga <- ga_data(
  PROPERTY_ID,
  metrics    = c("screenPageViews", "activeUsers"),
  dimensions = c("pagePath", "country"),
  date_range = c(date_start, date_end),
  limit      = 500
)


# =============================================================================
# PART 2 — TREND PERIOD PULLS
# =============================================================================

## monthly engagement - all regions -----
df_trend_engagement <- ga_data(
  PROPERTY_ID,
  metrics    = c("activeUsers", "averageSessionDuration", "engagementRate"),
  dimensions = c("year", "month", "country"),
  date_range = c(trend_start, trend_end)
)

## monthly engagement - US/CA only -----
df_trend_engagement_usca <- ga_data(
  PROPERTY_ID,
  metrics    = c("activeUsers", "averageSessionDuration", "engagementRate"),
  dimensions = c("year", "month", "country"),
  date_range = c(trend_start, trend_end)
)

## monthly search terms -----
df_trend_search <- ga_data(
  PROPERTY_ID,
  metrics    = c("eventCount"),
  dimensions = c("year", "month", "searchTerm", "country"),
  date_range = c(trend_start, trend_end),
  limit      = 1000
)

## monthly downloads -----
df_trend_downloads <- ga_data(
  PROPERTY_ID,
  metrics    = c("eventCount"),
  dimensions = c("year", "month", "eventName", "linkText", "linkUrl", "country"),
  date_range = c(trend_start, trend_end),
  limit      = 1000
)


# =============================================================================
# PART 1 — MONTHLY SNAPSHOT FILTERS & TRANSFORMS
# =============================================================================

clean_traffic <- df_traffic %>%
  filter(averageSessionDuration >= 10) %>%
  mutate(returningUsers = activeUsers - newUsers)

clean_channels <- df_channels %>%
  filter(!country %in% bot_countries) %>%
  group_by(sessionDefaultChannelGroup) %>%
  summarise(sessions = sum(sessions), activeUsers = sum(activeUsers), .groups = "drop")

clean_sources <- df_sources %>%
  filter(!country %in% bot_countries) %>%
  filter(!grepl("singapore|bot|spider|crawl", sessionSourceMedium, ignore.case = TRUE)) %>%
  filter(sessionSourceMedium != "(direct) / (none)") %>%
  group_by(sessionSourceMedium) %>%
  summarise(sessions = sum(sessions), activeUsers = sum(activeUsers), .groups = "drop") %>%
  arrange(desc(sessions))

clean_pages <- df_pages %>%
  filter(!grepl(paste(bot_patterns, collapse = "|"), pageTitle, ignore.case = TRUE)) %>%
  filter(!hostName %in% bot_hosts) %>%
  filter(!country %in% bot_countries)

df_geo <- df_geo %>% filter(!country %in% bot_countries)

clean_devices <- df_devices %>%
  filter(!country %in% bot_countries) %>%
  group_by(deviceCategory) %>%
  summarise(activeUsers = sum(activeUsers), sessions = sum(sessions), .groups = "drop")

clean_search <- df_search %>%
  filter(!country %in% bot_countries, !is.na(searchTerm),
         searchTerm != "(not set)", searchTerm != "",
         nchar(searchTerm) <= 100,
         !grepl("^[0-9]+$", searchTerm)) %>%
  group_by(searchTerm) %>%
  summarise(eventCount = sum(eventCount), activeUsers = sum(activeUsers), .groups = "drop")

clean_downloads <- df_downloads %>%
  filter(eventName == "file_download", !country %in% bot_countries) %>%
  group_by(linkText, linkUrl) %>%
  summarise(eventCount = sum(eventCount), .groups = "drop")

clean_outbound <- df_outbound %>%
  filter(eventName == "click", !country %in% bot_countries,
         !grepl("sipexchangebc.com", linkUrl, ignore.case = TRUE)) %>%
  group_by(linkText, linkUrl) %>%
  summarise(eventCount = sum(eventCount), .groups = "drop")

clean_tags_ga <- df_tags_ga %>%
  filter(grepl("tags=", pagePath), !country %in% bot_countries) %>%
  mutate(
    tag = gsub(".*[?&]tags=([^&]+).*", "\\1", pagePath),
    tag = gsub("\\+", " ", tag),
    tag = URLdecode(tag)
  ) %>%
  filter(tag != "", !is.na(tag)) %>%
  group_by(tag) %>%
  summarise(views = sum(screenPageViews), users = sum(activeUsers), .groups = "drop") %>%
  arrange(desc(views))

# US/CA toggle
if (usca_only) {
  clean_pages     <- clean_pages     %>% filter(country %in% c("United States", "Canada"))
  clean_channels  <- clean_channels  %>% filter(country %in% c("United States", "Canada"))
  df_geo          <- df_geo          %>% filter(country %in% c("United States", "Canada"))
  clean_search    <- clean_search    %>% filter(country %in% c("United States", "Canada"))
  clean_downloads <- clean_downloads %>% filter(country %in% c("United States", "Canada"))
  clean_outbound  <- clean_outbound  %>% filter(country %in% c("United States", "Canada"))
}

clean_pages <- clean_pages %>%
  group_by(pageTitle, pagePath, hostName) %>%
  summarise(screenPageViews = sum(screenPageViews), activeUsers = sum(activeUsers),
            averageSessionDuration = mean(averageSessionDuration), .groups = "drop")

wp_pages   <- clean_pages %>% filter(hostName %in% wp_hosts)
ckan_pages <- clean_pages %>% filter(hostName %in% ckan_hosts)

clean_cities <- df_geo %>%
  filter(city != "(not set)") %>%
  group_by(country, region, city) %>%
  summarise(activeUsers = sum(activeUsers), sessions = sum(sessions), .groups = "drop")

# CKAN content effectiveness via API
ckan_views <- ckan_pages %>%
  mutate(
    pageTitle = gsub(" - SIPex| - Organizations - SIPex| - Categories - SIPex| - Resource - SIPex", "", pageTitle),
    slug      = gsub("^/dataset/([^/]+).*", "\\1", pagePath)
  ) %>%
  filter(grepl("^/dataset/", pagePath)) %>%
  select(pageTitle, pagePath, slug, screenPageViews)

resolve_slug_to_uuid <- function(slug) {
  tryCatch({
    resp <- jsonlite::fromJSON(paste0(
      "https://resources.sipexchangebc.com/api/3/action/package_show?id=", slug))
    if (resp$success) resp$result$id else NA_character_
  }, error = function(e) NA_character_)
}

cat("Resolving", length(unique(ckan_views$slug)), "slugs via CKAN API...\n")
slug_uuid_map <- data.frame(
  slug       = unique(ckan_views$slug),
  dataset_id = sapply(unique(ckan_views$slug), resolve_slug_to_uuid),
  stringsAsFactors = FALSE
)

ckan_views <- ckan_views %>% left_join(slug_uuid_map, by = "slug")

ckan_downloads_hosted <- clean_downloads %>%
  filter(grepl("resources.sipexchangebc.com", linkUrl)) %>%
  mutate(dataset_id = gsub(".*dataset/([a-f0-9-]+)/resource.*", "\\1", linkUrl)) %>%
  group_by(dataset_id) %>%
  summarise(hosted_downloads = sum(eventCount), .groups = "drop")

content_effectiveness <- ckan_views %>%
  left_join(ckan_downloads_hosted, by = "dataset_id") %>%
  mutate(hosted_downloads = ifelse(is.na(hosted_downloads), 0, hosted_downloads),
         download_rate    = round(hosted_downloads / screenPageViews * 100, 1)) %>%
  filter(screenPageViews >= 5, !grepl("^Resource$", pageTitle)) %>%
  arrange(desc(screenPageViews))


# =============================================================================
# PART 2 — TREND PERIOD FILTERS & TRANSFORMS
# =============================================================================

clean_trend_engagement <- df_trend_engagement %>%
  filter(!country %in% bot_countries, averageSessionDuration >= 10) %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  group_by(month_date) %>%
  summarise(
    activeUsers            = sum(activeUsers),
    averageSessionDuration = sum(averageSessionDuration * activeUsers) / sum(activeUsers),
    engagementRate         = sum(engagementRate * activeUsers) / sum(activeUsers),
    .groups = "drop"
  ) %>%
  arrange(month_date)

clean_trend_engagement_usca <- df_trend_engagement_usca %>%
  filter(country %in% c("United States", "Canada"), averageSessionDuration >= 10) %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  group_by(month_date) %>%
  summarise(
    activeUsers            = sum(activeUsers),
    averageSessionDuration = sum(averageSessionDuration * activeUsers) / sum(activeUsers),
    engagementRate         = sum(engagementRate * activeUsers) / sum(activeUsers),
    .groups = "drop"
  ) %>%
  arrange(month_date)

clean_trend_search <- df_trend_search %>%
  filter(!country %in% bot_countries, !is.na(searchTerm),
         searchTerm != "(not set)", searchTerm != "",
         nchar(searchTerm) <= 100, !grepl("^[0-9]+$", searchTerm)) %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-")))

clean_trend_downloads <- df_trend_downloads %>%
  filter(eventName == "file_download", !country %in% bot_countries) %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-")))

prior_search_terms <- clean_trend_search %>%
  filter(month_date < as.Date(date_start)) %>%
  pull(searchTerm) %>% unique()

new_this_month <- clean_search %>%
  filter(!searchTerm %in% prior_search_terms) %>%
  arrange(desc(eventCount))

consistent_search <- clean_trend_search %>%
  group_by(searchTerm) %>%
  summarise(months_active  = n_distinct(month_date),
            total_searches = sum(eventCount), .groups = "drop") %>%
  filter(months_active >= 3) %>%
  arrange(desc(total_searches))

consistent_downloads <- clean_trend_downloads %>%
  group_by(linkText, linkUrl) %>%
  summarise(months_active   = n_distinct(month_date),
            total_downloads = sum(eventCount), .groups = "drop") %>%
  filter(months_active >= 3) %>%
  arrange(desc(total_downloads))

# CKAN tag analysis
cat("Fetching CKAN tags via API...\n")
fetch_ckan_tags <- function() {
  tryCatch({
    resp <- jsonlite::fromJSON(
      "https://resources.sipexchangebc.com/api/3/action/tag_list?all_fields=true")
    if (resp$success && is.data.frame(resp$result))
      resp$result %>% select(name, display_name) %>%
      mutate(across(everything(), as.character))
    else NULL
  }, error = function(e) NULL)
}

fetch_tag_count <- function(tag_name) {
  tryCatch({
    resp <- jsonlite::fromJSON(paste0(
      "https://resources.sipexchangebc.com/api/3/action/package_search?fq=tags:%22",
      URLencode(tag_name, reserved = TRUE), "%22&rows=0"))
    if (resp$success) resp$result$count else 0L
  }, error = function(e) 0L)
}

all_tags <- fetch_ckan_tags()
if (!is.null(all_tags) && nrow(all_tags) > 0) {
  cat("Found", nrow(all_tags), "tags. Fetching dataset counts...\n")
  all_tags$dataset_count <- sapply(all_tags$name, fetch_tag_count)
  tag_analysis <- all_tags %>%
    left_join(clean_tags_ga %>% rename(name = tag), by = "name") %>%
    mutate(views = ifelse(is.na(views), 0L, views),
           users = ifelse(is.na(users), 0L, users)) %>%
    arrange(desc(views))
  tag_gaps        <- tag_analysis %>% filter(views == 0, dataset_count > 0) %>% arrange(desc(dataset_count))
  tag_high_demand <- tag_analysis %>% filter(views > 0, dataset_count <= 3) %>% arrange(desc(views))
} else {
  tag_analysis <- tag_gaps <- tag_high_demand <- data.frame()
}


# =============================================================================
# PDF HELPERS
# =============================================================================

section_title <- function(title, subtitle = NULL, description = NULL) {
  grid.newpage()
  grid.rect(gp = gpar(fill = "#2c7bb6", col = NA))
  title_y <- ifelse(!is.null(description), 0.65, ifelse(!is.null(subtitle), 0.55, 0.5))
  grid.text(title, x = 0.5, y = title_y,
            gp = gpar(col = "white", fontsize = 22, fontface = "bold"))
  if (!is.null(subtitle))
    grid.text(subtitle, x = 0.5, y = title_y - 0.13,
              gp = gpar(col = "#d0e8ff", fontsize = 13))
  if (!is.null(description)) {
    for (j in seq_along(strwrap(description, width = 85)))
      grid.text(strwrap(description, width = 85)[j], x = 0.5,
                y = 0.30 - (j - 1) * 0.07,
                gp = gpar(col = "#d0e8ff", fontsize = 11), just = "centre")
  }
}

make_table <- function(df, title, subtitle = NULL, description = NULL, footnote = NULL) {
  section_title(title, subtitle, description)
  grid.newpage()
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
  grid.rect(gp = gpar(fill = "#2c7bb6", col = NA))
  grid.text(title,    x = 0.5, y = 0.62, gp = gpar(col = "white",   fontsize = 28, fontface = "bold"))
  grid.text(subtitle, x = 0.5, y = 0.50, gp = gpar(col = "white",   fontsize = 18))
  grid.text(label,    x = 0.5, y = 0.42, gp = gpar(col = "#d0e8ff", fontsize = 14))
  grid.text(paste("Generated:", Sys.Date()), x = 0.5, y = 0.32,
            gp = gpar(col = "#d0e8ff", fontsize = 12))
}


# =============================================================================
# PART 1 — MONTHLY SNAPSHOT PLOTS
# =============================================================================

p_trend <- ggplot(clean_traffic, aes(x = as.Date(date), y = sessions)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "#d7191c", linetype = "dashed") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
  labs(title = paste("Daily Sessions -", date_start, "to", date_end),
       subtitle = geo_label, x = NULL, y = "Sessions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

p_channels <- ggplot(clean_channels,
                     aes(x = reorder(sessionDefaultChannelGroup, sessions), y = sessions)) +
  geom_col(fill = "#2c7bb6") + coord_flip() +
  labs(title = "Sessions by Channel", subtitle = geo_label, x = NULL, y = "Sessions") +
  theme_minimal()

p_sources <- clean_sources %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(sessionSourceMedium, sessions), y = sessions)) +
  geom_col(fill = "#2c7bb6") + coord_flip() +
  labs(title = "Top Traffic Sources", subtitle = "Direct traffic excluded",
       x = NULL, y = "Sessions") +
  theme_minimal() + theme(axis.text.y = element_text(size = 8))

p_new_returning <- clean_traffic %>%
  summarise(New = sum(newUsers), Returning = sum(returningUsers)) %>%
  pivot_longer(everything(), names_to = "type", values_to = "users") %>%
  ggplot(aes(x = "", y = users, fill = type)) +
  geom_col(width = 1) + coord_polar("y") +
  labs(title = "New vs Returning Users", fill = NULL) + theme_void()

p_devices <- ggplot(clean_devices, aes(x = "", y = activeUsers, fill = deviceCategory)) +
  geom_col(width = 1) + coord_polar("y") +
  labs(title = "Users by Device", fill = "Device") + theme_void()

p_search <- clean_search %>% arrange(desc(eventCount)) %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(searchTerm, eventCount), y = eventCount)) +
  geom_col(fill = "#2c7bb6") + coord_flip() +
  labs(title = "Top Search Terms This Month", x = NULL, y = "Searches") + theme_minimal()

p_cities <- clean_cities %>% arrange(desc(activeUsers)) %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(city, activeUsers), y = activeUsers)) +
  geom_col(fill = "#2c7bb6") + coord_flip() +
  labs(title = "Top Cities by Users", x = NULL, y = "Active Users") + theme_minimal()


# =============================================================================
# PART 2 — TREND PERIOD PLOTS
# =============================================================================

p_engagement_time <- ggplot(clean_trend_engagement,
                            aes(x = month_date, y = averageSessionDuration / 60)) +
  geom_line(color = "#2c7bb6", linewidth = 1) + geom_point(color = "#2c7bb6", size = 2.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = paste("Avg Engagement Time per Session (minutes) -", trend_label),
       subtitle = "Average total time a visitor spent on the site per visit.",
       x = NULL, y = "Minutes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 9, color = "grey50"))

p_engagement_time_usca <- ggplot(clean_trend_engagement_usca,
                                 aes(x = month_date, y = averageSessionDuration / 60)) +
  geom_line(color = "#2c7bb6", linewidth = 1) + geom_point(color = "#2c7bb6", size = 2.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = paste("Avg Engagement Time - US & Canada -", trend_label),
       subtitle = "Average total time a US/CA visitor spent on the site per visit.",
       x = NULL, y = "Minutes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 9, color = "grey50"))

p_users_trend_usca <- ggplot(clean_trend_engagement_usca,
                             aes(x = month_date, y = activeUsers)) +
  geom_col(fill = "#2c7bb6") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Active Users per Month - US & Canada -", trend_label),
       x = NULL, y = "Active Users") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# =============================================================================
# EXPORT 1 — MONTHLY SNAPSHOT PDF
# =============================================================================

pdf_monthly <- paste0("SIPex_Monthly_", gsub("-", "", date_start), "_",
                      gsub("-", "", date_end), ".pdf")
pdf(pdf_monthly, width = 11, height = 8.5)

cover_page("SIPex Monthly Analytics Report",
           paste(date_start, "to", date_end), geo_label)

# --- 1. traffic snapshot ---
section_title("1. Traffic Snapshot", date_start,
              "Key activity metrics for the reporting month.")
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2, widths = unit(c(1,1), "null"))))
metrics_list <- list(
  list("Total Sessions",  sum(clean_traffic$sessions)),
  list("Active Users",    sum(clean_traffic$activeUsers)),
  list("New Users",       sum(clean_traffic$newUsers)),
  list("Returning Users", sum(clean_traffic$returningUsers)),
  list("Total Pageviews", sum(clean_traffic$screenPageViews)),
  list("Avg Engagement",  paste0(round(mean(clean_traffic$engagementRate) * 100, 1), "%"))
)
positions <- list(c(1,1), c(1,2), c(2,1), c(2,2), c(3,1), c(3,2))
for (i in seq_along(metrics_list)) {
  pushViewport(viewport(layout.pos.row = positions[[i]][1],
                        layout.pos.col = positions[[i]][2]))
  grid.rect(gp = gpar(fill = "#f0f6ff", col = "#2c7bb6", lwd = 1.5))
  grid.text(metrics_list[[i]][[1]], y = 0.62, gp = gpar(fontsize = 13, col = "grey40"))
  grid.text(format(metrics_list[[i]][[2]], big.mark = ","), y = 0.35,
            gp = gpar(fontsize = 26, fontface = "bold", col = "#2c7bb6"))
  popViewport()
}
popViewport()
print(p_trend)
print(p_new_returning)

# --- 2. traffic sources ---
section_title("2. Traffic Sources", geo_label,
              "Where visitors are coming from. Channel = type of traffic. Source/Medium = the actual website or campaign.")
print(p_channels)
print(p_sources)
sources_table <- clean_sources %>% head(TOP_N) %>%
  rename("Source / Medium" = sessionSourceMedium,
         "Sessions" = sessions, "Active Users" = activeUsers)
make_table(sources_table, "2b. Top Traffic Sources Detail", "Direct traffic excluded",
           "Source = the website that referred the visitor. Medium = how they arrived.")

# --- 3. users by city ---
section_title("3. Users by City", geo_label,
              "Geographic distribution of active users at the city level.")
print(p_cities)

# --- 4. top search terms ---
section_title("4. Top Search Terms", "Current month",
              "What visitors searched for on the site this month.")
print(p_search)

# --- 5a. most visited pages - wordpress ---
wp_table <- wp_pages %>%
  arrange(desc(screenPageViews)) %>% head(TOP_N) %>%
  mutate(pageTitle = gsub(" - Silviculture Innovation Program Exchange", "", pageTitle),
         url = paste0("https://sipexchangebc.com", pagePath)) %>%
  select(pageTitle, url, screenPageViews, activeUsers) %>%
  rename("Page" = pageTitle, "URL" = url, "Views" = screenPageViews, "Users" = activeUsers)
make_table(wp_table, "5a. Most Visited Pages - WordPress", "Current month",
           "Top pages visited on the main SIPex website.",
           footnote = "Views = total page loads including repeat visits. Users = distinct people who visited.")

# --- 5b. most visited pages - ckan ---
ckan_table <- ckan_pages %>%
  filter(!pagePath %in% c("/dataset/", "/dataset", "/", "")) %>%
  arrange(desc(screenPageViews)) %>% head(TOP_N) %>%
  mutate(
    pageTitle = gsub(" - SIPex| - Organizations - SIPex| - Categories - SIPex| - Resource - SIPex", "", pageTitle),
    pageTitle = sapply(pageTitle, function(x) {
      if (nchar(x) > 50) paste(strwrap(x, width = 50), collapse = "\n") else x }),
    url = paste0("https://resources.sipexchangebc.com", pagePath)
  ) %>%
  select(pageTitle, url, screenPageViews, activeUsers) %>%
  rename("Page" = pageTitle, "URL" = url, "Views" = screenPageViews, "Users" = activeUsers)
make_table(ckan_table, "5b. Most Visited Pages - CKAN", "Current month",
           "Most viewed resources and datasets on the knowledge hub.",
           footnote = "Views = total page loads including repeat visits. Users = distinct people who visited.")

# --- 6. most downloaded resources ---
dl_table <- clean_downloads %>%
  arrange(desc(eventCount)) %>% head(TOP_N) %>%
  mutate(linkText = ifelse(is.na(linkText) | linkText == "", basename(linkUrl), linkText)) %>%
  rename("Resource" = linkText, "URL" = linkUrl, "Downloads" = eventCount)
make_table(dl_table, "6. Most Downloaded Resources", "Current month",
           "Files and documents downloaded by visitors.")

# --- 7. outbound links ---
ob_table <- clean_outbound %>%
  arrange(desc(eventCount)) %>% head(TOP_N) %>%
  rename("Link" = linkText, "URL" = linkUrl, "Clicks" = eventCount)
make_table(ob_table, "7. Outbound Links", "Current month",
           "External links clicked by visitors.")

# --- devices ---
section_title("Device Breakdown", geo_label, "Breakdown of visits by device type.")
print(p_devices)

dev.off()
cat("\nMonthly report saved as:", pdf_monthly, "\n")


# =============================================================================
# EXPORT 2 — TREND PERIOD PDF
# =============================================================================

pdf_trend <- paste0("SIPex_Trends_", gsub("-", "", trend_start), "_",
                    gsub("-", "", trend_end), ".pdf")
pdf(pdf_trend, width = 11, height = 8.5)

cover_page("SIPex Trend Analytics Report", trend_label, geo_label)

# --- 8. engagement trends ---
section_title("8. Engagement Trends", trend_label,
              "Tracks whether visitors are spending more time and engaging more deeply with content.")
print(p_engagement_time)

section_title("8b. Engagement Trends - US & Canada", trend_label,
              "Same engagement metrics filtered to US and Canada visitors only.")
print(p_users_trend_usca)
print(p_engagement_time_usca)

# --- 9. search trends ---
section_title("9. Search Trends", trend_label,
              "Identifies emerging topics and recurring searches.")
print(p_search)

if (nrow(new_this_month) > 0) {
  new_search_table <- new_this_month %>% head(TOP_N) %>%
    mutate(searchTerm = sapply(searchTerm, function(x) {
      if (nchar(x) > 60) paste(strwrap(x, width = 60), collapse = "\n") else x })) %>%
    rename("New Search Term" = searchTerm, "Searches" = eventCount, "Users" = activeUsers)
  make_table(new_search_table, "9a. New Search Terms This Month",
             "Terms not searched in any prior month",
             "Newly emerging topics - consider whether existing content addresses them.")
}

if (nrow(consistent_search) > 0) {
  consistent_table <- consistent_search %>% head(TOP_N) %>%
    rename("Search Term" = searchTerm, "Months Active" = months_active,
           "Total Searches" = total_searches)
  make_table(consistent_table, "9b. Consistently Searched Terms",
             "Searched in 3 or more months across the trend period",
             "These terms have been searched repeatedly over multiple months. If resources exist but searching continues, content may be hard to find. If no resources exist, consider adding them.")
}

# --- 10. downloads trends ---
section_title("10. Download Trends", trend_label,
              "Shows which resources are consistently downloaded over time.")

if (nrow(consistent_downloads) > 0) {
  dl_trend_table <- consistent_downloads %>% head(TOP_N) %>%
    mutate(
      linkText = ifelse(is.na(linkText) | linkText == "", basename(linkUrl), linkText),
      linkText = sapply(linkText, function(x) {
        if (nchar(x) > 50) paste(strwrap(x, width = 50), collapse = "\n") else x }),
      linkUrl = sapply(linkUrl, function(x) {
        if (nchar(x) > 60) paste(strwrap(x, width = 60), collapse = "\n") else x })
    ) %>%
    rename("Resource" = linkText, "URL" = linkUrl,
           "Months Active" = months_active, "Total Downloads" = total_downloads)
  make_table(dl_trend_table, "10a. Consistently Downloaded Resources",
             "Downloaded in 3 or more months",
             "These resources have sustained demand and represent the most valued content.")
}

dev.off()
cat("Trend report saved as:", pdf_trend, "\n")
cat("Done!\n")