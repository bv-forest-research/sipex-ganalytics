# *****************************************************************************
#' 02_sipex-ga-trends.R
#' Outputs: SIPex_Trends_YYYYMMDD_YYYYMMDD.pdf
#'
#' Author: R. Vizcarra - Eclipse Geomatics Ltd.
# *****************************************************************************

source("00_init.R")

# *****************************************************************************
# update these each month
# *****************************************************************************

# current month (used to identify new search terms vs prior months)
date_start <- "2026-03-01"
date_end   <- "2026-03-31"

# trend period - any range, reported monthly
trend_start <- "2025-10-01"
trend_end   <- "2026-04-30"
trend_label <- paste(format(as.Date(trend_start), "%b %Y"), "to",
                     format(as.Date(trend_end),   "%b %Y"))

# set to TRUE to report US & Canada only, FALSE for all users
usca_only <- FALSE

# number of rows in top-N tables
TOP_N <- 20

geo_label <- if (usca_only) "US & Canada" else "All Regions"

# *****************************************************************************
# pull data
# *****************************************************************************

df_search_monthly <- ga_data(PROPERTY_ID,
                             metrics    = c("eventCount", "activeUsers"),
                             dimensions = c("searchTerm", "country"),
                             date_range = c(date_start, date_end), limit = 200)

df_trend_engagement <- ga_data(PROPERTY_ID,
                               metrics    = c("activeUsers", "averageSessionDuration", "engagementRate"),
                               dimensions = c("year", "month", "country"),
                               date_range = c(trend_start, trend_end))

df_trend_engagement_usca <- ga_data(PROPERTY_ID,
                                    metrics    = c("activeUsers", "averageSessionDuration", "engagementRate"),
                                    dimensions = c("year", "month", "country"),
                                    date_range = c(trend_start, trend_end))

df_trend_search <- ga_data(PROPERTY_ID,
                           metrics    = c("eventCount"),
                           dimensions = c("year", "month", "searchTerm", "country"),
                           date_range = c(trend_start, trend_end), limit = 1000)

df_trend_downloads <- ga_data(PROPERTY_ID,
                              metrics    = c("eventCount"),
                              dimensions = c("year", "month", "eventName", "linkText", "linkUrl", "country"),
                              date_range = c(trend_start, trend_end), limit = 1000)

# *****************************************************************************
# clean unicode minus
# *****************************************************************************

clean_unicode <- function(x) gsub("−", "-", x, fixed = TRUE)

df_trend_downloads <- df_trend_downloads %>%
  mutate(linkUrl = clean_unicode(linkUrl), linkText = clean_unicode(linkText))

# *****************************************************************************
# filter & transform
# *****************************************************************************

clean_search <- df_search_monthly %>%
  filter(!country %in% bot_countries, !is.na(searchTerm),
         searchTerm != "(not set)", searchTerm != "",
         nchar(searchTerm) <= 100, !grepl("^[0-9]+$", searchTerm)) %>%
  group_by(searchTerm) %>%
  summarise(eventCount = sum(eventCount), activeUsers = sum(activeUsers), .groups = "drop")

clean_trend_engagement <- df_trend_engagement %>%
  filter(!country %in% bot_countries, averageSessionDuration >= 10) %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  group_by(month_date) %>%
  summarise(
    activeUsers            = sum(activeUsers),
    averageSessionDuration = sum(averageSessionDuration * activeUsers) / sum(activeUsers),
    engagementRate         = sum(engagementRate * activeUsers) / sum(activeUsers),
    .groups = "drop") %>%
  arrange(month_date)

clean_trend_engagement_usca <- df_trend_engagement_usca %>%
  filter(country %in% c("United States", "Canada"), averageSessionDuration >= 10) %>%
  mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  group_by(month_date) %>%
  summarise(
    activeUsers            = sum(activeUsers),
    averageSessionDuration = sum(averageSessionDuration * activeUsers) / sum(activeUsers),
    engagementRate         = sum(engagementRate * activeUsers) / sum(activeUsers),
    .groups = "drop") %>%
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

# search vs tag gap
fetch_ckan_tags <- function() {
  tryCatch({
    resp <- jsonlite::fromJSON(
      "https://resources.sipexchangebc.com/api/3/action/tag_list?all_fields=true")
    if (resp$success && is.data.frame(resp$result))
      resp$result %>% select(name, display_name) %>% mutate(across(everything(), as.character))
    else NULL
  }, error = function(e) NULL)
}

# fetch all ckan tags
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
  all_tags$dataset_count <- sapply(all_tags$name, fetch_tag_count)
  tag_analysis  <- all_tags %>% arrange(desc(dataset_count))
  existing_tags <- tolower(tag_analysis$display_name)
  search_tag_gap <- clean_search %>%
    filter(!tolower(searchTerm) %in% existing_tags) %>%
    arrange(desc(eventCount)) %>% head(TOP_N) %>%
    rename("Search Term" = searchTerm, "Searches" = eventCount, "Users" = activeUsers)
} else {
  search_tag_gap <- data.frame()
}

# *****************************************************************************
# plots
# *****************************************************************************

dark_theme <- theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    text = element_text(color = "grey20"),
    axis.text = element_text(color = "grey20"),
    plot.title = element_text(color = col_dark, fontface = "bold"),
    plot.subtitle = element_text(color = col_dark)
  )

p_engagement_time <- ggplot(clean_trend_engagement,
                            aes(x = month_date, y = averageSessionDuration / 60)) +
  geom_line(color = col_gold, linewidth = 1) +
  geom_point(color = col_gold, size = 2.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = paste("Avg Engagement Time per Session (minutes) -", trend_label),
       subtitle = "Average total time a visitor spent on the site per visit.",
       x = NULL, y = "Minutes") +
  dark_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 9, color = "grey50"))

p_users_trend_usca <- ggplot(clean_trend_engagement_usca, aes(x = month_date, y = activeUsers)) +
  geom_col(fill = col_gold) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste("Active Users per Month - US & Canada -", trend_label),
       x = NULL, y = "Active Users") +
  dark_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_engagement_time_usca <- ggplot(clean_trend_engagement_usca,
                                 aes(x = month_date, y = averageSessionDuration / 60)) +
  geom_line(color = col_gold, linewidth = 1) +
  geom_point(color = col_gold, size = 2.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = paste("Avg Engagement Time - US & Canada -", trend_label),
       subtitle = "Average total time a US/CA visitor spent on the site per visit.",
       x = NULL, y = "Minutes") +
  dark_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 9, color = "grey50"))

p_search_trend <- clean_search %>% arrange(desc(eventCount)) %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(searchTerm, eventCount), y = eventCount)) +
  geom_col(fill = col_gold) + coord_flip() +
  labs(title = "Top Search Terms This Month", x = NULL, y = "Searches") +
  dark_theme +
  theme(axis.text.y = element_text(size = 7, color = "grey20"),
        plot.margin = margin(10, 10, 10, 120))

# *****************************************************************************
# export pdf
# *****************************************************************************

pdf_file <- paste0("SIPex_Trends_", gsub("-", "", trend_start), "_",
                   gsub("-", "", trend_end), ".pdf")
pdf(pdf_file, width = 11, height = 8.5)

cover_page("SIPex Trend Analytics Report", trend_label, geo_label)

# 1. engagement trends
section_title("1. Engagement Trends", trend_label,
              "Tracks whether visitors are spending more time and engaging more deeply with content.")
print(p_engagement_time)

section_title("1b. Engagement Trends - US & Canada", trend_label,
              "Same engagement metrics filtered to US and Canada visitors only.")
print(p_users_trend_usca)
print(p_engagement_time_usca)

# 2. search trends
section_title("2. Search Trends", trend_label, "Identifies emerging topics and recurring searches.")
print(p_search_trend)

if (nrow(new_this_month) > 0) {
  new_search_table <- new_this_month %>% head(TOP_N) %>%
    mutate(searchTerm = sapply(searchTerm, function(x) {
      if (nchar(x) > 60) paste(strwrap(x, width = 60), collapse = "\n") else x })) %>%
    rename("New Search Term" = searchTerm, "Searches" = eventCount, "Users" = activeUsers)
  make_table(new_search_table, "2a. New Search Terms This Month",
             "Terms not searched in any prior month",
             "Newly emerging topics - consider whether existing content addresses them.")
}

if (nrow(consistent_search) > 0) {
  consistent_table <- consistent_search %>% head(TOP_N) %>%
    rename("Search Term" = searchTerm, "Months Active" = months_active,
           "Total Searches" = total_searches)
  make_table(consistent_table, "2b. Consistently Searched Terms",
             "Searched in 3 or more months across the trend period",
             "These terms have been searched repeatedly. If resources exist but searching continues, content may be hard to find. If no resources exist, consider adding them.")
}

if (nrow(search_tag_gap) > 0) {
  make_table(search_tag_gap, "2c. Search Terms With No Matching Tag",
             "Users searched for these but no tag exists",
             "These topics are being searched but have no corresponding CKAN tag. Consider adding tags for high-volume terms to improve navigation.")
}

# 3. download trends
section_title("3. Download Trends", trend_label,
              "Shows which resources are consistently downloaded over time.")

if (nrow(consistent_downloads) > 0) {
  dl_trend_table <- consistent_downloads %>% head(TOP_N) %>%
    mutate(
      linkText = ifelse(is.na(linkText) | linkText == "", basename(linkUrl), linkText),
      linkText = sapply(linkText, function(x) {
        if (nchar(x) > 50) paste(strwrap(x, width = 50), collapse = "\n") else x }),
      linkUrl  = sapply(linkUrl, function(x) {
        if (nchar(x) > 60) paste(strwrap(x, width = 60), collapse = "\n") else x })
    ) %>%
    rename("Resource" = linkText, "URL" = linkUrl,
           "Months Active" = months_active, "Total Downloads" = total_downloads)
  make_table(dl_trend_table, "3a. Consistently Downloaded Resources",
             "Downloaded in 3 or more months",
             "These resources have sustained demand and represent the most valued content.")
}

dev.off()
cat("\nTrend report saved as:", pdf_file, "\n")