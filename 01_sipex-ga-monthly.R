# *****************************************************************************
#' SIPex - Monthly Snapshot Report (Sections 1-7)
#' Outputs: SIPex_Monthly_YYYYMMDD_YYYYMMDD.pdf
#'
#' Author: R. Vizcarra - Eclipse Geomatics Ltd.
# *****************************************************************************

source("00_init.R")

# *****************************************************************************
# set params
# *****************************************************************************

date_start <- "2026-05-01"
date_end   <- "2026-05-31"

# set to TRUE to report US & Canada only, FALSE for all users
usca_only <- FALSE

# num of rows in top-N tables
TOP_N <- 20

geo_label <- if (usca_only) "US & Canada" else "All Regions"

# *****************************************************************************
# pull data
# *****************************************************************************

df_traffic <- ga_data(PROPERTY_ID,
                      metrics = c("sessions", "activeUsers", "newUsers", "screenPageViews",
                                  "averageSessionDuration", "engagementRate"),
                      dimensions = c("date", "country"), date_range = c(date_start, date_end))

df_channels <- ga_data(PROPERTY_ID,
                       metrics = c("sessions", "activeUsers"),
                       dimensions = c("sessionDefaultChannelGroup", "country"),
                       date_range = c(date_start, date_end))

df_sources <- ga_data(PROPERTY_ID,
                      metrics = c("sessions", "activeUsers"),
                      dimensions = c("sessionSourceMedium", "country"),
                      date_range = c(date_start, date_end), limit = 200)

df_pages <- ga_data(PROPERTY_ID,
                    metrics = c("screenPageViews", "activeUsers", "averageSessionDuration"),
                    dimensions = c("pageTitle", "pagePath", "hostName", "country"),
                    date_range = c(date_start, date_end), limit = 500)

df_geo <- ga_data(PROPERTY_ID,
                  metrics = c("activeUsers", "sessions"),
                  dimensions = c("country", "region", "city"),
                  date_range = c(date_start, date_end), limit = 100)

df_devices <- ga_data(PROPERTY_ID,
                      metrics = c("activeUsers", "sessions"),
                      dimensions = c("deviceCategory", "country"),
                      date_range = c(date_start, date_end))

df_search <- ga_data(PROPERTY_ID,
                     metrics = c("eventCount", "activeUsers"),
                     dimensions = c("searchTerm", "country"),
                     date_range = c(date_start, date_end), limit = 200)

df_downloads <- ga_data(PROPERTY_ID,
                        metrics = c("eventCount"),
                        dimensions = c("eventName", "linkText", "linkUrl", "country"),
                        date_range = c(date_start, date_end), limit = 500)

df_outbound <- ga_data(PROPERTY_ID,
                       metrics    = c("eventCount"),
                       dimensions = c("eventName", "linkText", "linkUrl", "pagePath", "country"),
                       date_range = c(date_start, date_end), limit = 500)

# *****************************************************************************
# fix unicode
# *****************************************************************************

# GA4 uses a weird '-' symbol that breaks the urls, this fixes it
clean_unicode <- function(x) gsub("−", "-", x, fixed = TRUE)

if (exists("df_downloads"))
  df_downloads <- df_downloads %>% 
  mutate(linkUrl = clean_unicode(linkUrl), linkText = clean_unicode(linkText))
if (exists("df_outbound"))
  df_outbound <- df_outbound %>% 
  mutate(linkUrl = clean_unicode(linkUrl), linkText = clean_unicode(linkText))
if (exists("df_pages"))
  df_pages <- df_pages %>% 
  mutate(pagePath = clean_unicode(pagePath), pageTitle = clean_unicode(pageTitle))

# *****************************************************************************
# filter & transform
# *****************************************************************************

clean_traffic <- df_traffic %>%
  filter(!country %in% bot_countries, averageSessionDuration >= 10) %>%
  group_by(date) %>%
  summarise(
    sessions = sum(sessions),
    activeUsers = sum(activeUsers),
    newUsers = sum(newUsers),
    screenPageViews = sum(screenPageViews),
    averageSessionDuration = sum(averageSessionDuration * activeUsers) / sum(activeUsers),
    engagementRate = sum(engagementRate * activeUsers) / sum(activeUsers),
    .groups = "drop"
  ) %>%
  mutate(returningUsers = activeUsers - newUsers)

clean_channels <- df_channels %>%
  filter(!country %in% bot_countries) %>%
  group_by(sessionDefaultChannelGroup) %>%
  summarise(sessions = sum(sessions), activeUsers = sum(activeUsers), .groups = "drop")

clean_sources <- df_sources %>%
  filter(!country %in% bot_countries,
         !grepl("singapore|bot|spider|crawl", sessionSourceMedium, ignore.case = TRUE),
         sessionSourceMedium != "(direct) / (none)") %>%
  group_by(sessionSourceMedium) %>%
  summarise(sessions = sum(sessions), activeUsers = sum(activeUsers), .groups = "drop") %>%
  arrange(desc(sessions))

clean_pages <- df_pages %>%
  filter(!grepl(paste(bot_patterns, collapse = "|"), pageTitle, ignore.case = TRUE),
         !hostName %in% bot_hosts, !country %in% bot_countries)

df_geo <- df_geo %>% 
  filter(!country %in% bot_countries)

clean_devices <- df_devices %>%
  filter(!country %in% bot_countries) %>%
  group_by(deviceCategory) %>%
  summarise(activeUsers = sum(activeUsers), sessions = sum(sessions), .groups = "drop")

clean_search <- df_search %>%
  filter(!country %in% bot_countries, !is.na(searchTerm),
         searchTerm != "(not set)", searchTerm != "",
         nchar(searchTerm) <= 100, !grepl("^[0-9]+$", searchTerm)) %>%
  group_by(searchTerm) %>%
  summarise(eventCount = sum(eventCount), activeUsers = sum(activeUsers), .groups = "drop")

clean_downloads <- df_downloads %>%
  filter(eventName == "file_download", !country %in% bot_countries) %>%
  group_by(linkText, linkUrl) %>%
  summarise(eventCount = sum(eventCount), .groups = "drop")

# GA4 collects partial urls and this fixes it 
resolve_resource_url <- function(partial_url) {
  if (!grepl("resources.sipexchangebc.com/dataset/.+/resource/", partial_url))
    return(partial_url)
  resource_partial <- gsub(".*/resource/", "", partial_url)
  if (nchar(resource_partial) >= 36) return(partial_url)
  dataset_id <- gsub(".*/dataset/([^/]+)/resource/.*", "\\1", partial_url)
  tryCatch({
    resp <- jsonlite::fromJSON(paste0(
      "https://resources.sipexchangebc.com/api/3/action/package_show?id=", dataset_id))
    if (resp$success) {
      resources <- resp$result$resources
      match <- resources[startsWith(resources$id, substr(resource_partial, 1, 8)), , drop = FALSE]
      if (nrow(match) > 0) return(match$url[1])
    }
    partial_url
  }, error = function(e) partial_url)
}

clean_downloads <- clean_downloads %>%
  mutate(linkUrl = sapply(linkUrl, resolve_resource_url))

clean_outbound <- df_outbound %>%
  filter(eventName == "click", !country %in% bot_countries,
         !grepl("sipexchangebc.com", linkUrl, ignore.case = TRUE),
         grepl("^/dataset/", pagePath)) %>%
  group_by(linkText, linkUrl, pagePath) %>%
  summarise(eventCount = sum(eventCount), .groups = "drop")

if (usca_only) {
  clean_pages <- clean_pages %>% 
    filter(country %in% c("United States", "Canada"))
  
  clean_channels  <- clean_channels %>% 
    filter(country %in% c("United States", "Canada"))
  
  df_geo <- df_geo %>% 
    filter(country %in% c("United States", "Canada"))
  
  clean_search <- clean_search %>% 
    filter(country %in% c("United States", "Canada"))
  
  clean_downloads <- clean_downloads %>% 
    filter(country %in% c("United States", "Canada"))
  
  clean_outbound  <- clean_outbound %>% 
    filter(country %in% c("United States", "Canada"))
}

clean_pages <- clean_pages %>%
  group_by(pageTitle, pagePath, hostName) %>%
  summarise(screenPageViews = sum(screenPageViews), activeUsers = sum(activeUsers),
            averageSessionDuration = mean(averageSessionDuration), .groups = "drop")

wp_pages   <- clean_pages %>% 
  filter(hostName %in% wp_hosts)

ckan_pages <- clean_pages %>% 
  filter(hostName %in% ckan_hosts)

clean_cities <- df_geo %>%
  filter(city != "(not set)") %>%
  group_by(country, region, city) %>%
  summarise(activeUsers = sum(activeUsers), sessions = sum(sessions), .groups = "drop")

# finds the dataset title from slug using ckan api
resolve_slug_to_title <- function(slug) {
  if (is.na(slug)) return(NA_character_)
  tryCatch({
    resp <- jsonlite::fromJSON(paste0(
      "https://resources.sipexchangebc.com/api/3/action/package_show?id=", slug))
    if (resp$success) resp$result$title else NA_character_
  }, error = function(e) NA_character_)
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

p_trend <- ggplot(clean_traffic, aes(x = as.Date(date), y = sessions)) +
  geom_line(color = col_gold, linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = col_lime, linetype = "dashed") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
  labs(title = paste("Daily Sessions -", date_start, "to", date_end),
       subtitle = geo_label, x = NULL, y = "Sessions") +
  dark_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6, color = "grey20"))

p_channels <- ggplot(clean_channels,
                     aes(x = reorder(sessionDefaultChannelGroup, sessions), y = sessions)) +
  geom_col(fill = col_gold) + coord_flip() +
  labs(title = "Sessions by Channel", subtitle = geo_label, x = NULL, y = "Sessions") +
  dark_theme

p_sources <- clean_sources %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(sessionSourceMedium, sessions), y = sessions)) +
  geom_col(fill = col_gold) + coord_flip() +
  labs(title = "Top Traffic Sources", subtitle = "Direct traffic excluded",
       x = NULL, y = "Sessions") +
  dark_theme +
  theme(axis.text.y = element_text(size = 7, color = "grey20"),
        plot.margin = margin(10, 10, 10, 120))

p_new_returning <- clean_traffic %>%
  summarise(New = sum(newUsers), Returning = sum(returningUsers)) %>%
  pivot_longer(everything(), names_to = "type", values_to = "users") %>%
  ggplot(aes(x = "", y = users, fill = type)) +
  geom_col(width = 1) + coord_polar("y") +
  scale_fill_manual(values = c("New" = col_gold, "Returning" = col_lime)) +
  labs(title = "New vs Returning Users", fill = NULL) +
  dark_theme +
  theme(axis.text = element_blank(), panel.grid = element_blank())

p_devices <- ggplot(clean_devices, aes(x = "", y = activeUsers, fill = deviceCategory)) +
  geom_col(width = 1) + coord_polar("y") +
  scale_fill_manual(values = c("desktop" = col_gold, "mobile" = col_lime, "tablet" = col_white)) +
  labs(title = "Users by Device", fill = "Device") +
  dark_theme +
  theme(axis.text = element_blank(), panel.grid = element_blank())

p_search <- clean_search %>% arrange(desc(eventCount)) %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(searchTerm, eventCount), y = eventCount)) +
  geom_col(fill = col_gold) + coord_flip() +
  labs(title = "Top Search Terms This Month", x = NULL, y = "Searches") +
  dark_theme

p_cities <- clean_cities %>% arrange(desc(activeUsers)) %>% head(TOP_N) %>%
  ggplot(aes(x = reorder(city, activeUsers), y = activeUsers)) +
  geom_col(fill = col_gold) + coord_flip() +
  labs(title = "Top Cities by Users", x = NULL, y = "Active Users") +
  dark_theme

# *****************************************************************************
# export pdf
# *****************************************************************************

pdf_file <- paste0("SIPex_Monthly_", gsub("-", "", date_start), "_",
                   gsub("-", "", date_end), ".pdf")
pdf(pdf_file, width = 11, height = 8.5)

cover_page("SIPex Monthly Analytics Report", paste(date_start, "to", date_end), geo_label)

# 1. traffic snapshot
section_title("1. Traffic Snapshot", date_start, "Key activity metrics for the reporting month.")
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2, widths = unit(c(1,1), "null"))))
metrics_list <- list(
  list("Total Sessions", sum(clean_traffic$sessions)),
  list("Active Users", sum(clean_traffic$activeUsers)),
  list("New Users", sum(clean_traffic$newUsers)),
  list("Returning Users", sum(clean_traffic$returningUsers)),
  list("Total Pageviews", sum(clean_traffic$screenPageViews)),
  list("Avg Engagement",  paste0(round(mean(clean_traffic$engagementRate) * 100, 1), "%"))
)
positions <- list(c(1,1), c(1,2), c(2,1), c(2,2), c(3,1), c(3,2))
for (i in seq_along(metrics_list)) {
  pushViewport(viewport(layout.pos.row = positions[[i]][1], layout.pos.col = positions[[i]][2]))
  grid.rect(gp = gpar(fill = col_dark, col = col_gold, lwd = 1.5))
  grid.text(metrics_list[[i]][[1]], y = 0.62, gp = gpar(fontsize = 13, col = col_lime))
  grid.text(format(metrics_list[[i]][[2]], big.mark = ","), y = 0.35,
            gp = gpar(fontsize = 26, fontface = "bold", col = col_gold))
  popViewport()
}
popViewport()
print(p_trend)
print(p_new_returning)

# 2. traffic sources
section_title("2. Traffic Sources", geo_label,
              "Where visitors are coming from. Channel = type of traffic. Source/Medium = actual website or campaign.")
print(p_channels)
print(p_sources)

# 3. users by city
section_title("3. Users by City", geo_label, "Geographic distribution of active users at the city level.")
print(p_cities)

# 4. top search terms
section_title("4. Top Search Terms", "Current month", "What visitors searched for on the site this month.")
print(p_search)

# 5a. wordpress pages
wp_table <- wp_pages %>% arrange(desc(screenPageViews)) %>% head(TOP_N) %>%
  mutate(pageTitle = gsub(" - Silviculture Innovation Program Exchange", "", pageTitle),
         url = paste0("https://sipexchangebc.com", pagePath)) %>%
  select(pageTitle, url, screenPageViews, activeUsers) %>%
  rename("Page" = pageTitle, "URL" = url, "Views" = screenPageViews, "Users" = activeUsers)
make_table(wp_table, "5a. Most Visited Pages - WordPress", "Current month",
           "Top pages visited on the main SIPex website.",
           footnote = "Views = total page loads. Users = distinct people who visited.")

# 5b. ckan pages
ckan_table <- ckan_pages %>%
  filter(!pagePath %in% c("/dataset/", "/dataset", "/", "")) %>%
  arrange(desc(screenPageViews)) %>% head(TOP_N) %>%
  mutate(
    pageTitle = gsub(" - SIPex| - Organizations - SIPex| - Categories - SIPex| - Resource - SIPex", "", pageTitle),
    pageTitle = sapply(pageTitle, function(x) {
      if (nchar(x) > 50) paste(strwrap(x, width = 50), collapse = "\n") else x }),
    url = paste0("https://resources.sipexchangebc.com", pagePath),
    url = sapply(url, function(x) {
      if (nchar(x) > 60) paste(strwrap(x, width = 60), collapse = "\n") else x })) %>%
  select(pageTitle, url, screenPageViews, activeUsers) %>%
  rename("Page" = pageTitle, "URL" = url, "Views" = screenPageViews, "Users" = activeUsers)
make_table(ckan_table, "5b. Most Visited Pages - CKAN", "Current month",
           "Most viewed resources and datasets on the knowledge hub.",
           footnote = "Views = total page loads. Users = distinct people who visited.")

# 6. downloads
if (nrow(clean_downloads) > 0) {
  dl_table <- clean_downloads %>%
    mutate(
      dataset_slug = ifelse(
        grepl("resources.sipexchangebc.com/dataset/", linkUrl),
        gsub(".*/dataset/([^/]+)/resource.*", "\\1", linkUrl),
        NA_character_
      )
    ) %>%
    filter(!is.na(dataset_slug)) %>%
    group_by(dataset_slug) %>%
    summarise(eventCount = sum(eventCount), .groups = "drop") %>%
    arrange(desc(eventCount)) %>%
    head(TOP_N)
  
  dl_table <- dl_table %>%
    mutate(
      datasetTitle = sapply(dataset_slug, resolve_slug_to_title),
      datasetTitle = ifelse(is.na(datasetTitle), dataset_slug, datasetTitle),
      datasetTitle = sapply(datasetTitle, function(x) {
        if (nchar(x) > 80) paste(strwrap(x, width = 80), collapse = "\n") else x })
    ) %>%
    select(datasetTitle, eventCount) %>%
    rename("Dataset" = datasetTitle, "Downloads" = eventCount)
  
  make_table(dl_table, "6. Most Downloaded Resources", "Current month",
             "Total downloads per dataset.",
             footnote = "Downloads aggregated across all resources within each dataset.")
}

# 7. outbound
if (nrow(clean_outbound) > 0) {
  ob_raw <- clean_outbound %>% arrange(desc(eventCount)) %>% head(TOP_N) %>%
    mutate(dataset_slug = gsub("^/dataset/([^/]+).*", "\\1", pagePath))
  
  ob_raw <- ob_raw %>%
    mutate(datasetTitle = sapply(dataset_slug, resolve_slug_to_title),
           datasetTitle = ifelse(is.na(datasetTitle), dataset_slug, datasetTitle),
           datasetTitle = sapply(datasetTitle, function(x) {
             if (nchar(x) > 40) paste(strwrap(x, width = 40), collapse = "\n") else x }),
           linkUrl = sapply(linkUrl, function(x) {
             if (nchar(x) > 50) paste(strwrap(x, width = 50), collapse = "\n") else x }))
  
  ob_table <- ob_raw %>%
    select(datasetTitle, linkUrl, eventCount) %>%
    rename("Dataset" = datasetTitle, "Outbound URL" = linkUrl, "Clicks" = eventCount)
  
  make_table(ob_table, "7. Outbound Links", "Current month",
             "External links clicked from CKAN dataset pages.")
}

# devices
section_title("Device Breakdown", geo_label, "Breakdown of visits by device type.")
print(p_devices)

dev.off()
cat("\nMonthly report saved as:", pdf_file, "\n")