# May 2026 - Camryn Tobias
# Purpose: Replication Study - Hanberry (2022) density classification
#          applied to the Denver-Aurora MSA, Colorado
# GEOG 490

# Libraries
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(scales)
library(patchwork)

options(tigris_use_cache = TRUE)

#-------------------------------------------------

# Hanberry (2022) Table 4 - Worldpop thresholds (people per km²)
# Wildland:      0-1       } All collapsed
# Inhabited:     1-15      } into Exurban
# Exurban low:   15-100    }
# Exurban high:  100-250   }
# Suburban low:  250-550   -> "Suburban Low"
# Suburban high: 550-800   -> "Suburban High"
# Urban low:     800-1900  -> "Urban Low"
# Urban high:    >1900     -> "Urban High"

# Verify Denver MSA GEOID
all_cbsa <- core_based_statistical_areas(cb = TRUE, year = 2020)
filter(all_cbsa, str_detect(NAME, "Denver"))

# Denver MSA boundary
denver_metro <- core_based_statistical_areas(
  cb = TRUE,
  year = 2020
) %>%
  filter(GEOID == "19740") %>%
  st_transform(26954)  # Colorado Central projection

# Download Census tract population - 2020 Decennial Census
denver_tracts <- get_decennial(
  geography = "tract",
  variables = "P1_001N",
  state = "CO",
  year = 2020,
  geometry = TRUE
) %>%
  rename(population = value) %>%
  st_transform(26954)

# Filter to Denver MSA only
denver_tracts_metro <- st_filter(denver_tracts, denver_metro)

# How many tracts? Screenshot this for your appendix
nrow(denver_tracts_metro)

denver_tracts_density <- denver_tracts_metro %>%
  mutate(
    # Calculate area in square meters, convert to km²
    area_km2 = as.numeric(st_area(geometry)) / 1000000,
    # Calculate population density (people per km²)
    pop_density = population / area_km2
  )

denver_tracts_density <- denver_tracts_density %>%
  mutate(
    landscape = case_when(
      pop_density < 250            ~ "Exurban",
      pop_density < 550            ~ "Suburban Low",
      pop_density < 800            ~ "Suburban High",
      pop_density < 1900           ~ "Urban Low",
      pop_density >= 1900          ~ "Urban High",
      TRUE                         ~ NA_character_
    ),
    # Make it an ordered factor for better plotting
    landscape = factor(landscape,
                       levels = c("Exurban",
                                  "Suburban Low",
                                  "Suburban High",
                                  "Urban Low",
                                  "Urban High"))
  )

# Quick check - how many tracts in each class?
# Screenshot this for your appendix
count(denver_tracts_density, landscape)

# Check for problematic tracts
denver_tracts_density %>%
  st_drop_geometry() %>%
  filter(population == 0 | area_km2 == 0 | is.na(pop_density)) %>%
  select(GEOID, population, area_km2, pop_density)

denver_tracts_density %>%
  st_drop_geometry() %>%
  group_by(landscape) %>%
  summarize(
    min_density = min(pop_density, na.rm = TRUE),
    max_density = max(pop_density, na.rm = TRUE),
    mean_density = mean(pop_density, na.rm = TRUE),
    n_tracts = n()
  )

denver_tracts_density <- denver_tracts_density %>%
  filter(population > 0)

# Verify they are gone
nrow(filter(denver_tracts_density, population == 0))

count(denver_tracts_density, landscape)

# Map 1: Landscape Classification Map
landscape_map <- tm_shape(denver_tracts_density) +
  tm_polygons(
    col = "landscape",
    palette = c(
      "Exurban"       = "#d9f0a3",
      "Suburban Low"  = "#addd8e",
      "Suburban High" = "#41ab5d",
      "Urban Low"     = "#e08214",
      "Urban High"    = "#8c2d04"
    ),
    title = "Landscape Class\n(Hanberry 2022)",
    showNA = FALSE
  ) +
  tm_shape(denver_metro) +
  tm_borders(col = "black", lwd = 2) +
  tm_title("Denver MSA — Urban Density Classification\n2020 Census")

landscape_map

# Save
tmap_save(landscape_map,
          filename = "output/map1_landscape_classification.pdf",
          width = 10, height = 10, dpi = 300)


# ACS variables for Denver tracts
denver_acs <- get_acs(
  geography = "tract",
  state = "CO",
  variables = c(
    total_pop     = "B01003_001",   # Total population
    median_income = "B19013_001",   # Median household income
    below_poverty = "B17001_002",   # Below poverty line
    total_poverty = "B17001_001",   # Total for poverty calc
    bach_degree   = "B15003_022",   # Bachelor's degree
    grad_degree   = "B15003_023",   # Graduate degree
    total_educ    = "B15003_001",   # Total for education calc
    white         = "B03002_003",   # Non-Hispanic White
    black         = "B03002_004",   # Non-Hispanic Black
    hispanic      = "B03002_012",   # Hispanic
    asian         = "B03002_006",   # Asian
    total_race    = "B03002_001"    # Total for race calc
  ),
  year = 2020,
  output = "wide",
  geometry = FALSE
) %>%
  # Clean up column names - remove E suffix
  rename_with(~ sub("E$", "", .x), ends_with("E")) %>%
  select(-ends_with("M"))  # Remove margin of error columns

# Join ACS data to your density-classified tracts
denver_full <- denver_tracts_density %>%
  left_join(denver_acs, by = "GEOID") %>%
  mutate(
    pct_white   = white / total_race * 100,
    pct_black   = black / total_race * 100,
    pct_hisp    = hispanic / total_race * 100,
    pct_asian   = asian / total_race * 100,
    pct_poverty = below_poverty / total_poverty * 100,
    pct_college = (bach_degree + grad_degree) / total_educ * 100
  )

# Summarize median income by landscape class
income_summary <- denver_full %>%
  st_drop_geometry() %>%
  filter(!is.na(landscape), !is.na(median_income)) %>%
  group_by(landscape) %>%
  summarize(
    median_income_class = median(median_income, na.rm = TRUE),
    .groups = "drop"
  )

# Bar chart
income_chart <- ggplot(income_summary,
                       aes(x = landscape,
                           y = median_income_class,
                           fill = landscape)) +
  geom_col(width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c(
    "Exurban"       = "#d9f0a3",
    "Suburban Low"  = "#addd8e",
    "Suburban High" = "#41ab5d",
    "Urban Low"     = "#e08214",
    "Urban High"    = "#8c2d04"
  )) +
  scale_y_continuous(labels = label_dollar()) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  labs(
    title = "Median Household Income by Landscape Class",
    subtitle = "Denver MSA — 2016-2020 ACS 5-year estimates",
    x = "Landscape Class (Hanberry 2022)",
    y = "Median Household Income",
    caption = "Source: ACS Table B19013; tidycensus R package"
  )

income_chart

ggsave("output/chart_income_by_landscape.png",
       income_chart,
       width = 10, height = 6, dpi = 300, bg = "white")

# Map 2: percent Hispanic population by tract
hispanic_map <- tm_shape(denver_full) +
  tm_polygons(
    col = "pct_hisp",
    palette = "YlOrRd",
    style = "quantile",
    n = 5,
    title = "% Hispanic\n(2020 ACS)"
  ) +
  tm_shape(denver_metro) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_title("Hispanic Population Share by Census Tract\nDenver MSA, 2016-2020 ACS")

hispanic_map

tmap_save(hispanic_map,
          filename = "output/map2_hispanic_population.pdf",
          width = 10, height = 10, dpi = 300)

# Population Pyramids
# Get age/sex data by tract using ACS Table B01001
denver_age <- get_acs(
  geography = "tract",
  state = "CO",
  table = "B01001",
  year = 2020,
  geometry = FALSE
) %>%
  filter(GEOID %in% denver_full$GEOID)

# Join landscape class to age data
denver_age <- denver_age %>%
  left_join(
    st_drop_geometry(denver_full[c("GEOID", "landscape")]),
    by = "GEOID"
  )

# Define male and female variable codes
male_vars <- paste0("B01001_0", c("03","04","05","06","07","08",
                                  "09","10","11","12","13","14",
                                  "15","16","17","18","19","20",
                                  "21","22","23","24","25"))

female_vars <- paste0("B01001_0", c("27","28","29","30","31","32",
                                    "33","34","35","36","37","38",
                                    "39","40","41","42","43","44",
                                    "45","46","47","48","49"))

age_labels <- c("0-4","5-9","10-14","15-17","18-19","20",
                "21","22-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-61","62-64",
                "65-66","67-69","70-74","75-79","80-84","85+")

# Prepare male data
males <- denver_age %>%
  filter(variable %in% male_vars,
         !is.na(landscape)) %>%
  mutate(
    sex = "Male",
    age = age_labels[match(variable, male_vars)],
    age = factor(age, levels = age_labels),
    estimate = -estimate
  )

# Prepare female data
females <- denver_age %>%
  filter(variable %in% female_vars,
         !is.na(landscape)) %>%
  mutate(
    sex = "Female",
    age = age_labels[match(variable, female_vars)],
    age = factor(age, levels = age_labels)
  )

# Combine and aggregate by urban/suburban
pyramid_data <- bind_rows(males, females) %>%
  mutate(
    urban_suburban = case_when(
      landscape %in% c("Urban Low", "Urban High")         ~ "Urban",
      landscape %in% c("Suburban Low", "Suburban High")   ~ "Suburban",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(urban_suburban)) %>%
  group_by(urban_suburban, sex, age) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE),
            .groups = "drop")

# Urban pyramid
urban_pyramid <- ggplot(
  filter(pyramid_data, urban_suburban == "Urban"),
  aes(x = estimate, y = age, fill = sex)
) +
  geom_col(width = 0.85, alpha = 0.8) +
  scale_fill_manual(values = c("Female" = "#8c2d04",
                               "Male"   = "#e08214")) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x))
  ) +
  theme_minimal(base_size = 11) +
  labs(title = "Urban Population\n(Urban Low + Urban High)",
       x = "Population", y = "", fill = "") +
  theme(legend.position = "bottom")

# Suburban pyramid
suburban_pyramid <- ggplot(
  filter(pyramid_data, urban_suburban == "Suburban"),
  aes(x = estimate, y = age, fill = sex)
) +
  geom_col(width = 0.85, alpha = 0.8) +
  scale_fill_manual(values = c("Female" = "#41ab5d",
                               "Male"   = "#addd8e")) +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = c(-10000, 10000)
  ) +
  theme_minimal(base_size = 11) +
  labs(title = "Suburban Population\n(Suburban Low + Suburban High)",
       x = "Population", y = "", fill = "") +
  theme(legend.position = "bottom")

# Combine
combined_pyramids <- urban_pyramid + suburban_pyramid +
  plot_annotation(
    title = "Population Pyramids by Landscape Class — Denver MSA",
    subtitle = "2016-2020 ACS 5-year estimates",
    theme = theme(plot.background = element_rect(
      fill = "white", color = NA))
  )

combined_pyramids

ggsave("output/pyramids_urban_suburban.png",
       combined_pyramids,
       width = 14, height = 8, dpi = 300, bg = "white")