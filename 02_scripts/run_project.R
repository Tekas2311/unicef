library(readr)
library(readxl)
library(dplyr)
library(stringr)
library (janitor)
library(tidyr)
library(data.table)
library(stringr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(here)

zip_path <- here("01_rawdata", "fusion_GLOBAL_DATAFLOW_UNICEF_1.0_all.csv.zip")
unzip_dir <- here("01_rawdata", "unzipped")
coverage_file <- file.path(unzip_dir, "fusion_GLOBAL_DATAFLOW_UNICEF_1.0_all.csv")

if (!file.exists(coverage_file)) {
  dir.create(unzip_dir, showWarnings = FALSE)
  unzip(zip_path, exdir = unzip_dir)}

births_file   <- here("01_rawdata", "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx")
status_file   <- here("01_rawdata", "On-track and off-track countries.xlsx")
# Loading & Cleaning Data

coverage <- read_csv(coverage_file) %>%
  rename(
    country     = `REF_AREA:Geographic area`,
    indicator   = `INDICATOR:Indicator`,
    sex         = `SEX:Sex`,
    year        = `TIME_PERIOD:Time period`,
    value       = `OBS_VALUE:Observation Value`,
    unit        = `UNIT_MEASURE:Unit of measure`,
    obs_status  = `OBS_STATUS:Observation Status`,
    source      = `DATA_SOURCE:Data Source`
  ) %>%
  mutate(
    value = as.numeric(str_extract(as.character(value), "[0-9.]+")) 
  )

births <- read_excel(births_file, sheet = "Projections") %>%
  janitor::clean_names() %>%
  rename(
    country = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    births_2022 = births_thousands
  )%>%
  filter(year == 2022)%>%
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c"))

status <- read_excel(status_file) %>%
  rename(
    iso3    = ISO3Code,
    country = OfficialName,
    status  = `Status.U5MR`
  )

# Filtering for ANC4 and SBA indicators

coverage_filtered <- coverage %>%
  mutate(
    country = str_remove(country, ".*\\)\\s*"),
    year    = as.numeric(year)
  ) %>%
  filter(
    str_detect(indicator, "MNCH_ANC4|MNCH_SAB"),
    between(year, 2018, 2022)
  ) %>%
  arrange(country, indicator, desc(year)) %>%
  distinct(country, indicator, .keep_all = TRUE) %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c"))


# Merging Births & Status Data

merged_data <- coverage_filtered %>%
  left_join(births, by = "iso3") %>%
  left_join(status %>% select(iso3, status), by = "iso3")

merged_data <- merged_data %>%
  mutate(status = ifelse(status %in% c("Achieved", "On Track"),
                         "On-track",
                         ifelse(status == "Acceleration Needed", "Off-track", NA)))


# Calculating Weighted Average Coverage 

merged_data <- merged_data %>%
  mutate(
    value = as.numeric(value),
    births_2022 = as.numeric(births_2022)
  )

merged_data <- merged_data %>% filter(!is.na(status))

weighted_coverage <- merged_data %>%
  group_by(status, indicator) %>%
  summarise(
    weighted_avg = sum(value * births_2022, na.rm = TRUE) / sum(births_2022, na.rm = TRUE),
    .groups = "drop"
  )

summary_table <- weighted_coverage %>%
  pivot_wider(names_from = indicator, values_from = weighted_avg)
View(summary_table)


# Visualization

ggplot(weighted_coverage, aes(x = status, y = weighted_avg, fill = indicator)) +
  geom_col(position = "dodge") +
  labs(
    title = "Weighted Average Coverage by Country Status",
    x = "Country Status",
    y = "Weighted Coverage (%)"
  ) +
  theme_minimal() +
  geom_text(aes(label = paste0(round(weighted_avg, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5)