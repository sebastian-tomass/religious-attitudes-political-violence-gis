# Load libraries
library(tidyverse)
library(sf)
library(lubridate)
library(patchwork)
library(clubSandwich)

# Loading in datasets
SurveyDat <- read.csv("Barometer.csv")

# Filtering for key variables and filtering out Kuwait
SurveyDatFiltered <- SurveyDat %>% 
  filter(COUNTRY != 9) %>% 
  select(COUNTRY, ID, DATE, Q1, Q1A_PAL, Q13, Q1001, Q1003, Q1005, WT, Q605)

# Relabel the country codes as country names
SurveyDatCleaned <- SurveyDatFiltered %>%
  mutate(COUNTRY = factor(COUNTRY, 
                          levels = c(1, 5, 7, 8, 10, 11, 12, 13, 15, 19, 21),
                          labels = c("Algeria", "Egypt", "Iraq", "Jordan", 
                                     "Lebanon", "Libya", "Mauritania", "Morocco", 
                                     "Palestine", "Sudan", "Tunisia")))

# Keep valid Q605 responses, excluding non-respondents and missing values
SurveyDatCleanedComplete <- SurveyDatCleaned %>%
  filter(Q605 %in% 1:5)

# Re-index so higher numbers refers to more support for religious influence
SurveyDatCleanedComplete <- SurveyDatCleanedComplete %>%
  mutate(Q605 = 6 - Q605)

# Tables for response frequencies
table(SurveyDatCleanedComplete$Q605)

# Tables for response frequencies by country
table(SurveyDatCleanedComplete$COUNTRY, SurveyDatCleanedComplete$Q605)

# Normalize answer values
SurveyDatCleanedComplete <- SurveyDatCleanedComplete %>%
  mutate(Q605_n = (Q605 - 1) / 4)

# Sample size for each question per governorate
RegionalCounts <- SurveyDatCleaned %>%
  group_by(COUNTRY, Q1) %>%
  summarise(total_n  = n(),
            valid_Q605_n = sum(Q605 %in% 1:5),
            .groups = "drop")

RegionalCounts %>%
  summarise(TotalRegions = n(),
            RegionsWithValid_Q605 = sum(valid_Q605_n > 0),
            RegionsUnder30 = sum(valid_Q605_n < 30))

# Calculate survey-weighted governorate means, and examine variation between and within countries
GovernorateMeans <- SurveyDatCleanedComplete %>%
  group_by(COUNTRY, Q1) %>%
  summarise(WeightedMean = weighted.mean(Q605_n, WT),
            n_respondents = n(),
            .groups = "drop")

# Validate regional survey estimates
stopifnot(
  nrow(GovernorateMeans) == 206,
  !anyNA(GovernorateMeans$WeightedMean),
  all(GovernorateMeans$WeightedMean >= 0 & GovernorateMeans$WeightedMean <= 1),
  n_distinct(GovernorateMeans$Q1) == 206
)

CountrySummary <- GovernorateMeans %>%
  group_by(COUNTRY) %>%
  summarise(n_regions = n(),
            MeanSharia = mean(WeightedMean),
            SD_Sharia = sd(WeightedMean),
            MinSharia = min(WeightedMean),
            MaxSharia = max(WeightedMean),
            .groups = "drop")

# Boxplot for governorate support for sharia-based legislation across arab countries
OverallMean <- mean(GovernorateMeans$WeightedMean)

ShariaBoxplot <- ggplot(GovernorateMeans,
       aes(x = reorder(COUNTRY, WeightedMean, FUN = median), y = WeightedMean)) +
  geom_hline(yintercept = OverallMean, linetype = "dashed", color = "gray50") +
  geom_boxplot(width = 0.55, fill = "darkslategray", color = "darkslategray",
               alpha = 0.18, outlier.shape = NA) +
  geom_jitter(width = 0.14, height = 0, color = "steelblue4", alpha = 0.75, size = 2.2) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                     expand = expansion(mult = c(0.02, 0.04))) +
  coord_flip() +
  labs(title = "Governorate Support for Sharia-Based Legislation Across Arab Countries",
       subtitle = "Each point represents a governorate; dashed line indicates the regional sample mean",
       x = NULL,
       y = "Weighted Governorate Mean (0 = Popular Will, 1 = Sharia)",
       caption = "Source: Arab Barometer Wave VII.") +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title    = element_text(face = "bold", size = 13, color = "gray10"),
        plot.subtitle = element_text(size = 10, color = "slategray4", margin = margin(b = 12)),
        plot.caption  = element_text(color = "gray50"),
        axis.text.y = element_text(face = "bold", color = "darkslategray"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray92"),
        plot.margin = margin(15, 15, 15, 15))

print(ShariaBoxplot)

ConflictDat <- read.csv("ConflictDat.csv") 

# Filter conflict data for countries in analysis and conflict types of interest.
ConflictDatCleaned <- ConflictDat %>%
  filter(country %in% c("Algeria", "Egypt", "Iraq", "Jordan", "Lebanon", "Libya", 
                        "Mauritania", "Morocco", "Palestine", "Sudan", "Tunisia")) %>%
  filter(event_type %in% c("Battles", "Violence against civilians", "Explosions/Remote violence")) %>%
  # Further clean dataset by keeping initially relevant variables 
  select(event_id_cnty, event_date, country, event_type, sub_event_type, actor1, 
         actor2, assoc_actor_1, assoc_actor_2, notes, admin1, admin2, latitude, longitude, geo_precision, tags)

# Convert date class to date
ConflictDatCleaned <- ConflictDatCleaned %>%
  mutate(event_date = as.Date(event_date))

# Further cleaning to remove Israeli-Palestine conflict
ConflictDatCleaned <- ConflictDatCleaned %>%
  filter(!str_detect(coalesce(tags, ""), fixed("Foreign military engagement"))) %>%
  filter(!(country == "Palestine" &
             (actor1 %in% c("Settlers (Israel)",
                            "Settlement Emergency Squad",
                            "Private Security Forces (Israel)") |
                actor2 %in% c("Civilians (Israel)",
                              "Settlement Emergency Squad",
                              "Settlers (Israel)",
                              "Private Security Forces (Israel)")))) %>%
  filter(!(country == "Palestine" & grepl("israel", notes, ignore.case = TRUE)))

# Exclude Abyei of Sudan
ConflictDatCleaned <- ConflictDatCleaned %>%
  filter(!(country == "Sudan" & admin1 == "Abyei"))

# Calculate first and final interview dates by country, excluding an anomalous Sudan date
SurveyDates <- SurveyDatCleaned %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(!(COUNTRY == "Sudan" & DATE < as.Date("2022-01-30"))) %>%
  group_by(COUNTRY) %>%
  summarise(FirstInterview = min(DATE, na.rm = TRUE),
            LastInterview = max(DATE, na.rm = TRUE),
            .groups = "drop")

print(SurveyDates)

# Calculate start and end dates
SurveyDates <- SurveyDates %>%
  mutate(StartDate = FirstInterview %m-% months(12),
         EndDate = LastInterview %m+% months(12))

print(SurveyDates)

# Attach country-specific observation windows
ConflictDatWithDates <- ConflictDatCleaned %>%
  left_join(SurveyDates %>% 
              transmute(country = as.character(COUNTRY), StartDate, EndDate), by = "country")

# Keep events in the period surrounding survey fieldwork
ConflictDatClean1 <- ConflictDatWithDates %>%
  filter(event_date >= StartDate, event_date <= EndDate)

# Locate administrative boundary files
boundary_files <- list.files("boundaries",
                             pattern = "^geoBoundaries-.*\\.geojson$",
                             full.names = TRUE)

# Inspect the geographic files
BoundarySummary <- map_dfr(boundary_files, function(file) {
  shp <- st_read(file, quiet = TRUE)
  tibble(file = basename(file),
         n_polygons = nrow(shp),
         crs = st_crs(shp)$epsg,
         invalid_geometries = sum(!st_is_valid(shp)))
})

print(BoundarySummary)

# Repair Sudan boundaries
SudanBoundaries <- st_read(boundary_files[grepl("SDN-ADM1", boundary_files)], quiet = TRUE) %>%
  filter(!grepl("Abyei", shapeName, ignore.case = TRUE)) %>%
  st_transform(6933) %>% 
  st_make_valid()

# Load Lebanon boundaries and combine Keserwan-Jbeil with Mount Lebanon
LebanonBoundaries <- st_read(boundary_files[grepl("LBN-ADM1", boundary_files)], quiet = TRUE) %>%
  mutate(Region = if_else(shapeName %in% c("Mont-Liban", "Keserwan-Jbeil"), 
                          "Mount Lebanon", shapeName)) %>%
  st_transform(6933) %>%
  group_by(Region) %>%
  summarise(.groups = "drop") %>%
  st_make_valid()

# Inspect new Mauritania boundaries
MauritaniaBoundaries <- st_read("boundaries/mrt_admin1.geojson", quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

# Match Lebanon polygons to Arab Barometer Q1 codes
LebanonCrosswalk <- tibble(Q1 = 100001:100008,
                           Region = c("Aakkâr", "Baalbek-Hermel", "Beyrouth", "Béqaa", 
                                      "Nabatîyé", "Mount Lebanon", "Liban-Nord", "Liban-Sud"))

LebanonBoundaries <- LebanonBoundaries %>%
  left_join(LebanonCrosswalk, by = "Region") %>%
  mutate(country = "Lebanon")

# Match Mauritania polygons to Arab Barometer Q1 codes
MauritaniaCrosswalk <- tibble(Q1 = 120001:120015,
                              adm1_pcode = c("MR07", "MR03", "MR05", "MR08", "MR04",
                                             "MR10", "MR01", "MR02", "MR12", "MR14",
                                             "MR13", "MR15", "MR09", "MR11", "MR06"))

MauritaniaBoundaries <- MauritaniaBoundaries %>%
  left_join(MauritaniaCrosswalk, by = "adm1_pcode") %>%
  mutate(country = "Mauritania")

# Match Sudan polygons to Arab Barometer Q1 codes
SudanCrosswalk <- tibble(Q1 = 190001:190018,
                         shapeISO = c("SD-NO", "SD-NR", "SD-RS", "SD-GD", "SD-KA", "SD-SI", 
                                      "SD-KH", "SD-GZ", "SD-KN", "SD-NB", "SD-NW", "SD-KS", 
                                      "SD-GK", "SD-DE", "SD-DS", "SD-DC", "SD-DN", "SD-DW"))

SudanBoundaries <- SudanBoundaries %>%
  left_join(SudanCrosswalk, by = "shapeISO") %>%
  mutate(country = "Sudan")

# Load Iraq and Jordan boundaries
IraqBoundaries <- st_read(boundary_files[grepl("IRQ-ADM1", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

JordanBoundaries <- st_read(boundary_files[grepl("JOR-ADM1", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

# Match Iraq polygons to Arab Barometer Q1 codes
IraqCrosswalk <- tibble(Q1 = 70001:70018,
                        shapeISO = c("IQ-BG", "IQ-SD", "IQ-DI", "IQ-WA", "IQ-MA", "IQ-BA", 
                                     "IQ-DQ", "IQ-MU", "IQ-QA", "IQ-BB", "IQ-KA", "IQ-NA", 
                                     "IQ-AN", "IQ-NI", "IQ-DA", "IQ-AR", "IQ-KI", "IQ-SU"))

IraqBoundaries <- IraqBoundaries %>%
  left_join(IraqCrosswalk, by = "shapeISO") %>%
  mutate(country = "Iraq")

# Match Jordan polygons to Arab Barometer Q1 codes
JordanCrosswalk <- tibble(Q1 = c(80011, 80012, 80013, 80014, 80021, 80022, 
                                 80023, 80024, 80031, 80032, 80033, 80034),
                          shapeISO = c("JO-AM", "JO-BA", "JO-AZ", "JO-MD", "JO-IR", "JO-MA", 
                                       "JO-JA", "JO-AJ", "JO-KA", "JO-AT", "JO-MN", "JO-AQ"))

JordanBoundaries <- JordanBoundaries %>%
  left_join(JordanCrosswalk, by = "shapeISO") %>%
  mutate(country = "Jordan")

# Load Morocco boundaries and match to Arab Barometer Q1 codes
MoroccoBoundaries <- st_read(boundary_files[grepl("MAR-ADM1", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

MoroccoCrosswalk <- tibble(Q1 = 130001:130012,
                           shapeISO = sprintf("MA-%02d", 1:12))

MoroccoBoundaries <- MoroccoBoundaries %>%
  left_join(MoroccoCrosswalk, by = "shapeISO") %>%
  mutate(country = "Morocco")

# Load Libya administrative boundaries and match to Arab Barometer Q1 codes
LibyaBoundaries <- st_read("boundaries/lby_admin2.geojson", quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

LibyaCrosswalk <- tibble(Q1 = 11001:11022,
                         adm2_pcode = c(paste0("LY01", sprintf("%02d", 1:7)),
                                        paste0("LY02", sprintf("%02d", 8:16)),
                                        paste0("LY03", sprintf("%02d", 17:22))))

LibyaBoundaries <- LibyaBoundaries %>%
  left_join(LibyaCrosswalk, by = "adm2_pcode") %>%
  mutate(country = "Libya")

# Load Palestine and Tunisia boundaries
PalestineBoundaries <- st_read(boundary_files[grepl("PSE-ADM2", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

TunisiaBoundaries <- st_read(boundary_files[grepl("TUN-ADM1", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

# Match Palestine polygons to Arab Barometer Q1 codes
PalestineCrosswalk <- tibble(Q1 = 150001:150016,
                             shapeName = c("Bethlehem", "Deir Al Balah", "Gaza", "Hebron", 
                                           "North Gaza", "Jerusalem", "Jenin", "Jericho & Al Aghwar", 
                                           "Khan Yunis", "Nablus", "Qalqiliya", "Rafah", 
                                           "Ramallah & Al Bireh", "Salfit", "Tubas", "Tulkarm"))

PalestineBoundaries <- PalestineBoundaries %>%
  left_join(PalestineCrosswalk, by = "shapeName") %>%
  mutate(country = "Palestine")

# Match Tunisia polygons to Arab Barometer Q1 codes
TunisiaCrosswalk <- tibble(Q1 = 210001:210024,
                           shapeISO = c("TN-11", "TN-12", "TN-13", "TN-14", "TN-21", "TN-22", 
                                        "TN-23", "TN-31", "TN-32", "TN-33", "TN-34", "TN-51", 
                                        "TN-52", "TN-53", "TN-61", "TN-41", "TN-42", "TN-43", 
                                        "TN-81", "TN-82", "TN-83", "TN-71", "TN-72", "TN-73"))

TunisiaBoundaries <- TunisiaBoundaries %>%
  left_join(TunisiaCrosswalk, by = "shapeISO") %>%
  mutate(country = "Tunisia")

# Load Algeria and Egypt boundaries
AlgeriaBoundaries <- st_read(boundary_files[grepl("DZA-ADM1", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

EgyptBoundaries <- st_read(boundary_files[grepl("EGY-ADM1", boundary_files)], quiet = TRUE) %>%
  st_transform(6933) %>%
  st_make_valid()

# Match Algeria polygons to Arab Barometer Q1 codes
AlgeriaCrosswalk <- tibble(Q1 = 10001:10039,
                           shapeISO = c("DZ-16", "DZ-09", "DZ-17", "DZ-15", "DZ-35", "DZ-26", 
                                        "DZ-28", "DZ-42", "DZ-10", "DZ-19", "DZ-05", "DZ-25", 
                                        "DZ-21", "DZ-06", "DZ-23", "DZ-43", "DZ-18", "DZ-04", 
                                        "DZ-34", "DZ-07", "DZ-12", "DZ-24", "DZ-40", "DZ-41", 
                                        "DZ-36", "DZ-31", "DZ-13", "DZ-02", "DZ-14", "DZ-29", 
                                        "DZ-27", "DZ-22", "DZ-48", "DZ-46", "DZ-20", "DZ-38", 
                                        "DZ-03", "DZ-30", "DZ-39"))

AlgeriaBoundariesMatched <- AlgeriaBoundaries %>%
  left_join(AlgeriaCrosswalk, by = "shapeISO") %>%
  filter(!is.na(Q1)) %>%
  mutate(country = "Algeria")

# Match Egypt polygons to Arab Barometer Q1 codes
EgyptCrosswalk <- tibble(Q1 = c(50001, 50002, 50003, 50004, 50005, 50006, 50007, 50008, 
                                50010, 50014, 50015, 50017, 50018, 50019, 50020, 50021, 
                                50022, 50023, 50024, 50025, 50026, 50027),
                         shapeISO = c("EG-C", "EG-ALX", "EG-PTS", "EG-SUZ", "EG-DT", "EG-DK", 
                                      "EG-SHR", "EG-KB", "EG-KN", "EG-FYM", "EG-GZ", "EG-LX", 
                                      "EG-ASN", "EG-AST", "EG-MN", "EG-KFS", "EG-IS", "EG-BH", 
                                      "EG-GH", "EG-MNF", "EG-BNS", "EG-SHG"))

EgyptBoundariesMatched <- EgyptBoundaries %>%
  left_join(EgyptCrosswalk, by = "shapeISO") %>%
  filter(!is.na(Q1)) %>%
  mutate(country = "Egypt")

# Combine all prepared administrative boundaries
AllBoundaries <- bind_rows(AlgeriaBoundariesMatched, EgyptBoundariesMatched, IraqBoundaries, 
                           JordanBoundaries, LebanonBoundaries, LibyaBoundaries, 
                           MauritaniaBoundaries, MoroccoBoundaries, PalestineBoundaries, 
                           SudanBoundaries, TunisiaBoundaries) %>%
  select(country, Q1) %>%
  st_transform(6933)

# Validate geographic coverage against survey regions
SurveyRegions <- SurveyDatCleaned %>%
  transmute(country = as.character(COUNTRY), Q1) %>%
  distinct()

stopifnot(
  nrow(AllBoundaries) == 206,
  nrow(SurveyRegions) == 206,
  !anyNA(AllBoundaries$Q1),
  n_distinct(AllBoundaries$Q1) == 206,
  all(st_is_valid(AllBoundaries)),
  st_crs(AllBoundaries)$epsg == 6933,
  nrow(anti_join(st_drop_geometry(AllBoundaries), SurveyRegions,
                by = c("country", "Q1"))) == 0
)

# Convert ACLED event coordinates into spatial points and assign to survey regions
ConflictPoints <- st_as_sf(ConflictDatClean1, coords = c("longitude", "latitude"), 
                           crs = 4326, remove = FALSE) %>%
  st_transform(6933)

ConflictAssigned <- ConflictPoints %>%
  st_join(AllBoundaries %>% rename(polygon_country = country), join = st_within, left = TRUE)

# Validate spatial event assignments
stopifnot(
  nrow(ConflictAssigned) == nrow(ConflictPoints),
  !anyDuplicated(ConflictAssigned$event_id_cnty),
  nrow(ConflictAssigned %>%
         st_drop_geometry() %>%
         filter(!is.na(Q1), country != polygon_country)) == 0
)

# Inspect events without a spatial match
UnmatchedEvents <- ConflictAssigned %>%
  filter(is.na(Q1))

UnmatchedEvents %>%
  st_drop_geometry() %>%
  count(country, admin1, sort = TRUE)

# Retain valid matches, aggregate conflict events, and join to all 206 surveyed regions
ConflictMatched <- ConflictAssigned %>%
  filter(!is.na(Q1))

RegionConflictCounts <- ConflictMatched %>%
  st_drop_geometry() %>%
  count(polygon_country, Q1, name = "event_count") %>%
  rename(country = polygon_country)

RegionConflictData <- AllBoundaries %>%
  left_join(RegionConflictCounts, by = c("country", "Q1")) %>%
  mutate(event_count = coalesce(event_count, 0L))

# Validate regional conflict counts
stopifnot(
  nrow(RegionConflictData) == 206,
  !anyNA(RegionConflictData$event_count),
  sum(RegionConflictData$event_count) == nrow(ConflictMatched)
)

# Merge regional conflict data with survey-weighted attitudes
AnalysisDat <- RegionConflictData %>%
  st_drop_geometry() %>%
  left_join(GovernorateMeans %>% mutate(COUNTRY = as.character(COUNTRY)), 
            by = c("country" = "COUNTRY", "Q1")) %>%
  mutate(log_events = log1p(event_count))

# Validate final analysis dataset
stopifnot(
  nrow(AnalysisDat) == 206,
  !anyNA(AnalysisDat$WeightedMean),
  !anyNA(AnalysisDat$n_respondents),
  !anyNA(AnalysisDat$log_events),
  n_distinct(AnalysisDat$Q1) == 206
)

# Scatterplot of regional Sharia preferences and political violence
ShariaConflictPlot <- ggplot(AnalysisDat, aes(x = WeightedMean, y = log_events)) +
  geom_point(aes(color = country), alpha = 0.7, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Sharia-Based Legislation Preferences and Regional Political Violence",
       subtitle = "Each point represents a surveyed administrative region",
       x = "Weighted Regional Sharia Preference (0–1)",
       y = "Log(1 + ACLED Events)",
       color = "Country",
       caption = "Sources: Arab Barometer Wave VII and ACLED.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

print(ShariaConflictPlot)

# Pooled regression and regression with country fixed effects
PooledModel <- lm(log_events ~ WeightedMean, data = AnalysisDat)
FixedEffectsModel <- lm(log_events ~ WeightedMean + factor(country), data = AnalysisDat)

summary(PooledModel)
summary(FixedEffectsModel)

# Calculate within-country deviations and plot the association
WithinCountryDat <- AnalysisDat %>%
  group_by(country) %>%
  mutate(ShariaDeviation = WeightedMean - mean(WeightedMean),
         ConflictDeviation = log_events - mean(log_events)) %>%
  ungroup()

WithinCountryPlot <- ggplot(WithinCountryDat, aes(x = ShariaDeviation, y = ConflictDeviation)) +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  geom_point(aes(color = country), alpha = 0.7, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  labs(title = "Within-Country Association Between Sharia Preferences and Political Violence",
       subtitle = "Variables expressed as deviations from country averages",
       x = "Sharia Preference Relative to Country Mean",
       y = "Log Conflict Events Relative to Country Mean",
       color = "Country",
       caption = "Sources: Arab Barometer Wave VII and ACLED.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

print(WithinCountryPlot)

# Sensitivity check excluding regions with small survey samples
AnalysisDat30 <- AnalysisDat %>%
  filter(n_respondents >= 30)

Model30 <- lm(log_events ~ WeightedMean + factor(country), data = AnalysisDat30)
summary(Model30)

# Sensitivity check excluding Iraq and Sudan
AnalysisDatNoConflict <- AnalysisDat %>%
  filter(!country %in% c("Iraq", "Sudan"))

ModelNoConflict <- lm(log_events ~ WeightedMean + factor(country), data = AnalysisDatNoConflict)
summary(ModelNoConflict)

# Calculate country-clustered CR2 standard errors with Satterthwaite correction
PooledCR2 <- coef_test(PooledModel, vcov = "CR2", cluster = AnalysisDat$country,
                       coefs = "WeightedMean", test = "Satterthwaite")

FixedEffectsCR2 <- coef_test(FixedEffectsModel, vcov = "CR2", cluster = AnalysisDat$country,
                            coefs = "WeightedMean", test = "Satterthwaite")

Model30CR2 <- coef_test(Model30, vcov = "CR2", cluster = AnalysisDat30$country,
                       coefs = "WeightedMean", test = "Satterthwaite")

ModelNoConflictCR2 <- coef_test(ModelNoConflict, vcov = "CR2", cluster = AnalysisDatNoConflict$country,
                               coefs = "WeightedMean", test = "Satterthwaite")

# Inspect country-clustered regression results
print(PooledCR2)
print(FixedEffectsCR2)
print(Model30CR2)
print(ModelNoConflictCR2)

# Create separate maps of political violence for each country and combine
MaxLog <- log1p(max(RegionConflictData$event_count, na.rm = TRUE))

CountryMaps <- RegionConflictData %>%
  split(.$country) %>%
  imap(function(data, country_name) {
    ggplot(data) +
      geom_sf(aes(fill = log1p(event_count)), color = "gray70", linewidth = 0.15) +
      scale_fill_viridis_c(option = "C",
                           limits = c(0, MaxLog),
                           breaks = log1p(c(0, 10, 100, 1000)),
                           labels = function(x) scales::comma(round(expm1(x))),
                           name = "Events\n(log scale)") +
      labs(title = country_name) +
      theme_void(base_size = 16) +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 4)))
  })

ConflictMap <- wrap_plots(c(CountryMaps, list(guide_area())), ncol = 6, guides = "collect") +
  plot_annotation(title = "Regional Political Violence Across Arab Countries",
                  subtitle = "ACLED events recorded around Arab Barometer Wave VII fieldwork",
                  caption = "Source: ACLED. Only surveyed administrative regions are shown.",
                  theme = theme(plot.title = element_text(size = 24, face = "bold"),
                                plot.subtitle = element_text(size = 15),
                                plot.caption = element_text(size = 11, color = "gray40"))) &
  theme(legend.position = "right",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.7, "cm"))

ggsave("ConflictMap.png", ConflictMap, width = 13.33, height = 7.5, dpi = 300, bg = "white")