---
  title: "TEST3"
author: "Nicolas Thomas"
date: "2024-09-05"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2023_ADP.csv")
ADP2023$POS <- gsub("\\d+", "", ADP2023$POS)
ADP2023$RTSports <- as.integer(ADP2023$RTSports)

QB2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2023.csv")
RB2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2023.csv")
WR2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2023.csv")
TE2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2023.csv")
K2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2023.csv")
DST2023 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2023.csv")

# Select relevant columns
QB2023 <- QB2023 %>% select(Player, `FPTS`)
RB2023 <- RB2023 %>% select(Player, `FPTS`)
WR2023 <- WR2023 %>% select(Player, `FPTS`)
TE2023 <- TE2023 %>% select(Player, `FPTS`)
K2023 <- K2023 %>% select(Player, `FPTS`)
DST2023 <- DST2023 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2023 <- rbind(QB2023, RB2023, WR2023, TE2023, K2023, DST2023)

# Order the combined data frame by FPTS
Ordered2023 <- Stats2023 %>%  arrange(desc(`FPTS`)) %>%  mutate(rank = row_number())

Ordered2023 <- Ordered2023 %>%  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Rename ADP Rank and Final Rank
ADP2023 <- ADP2023 %>%  rename(ADP_Rank = Rank)

ADP2023 <- ADP2023 %>%  mutate(Sleeper = as.integer(Sleeper))

Ordered2023 <- Ordered2023 %>%  rename(Final_Rank = rank)

# Select relevant columns
ADP2023ESPN <- ADP2023 %>% dplyr::select(ADP_Rank, Player, Team, ESPN)
Ordered2023 <- Ordered2023 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2023_ESPN <- inner_join(ADP2023ESPN, Ordered2023, by = "Player")
# Order the combined data frame by FPTS
FinalData2023_ESPN <- FinalData2023_ESPN %>%  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2023_ESPN)) {  FinalData2023_ESPN <- FinalData2023_ESPN %>%    filter (ESPN < 193)}
FinalData2023_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2023 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2023

# Add the draft spot to your dataset based on the ESPN rank
FinalData2023_ESPN <- FinalData2023_ESPN %>%  mutate(draft_spot_12_ESPN_2023 = snake_draft_12_ESPN_2023[ESPN])

FinalData2023_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2023 <- FinalData2023_ESPN %>%
  group_by(draft_spot_12_ESPN_2023) %>%
  summarize(
    avg_fpts_12_ESPN_2023 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2023 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2023 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2023))  # Order by best average fantasy points

draft_analysis_12_ESPN_2023

###RTSPORTS
# Select relevant columns
ADP2023RTSports <- ADP2023 %>% dplyr::select(ADP_Rank, Player, Team, RTSports)
Ordered2023 <- Ordered2023 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2023_RTSports <- inner_join(ADP2023RTSports, Ordered2023, by = "Player")
# Order the combined data frame by FPTS
FinalData2023_RTSports <- FinalData2023_RTSports %>%  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2023_RTSports)) {  FinalData2023_RTSports <- FinalData2023_RTSports %>%    filter (RTSports < 193)
}
FinalData2023_RTSports

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_RTSports_2023 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_RTSports_2023

# Add the draft spot to your dataset based on the ESPN rank
FinalData2023_RTSports <- FinalData2023_RTSports %>%  mutate(draft_spot_12_RTSports_2023 = snake_draft_12_RTSports_2023[RTSports])

FinalData2023_RTSports

# Remove rows where draft spot is NA
FinalData2023_RTSports <- FinalData2023_RTSports %>%  filter(!is.na(draft_spot_12_RTSports_2023))

# View the updated dataframe
FinalData2023_RTSports

# Summarize fantasy points by draft spot
draft_analysis_12_RTSports_2023 <- FinalData2023_RTSports %>%
  group_by(draft_spot_12_RTSports_2023) %>%
  summarize(
    avg_fpts_12_RTSports_2023 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_RTSports_2023 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_RTSports_2023 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_RTSports_2023))  # Order by best average fantasy points

draft_analysis_12_RTSports_2023

###NFL
# Select relevant columns
ADP2023NFL <- ADP2023 %>% dplyr::select(ADP_Rank, Player, Team, NFL)
Ordered2023 <- Ordered2023 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2023_NFL <- inner_join(ADP2023NFL, Ordered2023, by = "Player")
# Order the combined data frame by FPTS
FinalData2023_NFL <- FinalData2023_NFL %>%  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2023_NFL)) {  FinalData2023_NFL <- FinalData2023_NFL %>%    filter (NFL < 193)}
FinalData2023_NFL

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_NFL_2023 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_NFL_2023

# Add the draft spot to your dataset based on the ESPN rank
FinalData2023_NFL <- FinalData2023_NFL %>%  mutate(draft_spot_12_NFL_2023 = snake_draft_12_NFL_2023[NFL])

FinalData2023_NFL

# Remove rows where draft spot is NA
FinalData2023_NFL <- FinalData2023_NFL %>%  filter(!is.na(draft_spot_12_NFL_2023))

# View the updated dataframe
FinalData2023_NFL

# Summarize fantasy points by draft spot
draft_analysis_12_NFL_2023 <- FinalData2023_NFL %>%
  group_by(draft_spot_12_NFL_2023) %>%
  summarize(
    avg_fpts_12_NFL_2023 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_NFL_2023 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_NFL_2023 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_NFL_2023))  # Order by best average fantasy points

draft_analysis_12_NFL_2023

###SLEEPER
# Select relevant columns
ADP2023Sleeper <- ADP2023 %>% dplyr::select(ADP_Rank, Player, Team, Sleeper)
Ordered2023 <- Ordered2023 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2023_Sleeper <- inner_join(ADP2023Sleeper, Ordered2023, by = "Player")
# Order the combined data frame by FPTS
FinalData2023_Sleeper <- FinalData2023_Sleeper %>%  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2023_Sleeper)) {  FinalData2023_Sleeper <- FinalData2023_Sleeper %>%    filter (Sleeper < 193)}
FinalData2023_Sleeper

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_Sleeper_2023 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_Sleeper_2023

# Add the draft spot to your dataset based on the ESPN rank
FinalData2023_Sleeper <- FinalData2023_Sleeper %>%  mutate(draft_spot_12_Sleeper_2023 = snake_draft_12_Sleeper_2023[Sleeper])

FinalData2023_Sleeper

# Remove rows where draft spot is NA
FinalData2023_Sleeper <- FinalData2023_Sleeper %>%  filter(!is.na(draft_spot_12_Sleeper_2023))

# View the updated dataframe
FinalData2023_Sleeper

# Summarize fantasy points by draft spot
draft_analysis_12_Sleeper_2023 <- FinalData2023_Sleeper %>%
  group_by(draft_spot_12_Sleeper_2023) %>%
  summarize(
    avg_fpts_12_Sleeper_2023 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_Sleeper_2023 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_Sleeper_2023 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_Sleeper_2023))  # Order by best average fantasy points

draft_analysis_12_Sleeper_2023

###COMBINING
colnames(draft_analysis_12_Sleeper_2023)

# Prepare ESPN dataframe
draft_analysis_12_ESPN_2023 <- draft_analysis_12_ESPN_2023 %>%  rename(draft_spot_2023 = draft_spot_12_ESPN_2023, total_fpts_ESPN_2023 = total_fpts_12_ESPN_2023)

# Prepare RTSports dataframe
draft_analysis_12_RTSports_2023 <- draft_analysis_12_RTSports_2023 %>%  rename(draft_spot_2023 = draft_spot_12_RTSports_2023, total_fpts_RTSports_2023 = total_fpts_12_RTSports_2023)

# Prepare NFL dataframe
draft_analysis_12_NFL_2023 <- draft_analysis_12_NFL_2023 %>%  rename(draft_spot_2023 = draft_spot_12_NFL_2023, total_fpts_NFL_2023 = total_fpts_12_NFL_2023)

# Prepare Sleeper dataframe
draft_analysis_12_Sleeper_2023 <- draft_analysis_12_Sleeper_2023 %>%  rename(draft_spot_2023 = draft_spot_12_Sleeper_2023, total_fpts_Sleeper_2023 = total_fpts_12_Sleeper_2023)

# Step 1: Join ESPN and RTSports first
combined_draft_analysis_ESPN_RTSports <- full_join( 
  draft_analysis_12_ESPN_2023, draft_analysis_12_RTSports_2023, by = "draft_spot_2023", suffix = c("_ESPN", "_RTSports")
)

# Step 2: Now join the result with NFL
combined_draft_analysis_2023_partial <- full_join(
  combined_draft_analysis_ESPN_RTSports, draft_analysis_12_NFL_2023, by = "draft_spot_2023", suffix = c("", "_NFL")
)

# Step 2: Now join the result with Sleeper
combined_draft_analysis_2023 <- full_join(
  combined_draft_analysis_2023_partial, draft_analysis_12_Sleeper_2023,by = "draft_spot_2023",suffix = c("", "_Sleeper")
)

# View combined dataframe
combined_draft_analysis_2023

combined_draft_analysis_2023 <- combined_draft_analysis_2023 %>%
  mutate(
    avg_fpts_2023 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2023))

colnames(combined_draft_analysis_2023)

# Select relevant columns
combined_draft_analysis_2023 <- combined_draft_analysis_2023 %>% dplyr::select(draft_spot_2023, total_fpts_ESPN_2023, total_fpts_RTSports_2023, total_fpts_NFL_2023, total_fpts_Sleeper_2023, avg_fpts_2023)
combined_draft_analysis_2023

# Select relevant columns
combined_draft_analysis_2023_simplified <- combined_draft_analysis_2023 %>% dplyr::select(draft_spot_2023, avg_fpts_2023)
combined_draft_analysis_2023_simplified

# Load necessary library
library(gt)

# Create a gt table
combined_draft_table_2023 <- combined_draft_analysis_2023 %>%
  select(draft_spot_2023, total_fpts_ESPN_2023, total_fpts_RTSports_2023, total_fpts_NFL_2023, total_fpts_Sleeper_2023, avg_fpts_2023) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2023",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2023, total_fpts_RTSports_2023, total_fpts_NFL_2023, total_fpts_Sleeper_2023, avg_fpts_2023),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2023 = "Draft Spot",
    total_fpts_ESPN_2023 = "Total Fpts (ESPN)",
    total_fpts_RTSports_2023 = "Total Fpts (RTSports)",
    total_fpts_NFL_2023 = "Total Fpts (NFL)",
    total_fpts_Sleeper_2023 = "Total Fpts (Sleeper)",
    avg_fpts_2023 = "Average Fpts 2023"
  )

# Display the gt table
combined_draft_table_2023

#save the gt table
gtsave(combined_draft_table_2023, "draft_table_2023.png")
```

```{r}
# Arrange the dataset by the websites
final_data_2023_ordered_ESPN <- FinalData2023_ESPN %>%  arrange(ESPN)
final_data_2023_ordered_ESPN

final_data_2023_ordered_NFL <- FinalData2023_NFL %>%  arrange(NFL)
final_data_2023_ordered_NFL

final_data_2023_ordered_Sleeper <- FinalData2023_Sleeper %>%  arrange(Sleeper)
final_data_2023_ordered_Sleeper

final_data_2023_ordered_RTSports <- FinalData2023_RTSports %>%  arrange(RTSports)
final_data_2023_ordered_RTSports
```

#2022
```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2022_ADP.csv")
ADP2022$POS <- gsub("\\d+", "", ADP2022$POS)
ADP2022$RTSports <- as.integer(ADP2022$RTSports)

QB2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2022.csv")
RB2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2022.csv")
WR2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2022.csv")
TE2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2022.csv")
K2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2022.csv")
DST2022 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2022.csv")

# Select relevant columns
QB2022 <- QB2022 %>% select(Player, `FPTS`)
RB2022 <- RB2022 %>% select(Player, `FPTS`)
WR2022 <- WR2022 %>% select(Player, `FPTS`)
TE2022 <- TE2022 %>% select(Player, `FPTS`)
K2022 <- K2022 %>% select(Player, `FPTS`)
DST2022 <- DST2022 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2022 <- rbind(QB2022, RB2022, WR2022, TE2022, K2022, DST2022)

# Order the combined data frame by FPTS
Ordered2022 <- Stats2022 %>%  arrange(desc(`FPTS`)) %>%  mutate(rank = row_number())

Ordered2022 <- Ordered2022 %>%  mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Rename ADP Rank and Final Rank
ADP2022 <- ADP2022 %>%  rename(ADP_Rank = Rank)

ADP2022 <- ADP2022 %>%  mutate(Sleeper = as.integer(Sleeper))

Ordered2022 <- Ordered2022 %>%  rename(Final_Rank = rank)

# Select relevant columns
ADP2022ESPN <- ADP2022 %>% dplyr::select(ADP_Rank, Player, Team, ESPN)
Ordered2022 <- Ordered2022 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2022_ESPN <- inner_join(ADP2022ESPN, Ordered2022, by = "Player")
# Order the combined data frame by FPTS
FinalData2022_ESPN <- FinalData2022_ESPN %>%  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2022_ESPN)) {  FinalData2022_ESPN <- FinalData2022_ESPN %>%    filter (ESPN < 193)}
FinalData2022_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2022 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2022

# Add the draft spot to your dataset based on the ESPN rank
FinalData2022_ESPN <- FinalData2022_ESPN %>%  mutate(draft_spot_12_ESPN_2022 = snake_draft_12_ESPN_2022[ESPN])

FinalData2022_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2022 <- FinalData2022_ESPN %>%
  group_by(draft_spot_12_ESPN_2022) %>%
  summarize(
    avg_fpts_12_ESPN_2022 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2022 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2022 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2022))  # Order by best average fantasy points

draft_analysis_12_ESPN_2022

###RTSPORTS
# Select relevant columns
ADP2022RTSports <- ADP2022 %>% dplyr::select(ADP_Rank, Player, Team, RTSports)
Ordered2022 <- Ordered2022 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2022_RTSports <- inner_join(ADP2022RTSports, Ordered2022, by = "Player")
# Order the combined data frame by FPTS
FinalData2022_RTSports <- FinalData2022_RTSports %>%  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2022_RTSports)) {  FinalData2022_RTSports <- FinalData2022_RTSports %>%    filter (RTSports < 193)}
FinalData2022_RTSports

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_RTSports_2022 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_RTSports_2022

# Add the draft spot to your dataset based on the ESPN rank
FinalData2022_RTSports <- FinalData2022_RTSports %>%  mutate(draft_spot_12_RTSports_2022 = snake_draft_12_RTSports_2022[RTSports])

FinalData2022_RTSports

# Remove rows where draft spot is NA
FinalData2022_RTSports <- FinalData2022_RTSports %>%  filter(!is.na(draft_spot_12_RTSports_2022))

# View the updated dataframe
FinalData2022_RTSports

# Summarize fantasy points by draft spot
draft_analysis_12_RTSports_2022 <- FinalData2022_RTSports %>%
  group_by(draft_spot_12_RTSports_2022) %>%
  summarize(
    avg_fpts_12_RTSports_2022 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_RTSports_2022 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_RTSports_2022 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_RTSports_2022))  # Order by best average fantasy points

draft_analysis_12_RTSports_2022

###NFL
# Select relevant columns
ADP2022NFL <- ADP2022 %>% dplyr::select(ADP_Rank, Player, Team, NFL)
Ordered2022 <- Ordered2022 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2022_NFL <- inner_join(ADP2022NFL, Ordered2022, by = "Player")
# Order the combined data frame by FPTS
FinalData2022_NFL <- FinalData2022_NFL %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2022_NFL)) {
  FinalData2022_NFL <- FinalData2022_NFL %>%
    filter (NFL < 193)
}
FinalData2022_NFL

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_NFL_2022 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_NFL_2022

# Add the draft spot to your dataset based on the ESPN rank
FinalData2022_NFL <- FinalData2022_NFL %>% mutate(draft_spot_12_NFL_2022 = snake_draft_12_NFL_2022[NFL])

FinalData2022_NFL

# Remove rows where draft spot is NA
FinalData2022_NFL <- FinalData2022_NFL %>% filter(!is.na(draft_spot_12_NFL_2022))

# View the updated dataframe
FinalData2022_NFL

# Summarize fantasy points by draft spot
draft_analysis_12_NFL_2022 <- FinalData2022_NFL %>%
  group_by(draft_spot_12_NFL_2022) %>%
  summarize(
    avg_fpts_12_NFL_2022 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_NFL_2022 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_NFL_2022 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_NFL_2022))  # Order by best average fantasy points

draft_analysis_12_NFL_2022

###SLEEPER
# Select relevant columns
ADP2022Sleeper <- ADP2022 %>% dplyr::select(ADP_Rank, Player, Team, Sleeper)
Ordered2022 <- Ordered2022 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2022_Sleeper <- inner_join(ADP2022Sleeper, Ordered2022, by = "Player")
# Order the combined data frame by FPTS
FinalData2022_Sleeper <- FinalData2022_Sleeper %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2022_Sleeper)) { FinalData2022_Sleeper <- FinalData2022_Sleeper %>% filter (Sleeper < 193)
}
FinalData2022_Sleeper

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_Sleeper_2022 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_Sleeper_2022

# Add the draft spot to your dataset based on the ESPN rank
FinalData2022_Sleeper <- FinalData2022_Sleeper %>% mutate(draft_spot_12_Sleeper_2022 = snake_draft_12_Sleeper_2022[Sleeper])

FinalData2022_Sleeper

# Remove rows where draft spot is NA
FinalData2022_Sleeper <- FinalData2022_Sleeper %>% filter(!is.na(draft_spot_12_Sleeper_2022))

# View the updated dataframe
FinalData2022_Sleeper

# Summarize fantasy points by draft spot
draft_analysis_12_Sleeper_2022 <- FinalData2022_Sleeper %>%
  group_by(draft_spot_12_Sleeper_2022) %>%
  summarize(
    avg_fpts_12_Sleeper_2022 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_Sleeper_2022 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_Sleeper_2022 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_Sleeper_2022))  # Order by best average fantasy points

draft_analysis_12_Sleeper_2022

###COMBINING
colnames(draft_analysis_12_Sleeper_2022)

# Prepare ESPN dataframe
draft_analysis_12_ESPN_2022 <- draft_analysis_12_ESPN_2022 %>% rename(draft_spot_2022 = draft_spot_12_ESPN_2022, total_fpts_ESPN_2022 = total_fpts_12_ESPN_2022)

# Prepare RTSports dataframe
draft_analysis_12_RTSports_2022 <- draft_analysis_12_RTSports_2022 %>% rename(draft_spot_2022 = draft_spot_12_RTSports_2022, total_fpts_RTSports_2022 = total_fpts_12_RTSports_2022)

# Prepare NFL dataframe
draft_analysis_12_NFL_2022 <- draft_analysis_12_NFL_2022 %>% rename(draft_spot_2022 = draft_spot_12_NFL_2022, total_fpts_NFL_2022 = total_fpts_12_NFL_2022)

# Prepare Sleeper dataframe
draft_analysis_12_Sleeper_2022 <- draft_analysis_12_Sleeper_2022 %>% rename(draft_spot_2022 = draft_spot_12_Sleeper_2022, total_fpts_Sleeper_2022 = total_fpts_12_Sleeper_2022)

# Step 1: Join ESPN and RTSports first
combined_draft_analysis_ESPN_RTSports <- full_join(
  draft_analysis_12_ESPN_2022, draft_analysis_12_RTSports_2022, by = "draft_spot_2022", suffix = c("_ESPN", "_RTSports")
)

# Step 2: Now join the result with NFL
combined_draft_analysis_2022_partial <- full_join(
  combined_draft_analysis_ESPN_RTSports, draft_analysis_12_NFL_2022, by = "draft_spot_2022", suffix = c("", "_NFL")
)

# Step 2: Now join the result with Sleeper
combined_draft_analysis_2022 <- full_join(
  combined_draft_analysis_2022_partial, draft_analysis_12_Sleeper_2022, by = "draft_spot_2022", suffix = c("", "_Sleeper")
)

# View combined dataframe
combined_draft_analysis_2022

combined_draft_analysis_2022 <- combined_draft_analysis_2022 %>%
  mutate(
    avg_fpts_2022 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2022))

colnames(combined_draft_analysis_2022)

# Select relevant columns
combined_draft_analysis_2022 <- combined_draft_analysis_2022 %>% dplyr::select(draft_spot_2022, total_fpts_ESPN_2022, total_fpts_RTSports_2022, total_fpts_NFL_2022, total_fpts_Sleeper_2022, avg_fpts_2022)
combined_draft_analysis_2022

# Select relevant columns
combined_draft_analysis_2022_simplified <- combined_draft_analysis_2022 %>% dplyr::select(draft_spot_2022, avg_fpts_2022)
combined_draft_analysis_2022_simplified

# Load necessary library
library(gt)

# Create a gt table
combined_draft_table_2022 <- combined_draft_analysis_2022 %>%
  select(draft_spot_2022, total_fpts_ESPN_2022, total_fpts_RTSports_2022, total_fpts_NFL_2022, total_fpts_Sleeper_2022, avg_fpts_2022) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2022",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2022, total_fpts_RTSports_2022, total_fpts_NFL_2022, total_fpts_Sleeper_2022, avg_fpts_2022),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2022 = "Draft Spot",
    total_fpts_ESPN_2022 = "Total Fpts (ESPN)",
    total_fpts_RTSports_2022 = "Total Fpts (RTSports)",
    total_fpts_NFL_2022 = "Total Fpts (NFL)",
    total_fpts_Sleeper_2022 = "Total Fpts (Sleeper)",
    avg_fpts_2022 = "Average Fpts 2022"
  )

# Display the gt table
combined_draft_table_2022

#save the gt table
gtsave(combined_draft_table_2022, "draft_table_2022.png")
```

```{r}
# Arrange the dataset by the websites
final_data_2022_ordered_ESPN <- FinalData2022_ESPN %>%  arrange(ESPN)
final_data_2022_ordered_ESPN

final_data_2022_ordered_NFL <- FinalData2022_NFL %>%  arrange(NFL)
final_data_2022_ordered_NFL

final_data_2022_ordered_Sleeper <- FinalData2022_Sleeper %>%  arrange(Sleeper)
final_data_2022_ordered_Sleeper

final_data_2022_ordered_RTSports <- FinalData2022_RTSports %>%  arrange(RTSports)
final_data_2022_ordered_RTSports
```

#2021
```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2021_ADP.csv")
ADP2021$POS <- gsub("\\d+", "", ADP2021$POS)
ADP2021$RTSports <- as.integer(ADP2021$RTSports)

QB2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2021.csv")
RB2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2021.csv")
WR2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2021.csv")
TE2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2021.csv")
K2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2021.csv")
DST2021 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2021.csv")

# Select relevant columns
QB2021 <- QB2021 %>% select(Player, `FPTS`)
RB2021 <- RB2021 %>% select(Player, `FPTS`)
WR2021 <- WR2021 %>% select(Player, `FPTS`)
TE2021 <- TE2021 %>% select(Player, `FPTS`)
K2021 <- K2021 %>% select(Player, `FPTS`)
DST2021 <- DST2021 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2021 <- rbind(QB2021, RB2021, WR2021, TE2021, K2021, DST2021)

# Order the combined data frame by FPTS
Ordered2021 <- Stats2021 %>% arrange(desc(`FPTS`)) %>% mutate(rank = row_number())

Ordered2021 <- Ordered2021 %>% mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Rename ADP Rank and Final Rank
ADP2021 <- ADP2021 %>% rename(ADP_Rank = Rank)

ADP2021 <- ADP2021 %>% mutate(Sleeper = as.integer(Sleeper))

Ordered2021 <- Ordered2021 %>% rename(Final_Rank = rank)

# Select relevant columns
ADP2021ESPN <- ADP2021 %>% dplyr::select(ADP_Rank, Player, Team, ESPN)
Ordered2021 <- Ordered2021 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2021_ESPN <- inner_join(ADP2021ESPN, Ordered2021, by = "Player")
# Order the combined data frame by FPTS
FinalData2021_ESPN <- FinalData2021_ESPN %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2021_ESPN)) { FinalData2021_ESPN <- FinalData2021_ESPN %>% filter (ESPN < 193)
}
FinalData2021_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2021 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2021

# Add the draft spot to your dataset based on the ESPN rank
FinalData2021_ESPN <- FinalData2021_ESPN %>% mutate(draft_spot_12_ESPN_2021 = snake_draft_12_ESPN_2021[ESPN])

FinalData2021_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2021 <- FinalData2021_ESPN %>%
  group_by(draft_spot_12_ESPN_2021) %>%
  summarize(
    avg_fpts_12_ESPN_2021 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2021 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2021 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2021))  # Order by best average fantasy points

draft_analysis_12_ESPN_2021

###RTSPORTS
# Select relevant columns
ADP2021RTSports <- ADP2021 %>% dplyr::select(ADP_Rank, Player, Team, RTSports)
Ordered2021 <- Ordered2021 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2021_RTSports <- inner_join(ADP2021RTSports, Ordered2021, by = "Player")
# Order the combined data frame by FPTS
FinalData2021_RTSports <- FinalData2021_RTSports %>% arrange(desc(`FPTS`))

# Only Players with fantasy points
if ("ESPN" %in% colnames(FinalData2021_RTSports)) { FinalData2021_RTSports <- FinalData2021_RTSports %>% filter (RTSports < 193)
}
FinalData2021_RTSports

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_RTSports_2021 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_RTSports_2021

# Add the draft spot to your dataset based on the ESPN rank
FinalData2021_RTSports <- FinalData2021_RTSports %>% mutate(draft_spot_12_RTSports_2021 = snake_draft_12_RTSports_2021[RTSports])

FinalData2021_RTSports

# Remove rows where draft spot is NA
FinalData2021_RTSports <- FinalData2021_RTSports %>% filter(!is.na(draft_spot_12_RTSports_2021))

# View the updated dataframe
FinalData2021_RTSports

# Summarize fantasy points by draft spot
draft_analysis_12_RTSports_2021 <- FinalData2021_RTSports %>%
  group_by(draft_spot_12_RTSports_2021) %>%
  summarize(
    avg_fpts_12_RTSports_2021 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_RTSports_2021 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_RTSports_2021 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_RTSports_2021))  # Order by best average fantasy points

draft_analysis_12_RTSports_2021

###NFL
# Select relevant columns
ADP2021NFL <- ADP2021 %>% dplyr::select(ADP_Rank, Player, Team, NFL)
Ordered2021 <- Ordered2021 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2021_NFL <- inner_join(ADP2021NFL, Ordered2021, by = "Player")
# Order the combined data frame by FPTS
FinalData2021_NFL <- FinalData2021_NFL %>%
  arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2021_NFL)) { FinalData2021_NFL <- FinalData2021_NFL %>% filter (NFL < 193)
}
FinalData2021_NFL

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_NFL_2021 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_NFL_2021

# Add the draft spot to your dataset based on the ESPN rank
FinalData2021_NFL <- FinalData2021_NFL %>% mutate(draft_spot_12_NFL_2021 = snake_draft_12_NFL_2021[NFL])

FinalData2021_NFL

# Remove rows where draft spot is NA
FinalData2021_NFL <- FinalData2021_NFL %>% filter(!is.na(draft_spot_12_NFL_2021))

# View the updated dataframe
FinalData2021_NFL

# Summarize fantasy points by draft spot
draft_analysis_12_NFL_2021 <- FinalData2021_NFL %>%
  group_by(draft_spot_12_NFL_2021) %>%
  summarize(
    avg_fpts_12_NFL_2021 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_NFL_2021 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_NFL_2021 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_NFL_2021))  # Order by best average fantasy points

draft_analysis_12_NFL_2021

###SLEEPER
# Select relevant columns
ADP2021Sleeper <- ADP2021 %>% dplyr::select(ADP_Rank, Player, Team, Sleeper)
Ordered2021 <- Ordered2021 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2021_Sleeper <- inner_join(ADP2021Sleeper, Ordered2021, by = "Player")
# Order the combined data frame by FPTS
FinalData2021_Sleeper <- FinalData2021_Sleeper %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2021_Sleeper)) { FinalData2021_Sleeper <- FinalData2021_Sleeper %>% filter (Sleeper < 193) }
FinalData2021_Sleeper

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_Sleeper_2021 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_Sleeper_2021

# Add the draft spot to your dataset based on the ESPN rank
FinalData2021_Sleeper <- FinalData2021_Sleeper %>% mutate(draft_spot_12_Sleeper_2021 = snake_draft_12_Sleeper_2021[Sleeper])

FinalData2021_Sleeper

# Remove rows where draft spot is NA
FinalData2021_Sleeper <- FinalData2021_Sleeper %>% filter(!is.na(draft_spot_12_Sleeper_2021))

# View the updated dataframe
FinalData2021_Sleeper

# Summarize fantasy points by draft spot
draft_analysis_12_Sleeper_2021 <- FinalData2021_Sleeper %>%
  group_by(draft_spot_12_Sleeper_2021) %>%
  summarize(
    avg_fpts_12_Sleeper_2021 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_Sleeper_2021 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_Sleeper_2021 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_Sleeper_2021))  # Order by best average fantasy points

draft_analysis_12_Sleeper_2021

###COMBINING
colnames(draft_analysis_12_Sleeper_2021)

# Prepare ESPN dataframe
draft_analysis_12_ESPN_2021 <- draft_analysis_12_ESPN_2021 %>% rename(draft_spot_2021 = draft_spot_12_ESPN_2021, total_fpts_ESPN_2021 = total_fpts_12_ESPN_2021)

# Prepare RTSports dataframe
draft_analysis_12_RTSports_2021 <- draft_analysis_12_RTSports_2021 %>% rename(draft_spot_2021 = draft_spot_12_RTSports_2021, total_fpts_RTSports_2021 = total_fpts_12_RTSports_2021)

# Prepare NFL dataframe
draft_analysis_12_NFL_2021 <- draft_analysis_12_NFL_2021 %>% rename(draft_spot_2021 = draft_spot_12_NFL_2021, total_fpts_NFL_2021 = total_fpts_12_NFL_2021)

# Prepare Sleeper dataframe
draft_analysis_12_Sleeper_2021 <- draft_analysis_12_Sleeper_2021 %>% rename(draft_spot_2021 = draft_spot_12_Sleeper_2021, total_fpts_Sleeper_2021 = total_fpts_12_Sleeper_2021)

# Step 1: Join ESPN and RTSports first
combined_draft_analysis_ESPN_RTSports_2021 <- full_join(
  draft_analysis_12_ESPN_2021, draft_analysis_12_RTSports_2021, by = "draft_spot_2021", suffix = c("_ESPN", "_RTSports")
)

# Step 2: Now join the result with NFL
combined_draft_analysis_2021_partial <- full_join(
  combined_draft_analysis_ESPN_RTSports_2021, draft_analysis_12_NFL_2021, by = "draft_spot_2021", suffix = c("", "_NFL")
)

# Step 2: Now join the result with Sleeper
combined_draft_analysis_2021 <- full_join(
  combined_draft_analysis_2021_partial, draft_analysis_12_Sleeper_2021, by = "draft_spot_2021", suffix = c("", "_Sleeper")
)

# View combined dataframe
combined_draft_analysis_2021

combined_draft_analysis_2021 <- combined_draft_analysis_2021 %>%
  mutate(
    avg_fpts_2021 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2021))

colnames(combined_draft_analysis_2021)

# Select relevant columns
combined_draft_analysis_2021 <- combined_draft_analysis_2021 %>% dplyr::select(draft_spot_2021, total_fpts_ESPN_2021, total_fpts_RTSports_2021, total_fpts_NFL_2021, total_fpts_Sleeper_2021, avg_fpts_2021)
combined_draft_analysis_2021

# Select relevant columns
combined_draft_analysis_2021_simplified <- combined_draft_analysis_2021 %>% dplyr::select(draft_spot_2021, avg_fpts_2021)
combined_draft_analysis_2021_simplified

# Load necessary library
library(gt)

# Create a gt table
combined_draft_table_2021 <- combined_draft_analysis_2021 %>%
  select(draft_spot_2021, total_fpts_ESPN_2021, total_fpts_RTSports_2021, total_fpts_NFL_2021, total_fpts_Sleeper_2021, avg_fpts_2021) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2021",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2021, total_fpts_RTSports_2021, total_fpts_NFL_2021, total_fpts_Sleeper_2021, avg_fpts_2021),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2021 = "Draft Spot",
    total_fpts_ESPN_2021 = "Total Fpts (ESPN)",
    total_fpts_RTSports_2021 = "Total Fpts (RTSports)",
    total_fpts_NFL_2021 = "Total Fpts (NFL)",
    total_fpts_Sleeper_2021 = "Total Fpts (Sleeper)",
    avg_fpts_2021 = "Average Fpts 2021"
  )

# Display the gt table
combined_draft_table_2021

#save the gt table
gtsave(combined_draft_table_2021, "draft_table_2021.png")

# Extract players drafted in spot 7 for NFL
players_spot_7_NFL_2021 <- FinalData2021_NFL %>%
  filter(draft_spot_12_NFL_2021 == 7)

players_spot_7_NFL_2021

# Extract players drafted in spot 7 for RTSports
players_spot_7_RTSports_2021 <- FinalData2021_RTSports %>% filter(draft_spot_12_RTSports_2021 == 7)

players_spot_7_RTSports_2021

# Extract players drafted in spot 9 for RTSports
players_spot_9_RTSports_2021 <- FinalData2021_RTSports %>% filter(draft_spot_12_RTSports_2021 == 9)

players_spot_9_RTSports_2021
```

```{r}
# Arrange the dataset by the websites
final_data_2021_ordered_ESPN <- FinalData2021_ESPN %>% arrange(ESPN)
final_data_2021_ordered_ESPN

final_data_2021_ordered_NFL <- FinalData2021_NFL %>%  arrange(NFL)
final_data_2021_ordered_NFL

final_data_2021_ordered_Sleeper <- FinalData2021_Sleeper %>%  arrange(Sleeper)
final_data_2021_ordered_Sleeper

final_data_2021_ordered_RTSports <- FinalData2021_RTSports %>%  arrange(RTSports)
final_data_2021_ordered_RTSports
#Missing: Gus Edwards, Michael Thomas
```

#2020
```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2020_ADP.csv")
ADP2020$POS <- gsub("\\d+", "", ADP2020$POS)
ADP2020$RTSports <- as.integer(ADP2020$RTSports)

QB2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2020.csv")
RB2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2020.csv")
WR2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2020.csv")
TE2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2020.csv")
K2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2020.csv")
DST2020 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2020.csv")

# Select relevant columns
QB2020 <- QB2020 %>% select(Player, `FPTS`)
RB2020 <- RB2020 %>% select(Player, `FPTS`)
WR2020 <- WR2020 %>% select(Player, `FPTS`)
TE2020 <- TE2020 %>% select(Player, `FPTS`)
K2020 <- K2020 %>% select(Player, `FPTS`)
DST2020 <- DST2020 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2020 <- rbind(QB2020, RB2020, WR2020, TE2020, K2020, DST2020)

# Order the combined data frame by FPTS
Ordered2020 <- Stats2020 %>% arrange(desc(`FPTS`)) %>% mutate(rank = row_number())

Ordered2020 <- Ordered2020 %>% mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Rename ADP Rank and Final Rank
ADP2020 <- ADP2020 %>% rename(ADP_Rank = Rank)

Ordered2020 <- Ordered2020 %>% rename(Final_Rank = rank)

# Select relevant columns
ADP2020ESPN <- ADP2020 %>% dplyr::select(ADP_Rank, Player, Team, ESPN)
Ordered2020 <- Ordered2020 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2020_ESPN <- inner_join(ADP2020ESPN, Ordered2020, by = "Player")
# Order the combined data frame by FPTS
FinalData2020_ESPN <- FinalData2020_ESPN %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2020_ESPN)) { FinalData2020_ESPN <- FinalData2020_ESPN %>% filter (ESPN < 193)
}
FinalData2020_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2020 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2020

# Add the draft spot to your dataset based on the ESPN rank
FinalData2020_ESPN <- FinalData2020_ESPN %>% mutate(draft_spot_12_ESPN_2020 = snake_draft_12_ESPN_2020[ESPN])

FinalData2020_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2020 <- FinalData2020_ESPN %>%
  group_by(draft_spot_12_ESPN_2020) %>%
  summarize(
    avg_fpts_12_ESPN_2020 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2020 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2020 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2020))  # Order by best average fantasy points

draft_analysis_12_ESPN_2020

###RTSPORTS
# Select relevant columns
ADP2020RTSports <- ADP2020 %>% dplyr::select(ADP_Rank, Player, Team, RTSports)
Ordered2020 <- Ordered2020 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2020_RTSports <- inner_join(ADP2020RTSports, Ordered2020, by = "Player")
# Order the combined data frame by FPTS
FinalData2020_RTSports <- FinalData2020_RTSports %>% arrange(desc(`FPTS`))

# Only Players with fantasy points
if ("ESPN" %in% colnames(FinalData2020_RTSports)) { FinalData2020_RTSports <- FinalData2020_RTSports %>% filter (RTSports < 193)
}
FinalData2020_RTSports

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_RTSports_2020 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_RTSports_2020

# Add the draft spot to your dataset based on the ESPN rank
FinalData2020_RTSports <- FinalData2020_RTSports %>% mutate(draft_spot_12_RTSports_2020 = snake_draft_12_RTSports_2020[RTSports])

FinalData2020_RTSports

# Remove rows where draft spot is NA
FinalData2020_RTSports <- FinalData2020_RTSports %>%  filter(!is.na(draft_spot_12_RTSports_2020))

# View the updated dataframe
FinalData2020_RTSports

# Summarize fantasy points by draft spot
draft_analysis_12_RTSports_2020 <- FinalData2020_RTSports %>%
  group_by(draft_spot_12_RTSports_2020) %>%
  summarize(
    avg_fpts_12_RTSports_2020 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_RTSports_2020 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_RTSports_2020 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_RTSports_2020))  # Order by best average fantasy points

draft_analysis_12_RTSports_2020

###COMBINING
# Prepare ESPN dataframe
draft_analysis_12_ESPN_2020 <- draft_analysis_12_ESPN_2020 %>% rename(draft_spot_2020 = draft_spot_12_ESPN_2020, total_fpts_ESPN_2020 = total_fpts_12_ESPN_2020)

# Prepare RTSports dataframe
draft_analysis_12_RTSports_2020 <- draft_analysis_12_RTSports_2020 %>% rename(draft_spot_2020 = draft_spot_12_RTSports_2020, total_fpts_RTSports_2020 = total_fpts_12_RTSports_2020)

# Step 1: Join ESPN and RTSports first
combined_draft_analysis_2020 <- full_join(
  draft_analysis_12_ESPN_2020, draft_analysis_12_RTSports_2020, by = "draft_spot_2020", suffix = c("_ESPN", "_RTSports")
)

# View combined dataframe
combined_draft_analysis_2020

combined_draft_analysis_2020 <- combined_draft_analysis_2020 %>%
  mutate(
    avg_fpts_2020 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2020))

colnames(combined_draft_analysis_2020)

# Select relevant columns
combined_draft_analysis_2020 <- combined_draft_analysis_2020 %>% dplyr::select(draft_spot_2020, total_fpts_ESPN_2020, total_fpts_RTSports_2020, avg_fpts_2020)
combined_draft_analysis_2020

# Select relevant columns
combined_draft_analysis_2020_simplified <- combined_draft_analysis_2020 %>% dplyr::select(draft_spot_2020, avg_fpts_2020)
combined_draft_analysis_2020_simplified

# Load necessary library
library(gt)

# Create a gt table
combined_draft_table_2020 <- combined_draft_analysis_2020 %>%
  select(draft_spot_2020, total_fpts_ESPN_2020, total_fpts_RTSports_2020, avg_fpts_2020) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2020",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2020, total_fpts_RTSports_2020, avg_fpts_2020),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2020 = "Draft Spot",
    total_fpts_ESPN_2020 = "Total Fpts (ESPN)",
    total_fpts_RTSports_2020 = "Total Fpts (RTSports)",
    avg_fpts_2020 = "Average Fpts 2020"
  )

# Display the gt table
combined_draft_table_2020

#save the gt table
gtsave(combined_draft_table_2020, "draft_table_2020.png")
```

```{r}
# Arrange the dataset by the  column
final_data_2020_ordered_ESPN <- FinalData2020_ESPN %>% arrange(ESPN)
final_data_2020_ordered_ESPN

final_data_2020_ordered_RTSports <- FinalData2020_RTSports %>%  arrange(RTSports)
final_data_2020_ordered_RTSports
#Missing: Bryce Love, Ryquell Armstead 
```

#2019
```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2019_ADP.csv")
ADP2019$POS <- gsub("\\d+", "", ADP2019$POS)
ADP2019$RTSports <- as.integer(ADP2019$RTSports)

QB2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2019.csv")
RB2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2019.csv")
WR2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2019.csv")
TE2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2019.csv")
K2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2019.csv")
DST2019 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2019.csv")

# Select relevant columns
QB2019 <- QB2019 %>% select(Player, `FPTS`)
RB2019 <- RB2019 %>% select(Player, `FPTS`)
WR2019 <- WR2019 %>% select(Player, `FPTS`)
TE2019 <- TE2019 %>% select(Player, `FPTS`)
K2019 <- K2019 %>% select(Player, `FPTS`)
DST2019 <- DST2019 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2019 <- rbind(QB2019, RB2019, WR2019, TE2019, K2019, DST2019)

# Order the combined data frame by FPTS
Ordered2019 <- Stats2019 %>% arrange(desc(`FPTS`)) %>% mutate(rank = row_number())

Ordered2019 <- Ordered2019 %>% mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Rename ADP Rank and Final Rank
ADP2019 <- ADP2019 %>% rename(ADP_Rank = Rank)

ADP2019<- ADP2019 %>% mutate(Sleeper = as.integer(Sleeper))

Ordered2019 <- Ordered2019 %>% rename(Final_Rank = rank)

# Select relevant columns
ADP2019ESPN <- ADP2019 %>% dplyr::select(ADP_Rank, Player, Team, ESPN)
Ordered2019 <- Ordered2019 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2019_ESPN <- inner_join(ADP2019ESPN, Ordered2019, by = "Player")
# Order the combined data frame by FPTS
FinalData2019_ESPN <- FinalData2019_ESPN %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2019_ESPN)) { FinalData2019_ESPN <- FinalData2019_ESPN %>% filter (ESPN < 193)}
FinalData2019_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2019 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2019

# Add the draft spot to your dataset based on the ESPN rank
FinalData2019_ESPN <- FinalData2019_ESPN %>% mutate(draft_spot_12_ESPN_2019 = snake_draft_12_ESPN_2019[ESPN])

FinalData2019_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2019 <- FinalData2019_ESPN %>%
  group_by(draft_spot_12_ESPN_2019) %>%
  summarize(
    avg_fpts_12_ESPN_2019 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2019 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2019 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2019))  # Order by best average fantasy points

draft_analysis_12_ESPN_2019

###RTSPORTS
# Select relevant columns
ADP2019RTSports <- ADP2019 %>% dplyr::select(ADP_Rank, Player, Team, RTSports)
Ordered2019 <- Ordered2019 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2019_RTSports <- inner_join(ADP2019RTSports, Ordered2019, by = "Player")
# Order the combined data frame by FPTS
FinalData2019_RTSports <- FinalData2019_RTSports %>% arrange(desc(`FPTS`))

# Only Players with fantasy points
if ("ESPN" %in% colnames(FinalData2019_RTSports)) { FinalData2019_RTSports <- FinalData2019_RTSports %>% filter (RTSports < 193)}
FinalData2019_RTSports

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_RTSports_2019 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_RTSports_2019

# Add the draft spot to your dataset based on the ESPN rank
FinalData2019_RTSports <- FinalData2019_RTSports %>% mutate(draft_spot_12_RTSports_2019 = snake_draft_12_RTSports_2019[RTSports])

FinalData2019_RTSports

# Remove rows where draft spot is NA
FinalData2019_RTSports <- FinalData2019_RTSports %>% filter(!is.na(draft_spot_12_RTSports_2019))

# View the updated dataframe
FinalData2019_RTSports

# Summarize fantasy points by draft spot
draft_analysis_12_RTSports_2019 <- FinalData2019_RTSports %>%
  group_by(draft_spot_12_RTSports_2019) %>%
  summarize(
    avg_fpts_12_RTSports_2019 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_RTSports_2019 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_RTSports_2019 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_RTSports_2019))  # Order by best average fantasy points

draft_analysis_12_RTSports_2019

###NFL
# Select relevant columns
ADP2019NFL <- ADP2019 %>% dplyr::select(ADP_Rank, Player, Team, NFL)
Ordered2019 <- Ordered2019 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2019_NFL <- inner_join(ADP2019NFL, Ordered2019, by = "Player")
# Order the combined data frame by FPTS
FinalData2019_NFL <- FinalData2019_NFL %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2019_NFL)) { FinalData2019_NFL <- FinalData2019_NFL %>% filter (NFL < 193)
}
FinalData2019_NFL

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_NFL_2019 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_NFL_2019

# Add the draft spot to your dataset based on the ESPN rank
FinalData2019_NFL <- FinalData2019_NFL %>% mutate(draft_spot_12_NFL_2019 = snake_draft_12_NFL_2019[NFL])

FinalData2019_NFL

# Remove rows where draft spot is NA
FinalData2019_NFL <- FinalData2019_NFL %>% filter(!is.na(draft_spot_12_NFL_2019))

# View the updated dataframe
FinalData2019_NFL

# Summarize fantasy points by draft spot
draft_analysis_12_NFL_2019 <- FinalData2019_NFL %>%
  group_by(draft_spot_12_NFL_2019) %>%
  summarize(
    avg_fpts_12_NFL_2019 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_NFL_2019 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_NFL_2019 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_NFL_2019))  # Order by best average fantasy points

draft_analysis_12_NFL_2019

###COMBINING
# Prepare ESPN dataframe
draft_analysis_12_ESPN_2019 <- draft_analysis_12_ESPN_2019 %>% rename(draft_spot_2019 = draft_spot_12_ESPN_2019, total_fpts_ESPN_2019 = total_fpts_12_ESPN_2019)

# Prepare RTSports dataframe
draft_analysis_12_RTSports_2019 <- draft_analysis_12_RTSports_2019 %>% rename(draft_spot_2019 = draft_spot_12_RTSports_2019, total_fpts_RTSports_2019 = total_fpts_12_RTSports_2019)

# Prepare NFL dataframe
draft_analysis_12_NFL_2019 <- draft_analysis_12_NFL_2019 %>% rename(draft_spot_2019 = draft_spot_12_NFL_2019, total_fpts_NFL_2019 = total_fpts_12_NFL_2019)

# Step 1: Join ESPN and RTSports first
combined_draft_analysis_ESPN_RTSports_2019 <- full_join(
  draft_analysis_12_ESPN_2019, draft_analysis_12_RTSports_2019, by = "draft_spot_2019", suffix = c("_ESPN", "_RTSports")
)

# Step 2: Now join the result with NFL
combined_draft_analysis_2019 <- full_join(
  combined_draft_analysis_ESPN_RTSports_2019, draft_analysis_12_NFL_2019, by = "draft_spot_2019", suffix = c("", "_NFL")
)

# View combined dataframe
combined_draft_analysis_2019

combined_draft_analysis_2019 <- combined_draft_analysis_2019 %>%
  mutate(
    avg_fpts_2019 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2019))

colnames(combined_draft_analysis_2019)

# Select relevant columns
combined_draft_analysis_2019 <- combined_draft_analysis_2019 %>% dplyr::select(draft_spot_2019, total_fpts_ESPN_2019, total_fpts_RTSports_2019, total_fpts_NFL_2019, avg_fpts_2019)
combined_draft_analysis_2019

# Select relevant columns
combined_draft_analysis_2019_simplified <- combined_draft_analysis_2019 %>% dplyr::select(draft_spot_2019, avg_fpts_2019)
combined_draft_analysis_2019_simplified

# Load necessary library
library(gt)

# Create a gt table
combined_draft_table_2019 <- combined_draft_analysis_2019 %>%
  select(draft_spot_2019, total_fpts_ESPN_2019, total_fpts_RTSports_2019, total_fpts_NFL_2019, avg_fpts_2019) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2019",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2019, total_fpts_RTSports_2019, total_fpts_NFL_2019, avg_fpts_2019),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2019 = "Draft Spot",
    total_fpts_ESPN_2019 = "Total Fpts (ESPN)",
    total_fpts_RTSports_2019 = "Total Fpts (RTSports)",
    total_fpts_NFL_2019 = "Total Fpts (NFL)",
    avg_fpts_2019 = "Average Fpts 2019"
  )

# Display the gt table
combined_draft_table_2019

#save the gt table
gtsave(combined_draft_table_2019, "draft_table_2019.png")

# Extract players drafted in spot 7 for NFL
players_spot_7_NFL_2019 <- FinalData2019_NFL %>% filter(draft_spot_12_NFL_2019 == 7)

players_spot_7_NFL_2019

# Extract players drafted in spot 7 for RTSports
players_spot_7_RTSports_2019 <- FinalData2019_RTSports %>% filter(draft_spot_12_RTSports_2019 == 7)

players_spot_7_RTSports_2019
```

```{r}
# Arrange the dataset by the websites
final_data_2019_ordered_ESPN <- FinalData2019_ESPN %>% arrange(ESPN)
final_data_2019_ordered_ESPN

final_data_2019_ordered_RTSports <- FinalData2019_RTSports %>%  arrange(RTSports)
final_data_2019_ordered_RTSports

final_data_2019_ordered_NFL <- FinalData2019_NFL %>%  arrange(NFL)
final_data_2019_ordered_NFL
```

#2018
```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2018_ADP.csv")
ADP2018$POS <- gsub("\\d+", "", ADP2018$POS)
ADP2018$RTSports <- as.integer(ADP2018$RTSports)

QB2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2018.csv")
RB2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2018.csv")
WR2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2018.csv")
TE2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2018.csv")
K2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2018.csv")
DST2018 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2018.csv")

# Select relevant columns
QB2018 <- QB2018 %>% select(Player, `FPTS`)
RB2018 <- RB2018 %>% select(Player, `FPTS`)
WR2018 <- WR2018 %>% select(Player, `FPTS`)
TE2018 <- TE2018 %>% select(Player, `FPTS`)
K2018 <- K2018 %>% select(Player, `FPTS`)
DST2018 <- DST2018 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2018 <- rbind(QB2018, RB2018, WR2018, TE2018, K2018, DST2018)

# Order the combined data frame by FPTS
Ordered2018 <- Stats2018 %>% arrange(desc(`FPTS`)) %>% mutate(rank = row_number())

Ordered2018 <- Ordered2018 %>% mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Rename ADP Rank and Final Rank
ADP2018 <- ADP2018 %>% rename(ADP_Rank = Rank)

Ordered2018 <- Ordered2018 %>% rename(Final_Rank = rank)

# Select relevant columns
ADP2018ESPN <- ADP2018 %>% dplyr::select(ADP_Rank, Player, Team, ESPN)
Ordered2018 <- Ordered2018 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2018_ESPN <- inner_join(ADP2018ESPN, Ordered2018, by = "Player")
# Order the combined data frame by FPTS
FinalData2018_ESPN <- FinalData2018_ESPN %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2018_ESPN)) { FinalData2018_ESPN <- FinalData2018_ESPN %>% filter (ESPN < 193)}
FinalData2018_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2018 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2018

# Add the draft spot to your dataset based on the ESPN rank
FinalData2018_ESPN <- FinalData2018_ESPN %>% mutate(draft_spot_12_ESPN_2018 = snake_draft_12_ESPN_2018[ESPN])

FinalData2018_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2018 <- FinalData2018_ESPN %>%
  group_by(draft_spot_12_ESPN_2018) %>%
  summarize(
    avg_fpts_12_ESPN_2018 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2018 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2018 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2018))  # Order by best average fantasy points

draft_analysis_12_ESPN_2018

###RTSPORTS
# Select relevant columns
ADP2018RTSports <- ADP2018 %>% dplyr::select(ADP_Rank, Player, Team, RTSports)
Ordered2018 <- Ordered2018 %>% dplyr::select(Player, FPTS, Final_Rank)

# Merge the data on Player
FinalData2018_RTSports <- inner_join(ADP2018RTSports, Ordered2018, by = "Player")
# Order the combined data frame by FPTS
FinalData2018_RTSports <- FinalData2018_RTSports %>% arrange(desc(`FPTS`))

# Only Players with fantasy points
if ("ESPN" %in% colnames(FinalData2018_RTSports)) { FinalData2018_RTSports <- FinalData2018_RTSports %>% filter (RTSports < 193)}
FinalData2018_RTSports

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_RTSports_2018 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_RTSports_2018

# Add the draft spot to your dataset based on the ESPN rank
FinalData2018_RTSports <- FinalData2018_RTSports %>% mutate(draft_spot_12_RTSports_2018 = snake_draft_12_RTSports_2018[RTSports])

FinalData2018_RTSports

# Remove rows where draft spot is NA
FinalData2018_RTSports <- FinalData2018_RTSports %>% filter(!is.na(draft_spot_12_RTSports_2018))

# View the updated dataframe
FinalData2018_RTSports

# Summarize fantasy points by draft spot
draft_analysis_12_RTSports_2018 <- FinalData2018_RTSports %>%
  group_by(draft_spot_12_RTSports_2018) %>%
  summarize(
    avg_fpts_12_RTSports_2018 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_RTSports_2018 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_RTSports_2018 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_RTSports_2018))  # Order by best average fantasy points

draft_analysis_12_RTSports_2018

###COMBINING
# Prepare ESPN dataframe
draft_analysis_12_ESPN_2018 <- draft_analysis_12_ESPN_2018 %>% rename(draft_spot_2018 = draft_spot_12_ESPN_2018, total_fpts_ESPN_2018 = total_fpts_12_ESPN_2018)

# Prepare RTSports dataframe
draft_analysis_12_RTSports_2018 <- draft_analysis_12_RTSports_2018 %>% rename(draft_spot_2018 = draft_spot_12_RTSports_2018, total_fpts_RTSports_2018 = total_fpts_12_RTSports_2018)

# Join ESPN and RTSports
combined_draft_analysis_2018 <- full_join(
  draft_analysis_12_ESPN_2018, draft_analysis_12_RTSports_2018, by = "draft_spot_2018", suffix = c("_ESPN", "_RTSports")
)

# View combined dataframe
combined_draft_analysis_2018

combined_draft_analysis_2018 <- combined_draft_analysis_2018 %>%
  mutate(
    avg_fpts_2018 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2018))

colnames(combined_draft_analysis_2018)

# Select relevant columns
combined_draft_analysis_2018 <- combined_draft_analysis_2018 %>% dplyr::select(draft_spot_2018, total_fpts_ESPN_2018, total_fpts_RTSports_2018, avg_fpts_2018)
combined_draft_analysis_2018

# Select relevant columns
combined_draft_analysis_2018_simplified <- combined_draft_analysis_2018%>% dplyr::select(draft_spot_2018, avg_fpts_2018)
combined_draft_analysis_2018_simplified

# Load necessary library
library(gt)

# Create a gt table
combined_draft_table_2018 <- combined_draft_analysis_2018 %>%
  select(draft_spot_2018, total_fpts_ESPN_2018, total_fpts_RTSports_2018, avg_fpts_2018) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2018",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2018, total_fpts_RTSports_2018, avg_fpts_2018),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2018 = "Draft Spot",
    total_fpts_ESPN_2018 = "Total Fpts (ESPN)",
    total_fpts_RTSports_2018 = "Total Fpts (RTSports)",
    avg_fpts_2018 = "Average Fpts 2018"
  )

# Display the gt table
combined_draft_table_2018

#save the gt table
gtsave(combined_draft_table_2018, "draft_table_2018.png")

# Extract players drafted in spot 7 for RTSports
players_spot_7_RTSports_2018 <- FinalData2018_RTSports %>% filter(draft_spot_12_RTSports_2018 == 7)

players_spot_7_RTSports_2018
```

```{r}
# Arrange the dataset by the websites
final_data_2018_ordered_ESPN <- FinalData2018_ESPN %>% arrange(ESPN)
final_data_2018_ordered_ESPN

final_data_2018_ordered_RTSports <- FinalData2018_RTSports %>%  arrange(RTSports)
final_data_2018_ordered_RTSports
```

#2017
```{r}
library(MASS)
library(caret)
library(glmnet)
library(bayesplot)
library(loo)
library(dplyr)
library(stringr)

# Load ADP and Stats data
ADP2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\2017_ADP.csv")
ADP2017$POS <- gsub("\\d+", "", ADP2017$POS)

QB2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\QB_Stats_2017.csv")
RB2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\RB_Stats_2017.csv")
WR2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\WR_Stats_2017.csv")
TE2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\TE_Stats_2017.csv")
K2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\K_Stats_2017.csv")
DST2017 <- read.csv(file = "C:\\Users\\Nicol\\OneDrive\\Desktop\\Projects\\Best Draft Spot\\Stats\\DST_Stats_2017.csv")

# Select relevant columns
QB2017 <- QB2017 %>% select(Player, `FPTS`)
RB2017 <- RB2017 %>% select(Player, `FPTS`)
WR2017 <- WR2017 %>% select(Player, `FPTS`)
TE2017 <- TE2017 %>% select(Player, `FPTS`)
K2017 <- K2017 %>% select(Player, `FPTS`)
DST2017 <- DST2017 %>% select(Player, `FPTS`)

# Combine all the tables
Stats2017 <- rbind(QB2017, RB2017, WR2017, TE2017, K2017, DST2017)

# Order the combined data frame by FPTS
Ordered2017 <- Stats2017 %>% arrange(desc(`FPTS`)) %>% mutate(rank = row_number())

Ordered2017 <- Ordered2017 %>% mutate(Player = str_replace(Player, " \\(.*\\)", ""))

# Select relevant columns
ADP2017ESPN <- ADP2017 %>% dplyr::select(Player, ESPN)
Ordered2017 <- Ordered2017 %>% dplyr::select(Player, FPTS)

# Merge the data on Player
FinalData2017_ESPN <- inner_join(ADP2017ESPN, Ordered2017, by = "Player")
# Order the combined data frame by FPTS
FinalData2017_ESPN <- FinalData2017_ESPN %>% arrange(desc(`FPTS`))

# Sorting
if ("ESPN" %in% colnames(FinalData2017_ESPN)) { FinalData2017_ESPN <- FinalData2017_ESPN %>% filter (ESPN < 193)}
FinalData2017_ESPN

# Create a vector that simulates the snake draft order for 192 picks
snake_draft_12_ESPN_2017 <- rep(c(1:12, 12:1), length.out = 192)

snake_draft_12_ESPN_2017

# Add the draft spot to your dataset based on the ESPN rank
FinalData2017_ESPN <- FinalData2017_ESPN %>% mutate(draft_spot_12_ESPN_2017 = snake_draft_12_ESPN_2017[ESPN])

FinalData2017_ESPN

# Summarize fantasy points by draft spot
draft_analysis_12_ESPN_2017 <- FinalData2017_ESPN %>%
  group_by(draft_spot_12_ESPN_2017) %>%
  summarize(
    avg_fpts_12_ESPN_2017 = mean(FPTS, na.rm = TRUE),   # Average fantasy points
    median_fpts_12_ESPN_2017 = median(FPTS, na.rm = TRUE),  # Median fantasy points
    total_fpts_12_ESPN_2017 = sum(FPTS, na.rm = TRUE)   # Total fantasy points
  ) %>%
  arrange(desc(avg_fpts_12_ESPN_2017))  # Order by best average fantasy points

draft_analysis_12_ESPN_2017

###COMBINING
# Prepare ESPN dataframe
draft_analysis_12_ESPN_2017 <- draft_analysis_12_ESPN_2017 %>% rename(draft_spot_2017 = draft_spot_12_ESPN_2017, total_fpts_ESPN_2017 = total_fpts_12_ESPN_2017)

combined_draft_analysis_2017 <- draft_analysis_12_ESPN_2017 %>%
  mutate(
    avg_fpts_2017 = rowMeans(select(., starts_with("total_fpts")), na.rm = TRUE)
  ) %>%
  arrange(desc(avg_fpts_2017))

colnames(combined_draft_analysis_2017)

# Select relevant columns
combined_draft_analysis_2017 <- combined_draft_analysis_2017 %>% dplyr::select(draft_spot_2017, total_fpts_ESPN_2017, avg_fpts_2017)
combined_draft_analysis_2017

# Select relevant columns
combined_draft_analysis_2017_simplified <- combined_draft_analysis_2017%>% dplyr::select(draft_spot_2017, avg_fpts_2017)
combined_draft_analysis_2017_simplified

# Load necessary library
library(gt)

# Create a gt table
draft_table_2017 <- combined_draft_analysis_2017 %>%
  select(draft_spot_2017, total_fpts_ESPN_2017, avg_fpts_2017) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis 2017",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(total_fpts_ESPN_2017, avg_fpts_2017),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot_2017 = "Draft Spot",
    total_fpts_ESPN_2017 = "Total Fpts (ESPN)",
    avg_fpts_2017 = "Average Fpts 2017"
  )

# Display the gt table
draft_table_2017

#save the gt table
gtsave(draft_table_2017, "draft_table_2017.png")

# Extract players drafted in spot 7 for RTSports
players_spot_7_ESPN_2017 <- FinalData2017_ESPN %>% filter(draft_spot_12_ESPN_2017 == 7)

players_spot_7_ESPN_2017
```

```{r}
# Arrange the dataset by the websites
final_data_2017_ordered_ESPN <- FinalData2017_ESPN %>% arrange(ESPN)
final_data_2017_ordered_ESPN
```

```{r}
combined_draft_analysis_2017_simplified
combined_data <- cbind(
  combined_draft_analysis_2023_simplified,
  combined_draft_analysis_2022_simplified,
  combined_draft_analysis_2021_simplified,
  combined_draft_analysis_2020_simplified,
  combined_draft_analysis_2019_simplified,
  combined_draft_analysis_2018_simplified,
  combined_draft_analysis_2017_simplified
)
combined_data
```

```{r}
#Separated Draft_Spot information by years
library(tidyr)
library(dplyr)

# Combine the data into a long format
combined_data_long <- combined_data %>%
  pivot_longer(
    cols = starts_with("draft_spot_") | starts_with("avg_fpts_"),  # Select columns that start with these prefixes
    names_to = c(".value", "Year"),  # Split the names into "draft_spot" and "avg_fpts" and "Year"
    names_pattern = "(draft_spot|avg_fpts)_(\\d{4})"  # Regex to separate the base name and the year
  )

# Order by highest avg fantasy points
combined_data_ordered <- combined_data_long %>%
  arrange(desc(avg_fpts))

# View the result
print(combined_data_ordered)

# Create a gt table
completed_draft_table_by_year <- combined_data_ordered %>%
  select(Year, draft_spot, avg_fpts) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis by Year",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(Year, draft_spot, avg_fpts=),
    decimals = 2
  ) %>%
  cols_label(
    Year = "Year",
    draft_spot = "Draft Spot",
    avg_fpts = "Average Fpts"
  )

# Display the gt table
completed_draft_table_by_year

#save the gt table
gtsave(completed_draft_table_by_year, "Final_Table_By_Year.png")

# Combined Draft_Spot information for each year
library(tidyr)
library(dplyr)

# Combine the data into a long format as before
combined_data_long <- combined_data %>%
  pivot_longer(
    cols = starts_with("draft_spot_") | starts_with("avg_fpts_"),  # Select columns that start with these prefixes
    names_to = c(".value", "Year"),  # Split the names into "draft_spot" and "avg_fpts" and "Year"
    names_pattern = "(draft_spot|avg_fpts)_(\\d{4})"  # Regex to separate the base name and the year
  )

# Summing the average fantasy points by draft spot
summed_data <- combined_data_long %>%
  group_by(draft_spot) %>%
  summarise(total_avg_fpts = sum(avg_fpts, na.rm = TRUE)) %>%
  arrange(desc(total_avg_fpts))

# View the result
print(summed_data)

# Create a gt table
completed_draft_table <- summed_data %>%
  select(draft_spot, total_avg_fpts) %>%
  gt() %>%
  tab_header(
    title = "Combined Draft Analysis",
    subtitle = "Fantasy Points by Platform"
  ) %>%
  fmt_number(
    columns = vars(draft_spot, total_avg_fpts=),
    decimals = 2
  ) %>%
  cols_label(
    draft_spot = "Draft Spot",
    total_avg_fpts = "Average Fpts"
  )

# Display the gt table
completed_draft_table

#save the gt table
gtsave(completed_draft_table, "Final_Table.png")
```