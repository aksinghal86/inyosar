library(tidyverse)

personnel <- openxlsx::read.xlsx('data/Contact_List.xlsx')

personnel |> group_by(Status) |> count()
personnel |> group_by(Status, Position) |> count()

personnel <- personnel |> 
  filter(!Position %in% c("ICSO coordinator", "ISCO Coordinator", "ISCO PIO", "Coordinator", "Slack Bot", "Dispatch"), 
         Personnel != 'Guido, Jake', 
         !is.na(Position)) |> 
  mutate(Type = ifelse(Position != 'Candidate', "Member", "Candidate"))

activity <- openxlsx::read.xlsx('data/Annual_Report_2025.xlsx') |> 
  janitor::clean_names(case = 'title') |> 
  right_join(personnel, by = 'Personnel') |> 
  separate(Duration, c("Hours", "Mins")) |> 
  mutate_at(vars(Hours, Mins), ~as.numeric(str_replace_all(., '[^0-9\\.]', ''))) |> 
  mutate(Duration = Hours + Mins/60) |> 
  select(-Hours, -Mins) |> 
  tibble()

oper_activity <- activity |> filter(Status == 'Operational') |> 
  filter(!is.na(Duration))

write_csv(personnel, 'data/personnel-clean.csv') 
write_csv(oper_activity, 'data/operational-activity-clean.csv')











df |>
  filter(Total <= 7) |>
  select(Personnel, Position, Incidents, Exercises, Events, Duration, Total) |>
  arrange(Personnel)


df |>
  filter(Type == "Member") |>
  ggplot(aes(x = reorder(Personnel, Total), y = Total)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Total activities (Member)") +
  theme_bw(base_size = 12)


### 5) High-achieving candidates exceeding members

# Plot 5.1: Total vs Duration (with standout candidate labels)
member_med_total <- median(df$Total[df$Type == "Member"], na.rm = TRUE)

top_candidates <- df |>
  filter(Type == "Candidate") |>
  arrange(desc(Total), desc(Duration)) |>
  slice_head(n = 8)

ggplot(df, aes(x = Total, y = Duration, color = Type)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_vline(xintercept = member_med_total, linetype = "dashed") +
  geom_text_repel(
    data = top_candidates,
    aes(label = Personnel),
    show.legend = FALSE,
    size = 3.5
  ) +
  labs(x = "Total activities", y = "Duration (hours)") +
  theme_bw(base_size = 14)

# Table 5.2: Candidates exceeding the Member median
df |>
  filter(Type == "Candidate", Total > member_med_total) |>
  arrange(desc(Total), desc(Duration)) |>
  select(Personnel, Position, Incidents, Exercises, Events, Total, Duration, Activity_intensity)





# 7) Should we have minimum requirements?
# Plot 7.1: What % fall below candidate thresholds?
thresholds <- c(5, 10, 20)

thresh_tbl <- expand.grid(Type = unique(df$Type), thresh = thresholds) |>
  as_tibble() |>
  rowwise() |>
  mutate(
    pct_below = mean(df$Total[df$Type == Type] < thresh, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(pct_below = scales::percent(pct_below, accuracy = 0.1))

thresh_tbl


# Plot 7.2: Visualize below-threshold counts (useful slide)
thresh_tbl |>
  mutate(thresh = factor(thresh, levels = thresholds)) |>
  ggplot(aes(x = thresh, y = as.numeric(gsub("%","",pct_below)), fill = Type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = pct_below),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(x = "Proposed minimum Total activities", y = "% below threshold") +
  theme_bw(base_size = 14) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))


library(ggtern)

ggtern(df, aes(x = Incidents, y = Exercises, z = Events, color = Type)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw(base_size = 14)
