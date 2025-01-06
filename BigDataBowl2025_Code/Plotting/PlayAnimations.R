library(tidyverse)
library(gganimate)


# tracking <- read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_1.csv") |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_2.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_3.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_4.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_5.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_6.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_7.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_8.csv")) |>
#   bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_9.csv"))

plays <- read_csv("nfl-big-data-bowl-2025/plays.csv")
player_plays <- read_csv("nfl-big-data-bowl-2025/player_play.csv")
games <- read_csv("nfl-big-data-bowl-2025/games.csv")
results_df <- read_csv("nfl-big-data-bowl-2025/test_results_mim.csv")

## Short motion, cross route
ex_df1 <- tracking |>
  filter(gameId == 2022103008, playId == 2809) |>
  mutate(start_frame = frameId[which(event == "man_in_motion")][1],
         end_frame = frameId[which(event == "out_of_bounds")][1]) |>
  filter(frameId >= start_frame & frameId <= end_frame) |>
  mutate(
    pt_size = ifelse(club == "football", 2.7, 5.4),
    pt_fill = case_when(
      club == "PHI" ~ "2",
      club == "PIT" & nflId == "52457" ~ "3",
      club == "PIT" & nflId != "52457" ~ "1",
      club == "football" ~ "4"
    )
  )

club_df1 <- ex_df1 |> 
  filter(club != "football")
football_df1 <- ex_df1 |>
  filter(club == "football")
full_route_path1 <- ex_df1 |>
  filter(nflId == 52457) |>
  mutate(motion_frame = frameId[which(event == "man_in_motion")][1],
         snap_frame = frameId[which(event == "ball_snap")][1])
route_path1 <- full_route_path1 |>
  filter(frameId >= snap_frame) |>
  mutate(motion_color = "red",
         motion_linetype = "solid",
         motion_linewidth = 1)
motion_path1 <- full_route_path1 |>
  filter(frameId >= motion_frame & frameId <= snap_frame) |> 
  mutate(motion_color = "red",
         motion_linetype = "solid",
         motion_linewidth = 1)


(claypool_route1 <- ggplot() +
    annotate("rect",
             xmin = 160/3,
             xmax = 0,
             ymin = 55,
             ymax = 120,
             fill = scales::alpha("white", 0.9),
             col = 'black') +
    annotate("text", 
             y = seq(60, 110, 10),
             x = 10,
             color = "black",
             label = c(seq(50, 10, -10), "G   "),
             linewidth = 6,
             angle = 90) +
    annotate("text", 
             y = seq(60, 110, 10),
             x = 40,
             color = "black",
             label = c(seq(50, 10, -10), "   G"),
             linewidth = 6,
             angle = 270) +
    annotate("text", 
             y = c(setdiff(seq(55, 110, 1), seq(55, 110, 5))),
             x = 160/3-1,
             size = 3,
             color = "black",
             label = "—") +
    annotate("text", 
             y = c(setdiff(seq(55, 110, 1), seq(55, 110, 5))),
             x = 1,
             size = 3,
             color = "black",
             label = "—") +
    annotate("text", 
             y = c(setdiff(seq(55, 110, 1), seq(55, 110, 5))),
             x = 23.36667,
             color = "black",
             size = 3,
             label = "—") +
    annotate("text", 
             y = c(setdiff(seq(55, 110, 1), seq(55, 110, 5))),
             x = 29.96667,
             color = "black",
             size = 3,
             label = "—") +
    annotate("segment",
             y = 55,
             yend = 110,
             x = c(160/3, 0),
             xend = c(160/3, 0),
             color = "black") +
    geom_segment(aes(x = 0, xend = 160/3, y = seq(55, 110, 5), yend = seq(55, 110, 5)), color = "black") +
    annotate("segment",
             y = 103,
             yend = 103,
             x = 0,
             xend = 160/3,
             size = 1.5,
             color = "#FDE725") +
    annotate("segment",
             y = 93,
             yend = 93,
             x = 0,
             xend = 160/3,
             size = 1.5,
             color = "black") +
    geom_point(data = club_df1, 
               aes(y, x, fill = pt_fill, size = pt_size, group = nflId), shape = 21) +
    geom_point(data = football_df1, 
               aes(y, x, size = pt_size, group = nflId), fill = "#654321", shape = 21) +
    geom_line(data = motion_path1, 
              aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
    geom_line(data = route_path1, 
              aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
    scale_fill_manual(values = c("#FFB612", "#004C54", "red")) +
    scale_size_identity() +
    scale_color_identity() +
    scale_linetype_identity() +
    scale_linewidth_identity() +
    scale_x_reverse() +
    transition_time(frameId) +
    transition_reveal(frameId) +
    ease_aes("linear") +
    guides(fill = guide_legend(override.aes = list(size = 5.4))) +
    labs(title = "Cross Route After Short Motion",
         subtitle = "Q3 - 2nd & 10: PHI vs. PIT, Week 8 2022 NFL Season",
         fill = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.subtitle = element_text(size = 9.5, hjust = 0.5),
          text = element_text(family = "sans"),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()))

anim_save(filename = "/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/claypool_motion_cross_route.gif",
          animation = animate(claypool_route1,
                              fps = 10, res = 240,
                              height = 900, width = 1600,
                              start_pause = 10, end_pause = 10,
                              renderer = gifski_renderer()))





## Short motion, flat route -- Kumerow Play
kumerow_df <- tracking |>
  filter(gameId == 2022092503, playId == 890) |>
  mutate(start_frame = frameId[which(event == "man_in_motion")][1],
         end_frame = frameId[which(event %in% c("out_of_bounds"))][1]) |>
  filter(frameId >= start_frame & frameId <= end_frame) |>
  mutate(
    pt_size = ifelse(club == "football", 2.7, 5.4),
    pt_fill = case_when(
      club == "MIA" ~ "2",
      club == "BUF" & nflId == "42818" ~ "3",
      club == "BUF" & nflId != "42818" ~ "1",
      club == "football" ~ "4"
    )
  )

bills_df <- kumerow_df |> 
  filter(club != "football")
football_df2 <- kumerow_df |>
  filter(club == "football")
full_route_path2 <- kumerow_df |>
  filter(nflId == 42818) |>
  mutate(motion_frame = frameId[which(event == "man_in_motion")][1],
         snap_frame = frameId[which(event == "ball_snap")][1])
route_path2 <- full_route_path2 |>
  filter(frameId >= snap_frame) |>
  mutate(motion_color = "red",
         motion_linetype = "solid",
         motion_linewidth = 1)
motion_path2 <- full_route_path2 |>
  filter(frameId >= motion_frame & frameId <= snap_frame) |> 
  mutate(motion_color = "red",
         motion_linetype = "solid",
         motion_linewidth = 1)



(kumerow_route <- ggplot() +
    annotate("rect",
             xmin = 160/3,
             xmax = 0,
             ymin = 25,
             ymax = 90,
             fill = scales::alpha("white", 0.9)) +
    annotate("text", 
             y = seq(30, 85, 10),
             x = 10,
             color = "black",
             label = c(seq(20, 50, 10), seq(40, 30, -10)),
             size = 6,
             angle = 90) +
    annotate("text", 
             y = seq(30, 85, 10),
             x = 40,
             color = "black",
             label = c(seq(20, 50, 10), seq(40, 30, -10)),
             size = 6,
             angle = 270) +
    annotate("text", 
             y = setdiff(seq(25, 90, 1), seq(25, 90, 5)),
             x = 160/3 - 1,
             size = 3,
             color = "black",
             label = "—") +
    annotate("text", 
             y = setdiff(seq(25, 90, 1), seq(25, 90, 5)),
             x = 1,
             size = 3,
             color = "black",
             label = "—") +
    annotate("text", 
             y = setdiff(seq(25, 90, 1), seq(25, 90, 5)),
             x = 23.36667,
             color = "black",
             size = 3,
             label = "—") +
    annotate("text", 
             y = setdiff(seq(25, 90, 1), seq(25, 90, 5)),
             x = 29.96667,
             color = "black",
             size = 3,
             label = "—") +
    annotate("segment", 
             y = 25,
             yend = 90,
             x = c(160/3, 0),
             xend = c(160/3, 0),
             color = "black") +
    geom_segment(aes(x = 0, xend = 160/3, y = seq(25, 90, 5), yend = seq(25, 90, 5)), color = "black") +
    annotate("segment", 
             y = 60,
             yend = 60,
             x = 0,
             xend = 160/3,
             size = 1.5,
             color = "#FDE725") +
    annotate("segment", 
             y = 58,
             yend = 58,
             x = 0,
             xend = 160/3,
             size = 1.5,
             color = "black") +
    geom_point(data = bills_df, 
               aes(y, x, fill = pt_fill, size = pt_size, group = nflId), shape = 21) +
    geom_point(data = football_df2, 
               aes(y, x, size = pt_size, group = nflId), fill = "#654321", shape = 21) +
    geom_line(data = motion_path2, 
              aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
    geom_line(data = route_path2, 
              aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
    scale_fill_manual(values = c("#00338D", "#008E97", "red")) +
    scale_size_identity() +
    scale_color_identity() +
    scale_linetype_identity() +
    scale_linewidth_identity() +
    scale_x_reverse() +
    transition_time(frameId) +
    transition_reveal(frameId) +
    ease_aes("linear") +
    guides(fill = guide_legend(override.aes = list(size = 5.4))) +
    labs(title = "Flat Route After Short Motion",
         subtitle = "Q1 - 2nd & 2: MIA vs BUF, Week 3 2022 NFL Season",
         fill = NULL) +
    theme_minimal() +
    theme(text = element_text(family = 'sans', color = 'black'),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.subtitle = element_text(size = 9.5, face = "italic", hjust = 0.5),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()))

anim_save(filename = "/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/kumerow_motion_flat_route.gif",
          animation = animate(kumerow_route,
                              fps = 10, res = 240,
                              height = 900, width = 1600,
                              start_pause = 10, end_pause = 10,
                              renderer = gifski_renderer()))


