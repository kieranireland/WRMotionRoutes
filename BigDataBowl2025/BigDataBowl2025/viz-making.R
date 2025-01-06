library(tidyverse)
library(gganimate)
library(nflfastR)
library(ggthemes)
library(gt)
library(ggridges)
library(ggimage)
library(dplyr)
library(tidyr)
library(fmsb)

tracking <- read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_1.csv") |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_2.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_3.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_4.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_5.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_6.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_7.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_8.csv")) |>
  bind_rows(read_csv("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/tracking_week_9.csv"))

final_df <- read_csv("nfl-big-data-bowl-2025/final_df_with_motion_mim.csv")
results_df <- read_csv("nfl-big-data-bowl-2025/test_results_mim.csv")
plays <- read_csv("nfl-big-data-bowl-2025/plays.csv")
player_plays <- read_csv("nfl-big-data-bowl-2025/player_play.csv")
players <- read_csv("nfl-big-data-bowl-2025/players.csv")
feature_importance <- read_csv("nfl-big-data-bowl-2025/feature_importance_mim.csv")

### ACCURACY + PRECISION/RECALL
# percRoutes_df = final_df %>% 
#   select(gameId, playId, nflId, routeRan) %>% 
#   group_by(routeRan) %>% 
#   summarize(numRoutes = n()) %>% 
#   mutate(percRoutes = numRoutes/nrow(final_df))

outcome_df = results_df %>% 
  select(gameId, playId, nflId, maxRoute, actualRoute,
         POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE) %>% 
  mutate(truePred = ifelse(maxRoute == actualRoute, 1, 0)) %>% 
  pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE),
               names_to = 'predRoute',
               values_to = 'probability') %>% 
  mutate(routeOutcome = ifelse(predRoute == actualRoute, "Equal", "Not Equal")) %>% 
  mutate(routeOutcome = factor(routeOutcome, levels = c("Equal", "Not Equal"))) %>% 
  group_by(actualRoute, routeOutcome) %>% 
  summarize(numRight = sum(truePred),
            prob = mean(probability),
            numTotal = n()) %>% 
  mutate(recall = numRight/numTotal) %>% 
  select(actualRoute, routeOutcome, prob)

accuracy_df = results_df %>% 
    select(gameId, playId, nflId, maxRoute, actualRoute,
           POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE) %>% 
    mutate(truePred = ifelse(maxRoute == actualRoute, 1, 0)) %>% 
    pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE),
                 names_to = 'predRoute',
                 values_to = 'probability') %>% 
    filter(actualRoute == predRoute) %>% 
    group_by(actualRoute) %>% 
    summarize(numRight = sum(truePred),
              prob = mean(probability),
              numTotal = n()) %>% 
    mutate(recall = numRight/numTotal) %>% 
  select(actualRoute, prob)


## TO-DO : Change colors of bars, reorder to be equal on top, fix legend position and title, fix x-axis to be percentages
outcome_df %>% 
  filter(actualRoute != "WHEEL") %>% 
  mutate(routeOutcome = factor(routeOutcome, levels = c("Not Equal", "Equal"))) %>% 
  ggplot(aes(x = prob, y = fct_reorder(actualRoute, prob))) +
    geom_bar(aes(fill = routeOutcome), position = "dodge", stat = "identity", alpha = 0.8) +
    labs(x = "Average Predicted Probability",
         y = "Route",
         title = "Average Predicted Probability based on Route Outcome",
         fill='Route Outcome') + 
    scale_x_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c("Equal" = "#1f77b4", "Not Equal" = "#FF474C"),  # Set custom colors for the levels
                    breaks = c("Equal", "Not Equal")) + 
    theme_fivethirtyeight() +
    theme(
      plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      axis.text = element_text(size = 17),
      legend.direction = "vertical",
      legend.position = "right")
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/outcomeaccuracy.png",
       width = 10, height = 10, dpi="retina")

accuracy_df %>% 
  filter(actualRoute != "WHEEL") %>% 
  ggplot(aes(x = prob, y = fct_reorder(actualRoute, prob))) +
  geom_bar(aes(fill = prob), stat = "identity", alpha = 0.8) +
  labs(x = "Average Predicted Probability",
       y = "Route",
       title = "Average Predicted Probability when Actual and Predicted Routes are Equal") + 
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_c()+
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    axis.text = element_text(size = 17),
    legend.position = 'None')
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/accuracy_equal.png",
       width = 12, height = 10, dpi="retina")


### DELTA SCATTERPLOTS
delta_results_df = results_df %>% 
  select(gameId, playId, nflId, delta_x, delta_y, distFromBall, distFromLOS, min_def_distance,
         POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER) %>% 
  pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER),
               names_to = 'route',
               values_to = 'probability')

delta_results_df

ggplot(delta_results_df, aes(x = delta_x, y = probability)) +
  geom_point(alpha = 0.6, size = 2, col = 'blue') +  
  facet_wrap(~ route, scales = "free") +
  labs(
    title = "Change in X-Coordinate From Start of Motion to Snap vs Expected Route Probability",
    x = "Change in X-Coordinate",
    y = "Expected Route Probability"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/deltaX_routefacet.png",
       width = 15, height = 10, dpi="retina")


ggplot(delta_results_df, aes(x = delta_y, y = probability)) +
  geom_point(alpha = 0.6, size = 2, col = "red") +  
  facet_wrap(~ route, scales = "free") +
  labs(
    title = "Change in Y-Coordinate From Start of Motion to Snap vs Expected Route Probability",
    x = "Change in Y-Coordinate",
    y = "Expected Route Probability"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/deltaY_routefacet.png",
       width = 15, height = 10, dpi="retina")


ggplot(delta_results_df, aes(x = distFromBall, y = probability)) +
  geom_point(alpha = 0.6, size = 2, col = 'black') +   
  # geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  facet_wrap(~ route, scales='free') +
  labs(
    title = "WR Distance From Ball at Snap vs Expected Route Probability",
    x = "Distance From Ball",
    y = "Expected Route Probability"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/distFromBall_routefacet.png",
       width = 15, height = 10, dpi="retina")

ggplot(delta_results_df, aes(x = distFromLOS, y = probability)) +
  geom_point(alpha = 0.6, size = 2, col = 'red') +  
  # geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  facet_wrap(~ route, scales = "free") +
  labs(
    title = "WR Distance From LOS at Snap vs Expected Route Probability",
    x = "Distance From LOS",
    y = "Expected Route Probability"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/distfromLOS_routefacet.png",
       width = 15, height = 10, dpi="retina")


ggplot(delta_results_df, aes(x = min_def_distance, y = probability)) +
  geom_point(alpha = 0.6, size = 2, col = 'blue') +  
  facet_wrap(~ route, scales = "free") +
  labs(
    title = "Scatterplot of Min Def Dist vs Expected Route Probability",
    x = "Min Def Dist",
    y = "Expected Route Probability"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 12)
  )


## STACKED BAR CHART BY CLUB + BY MOTION TYPE
club_results_df = inner_join(results_df, 
                             final_df %>%
                               select(gameId, playId, nflId, club_x),
                             by=c("gameId", "playId", "nflId")) %>% 
                rename("club" = "club_x")

# club_results_df %>% 
#   select(gameId, playId, nflId, club,
#          POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE) %>% 
#   pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE),
#                names_to = 'route',
#                values_to = 'probability') %>% 
#   group_by(club, route) %>% 
#   summarize(prob = mean(probability)) %>% 
#   ggplot(aes(fill = route, x = club, y = prob)) + 
#   geom_bar(position = "stack", stat = "identity")
#   # scale_fill_manual(values=c('red', 'purple', 'pink', ...))


# club_results_df %>% 
#   select(gameId, playId, nflId, club, motionType_orig,
#          POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE) %>% 
#   pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE),
#                names_to = 'route',
#                values_to = 'probability') %>% 
#   rename("motionType" = "motionType_orig") %>% 
#   group_by(motionType, route) %>% 
#   summarize(prob = mean(probability)) %>% 
#   ggplot(aes(fill = route, x = motionType, y = prob)) +
#   geom_bar(position = "stack", stat = "identity")
  

# club_results_df %>% 
#   pivot_longer(col=starts_with("quarter"), names_to="quarter", names_prefix="quarter_") %>%
#   filter(value==1) %>%
#   select(-value) %>%
#   select(gameId, playId, nflId, club, quarter,
#          POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER) %>%
#   pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER),
#                names_to = 'route',
#                values_to = 'probability') %>%
#   group_by(quarter, route) %>%
#   summarize(prob = mean(probability)) %>%
#   ggplot(aes(fill = route, x = quarter, y = prob)) +
#   geom_bar(position = "stack", stat = "identity")
# 
# 
# club_results_df %>%
#   pivot_longer(col=starts_with("down"), names_to="down", names_prefix="down_") %>%
#   filter(value==1) %>%
#   select(-value) %>%
#   select(gameId, playId, nflId, club, down,
#          POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER) %>%
#   pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER),
#                names_to = 'route',
#                values_to = 'probability') %>%
#   group_by(down, route) %>%
#   summarize(prob = mean(probability)) %>%
#   ggplot(aes(fill = route, x = down, y = prob)) +
#   geom_bar(position = "stack", stat = "identity")


results_df %>% 
  select(gameId, playId, nflId, motionType_orig, actualRoute) %>% 
  group_by(motionType_orig, actualRoute) %>% 
  count()


### FEATURE IMPORTANCE
feature_importance %>% 
  mutate(imp = `...1`, label = `0`) |> 
  filter(imp > 0.02) |> 
  ggplot(aes(x = imp, y = fct_reorder(label, imp))) +
  geom_point(, alpha = 0.8) + 
  labs(x = "Gain",
       y = "",
       title = "XGBClassifier Feature Importance",
       subtitle = "Inclusive of Features with a 0.02 importance or greater",
       caption = "*Scale of 0-1") +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(size=20),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(size = 35),
        plot.title = element_text(size = 50),
        plot.subtitle = element_text(size = 35),
        plot.caption = element_text(size = 20)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_size_continuous(range = c(2, 6))
ggsave("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/featureimportance.png",
       width = 10, height = 10, dpi="retina")



### RADAR PLOTS
team_data <- club_results_df %>%
  select(gameId, playId, nflId, club,
         POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE) %>% 
  pivot_longer(cols = c(POST, CROSS, SLANT, IN, HITCH, FLAT, OUT, GO, SCREEN, CORNER, ANGLE),
               names_to = 'route',
               values_to = 'probability') %>% 
  group_by(club, route) %>% 
  summarize(prob = mean(probability)) %>% 
  ungroup() %>% 
  filter(club %in% c("LA", "GB", "DEN", "CLE")) %>% 
  select(club, route, prob)

team_radar_data <- team_data %>%
  spread(key = route, value = prob, fill = 0)

team_radar_data


(team_radar <- as.data.frame(rbind(
  rep(.3, ncol(team_radar_data)),
  rep(0, ncol(team_radar_data)),
  team_radar_data)) %>% 
    select(-club)
  )


rownames(team_radar) = c('Max', 'Min', 'CLE', 'DEN', 'GB', 'LA')

team_radar

create_radarchart <- function(data, color = "#00AFBB",
                              vlabels = colnames(data), vlcex = 1.25,
                              caxislabels = NULL, title = NULL){
  radarchart(
    data, axistype = 1,
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "grey",
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title,
    cex.main = 2
  )
}

colors <- c("#311D00", "#FB4F14", "#FFB612", "#003594")
titles <- c("Cleveland Browns", "Denver Broncos", "Green Bay Packers", "Los Angeles Rams")

png("/Users/kieranireland/Desktop/Sports Analytics/BigDataBowl2025/BigDataBowl2025/nfl-big-data-bowl-2025/team_radarcharts.png",
    width = 1500, height = 1000)

op <- par(mar = c(1, 1, 3, 1))
par(mfrow = c(2, 2))

for(i in 1:4){
  create_radarchart(
    data = team_radar[c(1, 2, i+2), ], caxislabels = c(5, 10, 15, 20, 25),
    color = colors[i], title = titles[i]
  )
}

mtext("Team Expected Route Profiles", side = 3, outer = TRUE, line = -2, cex = 2, font = 2)
mtext("Comparing expected probability of each route across teams", side = 3, outer = TRUE, line = -4, cex = 1, font = 1)

par(op)

dev.off()



