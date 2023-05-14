library(tidyverse)
library(plotrix)

NBA = read.csv("NBA Player Stats.csv", sep = ";")

NBA = NBA |>
  mutate(MFG = FGA - FG) |>
  mutate(MFT = FTA - FT) |>
  select(Tm, Pos, Player, G, PTS, TRB, AST, STL, BLK, MFG, MFT, TOV) |>
  rename(Team = Tm)

write.csv(x = NBA, file = "data/NBA.csv")


NBA |>
  filter(Team == 'LAL') |>
  filter(Pos == 'SF') |>
  filter(Player == 'LeBron James') |>
  pivot_longer(PTS:BLK, names_to = 'Type', values_to = "Stat") |>
  group_by(Type) |>
  summarise(Stat = sum(Stat)) |>
  ggplot()+
  aes(x = Type, y = Stat, fill = Type) |>
  geom_bar(stat = "identity") +
  coord_polar(theta = "x", direction=1) +
  theme_bw()


