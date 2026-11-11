library(ggplot2)
library(socviz)
library(tidyverse)
library(ggthemes)
colnames(election24)
colnames(counties_sf)

us_states <- map_data("state") |> as_tibble()

us_states |>
  ggplot(mapping = aes(x = long, y = lat,
                       group = group)) +
  geom_polygon(fill = "white", color = "black")

party_colors <- c("Democratic" = "#2E74C0",
                  "Republican" = "#CB454A")

election24 |>
  select(state:total_vote, party)

library(sf)
states_sf

#Q1

election24_county_df

elections24_sf <- left_join(states_sf, election24,
                            by = join_by(fips, st, state, census))

elections24_sf

elections24_sf |>
  ggplot(mapping = aes(fill = party)) +
  geom_sf(color = "white") +
  scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2024", fill = NULL) +
  theme_map()



#Q2
colnames(counties_sf)
colnames(election24)
counties_sf |>
  select(fips:pop, pop_dens_disc) |>
  sample_n(10)

elections24_counties <- left_join(counties_sf, election24_county_df, 
                                  by = join_by(fips))


elections24_counties |>
  ggplot(mapping = aes(fill = winner)) +
  geom_sf(linewidth = 0.005,color = "white") +
  scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2024", fill = NULL) +
  theme_map()

#Q3 
elections24_counties |>
  ggplot(mapping = aes(fill = pop_dens_disc)) +
  geom_sf(linewidth = 0.01, color = "gray80") +
  scale_fill_brewer(palette = "Blues") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Population Density by County", fill = "Percent") +
  theme_map()

#Q4
base_colors <- c("#005F73", "#0A9396", "#E9D8A6", "#EE9B00", "#CA6702", "#BB3E03")
elections24_counties |>
  ggplot(mapping = aes(fill = black_disc)) +
  geom_sf(linewidth = 0.01, color = "gray80") +
  scale_fill_manual(values = colorRampPalette(base_colors)(7))+
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Percentage of Black by County", fill = "Percent") +
  theme_map()

#Q5


election26<- read.csv("C:/Users/Ivan Office/Dropbox/PC/Downloads/midterms_2026_preliminary.csv")
state_map<- states_sf

election26_recode<- election26%>% rename(st = state_abbr)

election26_senate<- election26_recode%>% filter(office == "U.S. Senate" | office == "U.S. Senate (special)")%>%drop_na(vote_pct)
election26_governor<- election26_recode%>% filter(office=="Governor")%>%drop_na(vote_pct)
elections26_state <- left_join(states_sf, election26_recode,
                                  by = join_by(st))      
election26