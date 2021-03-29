# set working directory (change as necessary) and load libraries-----------
rm(list = ls())
setwd('C:/Users/javiera/Cawthron/Coastal hardening paper - CH manuscript 1 (model)')
library(readxl)
library(tidyverse)
library(broom)
library(janitor)

# set plot theme------
theme_set(theme_bw())
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             strip.background = element_blank())

# read data and merge all spreadsheets ------------
d <-
  bind_rows(
    Total = read_excel('data/2043_2068 forecasts_output organised.xlsx', 2),
    Breakwalls = read_excel('data/2043_2068 forecasts_output organised.xlsx', 3),
    Pontoons = read_excel('data/2043_2068 forecasts_output organised.xlsx', 4),
    Jetties = read_excel('data/2043_2068 forecasts_output organised.xlsx', 5),
    Wharves = read_excel('data/2043_2068 forecasts_output organised.xlsx', 6),
    .id = 'structure'
  ) %>%
  filter(Year == 2043) %>%
  select(-Time2, -Year) %>%
  clean_names() %>% 
  mutate(
    scenario = fct_relevel(scenario, c("LO", "MED")),
    scenario = fct_recode(
      scenario,
      Low = "LO",
      Medium = "MED",
      High = "HI"
    )
  ) %>%
  group_by(structure, scenario) %>%
  rename(prediction = "measurement") %>%
  mutate(
    incr_no_neg = if_else(incr < 0, 0, incr),
    pc_incr_no_neg = if_else(pc_incr < 0, 0, incr),
    pc_contribution = incr_no_neg / sum(incr_no_neg) * 100
  ) %>%
  ungroup()
  

# data on current extension ---
cur <- 
  read_csv('Data/current_structure_extension.csv') %>% 
  rename(structure = "response") %>% 
  mutate(structure = fct_recode(structure, Total = "TOT",
                                Breakwalls = "BRK",
                                Jetties = "JET",
                                Wharves = "WHF",
                                Pontoons = "PON")) %>% 
  clean_names()

# merge all data and clean names----
dat_ch <- 
  left_join(d, cur, by = c("structure","location")) %>% 
  write_csv('Data/data_ch_all_clean.csv')

cur_total <- 
  cur %>% 
  group_by(structure) %>% 
  summarise(ext_2018 = sum(ext_2018))


# summarise data by structure and scenario--------- 
d_mean <-
  d %>%
  group_by(structure, scenario) %>%
  summarise_at(vars(prediction:se_incr), sum, na.rm = T) %>%
  left_join(cur_total, by = 'structure') %>%
  ungroup() %>%
  mutate(per_inc = (prediction / ext_2018 * 100) - 100,
         structure = fct_rev(fct_relevel(structure, "Total")))

# plot national figures with percentage increase----------------
plot1 <-
  ggplot(d_mean, aes(x = structure,
                     y = incr,
                     fill = scenario)) +
  geom_col(
    position = position_dodge(),
    width = .9,
    colour = "black",
    alpha = 0.5
  ) +
  geom_text(
    aes(y = incr , label = scales::percent(round(per_inc, 0) / 100)),
    position = position_dodge(width = 0.9),
    vjust = 0.4,
    hjust = -.15,
    size = 3.5
  ) +
  coord_flip() +
  labs(y = 'Predicted increase (km)') +
  theme(legend.position = c(.9, .15),
        axis.title.y = element_blank()) +
  scale_fill_discrete(name = 'Scenario') +
  scale_y_continuous(limits = c(0, 450))


print(plot1)
# save plot 1 as sgv-------
ggsave(plot1, filename = 'Figures/plot1.svg')

# Option 2 bubble plot ----------
plot1a <- 
ggplot(d_mean,
       aes(x = structure,
           y = incr,
           color = scenario,
           group = scenario)) +
  geom_linerange(aes(
    xmin = structure,
    xmax = structure,
    ymin = 0,
    ymax = incr - 11.5,
    color = scenario
  ), size = 1,
  alpha = .5,
  position = position_dodge(width = 0.9)) +
  geom_point(aes(size = per_inc), position = position_dodge(width = 0.9), alpha = .5) +
  geom_text(
    aes(y = incr , label = scales::percent(round(per_inc, 0)/100)),
    position = position_dodge(width = 0.9), color = 1,
    vjust = 0.3,
    hjust = .45,
    size = 3
  ) +
  coord_flip() +
  labs(y = 'Predicted increase (km)') +
  theme(legend.position = c(.8, .2),  axis.title.y = element_blank()) +
  scale_fill_discrete(name = 'Scenario') +
  scale_y_continuous(limits = c(0, 450)) +
  scale_size(range = c(5,13), guide = F)
print(plot1a)
# save plot 1 as sgv-------
ggsave(plot1a, filename = 'Figures/plot1a.svg')


## boxplots of absolute andrelative predictions -------
ggplot(d, aes(structure,  pc_incr, fill = scenario)) +
  geom_boxplot(alpha = .5) +
  scale_y_log10()


ggplot(d, aes(structure,  incr, color = scenario)) +
  geom_boxplot() +
  # scale_y_log10() +
  theme(axis.title.x = element_blank()) +
  labs('Predicted ')


# ANOVA to test differecnes between structure and scenario-----
m1 <- aov(prediction ~ structure * scenario, filter(d, structure!="Total"))
summary(m1)
tidy(m1) %>% write_csv('Tables/ANOVA_table.csv' , na = "")

thsd <- TukeyHSD(m1, "structure")
tidy(thsd) %>% write_csv('Tables/TukeyHSD_table.csv')

# plot relative contributions of each location by structure type --------
plot2 <- 
  d %>% 
  filter(scenario =="Medium" & structure != "Total") %>% 
  # mutate(structure  = fct_relevel(structure,"Total")) %>% 
  ggplot(aes(
    x = fct_reorder(location, pc_contribution),
    y = pc_contribution,
    fill = structure
  )) +
  geom_col(position = position_dodge(),
           colour = "black",
           alpha = 0.7) +
  facet_wrap( ~ structure, scales = 'fixed',nrow = 1) +
  coord_flip() +
  labs(y = '% contribution of predicted domestic increase') +
  theme(axis.title.y = element_blank()) +
  scale_fill_viridis_d(name = "Structure")

print(plot2)

# save plot 2 as sgv-------
ggsave(plot2, filename = 'Figures/plot2.svg')
ggsave(plot2, filename = 'Figures/plot2.tiff',compression = "lzw", device = 'tiff')
