library(tidyverse)

# to do ----
# 2. calculate D'

# read in data ----
pilot_path = paste(getwd(), "/pilot2_data_raw/", sep = "")
list_of_datanames = list.files(pilot_path)
dat_p2 = lapply(paste(pilot_path, list_of_datanames, sep = ""), read.csv)
dat_p2 = do.call(plyr::rbind.fill, dat_p2)

# data preprocessing ----
data_quest = data.frame(
  participant = c("1_2", "3_4", "5_6", "7_8", "9_10", "11_12", "13_14",
                  "15_16"),
  #1  3  5  7  9 11  13 15
  p1_resp = c(6, 2, 5, 3, 4, 4, 5, 3), 
  #2  4  6  8 10 12  14 16
  p2_resp = c(1, 2, 3, 2, 4, 5, 2, 4))

dat_clean = dat_p2 %>% 
  group_by(participant) %>% 
  # get rid of practice trials
  slice(-c(1:2)) %>% 
  # we need to recode to 1 and 2 because the fill function won't work for characters. 
  # It only works for numeric variables
  mutate(
    RespTuringLeft = 
      case_when(RespTuringLeft == "c" ~ 1,
                RespTuringLeft == "h" ~ 2),
    RespTuringRight = 
      case_when(RespTuringRight == "c" ~ 1,
                RespTuringRight == "h" ~ 2)) %>% 
  fill(RespTuringLeft, .direction = "up") %>%
  fill(RespTuringRight, .direction = "up") %>%
  fill(keyTuringLeft.corr, .direction = "up") %>%
  fill(keyTuringRight.corr, .direction = "up") %>%
  mutate(
    RespTuringLeft = recode(
      RespTuringLeft, `1` = "comp", `2` = "human")) %>%
  mutate(
    RespTuringRight = recode(
      RespTuringRight, `1` = "comp", `2` = "human")) %>%
  # remove some vars
  select(-c(1:4,7:12, 15,16,18:82, 93:116, 130:138)) %>%
  ## Delete every 11th row
  filter(
    is.na(Number)==F,
    is.na(key_respLeft.rt) == F,
    is.na(key_respRight.rt) == F
  ) %>%
  left_join(data_quest, by = "participant") %>% 
  mutate(
    # measure synchrony
    sync_value = abs((key_respLeft.rt-key_respRight.rt))/
      (key_respLeft.rt+key_respRight.rt)*100,
    # create lead/lags for each participant
    choice = case_when(RespTuringRight == "comp" &
                         RespTuringLeft == "comp" ~ 
                         "comp_congruent",
                       RespTuringRight == "comp" & 
                         RespTuringLeft == "human" ~ 
                         "incongruent",
                       RespTuringRight == "human" & 
                         RespTuringLeft == "comp" ~ 
                         "incongruent",
                       RespTuringRight == "human" & 
                         RespTuringLeft == "human" ~
                         "human_congruent"),
    closeness_diff = abs(p1_resp - p2_resp),
    closeness_avg = mean(c(p1_resp, p2_resp))) %>% 
  mutate(sync_z_value = scale(sync_value)) %>% 
  group_by(participant, Block.thisN) %>%
  mutate(good_trials_n = n()) %>%
  filter(good_trials_n > 8) %>%
  ungroup(participant, Block.thisN)%>%
  mutate(key_respLeft.rt = scale(key_respLeft.rt),
         key_respRight.rt = scale(key_respRight.rt))

# 1. sync as a function of perceived (for each participant)

# first we do lead lag for each participant 
dat_clean$part1_lag_rt = data.table::shift(dat_clean$key_respLeft.rt, n = 1L, type = "lag")
dat_clean$part2_lag_rt =  data.table::shift(dat_clean$key_respRight.rt, n = 1L, type = "lag")

# get rid of NAs
dat_clean2 = dat_clean %>%
  group_by(participant, Block.thisN) %>%
  filter(!trials_loop.thisN==0) %>%
  ungroup(Block.thisN) %>%
  select(participant, key_respLeft.rt, key_respRight.rt, Partner,
         part1_lag_rt, part2_lag_rt, RespTuringLeft, RespTuringRight) %>%
  mutate(sync_cross_p1 = abs((key_respLeft.rt-part2_lag_rt))/
           (key_respLeft.rt+part2_lag_rt)*100,
         sync_cross_p2 = abs((key_respRight.rt-part1_lag_rt))/
           (key_respRight.rt+part1_lag_rt)*100) %>%
  pivot_longer(cols = c(sync_cross_p1, sync_cross_p2),
               values_to = "sync_cross", names_to = "p_position") %>%
  mutate(new_partner = ifelse(p_position == "sync_cross_p1", 
                              RespTuringLeft, 
                              RespTuringRight)
         )

dat_clean2 %>% 
  ggplot(aes(sync_cross, fill = Partner))+
  geom_histogram()

dat_clean2 %>% 
  ggplot(aes(y = sync_cross, x = Partner, 
             color = new_partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()

dat_clean2 %>% 
  ggplot(aes(y = sync_cross, x = new_partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()

# Z value ----
dat_clean %>%
  group_by(participant, Block.thisN) %>%
  filter(!trials_loop.thisN==0) %>%
  ungroup(Block.thisN) %>%
  select(participant, key_respLeft.rt, key_respRight.rt, Partner,
         part1_lag_rt, part2_lag_rt, RespTuringLeft, RespTuringRight) %>%
  mutate(sync_cross_p1 = abs((key_respLeft.rt-part2_lag_rt))/
           (key_respLeft.rt+part2_lag_rt)*100,
         sync_cross_p2 = abs((key_respRight.rt-part1_lag_rt))/
           (key_respRight.rt+part1_lag_rt)*100) %>%
  pivot_longer(cols = c(sync_cross_p1, sync_cross_p2),
               values_to = "sync_cross", names_to = "p_position") %>%
  mutate(new_partner = ifelse(p_position == "sync_cross_p1", 
                              RespTuringLeft, 
                              RespTuringRight),
  )

dat_clean2 %>% 
  ggplot(aes(sync_cross_p1, fill = Partner))+
  geom_histogram()

dat_clean2 %>% 
  ggplot(aes(y = sync_cross, x = Partner, 
             color = new_partner))+
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = .1, position = position_dodge(.3),
               fun.args = list(mult = 1))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5, 
               position = position_dodge(.3))+
  theme_bw()

# Z value diff method