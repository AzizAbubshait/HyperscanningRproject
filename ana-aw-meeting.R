library(tidyverse)

# to do ----
# 2. calculate D'
# 3. calculate the matrix thing

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
  select(-c(1:4,7:12, 15:82, 93:116, 130:138)) %>%
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
    part1_lag_rt = data.table::shift(dat_clean$key_respLeft.rt, n = 1L, type = "lag"),
    part2_lag_rt = data.table::shift(dat_clean$key_respRight.rt, n = 1L, type = "lag"),
    
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
  ungroup(particiant, Block.thisN)

# 1. sync as a function of perceived (for each participant)

# first we do lead lag for each participant 
dat_clean$part1_lag_rt = data.table::shift(dat_clean$key_respLeft.rt, n = 1L, type = "lag")

dat_clean$part1_lag_rt =  data.table::shift(dat_clean$key_respLeft.rt, n = 1L, type = "lag")




# Trial number 1 is weird because it is deducted from the last trial
# of the previous block. We'll just replace all the first trials with actual data.
dat_t_full$chose.yes.no[dat_t_full$Trial_Number == 1] = 
  dat_t_full$Chose_Higher_Prob_Square[dat_t_full$Trial_Number == 1]


