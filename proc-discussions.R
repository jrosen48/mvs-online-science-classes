# processing_discussions.R

remove(list = ls())

setwd("~/Dropbox/1_research/utility_value_intervention_online_science/discussion_exports")

library(readxl)

out_df <- str_c("discussions/", list.files("discussions/")) %>% 
    map_df(read_excel)

names(out_df) <- c("course", "section", "user_pk", "thread_title", "thread_desc", "post_subject", "date_posted", "text")

# write.csv(out_df, "~/Dropbox/research/utility_value_intervention_online_science/all_discussions.csv", row.names = F)

# Loading processed data

library(readr)
ad <- read_csv("all_disc_12-20.csv")

write_csv(out_df, "all-disc.data.csv")

str(all_data)
