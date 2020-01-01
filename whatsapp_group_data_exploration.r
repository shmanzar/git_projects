# Sat Jul 28 19:29:57 2018 ------------------------------
library(tidyverse)
library(tidytext)
library(readr)
library(readxl)
library(lubridate)
library(stringr)

lunch_clean <- read_excel("FILENAME", 
                          col_types = c("text", "date", "text", 
                                        "text"))

lunch_clean %>% group_by(Sender) %>% tally()

lunch_tidy <- lunch_clean %>% 
    unnest_tokens(word, Message)

rm(lunch_clean)

lunch_tidy <- lunch_tidy %>% 
    anti_join(stop_words)

lunch_tidy %>% 
    count(Sender, word, sort = TRUE) %>% 
    filter(word != "media", word != "omitted") %>% 
    View()

lunch_tidy %>% 
    group_by(Sender) %>% 
    count(Sender, word, sort = TRUE) %>% 
    summarise(total = sum(n))


lunch_tidy %>% 
    count(word, sort = TRUE) %>% 
    filter(word != "media", word != "omitted") %>% 
    write_csv("test.csv")


lunch_tidy <- lunch_tidy %>% 
    mutate(hours = (hour(Time)), min = (minute(Time)), sec = (second(Time)),
           time = format(as.POSIXct(strptime(Time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S"))
          
lunch_tidy %>% 
    mutate(date1 = mdy(Date)) %>% 
    View()

lunch_tidy %>% 
    group_by(hours) %>% 
    tally() %>% 
    arrange(-n) %>% 
    View()
    ggplot(aes(hours)) +
    geom_bar(position= "dodge")
