att1 <- read.csv("/Users/krystalfeng/Desktop/Programming/Kaggle/Speed dating/attr1.csv")
att2 <- read.csv("/Users/krystalfeng/Desktop/Programming/Kaggle/Speed dating/attr2.csv")

install.packages("fmsb")
library(fmsb)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
install.packages("radarchart")
library("radarchart")

# library(gridBase)
library(scales)
test1 <- att1 %>%
  group_by(att1$gender) %>% summarise(att1, Attractive = mean(att1$attr1_1), Sincere = mean(att1$sinc1_1), Intelligent = mean(att1$intel1_1), Fun = mean(att1$fun1_1), Ambitious = mean(att1$amb1_1))
test1

rawdat <- read.csv('/Users/krystalfeng/Desktop/Programming/Kaggle/Speed dating/Speed Dating Data.csv', header = T, stringsAsFactors = F)
dat <- 
  rawdat %>% 
  select(-id, -idg, -condtn, -round, -position, -positin1, -order, -partner, -tuition, -undergra, -mn_sat)
at00 <-
  dat %>%
  select(iid, pid, dec, gender, attr, sinc, intel, fun, amb, shar, like, prob) %>% 
  filter(!pid == "NA")
at00[is.na(at00)] <- 1000

at00$total <- rowSums(at00[,c("attr", "sinc", "intel", "fun", "amb", "shar")])

at00 <-
  at00 %>% 
  filter(!total == "6000")

at00[at00 == "1000"] <- NA

at00$total <- rowSums(at00[,c("attr", "sinc", "intel", "fun", "amb", "shar")], na.rm=TRUE)

table(at00$total)

at00 <-
  at00 %>% 
  filter(!total == "0")

at00 <-
  at00 %>% 
  mutate(pgender = ifelse(gender == 0, 1, 0))
at11<-
  dat %>%
  group_by(gender) %>%
  select(iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1) %>% 
  unique()
at11[is.na(at11)] <- 0
at11$total <- rowSums(at11[,c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1", "shar1_1")])

table(at11$total)

at11<-
  at11 %>% 
  filter(!total == "0")

at11$attr1_1 <- round(at11$attr1_1/at11$total*100, digits = 2)
at11$sinc1_1 <- round(at11$sinc1_1/at11$total*100, digits = 2)
at11$intel1_1 <- round(at11$intel1_1/at11$total*100, digits = 2)
at11$fun1_1 <- round(at11$fun1_1/at11$total*100, digits = 2)
at11$amb1_1 <- round(at11$amb1_1/at11$total*100, digits = 2)
at11$shar1_1 <- round(at11$shar1_1/at11$total*100, digits = 2)

at11$total <- rowSums(at11[,c("attr1_1", "sinc1_1", "intel1_1", "fun1_1", "amb1_1")])

at11$total <- round(at11$total, digits = 0)
table(at11$total)

test1 <-
  at11 %>%
  group_by(gender) %>%
  summarise(Attractive = mean(attr1_1), Sincere = mean(sinc1_1), Intelligent = mean(intel1_1), Fun = mean(fun1_1), Ambitious = mean(amb1_1))

test1
test1forplot <-
  test1 %>% 
  select(-gender)
test1forplot

maxmin <- data.frame(
  Attractive = c(36, 0),
  Sincere = c(36, 0),
  Intelligent = c(36, 0),
  Fun = c(36, 0),
  Ambitious = c(36, 0))
maxmin

test11 <- rbind(maxmin, test1forplot)
test11male <- test11[c(1,2,4),]
test11female <- test11[c(1,2,3),]
test11male
test11female
test11

radarchart(test11,
           pty = 32,
           axistype = 0,
           pcol = c(adjustcolor("darkorchid1", 0.5), adjustcolor("dodgerblue", 0.5)),
           pfcol = c(adjustcolor("darkorchid3", 0.5), adjustcolor("dodgerblue2", 0.5)),
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 0.75,
           palcex = 0.75)

legend("topleft", 
       c("Male", "Female"),
       fill = c(adjustcolor("dodgerblue2", 0.5), 
                adjustcolor("darkorchid3", 0.5)))
title("What you look for in the opposite sex",
      cex.main = 1.0,
      col = "black")

### What Do Participants Think the Opposite Sex is Looking For
at21<-
  dat %>%
  group_by(gender) %>%
  select(iid, gender, attr2_1, sinc2_1, intel2_1, fun2_1, amb2_1, shar2_1) %>% 
  unique()
at21[is.na(at21)] <- 0
at21$attr2_1 <- round(at21$attr2_1/at21$total*100, digits = 2)
at21$sinc2_1 <- round(at21$sinc2_1/at21$total*100, digits = 2)
at21$intel2_1 <- round(at21$intel2_1/at21$total*100, digits = 2)
at21$fun2_1 <- round(at21$fun2_1/at21$total*100, digits = 2)
at21$amb2_1 <- round(at21$amb2_1/at21$total*100, digits = 2)

test2 <-
  at21 %>%
  group_by(gender) %>%
  summarise(Attractive = mean(attr2_1), Sincere = mean(sinc2_1), Intelligent = mean(intel2_1), Fun = mean(fun2_1), Ambitious = mean(amb2_1))

test2forplot <-
  test2 %>% 
  select(-gender)
test2forplot

test21 <- rbind(maxmin, test2forplot)
test21

radarchart(test21,
           pty = 32,
           axistype = 0,
           pcol = c(adjustcolor("darkorchid1", 0.5), adjustcolor("dodgerblue", 0.5)),
           pfcol = c(adjustcolor("darkorchid3", 0.5), adjustcolor("dodgerblue2", 0.5)),
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 0.75,
           palcex = 0.75)

legend("topleft", 
       c("Male", "Female"),
       fill = c(adjustcolor("dodgerblue2", 0.5), adjustcolor("darkorchid3", 0.5)))
title("What you think the opposite sex looks for",
      cex.main = 1.0,
      col = "black")








