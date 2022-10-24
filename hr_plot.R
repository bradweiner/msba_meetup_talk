library(Lahman)
library(janitor)
library(tidyverse,quietly = T)
library(magrittr)
library(zoo)

b <- Lahman::Batting
head(b)

b %<>% clean_names(.)

b.agg <- b %>%
    group_by(year_id) %>%
    summarise(total_ab = sum(ab,na.rm = T),
              total_hr = sum(hr,na.rm = T)) %>%
    ungroup() %>%
    mutate(hr_to_ab_ratio = total_hr/total_ab) %>%
    as.data.frame(.)

head(b.agg)

p <- ggplot(b.agg, aes(x = year_id, y = hr_to_ab_ratio)) +
    geom_line(size=1.5,
              color = "red") +
    #geom_line(aes(y=rollmean(hr_to_ab_ratio, 10, na.pad=TRUE)),size = 1.5, lty = "dashed") +
    #geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    scale_y_continuous(limits = c(0,0.04),labels = NULL) +
    scale_x_continuous(labels = NULL) +
    labs(x = "",
         y = "",
         title = "What is Going on Here?") +
    theme_minimal()

p

p <- ggplot(b.agg, aes(x = year_id, y = hr_to_ab_ratio)) +
    geom_line(size=1.5,
              color = "red") +
    #geom_line(aes(y=rollmean(hr_to_ab_ratio, 10, na.pad=TRUE)),size = 1.5, lty = "dashed") +
    #geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    scale_y_continuous(limits = c(0,0.04),labels = NULL) +
    scale_x_continuous(limits = c(1870,2025),n.breaks = 20) +
    labs(x = "",
         y = "",
         title = "Something Happening Between 1870 - 2018") +
    theme_minimal()


p

p <- ggplot(b.agg, aes(x = year_id, y = hr_to_ab_ratio)) +
    geom_line(size=1.5,
              color = "red",alpha = 0.25) +
    geom_line(aes(y=rollmean(hr_to_ab_ratio, 10, na.pad=TRUE)),size = 1.5, col = "blue", lty = "dashed") +
    #geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
    scale_y_continuous(limits = c(0,0.04),labels = NULL) +
    scale_x_continuous(limits = c(1870,2025),n.breaks = 20) +
    labs(x = "",
         y = "",
         title = "Something Happening Between 1870 - 2018",
         subtitle = "Ten Year Moving Average") +
    theme_minimal()


p


p <- ggplot(b.agg, aes(x = year_id, y = hr_to_ab_ratio)) +
    geom_line(size=1.5,
              color = "red",alpha = 0.25) +
    geom_line(aes(y=rollmean(hr_to_ab_ratio, 10, na.pad=TRUE)),
              size = 1.5,
              col = "blue",
              lty = "dashed",alpha = 0.25) +
    geom_smooth(method=lm , color="green", fill="#69b3a2", se=TRUE) +
    scale_y_continuous(limits = c(0,0.04),labels = NULL) +
    scale_x_continuous(limits = c(1870,2025),n.breaks = 20) +
    labs(x = "",
         y = "",
         title = "Something Happening Between 1870 - 2018",
         subtitle = "Ten Year Moving Average + Linear Trend") +
    theme_minimal()


p


p <- ggplot(b.agg, aes(x = year_id, y = hr_to_ab_ratio)) +
    geom_line(size=1.5,
              color = "red",alpha = 1) +
    geom_line(aes(y=rollmean(hr_to_ab_ratio, 10, na.pad=TRUE)),
              size = 1.5,
              col = "blue",
              lty = "dashed",alpha = 1) +
    geom_smooth(method=lm , color="green", fill="#69b3a2", se=TRUE) +
    scale_y_continuous(limits = c(0,0.04)) +
    scale_x_continuous(limits = c(1870,2025),n.breaks = 20) +
    labs(x = "",
         y = "",
         title = "Something Happening Between 1870 - 2018",
         subtitle = "Ten Year Moving Average + Linear Trend") +
    theme_minimal()


p


m <- 1:36
t <- c(sample(seq(-.015,.015,.001),size = 12,replace = T),
       sample(seq(-.02,-.01,.001),size = 12,replace = T),
       sample(seq(-.20,-.17,.001),size = 12,replace = T),
       sample(seq(.10,.45,.001),size = 12,replace = T))

t

write.csv(t,row.names = F)

df <- data.frame(
             month = c(1L,2L,3L,4L,5L,6L,7L,8L,
                       9L,10L,11L,12L,13L,14L,15L,16L,17L,18L,19L,
                       20L,21L,22L,23L,24L,25L,26L,27L,28L,29L,30L,31L,
                       32L,33L,34L,35L,36L),
         open_rate = c(0.03,0.02961,0.02931,0.02958,
                       0.02937,0.02978,0.02936,0.02928,0.02934,0.02945,
                       0.02978,0.02963,0.02948,0.02918,0.0286,0.0282,
                       0.02789,0.02733,0.02698,0.02652,0.02615,0.0257,0.02519,
                       0.02484,0.02451,0.02003,0.01624,0.01338,0.01084,
                       0.00891,0.00716,0.0058,0.00478,0.00384,0.00316,0.00255),
        pct_change = c(-0.013,-0.01,0.009,-0.007,
                       0.014,-0.014,-0.003,0.002,0.004,0.011,-0.005,-0.005,
                       -0.01,-0.02,-0.014,-0.011,-0.02,-0.013,-0.017,
                       -0.014,-0.017,-0.02,-0.014,-0.013,-0.183,-0.189,
                       -0.176,-0.19,-0.178,-0.196,-0.191,-0.175,-0.196,-0.179,
                       -0.192,-0.174)
      )



p <- ggplot(df,aes(x = month, y = open_rate)) + geom_line()
p    
    
head(df)

df <- data.frame(
       month = c(1L,2L,3L,4L,5L,6L,7L,8L,9L,10L,
                 11L,12L,13L,14L,15L,16L,17L,18L,19L,20L,21L,22L,23L,
                 24L,25L,26L,27L,28L,29L,30L,31L,32L,33L,34L,35L,36L),
   open_rate = c(0.03,0.02961,0.02931,0.02958,0.02937,
                 0.02978,0.02936,0.02928,0.02934,0.02945,0.02978,0.02963,
                 0.02948,0.02918,0.0286,0.0282,0.02789,0.02733,0.02698,
                 0.02652,0.02615,0.0257,0.02519,0.02484,0.02451,0.02003,
                 0.01624,0.01338,0.01084,0.00891,0.00716,0.0058,0.00478,
                 0.00384,0.00316,0.00255),
  pct_change = c(-0.013,-0.01,0.009,-0.007,0.014,-0.014,
                 -0.003,0.002,0.004,0.011,-0.005,-0.005,-0.01,-0.02,
                 -0.014,-0.011,-0.02,-0.013,-0.017,-0.014,-0.017,-0.02,
                 -0.014,-0.013,-0.183,-0.189,-0.176,-0.19,-0.178,-0.196,
                 -0.191,-0.175,-0.196,-0.179,-0.192,-0.174),
  click_rate = c(0.01,0.01161,0.01338633,0.014979303,
                 0.017316075,0.020779289,0.024623458,0.02969589,0.032695175,
                 0.037697537,0.04606639,0.051548291,0.05701241,0.0633978,
                 0.07518979,0.090603697,0.104013045,0.11992704,0.133358869,
                 0.158430336,0.184096051,0.221099357,0.267972421,0.305220587,
                 0.380915293,0.470049471,0.529745754,0.601261431,0.679425417,
                 0.846564069,0.85502971,0.863580007,0.889487407,0.916172029,
                 0.92533375,0.92533375),
  pct_change = c(0.161,0.153,0.119,0.156,0.2,0.185,
                 0.206,0.101,0.153,0.222,0.119,0.106,0.112,0.186,0.205,
                 0.148,0.153,0.112,0.188,0.162,0.201,0.212,0.139,0.248,
                 0.234,0.127,0.135,0.13,0.246,0.01,0.01,0.03,0.03,0.01,0,
                 0.01)

)
 
write.csv(sample(seq(-.01,.01,.001),size = 36,replace = T),row.names = F)

df <- tribble(
    ~domain, ~click_rate,
    "gmail",   .99,
    "microsoft",   .78,
    "other",   .25
    
)


df <- tribble(
    ~domain, ~link_click_rate,
    "gmail",   .99,
    "microsoft",   .78,
    "other",   .25
    
)

bc <- ggplot(df, aes(x = domain, y = link_click_rate)) +
    geom_bar(stat = "identity")

bc
