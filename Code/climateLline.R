library(tidyverse)
library(paletteer)
library(scales)
library(glue)
library(showtext)

font.add.google('Lato','Lato')
showtext_auto()

url = "https://raw.githubusercontent.com/riffomonas/climate_viz/a4feec9e261d17c2d0fb2b00f5a774e5e3d0d11e/data/GLB.Ts%2BdSST.csv"

t_data <- read_csv(url, skip = 1, na = "***")

month.abb

t_diff = t_data %>% 
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() %>% 
  # have months in numerical order not alphabetical
  mutate(month= factor(month, levels = month.abb))


last_dec = t_diff %>% 
  filter(month =="Dec") %>% 
  mutate(year = year + 1, month= "Last_Dec")


next_jan = t_diff %>% 
  filter(month =="Jan") %>% 
  mutate(year = year + 1, month= "Next_Jan")



bind_rows(last_dec, t_diff, next_jan) %>% count(month)

bind_rows(last_dec, t_diff, next_jan) %>% 
  mutate(month= factor(month, levels = c('last_Dec', 
                                         month.abb, 'next_Jan')),
         month_number = as.numeric(month),
         this_year = year == 2022
         ) %>% 
  ggplot(
    aes(x= month_number, y= t_diff, 
        color = year,
        group= year# ! need group
      ) 
  )+
  geom_line()+
  scale_color_paletteer_c(`"ggthemes::Temperature Diverging"`)+
  scale_x_continuous(breaks = 1:12, 
                     labels =month.abb,
                     sec.axis = dup_axis(name = NULL)
                     )+
  scale_y_continuous(breaks = seq(-2, 2, 0.2), 
                     sec.axis = dup_axis(name = NULL, labels = NULL))+
  scale_size_manual(breaks = c(FALSE, TRUE), 
                    values = c(0.25, 1),
                    guide = 'none'
                    )+
  labs(title = "\nGlobal Temperature change since 1880",
      caption = "\nData: NASA | @StarTrek_Lt May 30, 2022\n",
       y="Temperature difference in \u00B0C",
       x="Month"
       )+
  ggdark::dark_mode()+
  theme(
    text = element_text(family = 'Lato'),
    panel.background = element_rect(fill = 'black', color = 'white'),
    axis.ticks = element_line(color = 'white'),
    axis.ticks.length = unit(-5, 'pt'),
    plot.title = element_text(size = 15, hjust = 0.5, face = 'bold'),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    plot.caption = element_text(hjust = 0, 
                                size = 12,
                                colour = 'grey45')
  )
