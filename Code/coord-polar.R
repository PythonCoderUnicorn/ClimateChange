

library(tidyverse)
library(paletteer)
library(scales)
library(glue)
library(showtext)

font.add.google('Lato','Lato')
showtext_auto()

url = "https://raw.githubusercontent.com/riffomonas/climate_viz/a4feec9e261d17c2d0fb2b00f5a774e5e3d0d11e/data/GLB.Ts%2BdSST.csv"

t_data <- read_csv(url, skip = 1, na = "***")



t_diff = t_data %>% 
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() #%>% 
  # have months in numerical order not alphabetical
  # mutate(month= factor(month, levels = month.abb))


# last_dec = t_diff %>%
#   filter(month =="Dec") %>%
#   mutate(year = year + 1, month= "Last_Dec")
# 
# # 
next_jan = t_diff %>%
  filter(month =="Jan") %>%
  mutate(year = year - 1, month= "Next_Jan")



t_data = bind_rows(t_diff, next_jan) %>% 
  mutate(month= factor(month, levels = c(month.abb, 'next_Jan')),
         month_number = as.numeric(month)
         )

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)
         


# month labels

# temp lines
temp_lines = tibble(
  x = 12,
  y = c(1.5, 2.0),
  labels = c('1.5\u00B0C','2\u00B0C')
)













t_data %>% 
  ggplot(
    aes(x= month_number, y= t_diff, 
        color = year,
        group= year# ! need group
    ) 
  )+
  geom_hline(yintercept = c(1.5, 2), color= 'red')+
  geom_line()+
  geom_point(data = annotation,
             aes(y= t_diff, color= year))+
  geom_text(aes(x= 1, y= 2.4, label= "2022", family='Lato'), size= 5)+
  geom_label(data = temp_lines, aes(x=x, y=y, label=labels),
             color= 'orange',
             fill='black',
             label.size =0,
             inherit.aes = F)+
  coord_polar()+
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb ,
                     sec.axis = dup_axis(name=NULL, labels = NULL)
                     )+
  scale_y_continuous(breaks = seq(-2, 2, 0.2) ,
                     limits = c(-1 ,2.5), # -2, 2.4
                     sec.axis = dup_axis(name=NULL, labels = NULL)
                     )+
  scale_color_paletteer_c(`"ggthemes::Red-Blue-White Diverging"`,
                          direction = -1,
                          breaks = seq(1880,2020, 20))+
  labs(title = "\nGlobal Temperature change since 1880",
       caption = "\nData: NASA | @StarTrek_Lt  - June 2, 2022\n",
       # y="Temperature difference in \u00B0C",
       y=" ",
       x="Month"
  )+
  ggdark::dark_mode()+
  theme(
    text = element_text(family = 'Lato'),
    panel.background = element_rect(fill = 'black', color = 'white'),
    plot.title = element_text(size = 15, hjust = 0.5, face = 'bold'),
    axis.text.x = element_text(color = 'white',
                               size = 13),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # axis.title.y = element_text(size = 13),
    plot.caption = element_text(hjust = 0, 
                                size = 12,
                                colour = 'grey45')
  )


ggsave("~/desktop/temperature_lines.png", width=8, height=8)
























