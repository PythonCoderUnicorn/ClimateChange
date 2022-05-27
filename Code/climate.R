
# Climate Change data visualization

library(tidyverse)
library(scales)
library(glue)
library(paletteer)

url = "https://raw.githubusercontent.com/riffomonas/climate_viz/a4feec9e261d17c2d0fb2b00f5a774e5e3d0d11e/data/GLB.Ts%2BdSST.csv"

t_data <- read_csv(url, skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na()

annotation <- t_data %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(t_diff = 0,
         x = year + c(-5, 5))

max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall=1)

t_data %>%
  ggplot(aes(x=year, y=t_diff, fill=t_diff)) +
  geom_col(show.legend=FALSE) +
  geom_text(data = annotation, aes(x=x, label=year), color="white") +
  geom_text(x=1880, y=1, hjust=0,
            label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
            color="white") +
  # scale_fill_gradient2(low="darkblue", mid="white", high="darkred",
  #                     midpoint = 0, limits= c(-0.5, 1.5)) +
  # scale_fill_gradientn(colors=c("darkblue", "white", "darkred"),
  #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
  #                      limits = c(min(t_data$t_diff), max(t_data$t_diff))) +
  scale_fill_stepsn(colors=c("darkblue", "white", "darkred"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks=9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="black"),
    legend.text = element_text(color="white")
  )



# stripe ------------------------------------------------------------------

library(showtext)

font_add_google('Poppins','Poppins')
showtext_auto()

t_data %>%
  ggplot(aes(x = year, y = 1, fill = t_diff)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_paletteer_c(`"ggthemes::Orange-Blue-White Diverging"`, direction = -1)+
  # scale_fill_stepsn(colors=c("#08306B", "white", "#67000D"),
  #                   values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
  #                   n.breaks = 12) +
  # coord_cartesian(expand=FALSE) +
  scale_x_continuous(breaks=seq(1890, 2020, 30)) +
  labs(title= glue("Global temperature change ({min(t_data$year)}-{max(t_data$year)})"),
       caption  = "source: NASA | @StarTrek_Lt |May 26, 2022\n"
       ) +
  theme_void() +
  theme(
    text = element_text(family = 'Poppins'),
    axis.text.x = element_text(color="white",
                               margin =margin(t=5, b=10, unit="pt")),
    plot.title = element_text(color="white",
                              margin =margin(b=5, t=10, unit="pt"),
                              hjust= 0.5),
    plot.caption = element_text(hjust = 0.1, color = 'grey50'),
    plot.background = element_rect(fill="black")
  )

