##############################################################################
# Generates a line plot grid: Consumer Confidence Around the World
#
##############################################################################


#install.packages('tidyverse')
#install.packages('janitor')
#install.packages('showtext')
#install.packages('MetBrewer')
#install.packages('scico')
#install.packages('ggtext')
#install.packages('patchwork')
##install.packages('gghighlight')
library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)


df1 <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/dataConsumerConfidence.csv") %>% 
  mutate(date=lubridate::my(Time)) %>%  # setting the date column in a good format
  select(-Time) %>% # deselect Time, we don't need it
  pivot_longer(!date, names_to = "country", values_to = "value") %>%  # change the data format
  na.omit()


#### MISC ####
font <- "Gudea"
font_add_google(family=font, font, db_cache = TRUE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
txt_col <- "black"
showtext_auto(enable = TRUE)

caption_text  <- str_glue("**Design:** Gilbert Fontana<br>","**Data:** OECD, 2022")

p1 <- df1 %>% 
  ggplot() +
  geom_hline(yintercept = 100,linetype="solid", size=.25) +
  geom_point(data=df1 %>% 
               group_by(country) %>% 
               slice_max(date),
             aes(x=date, y=value, color=country),shape=16) +
  geom_line(aes(x=date, y=value, color=country)) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  geom_text(data=df1 %>% 
              group_by(country) %>% 
              slice_max(date),
            aes(x=date, y=value, color=country, label = round(value)),
            hjust = -.5, vjust = .5, size=2.5, family=font, fontface="bold") +
  scale_color_met_d(name="Redon") +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(breaks = c(90,95,100,105,110),
                     labels = c("","","100","","")
  ) +
  #facet_wrap(~ country) +
  facet_wrap(~  factor(country, levels=c('USA','China','Japan','Germany', 'UK','France', 'Italy', 'South Korea', 'Australia'))) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=7),
    strip.text.x = element_text(face="bold"),
    plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )

p1



text <- tibble(
  x = 0, y = 0,
  label = "The consumer confidence indicator provides an indication of future developments of households’ consumption and saving. An indicator above 100 signals a boost in the consumers’ confidence towards the future economic situation. Values below 100 indicate a pessimistic attitude towards future developments in the economy, possibly resulting in a tendency to save more and consume less. During 2022, the consumer confidence indicators have declined in many major economies around the world.<br>"
)

sub <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(10, "lines"),
    family=font, size = 3, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))


# TITLE
text2 <- tibble(
  x = 0, y = 0,
  label = "**Consumer Confidence Around the World**<br>"
)

title <- ggplot(text2, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(12, "lines"),
    family=font, size = 10, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

finalPlot <- (title+sub)/p1 +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    caption = caption_text,
    theme=theme(plot.caption = element_markdown(hjust=0, margin=margin(20,0,0,0), size=6, color=txt_col, lineheight = 1.2),
                plot.margin = margin(20,20,20,20),))


print(finalPlot)
