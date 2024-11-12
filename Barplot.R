###################################################
# Bar plot  per paper JSS
#
####################################################
rm(list=ls())

library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(bquote)
library(ggtext)
library(patchwork)
library(gghighlight)


###############################################################
# Frequency table  Scenario A
# p=100
###############################################################



# Define each dataset (replace these with actual values)
data_1 <- data.frame(
  Set = c("{x1, x2}", "{x2, x3}", "{x1, x2, x3}", "{x1, x2, Noisy}", "{x1, x2, x3, Noisy}"),
  ω = c(86, 2, 8, 0, 4),
  ϕ = c(76, 0, 24, 0, 0)
)

data_2 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, Noisy}", "{x1, x2, x3, Noisy}"),
  ω = c(0, 100, 0, 0),
  ϕ = c(2, 98, 0, 0)
)

data_3 <- data.frame(
  Set = c("{x1, x2}","{x2, x3}" ,"{x1, x2, x3}", "{x1, x2, Noisy}", "{x1, x2, x3, Noisy}"),
  ω = c(92, 0,  8, 0, 0),
  ϕ = c(70,0,  28, 0, 2)
)

data_4 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, Noisy}", "{x1, x2, x3, Noisy}"),
  ω = c(0, 100, 0, 0),
  ϕ = c(0, 100, 0, 0)
)

create_bar_plot <- function(data, title) {
  data_melted <- melt(data, id.vars = "Set", variable.name = "Measure", value.name = "Frequency")
  
  ggplot(data_melted, aes(x = Set, y = Frequency, fill = Measure)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = Frequency), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
    labs(
      title = title,
      x = "",
      y = "Frequency (%)"
    ) +
    ylim(0, 105) +  # Set y-axis limits
    scale_fill_manual(name = "Measure", values = c("ω" = "#377eb8", "ϕ" = "#e41a1c")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create each bar plot with specified titles
plot1 <- create_bar_plot(data_1, "First Survival")
plot2 <- create_bar_plot(data_2, "Second Survival")
plot3 <- create_bar_plot(data_3, "First Survival")
plot4 <- create_bar_plot(data_4, "Second Survival")

# Combine the plots into top and bottom rows
top_row <- plot_grid(plot1 + theme(legend.position = "none"), plot2 + theme(legend.position = "none"), ncol = 2)
bottom_row <- plot_grid(plot3 + theme(legend.position = "none"), plot4 + theme(legend.position = "none"), ncol = 2)

# Add centered titles for each row
top_row_title <- ggdraw() + 
  draw_label("n = 800; m = 400; p = 100", fontface = "bold", size = 14, hjust = 0.5)

bottom_row_title <- ggdraw() + 
  draw_label("n = 1000; m = 500; p = 100", fontface = "bold", size = 14, hjust = 0.5)

# Combine titles with rows
top_row_with_title <- plot_grid(top_row_title, top_row, ncol = 1, rel_heights = c(0.1, 1))
bottom_row_with_title <- plot_grid(bottom_row_title, bottom_row, ncol = 1, rel_heights = c(0.1, 1))

# Combine the top and bottom rows
combined_plot <- plot_grid(top_row_with_title, bottom_row_with_title, ncol = 1)

# Extract the shared legend from one plot
#legend <- get_legend(plot1 + theme(legend.position = "bottom"))

# Combine everything with the shared legend
final_bar_plot_1 <- plot_grid(combined_plot,  ncol = 1, rel_heights = c(10, 1))

# Display the final plot
print(final_bar_plot_1)


###############################################################
# Frequency table  Scenario A
# p=200
###############################################################



# Dataset for n=800; m=400; p=200 (First Survival)
data_1 <- data.frame(
  Set = c("{x1, x2}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(74, 14, 4, 8),
  ϕ = c(62, 30, 0, 8)
)

# Dataset for n=800; m=400; p=200 (Second Survival)
data_2 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(4, 96, 0, 0),
  ϕ = c(8, 92, 0, 0)
)

# Dataset for n=1000; m=500; p=200 (First Survival)
data_3 <- data.frame(
  Set = c("{x1, x2}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(88, 6, 0, 6),
  ϕ = c(70, 28, 0, 2)
)

# Dataset for n=1000; m=500; p=200 (Second Survival)
data_4 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(4, 94, 0, 2),
  ϕ = c(6, 94, 0, 0)
)

create_bar_plot <- function(data, title) {
  data_melted <- melt(data, id.vars = "Set", variable.name = "Measure", value.name = "Frequency")
  
  ggplot(data_melted, aes(x = Set, y = Frequency, fill = Measure)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = Frequency), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
    labs(
      title = title,
      x = "",
      y = "Frequency (%)"
    ) +
    ylim(0, 105) +  # Set y-axis limits
    scale_fill_manual(name = "Measure", values = c("ω" = "#377eb8", "ϕ" = "#e41a1c")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create each bar plot with specified titles
plot1 <- create_bar_plot(data_1, "First Survival")
plot2 <- create_bar_plot(data_2, "Second Survival")
plot3 <- create_bar_plot(data_3, "First Survival")
plot4 <- create_bar_plot(data_4, "Second Survival")

# Combine the plots into top and bottom rows
top_row <- plot_grid(plot1 + theme(legend.position = "none"), plot2 + theme(legend.position = "none"), ncol = 2)
bottom_row <- plot_grid(plot3 + theme(legend.position = "none"), plot4 + theme(legend.position = "none"), ncol = 2)

# Add centered titles for each row
top_row_title <- ggdraw() + 
  draw_label(" n = 800; m = 400; p = 200", fontface = "bold", size = 14, hjust = 0.5)

bottom_row_title <- ggdraw() + 
  draw_label("n = 1000; m = 500; p = 200", fontface = "bold", size = 14, hjust = 0.5)

# Combine titles with rows
top_row_with_title <- plot_grid(top_row_title, top_row, ncol = 1, rel_heights = c(0.1, 1))
bottom_row_with_title <- plot_grid(bottom_row_title, bottom_row, ncol = 1, rel_heights = c(0.1, 1))

# Combine the top and bottom rows
combined_plot <- plot_grid(top_row_with_title, bottom_row_with_title, ncol = 1)

# Extract the shared legend from one plot
#legend <- get_legend(plot1 + theme(legend.position = "bottom"))

# Combine everything with the shared legend
final_bar_plot_2 <- plot_grid(combined_plot,  ncol = 1, rel_heights = c(10, 1))

# Display the final plot
print(final_bar_plot_2)



text <- tibble(
  x = 0, y = 0,
  label = "Scenario A: *\u03B7*<sub>3i</sub> = *\u03B2*<sub>30</sub>"
)

Title <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = "white", fill = "white",
    width = unit(10, "lines"),
    size = 5, lineheight = 1.2, family = "sans",
    halign = 0.5, valign = 0.5,
    rich = TRUE
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(t = 0, b = 0, l = 0, r = 0)
  )

Title


legend_data <- data.frame(
  Measure = factor(c("ω", "ϕ"), levels = c("ω", "ϕ")),
  x = 1:2, 
  y = 1:2
)

custom_legend <- ggplot(legend_data, aes(x = x, y = y, fill = Measure)) +
  geom_tile(width = 0.5, height = 0.5, color = "black") +
  scale_fill_manual(
    values = c("ω" = "#377eb8", "ϕ" = "#e41a1c"),
    labels = c(expression(omega), expression(phi))
  ) +
  guides(fill = guide_legend(title = "Measure")) + # Legenda personalizzata
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, family = "serif"),  # Font serif per il titolo
    legend.text = element_text(size = 10, family = "serif"),   # Font serif per le etichette
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Estrai la legenda come oggetto separato
legend_grob <- get_legend(custom_legend)


p1 <- (final_bar_plot_1 + final_bar_plot_2)

(finalPlot_12 <- (Title + legend_grob) / p1 +
    plot_layout(heights = c(1, 8)) +
    plot_annotation(
      theme = theme(
        plot.caption = element_markdown(
          hjust = 0, margin = margin(0, 0, 0, 0), size = 6,
          color = txt_col, lineheight = 0.5
        ),
        plot.margin = margin(10, 10, 10, 10) # General plot margin
      )
    )
)





##################################################
# Scenario B 
# p=100
##################################################




# Dataset for n=800; m=400; p=100 (First Survival)
data_1 <- data.frame(
  Set = c("{x1, x2}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(94, 6, 0, 0),
  ϕ = c(74, 26, 0, 0)
)

# Dataset for n=800; m=400; p=100 (Second Survival)
data_2 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(4, 94, 0, 2),
  ϕ = c(2, 94, 0, 4)
)

# Dataset for n=1000; m=500; p=100 (First Survival)
data_3 <- data.frame(
  Set = c("{x1, x2}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(98, 2, 0, 0),
  ϕ = c(78, 22, 0, 0)
)

# Dataset for n=1000; m=500; p=100 (Second Survival)
data_4 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(0, 98, 0, 2),
  ϕ = c(2, 98, 0, 0)
)

create_bar_plot <- function(data, title) {
  data_melted <- melt(data, id.vars = "Set", variable.name = "Measure", value.name = "Frequency")
  
  ggplot(data_melted, aes(x = Set, y = Frequency, fill = Measure)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = Frequency), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
    labs(
      title = title,
      x = "",
      y = "Frequency (%)"
    ) +
    ylim(0, 105) +  # Set y-axis limits
    scale_fill_manual(name = "Measure", values = c("ω" = "#377eb8", "ϕ" = "#e41a1c")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create each bar plot with specified titles
plot1 <- create_bar_plot(data_1, "First Survival")
plot2 <- create_bar_plot(data_2, "Second Survival")
plot3 <- create_bar_plot(data_3, "First Survival")
plot4 <- create_bar_plot(data_4, "Second Survival")

# Combine the plots into top and bottom rows
top_row <- plot_grid(plot1 + theme(legend.position = "none"), plot2 + theme(legend.position = "none"), ncol = 2)
bottom_row <- plot_grid(plot3 + theme(legend.position = "none"), plot4 + theme(legend.position = "none"), ncol = 2)

# Add centered titles for each row
top_row_title <- ggdraw() + 
  draw_label(" n = 800; m = 400; p = 100", fontface = "bold", size = 14, hjust = 0.5)

bottom_row_title <- ggdraw() + 
  draw_label(" n = 1000; m = 500; p = 100", fontface = "bold", size = 14, hjust = 0.5)

# Combine titles with rows
top_row_with_title <- plot_grid(top_row_title, top_row, ncol = 1, rel_heights = c(0.1, 1))
bottom_row_with_title <- plot_grid(bottom_row_title, bottom_row, ncol = 1, rel_heights = c(0.1, 1))

# Combine the top and bottom rows
combined_plot <- plot_grid(top_row_with_title, bottom_row_with_title, ncol = 1)

# Extract the shared legend from one plot
#legend <- get_legend(plot1 + theme(legend.position = "bottom"))

# Combine everything with the shared legend
final_bar_plot_3 <- plot_grid(combined_plot,  ncol = 1, rel_heights = c(10, 1))

# Display the final plot
print(final_bar_plot_3)

##################################################
# Scenario B 
# p=200
##################################################


# Dataset for n=800; m=400; p=200 (First Survival)
data_1 <- data.frame(
  Set = c("{x1, x2}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(94, 6, 0, 0),
  ϕ = c(84, 16, 0, 0)
)

# Dataset for n=800; m=400; p=200 (Second Survival)
data_2 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(0, 96, 0, 4),
  ϕ = c(2, 96, 0, 2)
)

# Dataset for n=1000; m=500; p=200 (First Survival)
data_3 <- data.frame(
  Set = c("{x1, x2}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(92, 6, 2, 0),
  ϕ = c(66, 30, 0, 4)
)

# Dataset for n=1000; m=500; p=200 (Second Survival)
data_4 <- data.frame(
  Set = c("{x1, x3}", "{x1, x2, x3}", "{x1, x2, xj}", "{x1, x2, x3, xj}"),
  ω = c(8, 90, 0, 2),
  ϕ = c(6, 92, 0, 2)
)

create_bar_plot <- function(data, title) {
  data_melted <- melt(data, id.vars = "Set", variable.name = "Measure", value.name = "Frequency")
  
  ggplot(data_melted, aes(x = Set, y = Frequency, fill = Measure)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = Frequency), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
    labs(
      title = title,
      x = "",
      y = "Frequency (%)"
    ) +
    ylim(0, 105) +  # Set y-axis limits
    scale_fill_manual(name = "Measure", values = c("ω" = "#377eb8", "ϕ" = "#e41a1c")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create each bar plot with specified titles
plot1 <- create_bar_plot(data_1, "First Survival")
plot2 <- create_bar_plot(data_2, "Second Survival")
plot3 <- create_bar_plot(data_3, "First Survival")
plot4 <- create_bar_plot(data_4, "Second Survival")

# Combine the plots into top and bottom rows
top_row <- plot_grid(plot1 + theme(legend.position = "none"), plot2 + theme(legend.position = "none"), ncol = 2)
bottom_row <- plot_grid(plot3 + theme(legend.position = "none"), plot4 + theme(legend.position = "none"), ncol = 2)

# Add centered titles for each row
top_row_title <- ggdraw() + 
  draw_label("n = 800; m = 400; p = 200", fontface = "bold", size = 14, hjust = 0.5)

bottom_row_title <- ggdraw() + 
  draw_label("n = 1000; m = 500; p = 200", fontface = "bold", size = 14, hjust = 0.5)

# Combine titles with rows
top_row_with_title <- plot_grid(top_row_title, top_row, ncol = 1, rel_heights = c(0.1, 1))
bottom_row_with_title <- plot_grid(bottom_row_title, bottom_row, ncol = 1, rel_heights = c(0.1, 1))

# Combine the top and bottom rows
combined_plot <- plot_grid(top_row_with_title, bottom_row_with_title, ncol = 1)

# Extract the shared legend from one plot
#legend <- get_legend(plot1 + theme(legend.position = "bottom"))

# Combine everything with the shared legend
final_bar_plot_4 <- plot_grid(combined_plot,  ncol = 1, rel_heights = c(10, 1))

# Display the final plot
print(final_bar_plot_4)




text <- tibble(
  x = 0, y = 0,
  label = "Scenario B: *\u03B7*<sub>3i</sub> = *\u03B2*<sub>31</sub>*x*<sub>1i</sub> + *\u03B2*<sub>32</sub>*x*<sub>2i</sub> + *\u03B2*<sub>33</sub>*x*<sub>3i</sub>"
)

Title <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = "white", fill = "white",
    width = unit(20, "lines"),
    size = 5, lineheight = 1.2, family = "sans",
    halign = 0.5, valign = 0.5,
    rich = TRUE
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(t = 0, b = 0, l = 0, r = 0)
  )

Title


legend_data <- data.frame(
  Measure = factor(c("ω", "ϕ"), levels = c("ω", "ϕ")),
  x = 1:2, 
  y = 1:2
)

custom_legend <- ggplot(legend_data, aes(x = x, y = y, fill = Measure)) +
  geom_tile(width = 0.5, height = 0.5, color = "black") +
  scale_fill_manual(
    values = c("ω" = "#377eb8", "ϕ" = "#e41a1c"),
    labels = c(expression(omega), expression(phi))
  ) +
  guides(fill = guide_legend(title = "Measure")) + # Legenda personalizzata
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, family = "serif"),  # Font serif per il titolo
    legend.text = element_text(size = 10, family = "serif"),   # Font serif per le etichette
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Estrai la legenda come oggetto separato
legend_grob <- get_legend(custom_legend)


p2 <- (final_bar_plot_3 + final_bar_plot_4)

(finalPlot_23 <- (Title + legend_grob) / p2 +
    plot_layout(heights = c(1, 8)) +
    plot_annotation(
      theme = theme(
        plot.caption = element_markdown(
          hjust = 0, margin = margin(0, 0, 0, 0), size = 6,
          color = txt_col, lineheight = 0.5
        ),
        plot.margin = margin(10, 10, 10, 10) # General plot margin
      )
    )
)



jpeg(filename = "finalPlot_12_highres.jpeg", 
     width = 3500, height = 2500, 
     units = "px", res = 300, quality = 100)
print(finalPlot_12) # Stampa il grafico
dev.off() # Chiude il dispositivo grafico

# Specifica il percorso e il nome del file per il secondo plot con risoluzione più alta
jpeg(filename = "finalPlot_23_highres.jpeg", 
     width = 3500, height = 2500, 
     units = "px", res = 300, quality = 100)
print(finalPlot_23) # Stampa il grafico
dev.off() # Chiude il dispositivo grafico

