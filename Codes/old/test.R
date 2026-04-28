library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# ----------------------------
# 1. Simulate monthly SPI
# ----------------------------
months <- seq.Date(as.Date("2025-01-01"), as.Date("2026-12-01"), by = "month")

# Controlled SPI values: first negative month alone, second event with 2 consecutive negative months
SPI <- c(0.2, -0.5, 0.3, 0.1, -0.3, -0.7, -0.4, 0.2, 0.5, 0.1, -0.2, 0.3,
         0.4, -0.6, -0.8, 0.1, 0.5, 0.2, -0.3, -0.2, 0.1, 0.3, 0.2, -0.1)

df <- data.frame(Month = months, SPI = SPI)

# ----------------------------
# 2. Track cumulative drought
# ----------------------------
df$Drought <- FALSE
df$Cumulative <- 0
df$EventStart <- NA
df$EventEnd <- NA

cum_spi <- 0
drought_active <- FALSE
dry_months <- 0
start_month <- NA

for(i in 1:nrow(df)) {
  val <- df$SPI[i]
  
  # Count consecutive negative months
  if(val < 0) {
    dry_months <- dry_months + 1
  } else {
    dry_months <- 0
  }
  
  # Start drought if 2 consecutive negative months
  if(!drought_active & dry_months >= 2){
    drought_active <- TRUE
    cum_spi <- val
    df$Drought[i] <- TRUE
    start_month <- df$Month[i-1]
    df$EventStart[i] <- start_month
  } else if(drought_active){
    cum_spi <- cum_spi + val
    df$Drought[i] <- TRUE
    # End drought when cumulative deficit offset
    if(cum_spi >= 0){
      drought_active <- FALSE
      df$EventEnd[i] <- df$Month[i]
      cum_spi <- 0
      start_month <- NA
    }
  }
  
  df$Cumulative[i] <- cum_spi
}

# ----------------------------
# 3. Define bar colors
# ----------------------------
df$SPI_Type <- ifelse(df$SPI >= 0, "Positive SPI", "Negative SPI")

# ----------------------------
# 4. Prepare vertical lines and labels
# ----------------------------
vline_df <- df %>%
  select(EventStart, EventEnd) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Month") %>%
  filter(!is.na(Month)) %>%
  mutate(Label = ifelse(Type == "EventStart", "Start of Drought", "End of Drought"),
         LineColor = ifelse(Type == "EventStart", "red", "green"))

# ----------------------------
# 5. Prepare shaded drought periods
# ----------------------------
drought_shade <- df %>%
  filter(Drought) %>%
  group_by(group = cumsum(c(0, diff(as.numeric(Drought))) != 0)) %>%
  summarize(start = min(Month), end = max(Month), .groups = 'drop')

# ----------------------------
# 6. Set label positions
# ----------------------------
label_y_start <- max(df$SPI) + 0.3
label_y_end   <- max(df$SPI) + 0.3

# ----------------------------
# 7. Plot
# ----------------------------
ggplot(df, aes(x = Month, y = SPI, fill = SPI_Type)) +
  # Bars for SPI
  geom_col(color = "black") +
  scale_fill_manual(values = c("Positive SPI" = "#2c7fb8", "Negative SPI" = "#ff9331")) +
  
  # Horizontal line at SPI = 0
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Shaded drought period
  geom_rect(data = drought_shade, aes(xmin = start - 15, xmax = end + 15, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.1, fill = "orange") +
  
  # Vertical lines for start/end
  geom_vline(data = vline_df, aes(xintercept = Month, color = LineColor),
             linetype = "dotted", size = 1, inherit.aes = FALSE) +
  scale_color_identity() +
  
  # Labels for start (red) and end (green)
  geom_text(data = vline_df %>% filter(LineColor == "darkred"),
            aes(x = Month, y = label_y_start, label = Label),
            angle = 90, vjust = -0.5, hjust = 0, color = "darkred", size = 3.5, inherit.aes = FALSE) +
  
  geom_text(data = vline_df %>% filter(LineColor == "darkgreen"),
            aes(x = Month, y = label_y_end, label = Label),
            angle = 90, vjust = -0.5, hjust = 0, color = "darkgreen", size = 3.5, inherit.aes = FALSE) +
  
  labs(title = "",
       y = "SPI", fill = "SPI Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "none")


library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# ----------------------------
# 1. Simulate monthly SPI
# ----------------------------
months <- seq.Date(as.Date("2025-01-01"), as.Date("2026-12-01"), by = "month")

# SPI values to show example: first negative month alone, second event has 2 consecutive months < -0.5
SPI <- c(0.2, -0.4, 0.3, 0.1, -0.6, -0.7, -0.4, 0.2, 0.5, 0.1, -0.2, 0.3,
         0.4, -0.6, -0.8, -0.4, 0.5, 0.2, -0.3, -0.2, 0.1, 0.3, 0.2, -0.1)

df <- data.frame(Month = months, SPI = SPI)

# ----------------------------
# 2. Track drought events based on new logic
# ----------------------------
df$Drought <- FALSE
df$EventStart <- NA
df$EventEnd <- NA

drought_active <- FALSE
dry_months <- 0
start_month <- NA

threshold <- -0.5  # drought threshold

for(i in 1:nrow(df)) {
  val <- df$SPI[i]
  
  # Count consecutive months below threshold
  if(val < threshold){
    dry_months <- dry_months + 1
  } else {
    dry_months <- 0
  }
  
  # Start drought if 2 consecutive months below threshold
  if(!drought_active & dry_months >= 2){
    drought_active <- TRUE
    start_month <- df$Month[i-1]
    df$EventStart[i] <- start_month
    df$Drought[i] <- TRUE
  } else if(drought_active){
    df$Drought[i] <- TRUE
    # End drought if SPI >= threshold (even if negative)
    if(val >= threshold){
      drought_active <- FALSE
      df$EventEnd[i] <- df$Month[i]
      start_month <- NA
    }
  }
}

# ----------------------------
# 3. Define SPI bar colors
# ----------------------------
df$SPI_Type <- ifelse(df$SPI >= 0, "Positive SPI", "Negative SPI")

# ----------------------------
# 4. Prepare vertical lines for start/end
# ----------------------------
vline_df <- df %>%
  select(EventStart, EventEnd) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Month") %>%
  filter(!is.na(Month)) %>%
  mutate(Label = ifelse(Type == "EventStart", "Start of Drought", "End of Drought"),
         LineColor = ifelse(Type == "EventStart", "red", "green"))

# ----------------------------
# 5. Identify shaded drought periods
# ----------------------------
drought_shade <- df %>%
  filter(Drought) %>%
  group_by(group = cumsum(c(0, diff(as.numeric(Drought))) != 0)) %>%
  summarize(start = min(Month), end = max(Month), .groups = 'drop') %>%
  mutate(EventID = paste0("D", row_number()))

# ----------------------------
# 6. Label positions
# ----------------------------
label_y_start <- max(df$SPI) + 0.3
label_y_end   <- max(df$SPI) + 0.3

# ----------------------------
# 7. Plot
# ----------------------------
ggplot(df, aes(x = Month, y = SPI, fill = SPI_Type)) +
  # Bars
  geom_col(color = "black") +
  scale_fill_manual(values = c("Positive SPI" = "#2c7fb8", "Negative SPI" = "#ff9331")) +
  
  # Horizontal line
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Shaded drought periods
  geom_rect(data = drought_shade, aes(xmin = start - 15, xmax = end + 15, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.1, fill = "orange") +
  
  # Event ID labels
  geom_text(data = drought_shade, aes(x = start + (end-start)/2, y = max(df$SPI)+0.2, label = EventID),
            inherit.aes = FALSE, size = 4, fontface = "bold") +
  
  # Start/end vertical lines
  geom_vline(data = vline_df, aes(xintercept = Month, color = LineColor),
             linetype = "dotted", size = 1, inherit.aes = FALSE) +
  scale_color_identity() +
  
  # Labels for vertical lines
  geom_text(data = vline_df %>% filter(LineColor == "darkred"),
            aes(x = Month, y = label_y_start, label = Label),
            angle = 90, vjust = -0.5, hjust = 0, color = "darkred", size = 3.5, inherit.aes = FALSE) +
  geom_text(data = vline_df %>% filter(LineColor == "darkgreen"),
            aes(x = Month, y = label_y_end, label = Label),
            angle = 90, vjust = -0.5, hjust = 0, color = "darkgreen", size = 3.5, inherit.aes = FALSE) +
  
  labs(title = "Monthly SPI with Drought Events (Threshold -0.5)",
       y = "SPI", fill = "SPI Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

