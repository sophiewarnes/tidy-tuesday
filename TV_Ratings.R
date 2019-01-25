library(tidyverse)
library(extrafont)
loadfonts()

# Import data!
Ratings <- read.csv(file="https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# Create new theme!
soph_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Verdana", size = 12),
    panel.grid.major = element_line(size = 0.2, colour = "darkorchid4"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(size = 0.2, colour = "darkorchid4"),
    legend.position = "none",
    plot.background = element_rect(fill = "lavender"),
    plot.caption = element_text(face = "italic", size = 10, colour = "#333333"),
    plot.title = element_text(face = "bold", size = 16))

# Get year from $date, add as new column $year
Ratings$year <- as.numeric(substr(Ratings$date, 1, 4))

# Create data frame of all ratings by year
year_averages <- Ratings %>%
  group_by(year) %>%
  summarise(mean = mean(av_rating))

# General chart of ratings over time
ggplot(year_averages, aes(x = year, y = mean)) +
  geom_line(size=1, col = "darkorchid4") +
  scale_y_continuous(expand = c(0,0), limits = c(6,9)) +
  scale_x_continuous(expand = c(0,0), limits = c(1990,2018), breaks = seq(1990,2020, by =10)) +
  soph_theme +
  annotate("text", x = 2014, y = 8.45, label = "2014 was the \n best year...", family = "Verdana", size = 3, colour = "gray50") +
  annotate("text", x = 1999.75, y = 7.45, label = "...while 2000 \n was the worst!", family = "Verdana", size = 3, colour = "gray50") + 
  labs(title="The golden age of TV?",
       subtitle="Average IMDB ratings from 1990 to 2018",
       x="Year of TV show",
       y="Mean rating",
       caption="@SophieWarnes | Source: IMDB")

# Pick out series by Joss Whedon, regex ^ indicates start of string and $ indicates end

whedon <- Ratings %>%
  filter(grepl("^Angel$|Buffy the Vampire Slayer|Dollhouse|Firefly",Ratings$title))%>%
  group_by(title, seasonNumber, date)%>%
  summarise(avg=mean(av_rating))

# Convert date to actual date class
whedon$date <- as.Date(whedon$date)

# Create new plot of Whedon shows by rating and date!
ggplot(whedon, aes(x = date, y = avg, color = title)) +
  geom_point(size=2) +
  geom_line(size=1) +
  soph_theme +
  theme(legend.position="bottom",legend.title=element_blank(),panel.grid.major.x=element_blank()) +
  scale_y_continuous(expand = c(0,0), limits = c(6,10), breaks = seq(6,10, by =1)) +
  labs(title="Are Joss Whedon shows consistent?",
       subtitle="Average IMDB rating for Joss Whedon shows",
       x="Date",
       y="Mean rating",
       caption="@SophieWarnes | Source: IMDB")