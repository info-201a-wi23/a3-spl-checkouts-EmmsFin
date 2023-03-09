# relevant calculations
library("dplyr")
library("stringr")
Schwab_df <- read.csv("Schwab_Checkouts_by_Title.csv", stringsAsFactors = F)

Schwab_df$Title <- tolower(Schwab_df$Title)
Schwab_df$Title[str_detect(Schwab_df$Title, "vicious")] <- "Vicious"
Schwab_df$Title[str_detect(Schwab_df$Title, "a conjuring of light")] <- "A Conjuring of Light"
Schwab_df$Title[str_detect(Schwab_df$Title, "a gathering of shadows")] <- "A Gathering of Shadows"
Schwab_df$Title[str_detect(Schwab_df$Title, "new beginnings")] <- "New Beginnings"
Schwab_df$Title[str_detect(Schwab_df$Title, "broken ground")] <- "Broken Ground"
Schwab_df$Title[str_detect(Schwab_df$Title, "this savage song")] <- "This Savage Song"
Schwab_df$Title[str_detect(Schwab_df$Title, "the unbound")] <- "The Unbound"
Schwab_df$Title[str_detect(Schwab_df$Title, "a darker shade of magic")] <- "A Darker Shade of Magic"
Schwab_df$Title[str_detect(Schwab_df$Title, "the near witch")] <- "The Near Witch"
Schwab_df$Title[str_detect(Schwab_df$Title, "second chances")] <- "Second Chances"
Schwab_df$Title[str_detect(Schwab_df$Title, "last wishes")] <- "Last Wishes"
Schwab_df$Title[str_detect(Schwab_df$Title, "the archived")] <- "The Archived"
Schwab_df$Title[str_detect(Schwab_df$Title, "bridge of souls")] <- "Bridge of Souls"
Schwab_df$Title[str_detect(Schwab_df$Title, "city of ghosts")] <- "City of Ghosts"
Schwab_df$Title[str_detect(Schwab_df$Title, "conjuro de luz")] <- "A Conjuring of Light"
Schwab_df$Title[str_detect(Schwab_df$Title, "extraordinary")] <- "ExtraOrdinary"
Schwab_df$Title[str_detect(Schwab_df$Title, "gallant")] <- "Gallant"
Schwab_df$Title[str_detect(Schwab_df$Title, "la vida invisible de addie larue")] <- "The Invisible Life of Addie LaRue"
Schwab_df$Title[str_detect(Schwab_df$Title, "the invisible life of addie larue")] <- "The Invisible Life of Addie LaRue"
Schwab_df$Title[str_detect(Schwab_df$Title, "our dark duet")] <- "Our Dark Duet"
Schwab_df$Title[str_detect(Schwab_df$Title, "tunnel of bones")] <- "Tunnel of Bones"
Schwab_df$Title[str_detect(Schwab_df$Title, "vengeful")] <- "Vengeful"

# rise_df is the rise in checkouts per year and total in all years
rise_df <- Schwab_df %>% 
  group_by(CheckoutYear) %>%
  summarise(Checkouts = sum(Checkouts, na.rm = T)) %>%
  mutate(growth = lag(Checkouts, 1)) %>%  # the year before is in growth_pop
  mutate(growth = ((Checkouts - growth) / growth) * 100) %>%
  mutate(total_growth = ((Checkouts[12] - Checkouts[1]) / Checkouts[1] * 100)) %>%
  filter(CheckoutYear != 2023)

rise_total <- rise_df$total_growth[1]
greatest_rise <- max(rise_df$growth, na.rm = T)
pop_year <- rise_df$CheckoutYear[which(rise_df$growth== greatest_rise)]

# total_checkouts is the total amount of checkouts she has across all years

total_checkouts <- sum(Schwab_df$Checkouts, na.rm = T)

# most_used_medium are the mediums that are used the most across every book
most_used_medium <- Schwab_df %>% 
  group_by(MaterialType) %>%
  summarise(Checkouts = sum(Checkouts, na.rm = T)) %>%
  arrange(desc(Checkouts)) %>%
  mutate(MaterialType = tolower(MaterialType))

# Most_popular_book is a list of which books are most popular by checkouts
most_popular_book <- Schwab_df %>% 
  group_by(Title) %>%
  summarise(Checkouts = sum(Checkouts, na.rm = T)) %>%
  arrange(desc(Checkouts))

# Most_popular_series is a list of which series are the most popular by checkouts
most_popular_series <- Schwab_df %>% 
  mutate(Series = Title) %>%
  group_by(Series) %>%
  filter(CheckoutYear == "2022") %>%
  select(Title, Checkouts, CheckoutMonth) %>%
  mutate(Series = gsub("A Conjuring of Light", "Shades of Magic Series", Series)) %>%
  mutate(Series = gsub("A Darker Shade of Magic", "Shades of Magic Series", Series)) %>%
  mutate(Series = gsub("A Gathering of Shadows", "Shades of Magic Series", Series)) %>%
  mutate(Series = gsub("Bridge of Souls", "City of Ghosts Series", Series)) %>%
  mutate(Series = gsub("City of Ghosts", "City of Ghosts Series", Series)) %>%
  mutate(Series = gsub("City of Ghosts Series Series", "City of Ghosts Series", Series)) %>%
  mutate(Series = gsub("Tunnel of Bones", "City of Ghosts Series", Series)) %>%
  mutate(Series = gsub("This Savage Song", "Monsters of Verity Series", Series)) %>%
  mutate(Series = gsub("Our Dark Duet", "Monsters of Verity Series", Series)) %>%
  mutate(Series = gsub("The Archived", "The Archived Series", Series)) %>%
  mutate(Series = gsub("The Unbound", "The Archived Series", Series)) %>%
  mutate(Series = gsub("Vicious", "Villains Series", Series)) %>%
  mutate(Series = gsub("Vengeful", "Villains Series", Series)) %>%
  mutate(Series = gsub("ExtraOrdinary", "Villains Series", Series)) %>%
  mutate(Series = gsub("New Beginnings", "Everyday Angel Series", Series)) %>%
  mutate(Series = gsub("Last Wishes", "Everyday Angel Series", Series)) %>%
  mutate(Series = gsub("Second Chances", "Everyday Angel Series", Series)) %>%
  filter(Series != "Broken Ground", Series != "The Invisible Life of Addie LaRue", Series != "Gallant", Series != "The Near Witch") %>% 
  group_by(Series) %>%
  summarise(Checkouts = sum(Checkouts, na.rm = T))%>%
  arrange(desc(Checkouts))