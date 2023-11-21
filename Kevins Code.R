install.packages("devtools")
devtools::install_github("vdeminstitute/vdemdata")
pkg_list <- c("wbstats", "countrycode") # create a list of packages
install.packages(pkg_list)


#installing all packages for the code to function 
library(wbstats) # for downloading WB data
library(dplyr) # for selecting, renaming and mutating
library(janitor) # for rounding

#STEP 1: GATHERING DATA #################  
#dowloading the data
# indicator I chose is (v2x_egaldem) .Egalitaarian Democracy Index and as my background factor and GDP v2x_egaldem
devtools::install_github("vdeminstitute/vdemdata")
dem_data <- vdem |> # download the V-Dem dataset
  filter(year >= 2000)  |> # filter out years less than 1990
  select(                  # select (and rename) these variables
    country = country_name,     # the name before the = sign is the new name  
    vdem_ctry_id = country_id,  # the name after the = sign is the old name
    year, 
    egal = v2x_egaldem, 
    gdp = e_gdp, 
    region = e_regionpol_6C
  ) |>
  mutate(
    region = case_match(region, # replace the values in region with names
                        1 ~ "Eastern Europe", 
                        2 ~ "Latin America",  
                        3 ~ "Middle East",   
                        4 ~ "Africa", 
                        5 ~ "The West", 
                        6 ~ "Asia")
    # number on the left of the ~ is the V-Dem region code
    # we are changing the number to the country name on the right
    # of the equals sign
  )

# View the data
glimpse(dem_data)

#STEP 2: MAKE A BAR CHART   #################  
#install.packages("scales")
library(readr)
library(dplyr)
library(ggplot2)

#Summarize the data 

bar_chart_data <- dem_data |> # save result as new object
  group_by(region)  |> # group dem_women data by region
  summarize(           # summarize following vars (by region)
    egal = mean(egal, na.rm = TRUE), # calculate mean, remove NAs
    gdp = mean(gdp, na.rm = TRUE), 
  ) |> 
  arrange(desc(egal)) # arrange in descending order by polyarchy score

# View the data
glimpse(bar_chart_data)

#bar chart

ggplot(bar_chart_data, aes(x = reorder(region, -egal), y = egal)) + # ggplot call
  geom_col(fill = "steelblue") + # we use geom_col() for a a bar chart
  labs(
    x = "Region", 
    y = "Avergae Egalitarian Index", 
    title = "Democracy by region, 2000 - present", 
    caption = "Source: V-Dem Institute"
  )
print(bar_chart_data)


#STEP 3: Make a colorblind-friendly line chart  #################  


#First, get our data

dem_data_line <- dem_data|>
  select(
    country,     
    year, 
    egal,
  ) |>
  filter( 
    country %in% c("Germany", # select countries in this list
                   "Iran", 
                   "Mexico")
  )
#print(dem_data_line)
dem_data_line <- ggplot(dem_data_line, aes(x = year, y = egal, color = country)) +
  geom_line(linewidth = 1) + # our geom is a line with a width of 1
  labs(
    x = "Year", 
    y = "Egalitarian Index", 
    title = 'Democracy in countries representing three different "waves"', 
    caption = "Source: V-Dem Institute", 
    color = "Country" # make title of legend to upper case
  )

#Assuring that packages for accesbility are installed
#install.packages("plotly")
#install.packages("colorBlindness")
library(colorBlindness)
cvdPlot(dem_data_line )
#b)  red-green color blindness is deuteranopia which
#only I can only see blue/purple with some dark yellow/brown line


dem_data_line <- dem_data_line + scale_color_brewer(palette = "YlGn")
cvdPlot(dem_data_line)


#Step 4: MAKE A SCATTER PLOT WITH ANNOTATION  #################  


#a) 
start_year <- 2009
end_year <- 2019

dem_data_scatter <- dem_data |>
  filter(year >= start_year & year <= end_year) |>
  group_by(region) |>
  summarize(
    egal = egal,
    gdp = gdp,
  )

# View the summarized data
glimpse(dem_data_scatter)

#  dem_data_scatter object is already loaded and contains the necessary columns
#install.packages("viridis")
library(viridis)

library(ggplot2)
library(viridis)  # For viridis color map


# b) Make a scatter plot 


dem_data_l <- ggplot(dem_data_scatter, aes(x = gdp, y = egal)) + 
  geom_point(aes(color = region), size =.5) + # color points by region
  geom_smooth(method = "loess", linewidth = 0.3) +  # make the line a loess curve
  scale_x_log10(labels = scales::label_dollar()) + # stretch axis, add '$' format
  scale_y_continuous(labels = scales::label_percent(scale = .1, suffix = "")) + # add % label
  labs(
    x= "GDP", # x-axis title
    y = "Egalitarian Index", # y-axis title
    title = "GDP and Egalitarian Level 2009-2019", # plot title
    caption = "Source: V-Dem Data", # caption
    color = "Region" # legend title
  )
print(dem_data_l)
dem_data_l+ scale_color_viridis_d(option = "plasma") + theme_minimal()

dem_data_l+
  annotate("text", x = 160000, y = .55, label = "Highest Trend")
#Included this annotation because it shows the highest trend corrolation between the egalitarian index and GDP 

#Step 5: MAKE YOYR SCATTER PLOT INTERACTIVE  #################  
library(plotly)

ggplotly(dem_data_l, tooltip = c("region", "egal", "gdp"))
# The plot indicates a discernible correlation between the egalitarian index and GDP across countries. 
# A notable trend emerges, suggesting that countries with higher GDP tend to exhibit greater egalitarianism. 
# However, an intriguing observation pertains to Eastern Europe, identified as an outlier region. 
# Within this region, countries are dispersed across the egalitarian index, prompting further analysis.

# The variance in Eastern European countries could be attributed to factors such as diverse levels of integration 
# into the European Union (EU) or lingering effects of Post-Soviet relations and politics. 
# This regional divergence aligns with Lipset's hypothesis, which posits that economic development 
# and modernization are associated with the adoption of democratic values. 
# Thus, the identified correlation between GDP and the egalitarian index, 
# coupled with the distinct pattern in Eastern Europe, offers support for the overarching principles of Lipset's hypothesis.