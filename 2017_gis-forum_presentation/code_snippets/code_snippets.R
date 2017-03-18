# UTF-8
# load libraries
library(dplyr)
library(ggplot2)
library(reshape2)

# setup locale if necessary
Sys.setlocale(category = "LC_ALL", locale = "Ukrainian")

# download and prep data
file_url <- "https://raw.githubusercontent.com/darsvid/menr_pa_data/master/tidy_data_01/ukraine.csv"
file <- read.csv(file_url, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
data <- tbl_df(file)

#-------------------------------------------------------------------------------
# ANALYSE COMPLETENESS
#-------------------------------------------------------------------------------
# calculate general completeness percentages
completeness_simple <- group_by(data, importance) %>%
        summarise(name = sum(is.na(name))/nrow(data)*100,
                  area = sum(is.na(area.ha))/nrow(data)*100,
                  desig.decision = sum(is.na(desig.decision))/nrow(data)*100,
                  operator = sum(is.na(operator))/nrow(data)*100,
                  location = sum(is.na(location))/nrow(data)*100)

# create simple completeness graph
graph_data <- melt(completeness_simple)
simple_completeness_graph <- ggplot(data=graph_data,
                                    aes(x=reorder(variable, desc(value)),
                                        y=value, fill=importance)) +
        geom_bar(stat="identity") +
        labs(x = "", y = "% пропусків", fill="значення") +
        theme(legend.position = c(0.87, 0.93),
              legend.text = element_text(size = rel(1.2), colour = "gray29"),
              legend.title = element_text(size = rel(1.5), colour = "gray29"),
              axis.text = element_text(size = rel(1.2)),
              axis.title.y = element_text(size = rel(1.5), colour = "gray29"))
ggsave("simple_completeness_graph.png", dpi = 300)

#-------------------------------------------------------------------------------
# COUNTS AND AREAS
# ------------------------------------------------------------------------------
# calculate count by categories
##  correct some mistakes
data$category <- gsub("закзник", "заказник", data$category) 
data$category <- gsub("пам’ятка природи ", "пам’ятка природи", data$category)

## prep and analyze data
categ_count <- group_by(data, category, importance) %>%
        summarise(count = n(),
                  area_sqkm = sum(area.ha, na.rm = TRUE)*0.01)
categ_count$factor <- NA
categ_count$factor <- factor(categ_count$category,
                             levels = c("природний заповідник", "біосферний заповідник",
                                        "національний природний парк",
                                        "регіональний ландшафтний парк", "заказник",
                                        "пам’ятка природи", "заповідне урочище",
                                        "ботанічний сад", "дендрологічний парк",
                                        "зоологічний парк",
                                        "парк-пам’ятка садово-паркового мистецтва"),
                             labels = c(1:11)
)
sums <- group_by(categ_count, category) %>%
        summarise(total_n = sum(count),
                  total_area = sum(area_sqkm))
graph_data <- left_join(categ_count, sums, by = "category")
graph_data_melt <- melt(graph_data)

## create quantity graph
quantity <- ggplot(data=graph_data,
                   aes(x=reorder(factor, -total_n),
                       y=count, fill=importance)) +
        geom_bar(stat="identity") +
        labs(x = "", y = "кількість", fill="значення") +
        theme(legend.position = c(0.82, 0.93),
              legend.text = element_text(size = rel(1.2), colour = "gray29"),
              legend.title = element_text(size = rel(1.5), colour = "gray29"),
              axis.text = element_text(size = rel(1.2)),
              axis.text.y = element_text(angle=90),
              axis.title.y = element_text(size = rel(1.5), colour = "gray29"))
ggsave("quantity.png", dpi = 300)

## create area graph
area <- ggplot(data=graph_data,
               aes(x=reorder(factor, -total_area),
                   y=area_sqkm, fill=importance)) +
        geom_bar(stat="identity") +
        labs(x = "", y = expression(площа~км^2), fill="значення") +
        theme(legend.position = c(0.82, 0.93),
              legend.text = element_text(size = rel(1.2), colour = "gray29"),
              legend.title = element_text(size = rel(1.5), colour = "gray29"),
              axis.text = element_text(size = rel(1.2)),
              axis.text.y = element_text(angle=90),
              axis.title.y = element_text(size = rel(1.5), colour = "gray29"))
ggsave("area.png", dpi = 300)

#-------------------------------------------------------------------------------
# MAPS STATISTICS
# ------------------------------------------------------------------------------
# correct some mistakes
data$category <- gsub("закзник", "заказник", data$category)
data$category <- gsub("пам’ятка природи ", "пам’ятка природи", data$category)

# group the data
map_count <- group_by(data, oblast, importance, category) %>%
        summarise(count = n(),
                  area_sqkm = sum(area.ha, na.rm = TRUE)*0.01)

# calculate counts
count_dcast <- dcast(map_count, oblast ~ category + importance, value.var = "count")
new_names <- c("oblast", "br_01_n", "bg_01_n", "bg_02_n", "dp_01_n", "dp_02_n",
               "zak_01_n", "zak_02_n", "zy_02_n", "zoo_01_n", "zoo_02_n", "np_01_n",
               "nm_01_n", "nm_02_n", "pp_01_n", "pp_02_n", "nr_01_n", "rlp_02_n")
colnames(count_dcast) <- new_names
write.csv(count_dcast, "count_dcast.csv", na = "0")

#calculate areas
area_dcast <- dcast(map_count, oblast ~ category + importance, value.var = "area_sqkm")
new_names <- c("oblast", "br_01_s", "bg_01_s", "bg_02_s", "dp_01_s", "dp_02_s",
               "zak_01_s", "zak_02_s", "zy_02_s", "zoo_01_s", "zoo_02_s", "np_01_s",
               "nm_01_s", "nm_02_s", "pp_01_s", "pp_02_s", "nr_01_s", "rlp_02_s")
colnames(area_dcast) <- new_names
write.csv(area_dcast, "area_dcast.csv", na = "0")