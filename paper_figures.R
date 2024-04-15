########################
### Prerequisites ###
########################

library(countrycode)
library(stringr)

col_m <- "turquoise3"
col_f <- "mediumorchid1"

col_m_dark <- "turquoise4"
col_f_dark <- "darkorchid4"

col_nb <- "yellow2"

continent_cols <- c(Africa = "#000000",
                    Americas = "#EE334E",
                    Asia = "#FCB131",
                    Europe = "#0081C8",
                    Oceania = "#00A651")

country_colours <- c(Canada = "red4",
              China = "#FCB131",
              France = "navy",
              UK = "#0081C8",
              USA = "#EE334E")

########################
### Figure 2 ###
########################

###### panel A ######

### proportion of women at different levels of UK higher education
### (from postgraduate to prof)
### source: https://www.advance-he.ac.uk/news-and-views/women-he-accelerating-change-tackle-treatment-and-inclusion-women

p_f <- c(.559, .486, .459, .255)
burden_f <- (1-p_f)/p_f

p_f_seq <- seq(0.01, .5, 0.01)
burden_f_seq <- (1-p_f_seq)/p_f_seq

###### panel B ######

### source: https://www.gov.uk/government/publications/scientific-advisory-group-for-emergencies-sage-coronavirus-covid-19-response-membership/list-of-participants-of-sage-and-related-sub-groups
spim <- read.csv("data/anonymised-spi-m-members.csv")
spim$sexethnicity <- as.factor(spim$sexethnicity)
spim_table <- table(spim$ethnicity, spim$sex) / nrow(spim)

###### panel C ######

## compared to the whole country:
# https://www.ethnicity-facts-figures.service.gov.uk/uk-population-by-ethnicity/demographics/male-and-female-populations/latest#:~:text=women%20and%20girls%20made%20up%2052.6%25%20of%20the%20black%20ethnic,of%20the%20%27other%27%20ethnic%20group
# according to the 2021 Census, females made up 51.0% of the population of England and Wales
# 81.7% of both genders are white
prop_F_census <- 0.51
prop_W_census <- 0.817
census_2021 <- matrix(c(prop_F_census*(1-prop_W_census),
                        (1-prop_F_census)*(1-prop_W_census),
                        prop_F_census*(prop_W_census),
                        (1-prop_F_census)*(prop_W_census)),
                      nrow = 2, byrow = TRUE)
colnames(census_2021) <- colnames(spim_table)
rownames(census_2021) <- rownames(spim_table)

###### plotting ######

pdf("figures/Figure2-gender_multiplot.pdf", width = 8, height = 8)
mat <- matrix(c(1,1,2, 3), ncol=2, byrow = TRUE)
layout(mat)
par(mar = c(7, 5, 5, 2))
plot(p_f_seq, burden_f_seq, type = "l",
     xlab = "Proportion of females",
     ylab = "Relative individual burden",
     ylim = c(0, 10),
     bty = "n",  xaxs="i", yaxs="i",
     axes = FALSE)
axis(side = 1, at = seq(0, 0.5, 0.1))
axis(side = 2)
abline(h = 1, lty = 2)
points(p_f[4], burden_f[4], col = "red", pch = 19)
title("A. Impact of equality on minority individuals' workload", adj = 0)
par(mar = c(2, 2, 2, 3))
pie(spim_table, col = c(col_f_dark, col_f, col_m_dark, col_m),
    labels = c("Female\nEthnic\nminority", "Female\nWhite", "Male\nEthnic\nminority", "Male\nWhite"))
title("B. SPI-M\ngender and ethnicity diversity", adj = 0)
pie(census_2021, col = c(col_f_dark, col_f, col_m_dark, col_m),
    labels = c("Female\nEthnic\nminority", "Female\nWhite", "Male\nEthnic\nminority", "Male\nWhite"))
title("C. UK Census\ngender and ethnicity diversity", adj = 0)
dev.off()

###### additional stats for the text: ######
### women made up only 31% of the group:
100 * signif(sum(spim_table[, "F"]), 2)

########################
### Figure 3 ###
########################

###### reading and processing data ######

### source: courtesy of Elsevier and Epidemics9 conference organisers
epidemics9_presenters <- read.csv("data/anonymised-Epidemics9_presenters.csv")

### data cleaning + adding continent info
epidemics9_presenters$Country[epidemics9_presenters$Country %in% ""] <- NA
epidemics9_presenters$continent <- factor(countrycode(sourcevar = epidemics9_presenters$Country,
                                                      origin = "country.name",
                                                      destination = "continent"))

names(epidemics9_presenters)[names(epidemics9_presenters) %in% "Presenter.gender.identity"] <- "Gender"
epidemics9_presenters$Gender[epidemics9_presenters$Gender %in% ""] <- NA

names(epidemics9_presenters)[names(epidemics9_presenters) %in% "Career.stage"] <- "career_stage"
epidemics9_presenters$career_stage[epidemics9_presenters$career_stage %in% ""] <- NA
epidemics9_presenters$career_stage[epidemics9_presenters$career_stage %in% "Full professor or equivalent"] <- "Full prof"
epidemics9_presenters$career_stage[epidemics9_presenters$career_stage %in% "Associate professor or equivalent"] <- "Associate prof"
epidemics9_presenters$career_stage[epidemics9_presenters$career_stage %in% "Assistant professor or equivalent"] <- "Assistant prof"
epidemics9_presenters$career_stage[epidemics9_presenters$career_stage %in% "Tenure track researcher"] <- "TT researcher"
epidemics9_presenters$career_stage[epidemics9_presenters$career_stage %in% "Postdoctoral researcher"] <- "Postdoc"
epidemics9_presenters$career_stage <- factor(epidemics9_presenters$career_stage)

### summary of country/continent info:
epidemics9_presenters_countries <- sort(table(epidemics9_presenters$Country))
countries <- data.frame(country = names(epidemics9_presenters_countries))
countries$continent <- countrycode(sourcevar = countries$country,
                                   origin = "country.name",
                                   destination = "continent")

### function to aggregate tables into fewer career levels
aggregate_table_career <- function(tab) {
  tab_agg <- matrix(NA, nrow = nrow(tab), ncol = 4)
  row.names(tab_agg) <- row.names(tab)
  colnames(tab_agg) <- c("Junior\nresearcher", "Senior\nresearcher", "TT\nnon prof", "Professor")
  tab_agg[, 1] <- rowSums(tab[, c("Research\nassistant", "PhD\nstudent", "Junior\nresearcher")])
  tab_agg[, 2] <- rowSums(tab[, c("Postdoc", "Senior\nresearcher")])
  tab_agg[, 3] <- rowSums(tab[, c("Assistant\nprof", "Associate\nprof", "Full\nprof" )])
  tab_agg[, 4] <- tab[, "Full\nprof"]
  tab_agg
}

### summary of number of presenters by country
epidemics9_presenters_countries_summary <- c(sum(epidemics9_presenters_countries[epidemics9_presenters_countries<10]), epidemics9_presenters_countries[epidemics9_presenters_countries>=10])
names(epidemics9_presenters_countries_summary)[1] <- "Other"
### create corresponding colours by continent:
country_cols_summary <- data.frame(country = names(epidemics9_presenters_countries_summary))
country_cols_summary$continent <- suppressWarnings(countrycode(country_cols_summary$country, origin = 'country.name',
                                      destination = "continent"))
country_cols_summary$continent[is.na(country_cols_summary$continent)] <- "Mixed"
country_cols_summary$col <- continent_cols[country_cols_summary$continent]
country_cols_summary$col[is.na(country_cols_summary$col)] <- "grey"

###### plotting ######

mat = matrix(c(1, 3, 2, 2), nrow = 2)

pdf("figures/Figure3-Epidemic9_presenters_overall_fig.pdf", width = 10, height = 10)
par(las = 1, mar = c(5, 5, 5, 5))
layout(mat)

# Panel A
tab_5 <- table(epidemics9_presenters[, c("Gender", "career_stage")])
colnames(tab_5) <- gsub(" ", "\n", colnames(tab_5))
tab_5_agg <- aggregate_table_career(tab_5)
tab_5_agg_prop <- apply(tab_5_agg, 2, prop.table)
ord <- c(1, 4, 2, 3)
tab_5_agg_prop <- tab_5_agg_prop[ord,]
b <- barplot(tab_5_agg_prop, col = c(col_m, col_nb, "grey", col_f)[ord], border = NA)
nms <- rownames(tab_5_agg_prop)
nms[3] <- "Non-binary/gender diverse"
legend("bottomright", nms, pch = 15,
       col = c(col_m, col_nb, "grey", col_f)[ord], bg = "white", pt.cex = 2)
mtext(side = 3, at = b, paste("n =", as.numeric(colSums(tab_5_agg))))
mtext(side = 2, line = 2.5, at = .5, "Proportion of presenters", las = 3)
title("A. Gender distribution by career stage")

# Panel B
barplot(epidemics9_presenters_countries_summary,
        horiz = TRUE, cex.names = .75,
        col = country_cols_summary$col, border = NA, xlim = c(0, 250))
abline(v = seq(50, 300, 50), col = "lightgrey", lwd = .25)
mtext("Number of presenters", side = 1, line = 3)
legend("bottomright", names(continent_cols),
       col = continent_cols, pch = 15, bg = "white", pt.cex = 2)
title("B. Number of presenters by country")

# Panel C
tab_6 <- table(epidemics9_presenters[, c("continent", "career_stage")])
colnames(tab_6) <- gsub(" ", "\n", colnames(tab_6))
tab_6_agg <- aggregate_table_career(tab_6)
tab_6_agg_prop <- apply(tab_6_agg, 2, prop.table)
ord <- c(2, 4, 3, 1, 5)
tab_6_agg_prop <- tab_6_agg_prop[ord,]
b <- barplot(tab_6_agg_prop, col = continent_cols[rownames(tab_6_agg_prop)], border = NA)
#legend("bottomright", rownames(tab_6_agg_prop), pch = 15, col = continent_cols[rownames(tab_6_agg_prop)], bg = "white")
mtext(side = 3, at = b, paste("n =", as.numeric(colSums(tab_6_agg))))
mtext(side = 2, line = 2.5, at = .5, "Proportion of presenters", las = 3)
title("C. Geographic distribution by career stage")
dev.off()

###### stats for the paper ######

### for the figure legend: list of countries with less than 10 presenters:
countries_less_than_10_presenters <- paste(sort(names(epidemics9_presenters_countries[epidemics9_presenters_countries<10])), collapse = ", ")
countries_less_than_10_presenters

### the proportion of women decreased steadily from
### 55.3% among junior researchers to 29.5% among professors
100 * signif(tab_5_agg_prop["Woman", ], 3)

### Only 17 (1.7%) of those who disclosed their gender were non-binary or
### gender diverse, and none of them were at tenure-track career stage.
# total number:
sum(tab_5_agg["Non-binary or Gender diverse", ])
# percentage:
100 * signif(sum(tab_5_agg["Non-binary or Gender diverse", ]) /
               sum(tab_5_agg[c("Man", "Woman",
                               "Non-binary or Gender diverse"), ]), 2)
# number at TT stage:
sum(tab_5_agg["Non-binary or Gender diverse", c("TT\nnon prof", "Professor")])

### 72 countries were represented, but only 5.8% and 3.3% of presenters
### were from Africa and Oceania respectively,
# n countries:
length(unique(na.omit(epidemics9_presenters$Country)))
# % per continent
100 * signif(prop.table(table(epidemics9_presenters$continent)), 2)

### half of those based in Africa were from only two countries (37.3% were from
### South Africa and 15.3% from Ethiopia
tmp <- epidemics9_presenters$Country[epidemics9_presenters$continent %in% "Africa"]
100 * signif(sort(prop.table(table(tmp)), decreasing = TRUE), 3)

### the proportion of presenters from Africa decreased from 7.6% at junior
### researcher level to 2.3% at professor level.
### Conversely, the Americas represented 24.4% of junior presenters but
### 47.7% of professors
100 * signif(tab_6_agg_prop["Africa", ], 2)
100 * signif(tab_6_agg_prop["Americas", ], 3)

########################
### Figure 4 ###
########################

###### reading and processing data ######

### source: chatGPT request on "Who are the 10 best infectious disease modellers in the world?”

chatgpt_top10 <- read.csv("data/anonymised-chatgpt_top10_IDmodellers.csv")

### data cleaning
chatgpt_top10$type <- as.factor(paste0(chatgpt_top10$Gender, chatgpt_top10$Ethnicity, chatgpt_top10$native.English.speaker))
chatgpt_top10$my_type <- as.factor(chatgpt_top10$type %in% "FWN")
chatgpt_top10$white_male_native <- as.factor(chatgpt_top10$type %in% "MWY")

chatgpt_top10$Gender <- as.factor(chatgpt_top10$Gender)
chatgpt_top10$Ethnicity <- as.factor(chatgpt_top10$Ethnicity)
chatgpt_top10$native.English.speaker <- as.factor(chatgpt_top10$native.English.speaker)
chatgpt_top10$Country <- as.factor(chatgpt_top10$Country)

### summarise types of individuals ###

# 1. by gender and ethnicity
type_percentage <- sapply(1:10, function(i) prop.table(table(chatgpt_top10$type[which(chatgpt_top10$stochastic.repeat %in% i)])))
colnames(type_percentage) <- 1:10

# remove info about native speaking
type_percentage_simple <- type_percentage[c(1, 2, 4, 5),]
type_percentage_simple[2, ] <- colSums(type_percentage[2:3,])
type_percentage_simple[4, ] <- colSums(type_percentage[5:6,])
# remove last letter of rownmaes corresponding to native speaking status:
rownames(type_percentage_simple) <- str_sub(rownames(type_percentage_simple),
                                            end = -2)

# 2. by country
country_percentage <- sapply(1:10, function(i) prop.table(table(chatgpt_top10$Country[which(chatgpt_top10$stochastic.repeat %in% i)])))
colnames(country_percentage) <- 1:10

###### plotting ######

pdf("figures/Figure4-chatgpt_barplot_2.pdf", width = 8, height = 8)
par(mfcol = c(2, 1), mar = c(5, 5, 3, 5.5))
barplot(type_percentage_simple,
        xlab = "Stochastic repeat",
        ylab = "Proportion",
        col =  c(col_f_dark, col_f, col_m_dark, col_m),
        border = NA)
par(xpd = TRUE)
legend(13.5, .5, c("EM women", "White women",
                   "EM men", "White men"), pch = 15,
       col = c(col_f_dark, col_f, col_m_dark, col_m),
       bg = "white", pt.cex = 1.7, xjust = 0.5, yjust = 0.5, cex = .8)
par(xpd = FALSE)
title("A. Gender and ethnicity")

barplot(country_percentage,
        xlab = "Stochastic repeat",
        ylab = "Proportion",
        col =  country_colours[rownames(country_percentage)],
        border = NA)
par(xpd = TRUE)
legend(13.5, .5, rownames(country_percentage), pch = 15,
       col = country_colours[rownames(country_percentage)], bg = "white", pt.cex = 1.7, xjust = 0.5,
       yjust = 0.5, cex = .8)
par(xpd = FALSE)
title("B. Country of affiliation")

dev.off()