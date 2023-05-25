library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(modelsummary)
library(car)
library(kableExtra)

# Create theme for figures
ifau_colors <- c("#122f49",
                 "#cb4f7d",
                 "#4ca7e0",
                 "#8fb955",
                 "#72418d",
                 "#d4762e")

ifau_fills <- c("#122f49",
                "#f1ca13",
                "#cfd2d3",
                "#cb4f7d",
                "#4ca7e0",
                "#8fb955",
                "#72418d",
                "#d4762e")

theme_ifau <-
  theme_minimal() +
  theme()

theme_ifau_color <- list(theme_ifau,
                         scale_color_manual(values = ifau_colors),
                         scale_fill_manual(values = ifau_fills))

# Test difference between traditional and gig experience
glance_custom.fixest <- function(model, ...) {
  test <- linearHypothesis(model, "traditional = gig")
  pval <- test["Pr(>Chisq)"][2, ]
  pval_rounded <- formatC(pval, digits = 2)
  out <- data.frame("pval" = pval_rounded)
  return(out)
}

# Prepare data
dt <- fread("data.csv", encoding = "UTF-8")

dt[, callback := as.integer(response %in% c("Ja", "Intresserad"))]
dt[, callback2 := as.integer(response == "Ja")]

dt[, application := .GRP, by = .(cv, region)]

dt[, arabic_name := as.integer(identity %in% c(3, 4))]

dt[, traditional := as.integer(experience == 3)]
dt[, gig := as.integer(experience == 2)]

dt[, y := callback * 100]
dt[, y2 := callback2 * 100]


dt[, exp := fcase(gig == 1, "Gig", traditional == 1, "Traditional", default = "Unemployed")]
dt[, exp := factor(exp, levels = c("Unemployed", "Gig", "Traditional"))]
dt[, name := fcase(arabic_name == 0, "Swedish", default = "Arabic")]
dt[, name := factor(name, levels = c("Swedish", "Arabic"))]


# Get first 8,001 observations
setorder(dt, date)
dt_plan <- dt[1:8001]


occ_eng <- data.table(ssyk1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
           occupation_coarse_eng = 
	 c("Managers",
	 "Occupations requiring advanced level of higher education",
	 "Occupations requiring higher education qualifications or equivalent",
 	 "Administration and customer service clerks",
 	 "Service, care and shop sales workers",
 	 "Agricultural, horticultural, forestry and fishery workers",
	 "Building and manufacturing workers",
 	 "Mechanical manufacturing and transport workers, etc.",
 	 "Elementary occupations"))

dt <- dt[occ_eng, on = "ssyk1"]

dt[, occ_share := 100*.N/nrow(dt), by = occupation_coarse_eng]

dt[, occupation_coarse_eng := reorder(occupation_coarse_eng, occ_share)]


# Figure 1
#---------

ggplot(dt, aes(x = occupation_coarse_eng, y = occ_share)) +
  stat_summary(fun = "mean", geom = "bar", fill = "#122f49") +
  coord_flip() +
  scale_x_discrete(labels = wrap_format(40)) +
  labs(x = "", y = "Share of all applications") +
  theme_ifau_color +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("results/figure1.pdf",
       width = 13.7, height = 11, units = "cm")
  
# Figure 2
#---------

dodge <- position_dodge2(width = 0.25, padding = 0.75)
ggplot(dt, aes(x = exp, y = y, fill = name)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge2") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", fun.args = list(mult = 1.96), position = dodge) +
  labs(x = "Work experience", y = "Callback rate", fill = "Name") +
  theme_ifau_color +
  theme(legend.position = "bottom",
  panel.grid.major.x = element_blank())

ggsave("results/figure2.pdf",
       width = 13.7, height = 11, units = "cm")

# Table 1
#--------

ols1 <- feols(y ~ traditional + gig,
              vcov = cluster ~ vacancy,
              data = dt)

fe1 <- feols(y ~ traditional + gig | vacancy + application,
             vcov = cluster ~ vacancy,
             data = dt)

ols2 <- feols(y ~ traditional * arabic_name + gig * arabic_name,
              vcov = cluster ~ vacancy,
              data = dt)

fe2 <- feols(y ~ traditional * arabic_name + gig * arabic_name
             | vacancy + application,
             vcov = cluster ~ vacancy,
             data = dt)

models <- list(ols1, fe1, ols2, fe2)

names(models) <- paste0("(", 1:4, ")")

modelsummary(models,
             fmt = 2,
             gof_omit = "AIC|BIC|R2 |Log.Lik.",
             title = "Main results",
             coef_map = c("traditional" = "Traditional work experience",
               "gig" = "Gig experience",
               "arabic_name" = "Arabic-sounding name",
               "traditional:arabic_name" = "Arabic $\\times$ Traditional",
               "arabic_name:gig" = "Arabic x Gig",
               "(Intercept)" = "Intercept"),
             gof_map = list(
                 list("raw" = "pval",
                      "clean" = "P-value, $H_0$: Trad. = Gig",
                      "fmt" = 2),
                 list("raw" = "FE: vacancy",
                      "clean" = "Vacancy and application fixed effects",
                      "fmt" = 2),
                 list("raw" = "nobs",
                      "clean" = "Observations",
                      "fmt" = \(x) format(round(x, 0), big.mark = ",")),
                 list("raw" = "r.squared",
                      "clean" = "$R^2$",
                      "fmt" = 3)),
             escape = FALSE,
             output = "results/table1.tex")


# Appendix figures and tables
#----------------------------

dt[, two_arabic := sum(arabic_name) == 2 , by = vacancy]
dt[, name_mix := fcase(two_arabic == TRUE, "One Swedish, two Arabic", default = "Two Swedish, one Arabic")]

dt[, ord := frank(sent), by = "vacancy"]
dt[, sent_order := fcase(ord == 1, "First", ord == 2, "Second", default = "Third")]
dt[, sent_order := factor(sent_order, levels = c("First", "Second", "Third"))]

# Figure B1
#----------

dodge <- position_dodge2(width = 0.25, padding = 0.75)

ggplot(dt, aes(x = name, y = y, fill = name_mix)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge2") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", fun.args = list(mult = 1.96), position = dodge) +
  facet_wrap(vars(exp), strip.position = "bottom") +
  labs(x = "Name and work experience", y = "Callback rate", fill = "Name composition") +
  theme_ifau_color +
  theme(legend.position = "bottom",
  strip.placement = "outside",
  panel.grid.major.x = element_blank())

ggsave("results/figureB1.pdf",
       width = 13.7, height = 11, units = "cm")

# Figure B2
#----------

ggplot(dt, aes(x = name, y = y, fill = sent_order)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge2") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", fun.args = list(mult = 1.96), position = dodge) +
  facet_wrap(vars(exp), strip.position = "bottom") +
  labs(x = "Name and work experience", y = "Callback rate", fill = "Application rank") +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  theme_ifau_color +
  theme(legend.position = "bottom",
  strip.placement = "outside",
  panel.grid.major.x = element_blank())

ggsave("results/figureB2.pdf",
       width = 13.7, height = 11, units = "cm")

# Figure B3
#----------

dt[, exp := fcase(gig == 1, "Gig", traditional == 1, "Traditional", default = "Unemployed")]
dt[, exp := factor(exp, levels = c("Unemployed", "Gig", "Traditional"))]

dt[, reg := region]
dt[reg == "Göteborg", reg := "Gothenburg"]
dt[, reg := factor(reg, levels = c("Stockholm", "Gothenburg", "Malmö"))]

ggplot(dt, aes(x = name, y = y, fill = reg)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge2") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", fun.args = list(mult = 1.96), position = dodge) +
  facet_wrap(vars(exp), strip.position = "bottom") +
  labs(x = "Name and work experience", y = "Callback rate", fill = "Region") +
  theme_ifau_color +
  theme(legend.position = "bottom",
  strip.placement = "outside",
  panel.grid.major.x = element_blank())

ggsave("results/figureB3.pdf",
       width = 13.7, height = 11, units = "cm")

# Table B1
#---------

occs_table <- dt[, .(ssyk = substr(as.character(ssyk), 1, 3))][
  , .N, by = ssyk][order(-N)]

isco_occs <- data.table(ssyk = c("941", "534", "522", "524", "422", "513",
                                 "911", "411", "332", "432", "531", "832",
                                 "512", "532", "912", "351", "523", "821",
                                 "611", "962"),
                        isco_occ = c("Food Preparation Assistants",
                                     "Personal Care Workers in Health Services",
                                     "Shop Salespersons",
                                     "Other Sales Workers",
                                     "Client Information Workers",
                                     "Waiters and Bartenders",
                                     "Domestic, Hotel and Office Cleaners and Helpers",
                                     "Clerks and Secretaries",
                                     "Sales and Purchasing Agents and Brokers",
                                     "Material Recording and Transport Clerks",
                                     "Child Care Workers and Teachers' Aides",
                                     "Car, Van and Motorcycle Drivers",
                                     "Cooks", "Personal Care Workers in Health Services",
                                     "Vehicle, Window, Laundry and Other Hand Cleaning Workers",
                                     "Information and Communications Technology Operations and User Support Technicians",
                                     "Cashiers and Ticket Clerks",
                                     "Assemblers",
                                     "Market Gardeners and Crop Growers",
                                     "Other Elementary Workers"))

occs_table <- merge(occs_table, isco_occs)[order(-N)]

total_obs <- dt[, .N]

occs_table[, share := 100 * (N / total_obs)]

share_other <- 100 * (total_obs - occs_table[, sum(N)]) / total_obs

isco_table <- rbind(occs_table[, .(ssyk, isco_occ, share)],
                    data.table(isco_occ = "Other occupations",
                               share = share_other, ssyk = ""))
kable(isco_table,
      format = "latex",
      booktabs = TRUE,
      digits = 1,
      linesep = c("\\addlinespace"),
      align = c("l", "p{8cm}", "r"),
      col.names = c("SSYK", "Occupation", "Share percent"), 
      caption = "20 largest occupations") %>%
    cat(file = "results/tableB1.tex")

# Table B2
#---------

occs_n <- groupingsets(dt,
             .(N = .N),
             by = c("occupation_coarse_eng", "name", "exp"),
             sets = list(c("occupation_coarse_eng", "name", "exp"), c("occupation_coarse_eng"), c("name", "exp"), character(0)))

occs_n[is.na(occupation_coarse_eng), occupation_coarse_eng := "Total"]
occs_n[is.na(name), name := "All"]
occs_n[is.na(exp), exp := "All"]

occs_n <- dcast(occs_n, occupation_coarse_eng ~ name + exp, value.var = "N")[order(-All_All)]

occs_n <- rbind(occs_n[-1], occs_n[1])

ex <- c("Unemployed", "Gig", "Traditional")
n_cols <- c("All_All", paste("Swedish", ex, sep = "_"), paste("Arabic", ex, sep = "_"))
setcolorder(occs_n, c("occupation_coarse_eng", n_cols))

kable(occs_n,
      format = "latex",
      booktabs = TRUE,
      digits = 1,
      format.args = list(big.mark = ","),
      linesep = c(rep("", 8), "\\addlinespace"),
      align = c("l", rep("r", 7)),
      col.names = c("Occupation group", "Total", 
                    rep(c("Unempl.", "Gig", "Trad."), 2)),
      caption = "Number of observations by occupation") %>%
  add_header_above(c(" " = 2, "Swedish name" = 3, "Arabic name" = 3)) %>%
  landscape() %>% 
  cat(file = "results/tableB2.tex")

# Table B3
#---------

datasummary(exp * y ~ name * (Mean * Arguments(fmt = 1) + SD * Arguments(fmt = 1) + N),
            data = dt,
            options("modelsummary_format_numeric_latex" = "plain"),
            output = "results/tableB3.tex")

# Table B4
#---------

datasummary(exp * y ~ name * (Mean * Arguments(fmt = 1) + SD * Arguments(fmt = 1) + N),
            data = dt_plan,
            options("modelsummary_format_numeric_latex" = "plain"),
            output = "results/tableB4.tex")

# Table B5
#---------

ols1_plan <- feols(y ~ traditional + gig, vcov = cluster ~ vacancy, data = dt_plan)

fe1_plan <- feols(y ~ traditional + gig | vacancy + application, vcov = cluster ~ vacancy, data = dt_plan)

ols2_plan <- feols(y ~ traditional * arabic_name + gig * arabic_name, vcov = cluster ~ vacancy, data = dt_plan)

fe2_plan <- feols(y ~ traditional * arabic_name + gig * arabic_name | vacancy + application, vcov = cluster ~ vacancy, data = dt_plan)

models_plan <- list(ols1_plan, fe1_plan, ols2_plan, fe2_plan)

names(models_plan) <- paste0("(", 1:4, ")")

modelsummary(models_plan, fmt = 2, gof_omit = "AIC|BIC|R2 |Log.Lik.", title = "Main results",
  coef_map = c("traditional" = "Traditional work experience",
               "gig" = "Gig experience",
               "arabic_name" = "Arabic-sounding name",
               "traditional:arabic_name" = "Arabic $\\times$ Traditional",
               "arabic_name:gig" = "Arabic x Gig",
               "(Intercept)" = "Intercept"),
  gof_map = list(
                 list("raw" = "pval", "clean" = "P-value, $H_0$: Trad. = Gig", "fmt" = 2),
                 list("raw" = "FE: vacancy", "clean" = "Vacancy and application fixed effects", "fmt" = 2),
                 list("raw" = "nobs", "clean" = "Observations", "fmt" = \(x) format(round(x, 0), big.mark = ",")),
                 list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3)
                 ),
  escape = FALSE,
  output = "results/tableB5.tex")

# Table B6
#---------

datasummary(exp * y2 ~ name * (Mean * Arguments(fmt = 1) + SD * Arguments(fmt = 1) + N),
            data = dt,
            options("modelsummary_format_numeric_latex" = "plain"),
            output = "results/tableB6.tex")

# Table B7
#---------

ols1_alt <- feols(y2 ~ traditional + gig, vcov = cluster ~ vacancy, data = dt)

fe1_alt <- feols(y2 ~ traditional + gig | vacancy + application, vcov = cluster ~ vacancy, data = dt)

ols2_alt <- feols(y2 ~ traditional * arabic_name + gig * arabic_name, vcov = cluster ~ vacancy, data = dt)

fe2_alt <- feols(y2 ~ traditional * arabic_name + gig * arabic_name | vacancy + application, vcov = cluster ~ vacancy, data = dt)

models_alt <- list(ols1_alt, fe1_alt, ols2_alt, fe2_alt)

names(models_alt) <- paste0("(", 1:4, ")")

modelsummary(models_alt, fmt = 2, gof_omit = "AIC|BIC|R2 |Log.Lik.", title = "Main results",
  coef_map = c("traditional" = "Traditional work experience",
               "gig" = "Gig experience",
               "arabic_name" = "Arabic-sounding name",
               "traditional:arabic_name" = "Arabic $\\times$ Traditional",
               "arabic_name:gig" = "Arabic x Gig",
               "(Intercept)" = "Intercept"),
  gof_map = list(
                 list("raw" = "pval", "clean" = "P-value, $H_0$: Trad. = Gig", "fmt" = 2),
                 list("raw" = "FE: vacancy", "clean" = "Vacancy and application fixed effects", "fmt" = 2),
                 list("raw" = "nobs", "clean" = "Observations", "fmt" = \(x) format(round(x, 0), big.mark = ",")),
                 list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3)
                 ),
  escape = FALSE,
  output = "results/tableB7.tex")



