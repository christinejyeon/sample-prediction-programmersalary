setwd("/Users/Christine/Downloads/")

og_survey <- read.csv("stack-overflow-2018-developer-survey/survey_results_public.csv")
knitr::kable(head(og_survey[1:5, 1:5]), format = "markdown")
# library(jsonlite)
# og_bbquery <- data.frame(readLines("Best Buy E-commerce NER dataset.json"))
# fromJSON("Best Buy E-commerce NER dataset.json", flatten=TRUE)

library(data.table)
salary_byyrscoding <- data.table(og_survey)
salary_byyrscoding <- subset(salary_byyrscoding, !is.na(Salary))
salary_byyrscoding$Salary <- as.numeric(as.character(salary_byyrscoding$Salary))
salary_byyrscoding <- subset(salary_byyrscoding, !is.na(Salary))
salary_byyrscoding <- subset(salary_byyrscoding, (Salary>=(quantile(Salary,probs=0.25)-IQR(Salary)))&(Salary<=(quantile(Salary,probs=0.75)+IQR(Salary))))

salary_bybg <- data.table(og_survey)
salary_bybg$Salary <- as.numeric(as.character(salary_bybg$Salary))
salary_bybg <- subset(salary_bybg, !is.na(Salary))
salary_bybg <- subset(salary_bybg, (Salary>=(quantile(Salary,probs=0.25)-IQR(Salary)))&(Salary<=(quantile(Salary,probs=0.75)+IQR(Salary))))
salary_bybg <- salary_bybg[,list(mean_salary = mean(Salary), min_salary = min(Salary), max_salary = max(Salary)), by=UndergradMajor]
salary_bybg <- salary_bybg[c(-3)]


# temp <- summ_survey
# temp <- temp[,list(bottomwhisker = (quantile(Salary,probs=0.25)-IQR(Salary)), topwhisker = (quantile(Salary,probs=0.75)+IQR(Salary))), by=YearsCoding]
salary_byyrscoding <- salary_byyrscoding[,list(mean_salary = mean(Salary), min_salary = min(Salary), max_salary = max(Salary)), by=YearsCoding]

salary_byyrscoding <- salary_byyrscoding[c(4,7,2,3,8,5,6,10,9,11,1)]


##### drawing plots
library(highcharter)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)
highchart() %>% 
  hc_add_series(salary_byyrscoding, "column", hcaes(x = YearsCoding, y = mean_salary), name="Responses",
                tooltip = list(pointFormat = "<span style=\"color:{series.color}\">\u25CF</span> Average salary: {point.mean_salary}")) %>%
  hc_xAxis(categories=salary_byyrscoding$YearsCoding,
           title = list(text = "# of years that one has been coding")
  ) %>%
  hc_yAxis(
    title = list(text = "Annual salary")
  ) %>%
  hc_plotOptions(
    series = list(
      showInLegend = TRUE
    )
    # column = list(
    #   colorByPoint = TRUE
    # )
  ) %>%
  hc_title(
    text = "Annual salary according to the number of years that one has been coding"
  )

highchart() %>% 
  hc_add_series(salary_bybg, "column", hcaes(x = UndergradMajor, y = mean_salary), name="Responses",
                tooltip = list(pointFormat = "<span style=\"color:{series.color}\">\u25CF</span> Average salary: {point.mean_salary}")) %>%
  hc_xAxis(categories=salary_bybg$UndergradMajor,
           title = list(text = "Undergraduate major")
  ) %>%
  hc_yAxis(
    title = list(text = "Annual salary")
  ) %>%
  hc_plotOptions(
    series = list(
      showInLegend = TRUE
    )
    # column = list(
    #   colorByPoint = TRUE
    # )
  ) %>%
  hc_title(
    text = "Annual salary according to one's undergraduate major"
  )
