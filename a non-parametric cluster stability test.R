cat("\014"); rm(list=ls()); set.seed(12345)


if (!require("readxl")) install.packages("readxl")
if (!require("lavaan")) install.packages("lavaan")
if (!require("dplyr")) install.packages("dplyr")

library(readxl)
library(lavaan)
library(dplyr)


file_path <- "C:/Users/fardo/OneDrive/covid data/covid_mental_health.xlsx"
df <- read_excel(file_path)


df <- df %>% select(-what_things_you_done_toTakeCare_ofMentalHealth_during_covid_others,
                    -pre_what_things_you_done_toTakeCare_ofMentalHealth_during_covid_others)


ptss_post <- c(
  "repeated_disturbing_dream_ofStressfull_experiencce_from_past",
  "repeated_disturbing_memory_though_image_ofStressfull_experiencce_from_past",
  "suddenly_feel_asIf_aStressfull_experience_were_happening_again",
  "feeling_upset_when_something_remind_you_ofStressful_experience_fromPast",
  "having_physical_reaction_when_somthing_remind_you_stressful_experience",
  "avoiding_activy_because_they_remind_you_stressful_experience_from_past",
  "avoiding_thinking_about_stressful_experience_from_past",
  "trouble_memembering_important_part_ofStressful_experience_from_past",
  "loss_ofInterest_activity_that_you_enjoyed",
  "feeling_distant_from_other_people",
  "feeling_emotional_numb_orBeeing_unble_toLoving_feeling_forThose_closeTo_you",
  "feeling_asIfYourFeature_willBer_cut_short",
  "having_difficulty_concentrating",
  "feeling_irretablee_orHaving_angry_outburst",
  "trouble_falling_orStaying_asSleep"
)

coping_post <- c(
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_taked_toFNF_onPhone",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_taked_toFNF_video_chat",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_taked_toFNF_inPerson",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_exercise_inHome",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_exercise_outdoor",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_gardening",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_meditation",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_creative_activity",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_learn_new_skill",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_taking_breaks_from_the_news",
  "what_things_you_done_toTakeCare_ofMentalHealth_during_covid_spent_time_with_pet"
)

stress_post <- c(
  "what_you_experiencing_during_covid_period_anxiety",
  "what_you_experiencing_during_covid_period_depression",
  "what_you_experiencing_during_covid_period_loneliness",
  "what_you_experiencing_during_covid_period_anger",
  "what_you_experiencing_during_covid_period_frustration",
  "what_you_experiencing_during_covid_period_grief_ofFeeling_ofLoss",
  "what_you_experiencing_during_covid_period_changing_sleep_patern",
  "what_you_experiencing_during_covid_period_notGetting_exercise",
  "what_you_experiencing_during_covid_period_fear_ofGetting_covid",
  "what_you_experiencing_during_covid_period_fear_ofGiving_covid",
  "what_you_experiencing_during_covid_period_worrying_about_people_otherThanMe",
  "what_you_experiencing_during_covid_period_stigma_from_other_people",
  "what_you_experiencing_during_covid_period_notGetting_enough_emotionOrSocial_support",
  "what_you_experiencing_during_covid_period_confusion_about_covid",
  "what_you_experiencing_during_covid_period_confusion_about_where_toGet_info_about_covid"
)


ptss_pre <- paste0("pre_", ptss_post)
coping_pre <- paste0("pre_", coping_post)
stress_pre <- paste0("pre_", stress_post)


combine_pre_post <- function(pre_df, post_df, pre_items, post_items, base_names) {
  pre_sub <- pre_df[, pre_items, drop = FALSE]
  colnames(pre_sub) <- base_names
  pre_sub$wave <- 1
  
  post_sub <- post_df[, post_items, drop = FALSE]
  colnames(post_sub) <- base_names
  post_sub$wave <- 2
  
  rbind(pre_sub, post_sub)
}

ptss_data <- combine_pre_post(df, df, ptss_pre, ptss_post, ptss_post)
coping_data <- combine_pre_post(df, df, coping_pre, coping_post, coping_post)
stress_data <- combine_pre_post(df, df, stress_pre, stress_post, stress_post)


ptss_data$wave <- factor(ptss_data$wave, levels = c(1,2))
coping_data$wave <- factor(coping_data$wave, levels = c(1,2))
stress_data$wave <- factor(stress_data$wave, levels = c(1,2))


model_ptss <- paste("PTSS =~", paste(ptss_post, collapse = " + "))
model_coping <- paste("Coping =~", paste(coping_post, collapse = " + "))
model_stress <- paste("Stress =~", paste(stress_post, collapse = " + "))


test_configural_metric <- function(data, model_syntax, construct_name) {
  fit_config <- cfa(model_syntax, data = data, group = "wave",
                    estimator = "WLSMV", ordered = TRUE,
                    group.equal = "", std.lv = TRUE)
  
  fit_metric <- cfa(model_syntax, data = data, group = "wave",
                    estimator = "WLSMV", ordered = TRUE,
                    group.equal = "loadings", std.lv = TRUE)
  

  fit_c <- fitMeasures(fit_config, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "rmsea.scaled"))
  print(round(fit_c, 3))
  
  cat("\n--- Metric fit ---\n")
  fit_m <- fitMeasures(fit_metric, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                     "cfi.scaled", "rmsea.scaled"))
  print(round(fit_m, 3))
  

  delta_cfi <- fit_m["cfi.scaled"] - fit_c["cfi.scaled"]
  delta_rmsea <- fit_m["rmsea.scaled"] - fit_c["rmsea.scaled"]
  
  cat("\n--- Metric vs Configural (ΔCFI, ΔRMSEA) ---\n")
  cat("  ΔCFI =", round(delta_cfi, 4), "\n")
  cat("  ΔRMSEA =", round(delta_rmsea, 4), "\n")
  

  metric_inv <- (delta_cfi >= -0.01) & (delta_rmsea <= 0.015)
  cat("\n  Metric invariance:", ifelse(metric_inv, "HOLDS", "FAILS"), "\n")
  

  diff <- tryCatch(lavTestLRT(fit_config, fit_metric, method = "satorra.2000"),
                   error = function(e) NULL)
  if (!is.null(diff)) {
    cat("\n  Robust LRT p-value:", round(diff[2, "Pr(>Chisq)"], 4), "\n")
  }
  
  return(list(configural = fit_config, metric = fit_metric,
              delta_cfi = delta_cfi, delta_rmsea = delta_rmsea,
              metric_invariant = metric_inv))
}


res_ptss   <- test_configural_metric(ptss_data,   model_ptss,   "PTSS (15 items)")
res_coping <- test_configural_metric(coping_data, model_coping, "Coping Strategy (11 items)")
res_stress <- test_configural_metric(stress_data, model_stress, "Pandemic Stress (15 items)")


cat("\nPTSS:\n")
cat("  ΔCFI =", round(res_ptss$delta_cfi, 4), "\n")
cat("  ΔRMSEA =", round(res_ptss$delta_rmsea, 4), "\n")
cat("  Metric invariance:", ifelse(res_ptss$metric_invariant, "YES", "NO"), "\n")

cat("\nCoping Strategy:\n")
cat("  ΔCFI =", round(res_coping$delta_cfi, 4), "\n")
cat("  ΔRMSEA =", round(res_coping$delta_rmsea, 4), "\n")
cat("  Metric invariance:", ifelse(res_coping$metric_invariant, "YES", "NO"), "\n")

cat("\nPandemic Stress:\n")
cat("  ΔCFI =", round(res_stress$delta_cfi, 4), "\n")
cat("  ΔRMSEA =", round(res_stress$delta_rmsea, 4), "\n")
cat("  Metric invariance:", ifelse(res_stress$metric_invariant, "YES", "NO"), "\n")

