library(shinytest2)

test_that("{shinytest2} recording: Poisson_with_Interactions", {
  app <- AppDriver$new(test_path("../.."), variant = platform_variant(), name = "Poisson_with_Interactions", 
      seed = 123, height = 585, width = 979)
  app$set_inputs(tabs = "Dataset & Model")
  app$expect_values()
  app$click("sample")
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(sample_data_choice = "Ache Monkey")
  app$set_inputs(equation = "Kills ")
  app$set_inputs(equation = "Kills ~ Age ")
  app$set_inputs(equation = "Kills ~ Age * Days")
  app$set_inputs(equation = "Kills ~ Age * Dayss")
  app$click("DoCompute")
  app$expect_screenshot()
  app$set_inputs(shinyalert = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(equation = "Kills ~ Age * Days")
  app$click("DoCompute")
  app$set_inputs(ggpairs_summary_rows_current = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(ggpairs_summary_rows_all = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(ggpairs_summary_state = c(1777649447283, 0, 10, "", TRUE, FALSE, 
      TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, 
          "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$set_inputs(workPanel = "assumptions")
  app$set_inputs(vifTab_rows_current = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(vifTab_rows_all = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(vifTab_state = c(1777649481981, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, 
      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(modsumTab2_rows_current = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(modsumTab2_rows_all = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(modsumTab2_state = c(1777649482100, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), 
      allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$set_inputs(workPanel = "checks")
  app$expect_screenshot()
  app$set_inputs(workPanel = "plot")
  app$expect_screenshot()
  app$set_inputs(workPanel = "anova")
  app$set_inputs(anovaTab_rows_current = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(anovaTab_rows_all = c(1, 2, 3), allow_no_input_binding_ = TRUE)
  app$set_inputs(anovaTab_state = c(1777649507183, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$set_inputs(workPanel = "interpretation")
  app$set_inputs(marginsTab_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(marginsTab_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(marginsTab_state = c(1777649512423, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(modsumTab_rows_current = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(modsumTab_rows_all = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(modsumTab_state = c(1777649512538, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), 
      allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$set_inputs(var_inter = "Age:Days")
  app$set_inputs(var_moderator = "Age")
  app$set_inputs(workPanel = "interaction")
  app$set_inputs(interaction_emtrendscontrast_tab_rows_current = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emtrendscontrast_tab_rows_all = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emtrendscontrast_tab_state = c(1777649532466, 0, 10, 
      "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emtrends_tab_rows_current = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emtrends_tab_rows_all = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emtrends_tab_state = c(1777649532489, 0, 10, "", TRUE, 
      FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emmeanscontrast_tab_rows_current = c(1, 2, 3, 4, 5, 
      6), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emmeanscontrast_tab_rows_all = c(1, 2, 3, 4, 5, 6), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emmeanscontrast_tab_state = c(1777649532504, 0, 10, 
      "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emmeans_tab_rows_current = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emmeans_tab_rows_all = c(1, 2, 3, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(interaction_emmeans_tab_state = c(1777649532655, 0, 10, "", TRUE, 
      FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", 
          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, 
          TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$click("code_ggemmeans")
  app$set_inputs(`a5b58224b10e6d161c90-editor` = "# Ensure to load your data as an object called dat.\nlibrary(tidyverse)\nlibrary(ggeffects)\n####################################\n# Load Data\n####################################\ndat <- read_csv(\"www/McMillanAcheMonkey.csv\")\n####################################\npaste(\"# Fit Model:\", \"Poisson\")\n####################################\nmodel <- glm(formula, data = dat, family = poisson(link = \"log\"))\n####################################\n# Effects of Interest\n####################################\nm.mod <- mean(unlist(model.frame(model)[\"Age\"]), na.rm = T)\ns.mod <- sd(unlist(model.frame(model)[\"Age\"]), na.rm = T)\nmeffectsfor <- c(\"Days\", paste(\"Age\", \"[\", round(m.mod - s.mod, 2), \",\", round(m.mod + s.mod, 2), \"]\", sep = \"\"))\nmod.emmeans <- ggemmeans(model = model, terms = meffectsfor, interval = \"confidence\")\nmod.emmeans <- ggemmeans(model = model, terms = meffectsfor, interval = \"confidence\")\nggdat <- data.frame(mod.emmeans) %>%\n  mutate(group = case_when(group == round(m.mod - s.mod, 2) ~ paste(\"Low (Mean - 1SD = \", round(m.mod - s.mod, 2), \")\", sep = \"\"), TRUE ~ paste(\"High (Mean + 1SD = \", round(m.mod + s.mod, 2), \")\", sep = \"\")))\n####################################\n# Plot\n####################################\nggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +\n  geom_line() +\n  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = \"dotted\", alpha = ifelse(0.4, 0.4, 0)) +\n  xlab(\"Days\") +\n  ylab(\"Predicted Kills\") +\n  scale_color_brewer(\"Age\", palette = \"Pastel1\") +\n  scale_fill_brewer(\"Age\", palette = \"Pastel1\") +\n  theme_bw()")
  app$expect_screenshot()
})

