# regime 3 hist
regime3.Sigma.unknown.hist_sample <- reshape2::melt(dicker.Sigma.unknown_sample.sample[c("n1000d10", "n1000d50", "n1000d500")]) %>% 
  rename(estimand=Var1, scenario=L1) %>% mutate(Estimator="Empiric") %>% select(-Var2)
regime3.Sigma.unknown.hist_banded <- reshape2::melt(dicker.Sigma.unknown_banded.sample[c("n1000d10", "n1000d50", "n1000d500")]) %>% 
  rename(estimand=Var1, scenario=L1) %>% mutate(Estimator="Banded") %>% select(-Var2)
regime3.Sigma.unknown.hist <- bind_rows(regime3.Sigma.unknown.hist_sample, regime3.Sigma.unknown.hist_banded)

plugin.Sigma.unknown.regime3.hist <- ggplot(data=regime3.Sigma.unknown.hist %>% 
                                            mutate(scenario=case_when(scenario=="n1000d10"~"n=1000, d=10",
                                                                      scenario=="n1000d50"~"n=1000, d=50",
                                                                      scenario=="n1000d500"~"n=1000, d=500"))) +
  geom_histogram(mapping=aes(x=value, fill=Estimator), bins=40, color="black", alpha=0.6) +
  facet_grid(rows = vars(estimand), cols = vars(scenario)) +
  scale_y_continuous(name="Count") + 
  scale_x_continuous(name="Residual variance",
                     limits=c(0.25,1.75)) + 
  theme_classic() + scale_color_aaas() + theme(text = element_text(size=30),
                                               legend.title = element_text(size=14), 
                                               legend.text = element_text(size=12),
                                               legend.justification=c(0,1), 
                                               legend.position=c(0.01, .99))
ggsave(plugin.Sigma.unknown.regime3.hist, file=here("figures","fig-regime3-hist.pdf"),
       width=14, height=7)
