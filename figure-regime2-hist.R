# regime 2 hist
regime2.Sigma.known.hist_dicker <- reshape2::melt(dicker.Sigma.known.sample[c("n1000d10", "n1000d50", "n1000d500")]) %>% 
  rename(estimand=Var1, scenario=L1) %>% mutate(Estimator="Dicker") %>% select(-Var2)
regime2.Sigma.known.hist_plugin <- reshape2::melt(plugin.Sigma.known.sample[c("n1000d10", "n1000d50", "n1000d500")]) %>% 
  rename(estimand=Var1, scenario=L1) %>% mutate(Estimator="Plug-in") %>% select(-Var2)
regime2.Sigma.known.hist <- bind_rows(regime2.Sigma.known.hist_dicker, regime2.Sigma.known.hist_plugin)
  
plugin.Sigma.known.regime2.hist <- ggplot(data=regime2.Sigma.known.hist %>% 
                                            mutate(scenario=case_when(scenario=="n1000d10"~"n=1000, d=10",
                                                                      scenario=="n1000d50"~"n=1000, d=50",
                                                                      scenario=="n1000d500"~"n=1000, d=500"))) +
  geom_histogram(mapping=aes(x=value, fill=Estimator), bins=40, color="black", alpha=0.6) +
  facet_grid(rows = vars(estimand), cols = vars(scenario)) +
  scale_y_continuous(name="Count") + 
  scale_x_continuous(name="Residual variance",
                     limits=c(0.7,1.3)) + 
  theme_classic() + scale_color_aaas() + theme(text = element_text(size=30),
                                               legend.title = element_text(size=14), 
                                               legend.text = element_text(size=12),
                                               legend.justification=c(0,1), 
                                               legend.position=c(0.01, .99))
ggsave(plugin.Sigma.known.regime2.hist, file=here("figures","fig-regime2-hist.pdf"),
       width=14, height=7)
