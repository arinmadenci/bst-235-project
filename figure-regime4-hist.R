# regime 4 hist
regime4.Sigma.known.hist <- reshape2::melt(dicker.Sigma.known.sample[c("n1000d1200")]) %>% 
  rename(estimand=Var1, scenario=L1) %>% select(-Var2)

plugin.Sigma.unknown.regime4.hist <- ggplot(data=regime4.Sigma.known.hist %>% 
                                              mutate(scenario=case_when(scenario=="n1000d1200"~"n=1000, d=1200"))) +
  geom_histogram(mapping=aes(x=value), bins=40, color="black") +
  scale_y_continuous(name="Count") + 
  scale_x_continuous(name="Residual variance",
                     limits=c(0.25,1.75)) + 
  theme_classic() + scale_color_aaas() + theme(text = element_text(size=30),
                                               legend.title = element_text(size=14), 
                                               legend.text = element_text(size=12),
                                               legend.justification=c(0,1), 
                                               legend.position=c(0.01, .99))
ggsave(plugin.Sigma.unknown.regime4.hist, file=here("figures","fig-regime4-hist.pdf"),
       width=14, height=7)
