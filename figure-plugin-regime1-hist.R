plugin.Sigma.known.hist <- reshape2::melt(plugin.Sigma.known.sample[c("n10000d10", "n10000d50", "n10000d500")]) %>% 
  rename(estimand=Var1, statistic=Var2, scenario=L1)

plugin.Sigma.known.regime1.hist<- ggplot(data=plugin.Sigma.known.hist %>% 
                                           mutate(scenario=case_when(scenario=="n10000d10"~"n=10000, d=10",
                                                                     scenario=="n10000d50"~"n=10000, d=50",
                                                                     scenario=="n10000d500"~"n=10000, d=500"))) +
  geom_histogram(mapping=aes(x=value), bins=40, fill="grey", color="black") +
  facet_grid(rows = vars(estimand), cols = vars(scenario)) +
  scale_y_continuous(name="Count") + 
  scale_x_continuous(name="Residual variance",
                     limits=c(0.7,1.3)) + 
  theme_classic() + scale_color_aaas() + theme(text = element_text(size=30))
ggsave(plugin.Sigma.known.regime1.hist, file=here("figures","fig1-plugin-regime1-hist.pdf"),
       width=14, height=7)
