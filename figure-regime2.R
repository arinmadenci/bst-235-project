# setup
dat.regime2.sigma.plot <- bind_rows(plugin.Sigma.known.plot$sigma %>% mutate(estimator="plugin") %>% 
                                      filter(scenario %in% c("n1000d10","n1000d50","n1000d500")),
                                    dicker.Sigma.known.plot$sigma %>% mutate(estimator="dicker") %>% 
                                      filter(scenario %in% c("n1000d10","n1000d50","n1000d500"))
)
dat.regime2.tau.plot <- bind_rows(plugin.Sigma.known.plot$tau %>% mutate(estimator="plugin") %>% 
                                      filter(scenario %in% c("n1000d10","n1000d50","n1000d500")),
                                    dicker.Sigma.known.plot$tau %>% mutate(estimator="dicker") %>% 
                                      filter(scenario %in% c("n1000d10","n1000d50","n1000d500"))
)

# Figure 3A: plug-in and Dicker estimator for \sigma^2
plugin.Sigma.known.regime2.plot_sigma <- ggplot(data=dat.regime2.sigma.plot %>% 
                                                  rename(Estimator=estimator) %>% 
                                                  mutate(Estimator=ifelse(Estimator=="dicker","Dicker","Plug-in"),
                                                         Estimator=fct_relevel(Estimator, "Plug-in"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=scenario, group=Estimator, color=Estimator), 
                  size=2, #show.legend = FALSE, 
                  position=position_dodge(width=0.5)) +
  geom_hline(yintercept=1, linetype="dashed") +
  ggrepel::geom_text_repel(mapping=aes(x=scenario,
                                       y=ifelse(Estimator=="Dicker",lower-0.05,lower),
                                       label = paste0(Estimator, ": ", round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
                           position=position_dodge2(width=1),
                           #show.legend = FALSE
                           ) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.75,1.25)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=1000, d=10","n=1000, d=50","n=1000, d=500")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plugin.Sigma.known.regime2.plot_sigma, file=here("figures","fig-sigma-regime2.pdf"),
       width=14, height=7)







# Figure 3B: plug-in estimator for \tau^2
plugin.Sigma.known.regime2.plot_tau <- ggplot(data=dat.regime2.tau.plot %>% 
                                                rename(Estimator=estimator) %>% 
                                                mutate(Estimator=ifelse(Estimator=="dicker","Dicker","Plug-in"),
                                                       Estimator=fct_relevel(Estimator, "Plug-in"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=scenario, group=Estimator, color=Estimator), 
                  size=2, #show.legend = FALSE, 
                  position=position_dodge(width=0.5)) +
  geom_hline(yintercept=1, linetype="dashed") +
  ggrepel::geom_text_repel(mapping=aes(x=scenario,
                                       y=ifelse(Estimator=="Dicker",lower-0.05,lower),
                                       label = paste0(Estimator, ": ", round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
                           position=position_dodge2(width=1),
                           #show.legend = FALSE
  ) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.75,1.25)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=1000, d=10","n=1000, d=50","n=1000, d=500")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plugin.Sigma.known.regime2.plot_tau, file=here("figures","fig-tau-regime2.pdf"),
       width=14, height=7)
