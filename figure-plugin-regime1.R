# Figure 1A: plug-in estimator for \sigma^2
plugin.Sigma.known.regime1.plot_sigma <- ggplot(data=plugin.Sigma.known.plot$sigma %>% 
                                                  filter(scenario %in% c("n10000d10","n10000d50","n10000d500"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower,ymax=upper, x=scenario, color=scenario), size=2, show.legend = FALSE) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_text(mapping=aes(x=scenario, y=mean, 
                        label = paste0(round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
            #vjust = "outward", hjust = "outward",
            nudge_y=.035, size=4,
            show.legend = FALSE) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.95,1.05)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=10000, d=10","n=10000, d=50","n=10000, d=500")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plugin.Sigma.known.regime1.plot_sigma, file=here("figures","fig1-plugin-sigma-regime1.pdf"),
       width=7, height=7)







# Figure 1B: plug-in estimator for \tau^2
plugin.Sigma.known.regime1.plot_tau <- ggplot(data=plugin.Sigma.known.plot$tau %>% 
                                                filter(scenario %in% c("n10000d10","n10000d50","n10000d500"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower,ymax=upper, x=scenario, color=scenario), size=2, show.legend = FALSE) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_text(mapping=aes(x=scenario, y=mean, 
                        label = paste0(round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
            #vjust = "outward", hjust = "outward",
            nudge_y=.035, size=4,
            show.legend = FALSE) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.95,1.05)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=10000, d=10","n=10000, d=50","n=10000, d=500")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(plugin.Sigma.known.regime1.plot_tau, file=here("figures","fig1-plugin-tau-regime1.pdf"),
       width=7, height=7)
