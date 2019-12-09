# figure for regime 3: d/n to rho <1 with unknown \Sigma

# setup 
dat.regime3.sigma.plot <- bind_rows(dicker.Sigma.unknown_sample.plot$sigma %>% mutate(estimator="sample") %>% 
                                      filter(scenario %in% c("n1000d10","n1000d50","n1000d500")),
                                    dicker.Sigma.unknown_banded.plot$sigma %>% mutate(estimator="banded") %>% 
                                      filter(scenario %in% c("n1000d10","n1000d50","n1000d500"))
)
dat.regime3.tau.plot <- bind_rows(dicker.Sigma.unknown_sample.plot$tau %>% mutate(estimator="sample") %>% 
                                    filter(scenario %in% c("n1000d10","n1000d50","n1000d500")),
                                  dicker.Sigma.unknown_banded.plot$tau %>% mutate(estimator="banded") %>% 
                                    filter(scenario %in% c("n1000d10","n1000d50","n1000d500"))
)


# regime 3: empiric Sigma and banded: sigma^2
Sigma.unknown.regime3.plot_sigma <- ggplot(data=dat.regime3.sigma.plot %>% 
                                                  rename(Estimator=estimator) %>% 
                                                  mutate(Estimator=ifelse(Estimator=="banded","Banded","Empiric"),
                                                         Estimator=fct_relevel(Estimator, "Empiric"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=scenario, group=Estimator, color=Estimator), 
                  size=2, #show.legend = FALSE, 
                  position=position_dodge(width=0.5)) +
  geom_hline(yintercept=1, linetype="dashed") +
  ggrepel::geom_text_repel(mapping=aes(x=scenario,
                                       y=ifelse(Estimator=="Banded",upper+0.1,upper),
                                       label = paste0(Estimator, ": ", round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
                           position=position_dodge2(width=1),
                           #show.legend = FALSE
  ) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.75,1.75)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=1000, d=10","n=1000, d=50","n=1000, d=500")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Sigma.unknown.regime3.plot_sigma, file=here("figures","fig-sigma-regime3.pdf"),
       width=14, height=7)


# regime 3: empiric Sigma and banded: tau^2
Sigma.unknown.regime3.plot_tau <- ggplot(data=dat.regime3.tau.plot %>% 
                                             rename(Estimator=estimator) %>% 
                                             mutate(Estimator=ifelse(Estimator=="banded","Banded","Empiric"),
                                                    Estimator=fct_relevel(Estimator, "Empiric"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=scenario, group=Estimator, color=Estimator), 
                  size=2, #show.legend = FALSE, 
                  position=position_dodge(width=0.5)) +
  geom_hline(yintercept=1, linetype="dashed") +
  ggrepel::geom_text_repel(mapping=aes(x=scenario,
                                       y=ifelse(Estimator=="Banded",upper+0.1,upper),
                                       label = paste0(Estimator, ": ", round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
                           position=position_dodge2(width=1),
                           #show.legend = FALSE
  ) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.25,1.5)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=1000, d=10","n=1000, d=50","n=1000, d=500")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Sigma.unknown.regime3.plot_tau, file=here("figures","fig-tau-regime3.pdf"),
       width=14, height=7)
