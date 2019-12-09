# figure for regime 4: d/n to rho >1 with known \Sigma

# setup 
dat.regime4.sigma.plot <- bind_rows(dicker.Sigma.known.plot$sigma %>% mutate(estimator="dicker") %>% filter(scenario %in% c("n1000d1200")),
                                    data.frame(t(eigenprism.Sigma.known.plot$tau)) %>% mutate(estimator="eigenprism", scenario="n1000d1200"))
dat.regime4.tau.plot <- bind_rows(dicker.Sigma.known.plot$tau %>% mutate(estimator="dicker") %>% filter(scenario %in% c("n1000d1200")),
                                  data.frame(t(eigenprism.Sigma.known.plot$tau)) %>% mutate(estimator="eigenprism", scenario="n1000d1200"))


# regime 4: d>n and known Sigma: sigma^2
Sigma.known.regime4.plot_sigma <- ggplot(data=dat.regime4.sigma.plot %>% 
                                         rename(Estimator=estimator) %>% 
                                         mutate(Estimator=ifelse(Estimator=="dicker","Dicker","EigenPrism"),
                                                Estimator=fct_relevel(Estimator, "Dicker"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=scenario, group=Estimator, color=Estimator), 
                  size=2, #show.legend = FALSE, 
                  position=position_dodge(width=0.5)) +
  geom_hline(yintercept=1, linetype="dashed") +
  ggrepel::geom_text_repel(mapping=aes(x=scenario,
                                       y=ifelse(Estimator=="Dicker",upper+0.1,upper),
                                       label = paste0(Estimator, ": ", round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
                           position=position_dodge2(width=1),
                           #show.legend = FALSE
  ) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.7,1.5)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=1000, d=1200")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Sigma.known.regime4.plot_sigma, file=here("figures","fig-sigma-regime4.pdf"),
       width=7, height=7)


# regime 4: d>n and known Sigma: tau^2
Sigma.known.regime4.plot_tau <- ggplot(data=dat.regime4.tau.plot %>% 
                                             rename(Estimator=estimator) %>% 
                                             mutate(Estimator=ifelse(Estimator=="dicker","Dicker","EigenPrism"),
                                                    Estimator=fct_relevel(Estimator, "Dicker"))) + 
  geom_pointrange(mapping=aes(y=mean, ymin=lower, ymax=upper, x=scenario, group=Estimator, color=Estimator), 
                  size=2, #show.legend = FALSE, 
                  position=position_dodge(width=0.5)) +
  geom_hline(yintercept=1, linetype="dashed") +
  ggrepel::geom_text_repel(mapping=aes(x=scenario,
                                       y=ifelse(Estimator=="Dicker",upper+0.1,upper),
                                       label = paste0(Estimator, ": ", round(mean, 3), " (", round(lower,3), "-", round(upper, 3), ")") ),
                           position=position_dodge2(width=1),
                           #show.legend = FALSE
  ) +
  scale_y_continuous(name="Residual variance",
                     limits=c(0.7,1.5)) + 
  scale_x_discrete(name="Scenario", 
                   labels=c("n=1000, d=1200")) +
  theme_classic() + scale_color_aaas() + 
  theme(text = element_text(size=30), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(Sigma.known.regime4.plot_tau, file=here("figures","fig-tau-regime4.pdf"),
       width=7, height=7)
