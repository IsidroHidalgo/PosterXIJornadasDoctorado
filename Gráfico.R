ggplot(data = municipios) +
  geom_sf(fill="#30AF80", colour = gris_muni) + theme_bw() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

library(viridis)
lollipop <- municipios |>
  mutate(indicador = round(indicador, 1)) |>
  select(indicador, name, cpro) |>
  arrange(desc(indicador))
lollipop <- rbind(head(lollipop, 20), tail(lollipop, 20))
lollipop$geometry <- NULL
lollipop$cpro <- viridis(length(lollipop$cpro))
grafA <- ggplot(lollipop, aes(x = reorder(name, indicador), y = indicador)) +
  geom_segment(aes(x = reorder(name, indicador),
                   xend = reorder(name, indicador),
                   y = 0, yend = indicador),
               color = lollipop$cpro, lwd = 1) +
  geom_point(size = 7.5, pch = 21, bg = lollipop$cpro, col = 1) +
  geom_text(aes(label = indicador),
            color = c(rep("white", 23), rep("black", 17)), size = 3) +
  coord_flip() + xlab("") + ylab("") + ylim(0,100) +
  theme(plot.margin = unit(c(0.28, 0.1, 0.1, 0.1),"inches"))
municipios$riesgo_clm_sPCA_EN <- factor(municipios$riesgo_clm_sPCA,
                                        labels = c("No Risk", "Medium Risk",
                                                   "High Risk", "Extreme Risk"))
grafB <- ggplot(data = municipios) +
  geom_sf(aes(fill = riesgo_clm_sPCA_EN), colour = gris_muni) +
  scale_fill_discrete(type = colores_riesgo_viridis) +
  theme(legend.position="bottom") +
  easy_add_legend_title("Depopulation Risk: ")

png(filename = "Fig5-Lollipop-sDRI.png", width = 7644, height = 4608,
    units = "px", res = 600)
ggarrange(grafA, grafB, common.legend = TRUE, legend = "bottom", widths = c(1, 1.19))
dev.off()

png(filename = "ga-sDRI.png", width = 2000, height = 2000,
    units = "px", res = 600)
ggplot(data = municipios) +
  geom_sf(aes(fill = riesgo_clm_sPCA_EN), colour = gris_muni) +
  scale_fill_discrete(type = colores_riesgo_viridis) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background = element_rect(fill='transparent'),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
dev.off()


library(ggallin)
municipios$riesgo_clm_sPCA_EN <- factor(municipios$riesgo_clm_sPCA,
                                        labels = c("No risk", "Medium risk",
                                        "High risk", "Extreme Risk"))

png(filename = "sDRI.png", width = 9216, height = 9216, units = "px", res = 1200)
ggplot(data = municipios) +
  geom_sf(aes(fill = riesgo_clm_sPCA_EN), colour = gris_muni) +
  scale_fill_discrete(type = colores_riesgo_viridis) +
  theme_bw() +
  theme(legend.position="bottom") +
  easy_add_legend_title("Depopulation Risk: ")
dev.off()

tasa_crec <- cut(datos$tasa_crec_dem,
                 breaks = c(-100, -20, -10, -5, 0, 5, 20, 2500),
                 labels = c("loss >20%", "loss 10-20%",
                            "loss 5-10%",
                            "loss <5%", "gain <5%",
                            "gain 5-20%", "gain >20%"))

png(filename = "tasa_crec_dem.png", width = 9000, height = 7000, units = "px", res = 1200)
ggplot(data = municipios) +
  geom_sf(aes(fill = tasa_crec), color = gris_muni) +
  scale_fill_discrete(type = gama_viridis) +
  geom_sf(data = nucleos, col = gris_muni, size = .001) +
  easy_add_legend_title("Growth Rate: ") +
  theme_bw()
dev.off()

png(filename = "result_1.png", width = 5100, height = 3400, units = "px", res = 600)
plot(semivariograma, col= azul, xlab= "distance(m)", ylab= "semivariance",
     pch= 19, main = "(a)", col.lab = azul, col.axis = azul)
lines.variomodel(cov.model= "sph", cov.pars= c(1751.889, 64820.44), lw = 3,
                 nug= 1677.458, max.dist= 80000, col= naranja, lty= "dashed")
dev.off()


png(filename = "result_2.png", width = 5100, height = 3400, units = "px", res = 600)
barplot(fit_sACP$eig, main = "(b)",
        col = spectral(12), names.arg = varianza, xaxs = "i", cex.names = .75,
        xlab = "explained variance", col.lab = azul, col.axis = azul,
        cex.axis = .75)
dev.off()

png(filename = "result_3.png", width = 15300, height = 10200, units = "px", res = 600)
plot(fit_sACP, main = "(c)", col.lab = azul, col.axis = azul)
dev.off()

png(filename = "result_4.png", width = 5100, height = 3400, units = "px", res = 600)
ggplot(lollipop, aes(x = reorder(name, indicador), y = indicador)) +
  geom_segment(aes(x = reorder(name, indicador),
                   xend = reorder(name, indicador),
                   y = 0, yend = indicador),
               color = "gray", lwd = 1) +
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
  geom_text(aes(label = indicador), color = "white", size = 3) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_minimal()
dev.off()
