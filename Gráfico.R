municipios$riesgo_clm_sPCA_EN <- factor(municipios$riesgo_clm_sPCA,
                                        labels = c("No risk", "Low risk", "Medium risk",
                                        "High risk"))
png(filename = "sDRI.png", width = 9216, height = 9216, units = "px", res = 1200,
    bg = )
ggplot(data = municipios) +
  geom_sf(aes(fill = riesgo_clm_sPCA_EN), colour = "grey") +
  scale_fill_discrete(type = colores_riesgo_azul) +
  ggtitle(label = "Depopulation risk in Castilla-La Mancha (sDRI indicator)") +
  theme(legend.position="bottom") +
  easy_add_legend_title("Risk: ")
dev.off()

png(filename = "tasa_crec_dem.png", width = 9000, height = 7000, units = "px", res = 1200)
ggplot(data = municipios) +
  geom_sf(aes(fill = datos$riesgo_CLM_EN), colour = "grey") +
  scale_fill_discrete(type = gama_JCCM) +
  easy_add_legend_title("Type of zone: ")
dev.off()


grafA <- ggplot(data = municipios) +
  geom_sf(aes(fill = riesgo_clm_sPCA_EN), colour = "grey") +
  scale_fill_discrete(type = colores_riesgo_azul) +
  geom_sf(data = nucleos, col = "grey", size = .001) +
  ggtitle(label = "Depopulation risk in Castilla-La Mancha",
          subtitle = "(sDRI indicator)") +
  theme(legend.position="bottom") +
  easy_add_legend_title("Risk: ")

grafB <- ggplot(data = municipios) +
  geom_sf(aes(fill = datos$riesgo_CLM_EN), colour = "grey") +
  scale_fill_discrete(type = gama_JCCM) +
  geom_sf(data = nucleos, col = "grey", size = .001) +
  ggtitle(label = "",
          subtitle = "(Counter-depopulation strategy)") +
  easy_add_legend_title("Type of zone: ")

ggarrange(grafA, grafB, widths = c(1, 1.08))

# En español:
grafC <- ggplot(data = municipios) +
  geom_sf(aes(fill = riesgo_clm_sACP), colour = "grey") +
  scale_fill_discrete(type = colores_riesgo) +
  geom_sf(data = nucleos, col = "grey", size = .001) +
  ggtitle(label = "Riesgo de despoblamiento en CLM",
          subtitle = "(Según indicador sACP60km)") +
  easy_add_legend_title("Riesgo: ")

grafD <- ggplot(data = municipios) +
  geom_sf(aes(fill = datos$riesgo_CLM), colour = "grey") +
  scale_fill_discrete(type = gama_JCCM) +
  geom_sf(data = nucleos, col = "grey", size = .001) +
  ggtitle(label = "",
          subtitle = "(Estrategia contra la despoblación)") +
  easy_add_legend_title("Tipo de zona: ")

ggarrange(grafC, grafD, widths = c(1, 1.05))
