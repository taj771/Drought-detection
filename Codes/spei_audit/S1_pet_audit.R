# =============================================================================
# S1 — PET sanity audit (Thornthwaite) + S3 P vs PET trend decomposition
# Supervisor concern: "is our ET calculation correct?"
# Thornthwaite PET = monthly_precip - water_balance  (water_balance = P - PET).
# Checks:
#   (1) PET magnitude sane? Benin expect ~1300-1700 mm/yr.
#   (2) Does PET RISE 1973-2023 with warming? (Thornthwaite is temp-only.)
#   (3) Compare Thornthwaite PET vs CRU Penman PET (trusted).
#   (4) Decompose P-PET trend: is the drying driven by P down or PET up?
# Local run. Reads speit + spei_penman_CRU parquets.
# =============================================================================
suppressMessages({library(arrow);library(dplyr);library(tidyr);library(ggplot2)})
setwd("c:/Users/tajayalath/Box/R - Projects - Tharaka/Drought-indices")
OUT <- "Codes/spei_audit/out"; FIG <- file.path(OUT,"figs")
REF0<-1973; REF1<-2002; OBS0<-2003; OBS1<-2023

spt <- as.data.frame(read_parquet("Codes/_archive/_data/speit.parquet")) |>
  filter(year>=REF0, year<=OBS1) |>
  mutate(pet_thorn = monthly_precip - water_balance)   # P - (P-PET) = PET

cru <- as.data.frame(read_parquet("Codes/_archive/_data/spei_penman_CRU.parquet")) |>
  filter(year>=REF0, year<=OBS1) |>
  select(x,y,year,month,pet_cru=pet_mm, precip_cru=monthly_precip)

# ---- (1) magnitude: annual PET mm/yr, mean over pixels ----------------------
ann <- spt |> group_by(x,y,year) |>
  summarise(pet_yr=sum(pet_thorn), P_yr=sum(monthly_precip),
            temp_yr=mean(mean_temp_monthly), .groups="drop") |>
  group_by(year) |>
  summarise(PET=mean(pet_yr), P=mean(P_yr), Temp=mean(temp_yr), .groups="drop")

cru_ann <- cru |> group_by(x,y,year) |>
  summarise(pet_yr=sum(pet_cru), .groups="drop") |>
  group_by(year) |> summarise(PET_CRU=mean(pet_yr), .groups="drop")
ann <- ann |> left_join(cru_ann, by="year")

cat("=== Annual PET (mm/yr), Thornthwaite vs CRU-Penman, mean over 154 px ===\n")
cat(sprintf("Thornthwaite: mean %.0f  (Ref %.0f, Obs %.0f)\n",
    mean(ann$PET), mean(ann$PET[ann$year<=REF1]), mean(ann$PET[ann$year>=OBS0])))
cat(sprintf("CRU-Penman  : mean %.0f  (Ref %.0f, Obs %.0f)\n",
    mean(ann$PET_CRU,na.rm=TRUE),
    mean(ann$PET_CRU[ann$year<=REF1],na.rm=TRUE),
    mean(ann$PET_CRU[ann$year>=OBS0],na.rm=TRUE)))
cat(sprintf("Temp (C)    : Ref %.2f  Obs %.2f  (warming %+.2f C)\n",
    mean(ann$Temp[ann$year<=REF1]), mean(ann$Temp[ann$year>=OBS0]),
    mean(ann$Temp[ann$year>=OBS0])-mean(ann$Temp[ann$year<=REF1])))

# linear trends per decade
trend <- function(y,x) round(coef(lm(y~x))[2]*10,2)
cat("\n=== trends per decade (1973-2023) ===\n")
cat(sprintf("PET Thornthwaite : %+.1f mm/decade\n", trend(ann$PET, ann$year)))
cat(sprintf("PET CRU-Penman   : %+.1f mm/decade\n", trend(ann$PET_CRU, ann$year)))
cat(sprintf("Precip           : %+.1f mm/decade\n", trend(ann$P, ann$year)))
cat(sprintf("Temp             : %+.2f C/decade\n", trend(ann$Temp, ann$year)))
cat(sprintf("P - PET (Thorn)  : %+.1f mm/decade\n", trend(ann$P-ann$PET, ann$year)))

write.csv(ann, file.path(OUT,"S1_annual_pet_P_temp.csv"), row.names=FALSE)

# ---- fig: PET Thornthwaite vs CRU + temp, 1973-2023 -------------------------
g1 <- ann |> select(year, `Thornthwaite`=PET, `CRU-Penman`=PET_CRU) |>
  pivot_longer(-year, names_to="PET method", values_to="mm") |>
  ggplot(aes(year, mm, colour=`PET method`)) +
  annotate("rect", xmin=OBS0-0.5, xmax=OBS1+0.5, ymin=-Inf, ymax=Inf,
           fill="grey88", alpha=0.5) +
  geom_line(linewidth=0.7) + geom_smooth(method="lm", se=FALSE, linetype=2, linewidth=0.4) +
  scale_colour_manual(values=c("Thornthwaite"="#c0392b","CRU-Penman"="#2c6fb5")) +
  labs(title="Annual PET: Thornthwaite vs CRU-Penman (Benin, 154-px mean)",
       subtitle="Grey = Observation period (2003-2023). Dashed = linear trend.",
       x=NULL, y="PET (mm/yr)") + theme_bw(base_size=11) +
  theme(legend.position="bottom", panel.grid.minor=element_blank())
ggsave(file.path(FIG,"S1_pet_thorn_vs_cru.png"), g1, width=8, height=4.5, dpi=150, bg="white")

# ---- fig: P, PET, P-PET decomposition (Thornthwaite) ------------------------
g2 <- ann |> transmute(year, Precip=P, PET=PET, `P - PET`=P-PET) |>
  pivot_longer(-year, names_to="term", values_to="mm") |>
  mutate(term=factor(term, levels=c("Precip","PET","P - PET"))) |>
  ggplot(aes(year, mm, colour=term)) +
  annotate("rect", xmin=OBS0-0.5, xmax=OBS1+0.5, ymin=-Inf, ymax=Inf,
           fill="grey88", alpha=0.5) +
  geom_line(linewidth=0.7) + geom_smooth(method="lm", se=FALSE, linetype=2, linewidth=0.4) +
  scale_colour_manual(values=c("Precip"="#2c6fb5","PET"="#c0392b","P - PET"="#555555")) +
  labs(title="Water-balance decomposition (Thornthwaite): P, PET, P-PET",
       subtitle="Is the Observation-period drying driven by less rain or more PET?",
       x=NULL, y="mm/yr") + theme_bw(base_size=11) +
  theme(legend.position="bottom", panel.grid.minor=element_blank())
ggsave(file.path(FIG,"S1_water_balance_decomp.png"), g2, width=8, height=4.5, dpi=150, bg="white")

cat("\nFigures ->", FIG, "\n")
