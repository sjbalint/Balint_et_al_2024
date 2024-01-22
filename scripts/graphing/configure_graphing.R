
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)


# create default graphing theme -------------------------------------------

basetheme <- list(
  theme_classic(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.top = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "bottom"),
  scale_x_continuous(position = "top"),
  scale_color_viridis_d(option="cividis", direction = -1, aesthetics = c("colour", "fill")),
  scale_shape_manual(values=c(21:26)),
  scale_linetype_manual(values=c(3,2,1))
)

save(basetheme,file="Rdata/basetheme.Rdata")


# create a dataframe for axis labels --------------------------------------

ylabels.df <- data.frame(name=c('location','depth.cm','%N', "d15N.permil", "%C.total",
                                'd13C.total',"%C.organic",'d13C.organic',"n","P.pct.inorg",
                                "P.total.pct.e2", "P.pct.org","N.P.ratio","C.N.ratio",
                                "SiO2.prct","Si.P.ratio", "Si.N.ratio",
                                "N.storage","year.mean","clay.pct","sand.pct","gravel.pct",
                                "median.grainsize.phi","accretion.rate.gcm2yr",
                                "mean.phi","sd.phi","C.P.ratio",
                                "137Cs_activity.bqkg","210Pb_excess.bqkg",
                                "137Cs_uncertainty.bqkg","210Pb_uncertainty.bqkg"),
                         factor1=as.character(
                           c(
                             bquote("Location"),
                             bquote(atop("Depth","(cm)")),
                             bquote("%"*"N"),
                             bquote(delta^15*N~'(‰)'),
                             bquote("%"*C[Total]),
                             bquote(delta^13*C[Total]~'(‰)'),
                             bquote("%"*C[Organic]),
                             bquote(delta^13*C[Organic]~'(‰)'),
                             bquote("Count"),
                             bquote("%"*P[inorg]),
                             bquote(P[Total]~x~10^-2~'(%)'),
                             bquote("%"*P[organic]),
                             bquote("N:P"~"Ratio"),
                             bquote("C:N"~"Ratio"),
                             bquote("BSi"~"(%)"),
                             bquote("Si:P"~"Ratio"),
                             bquote("Si:N"~"Ratio"),
                             bquote("N"~"Accumulation"~"Rate"),
                             bquote("Year"),
                             bquote("Clay"~"(%)"),
                             bquote("Sand"~"(%)"),
                             bquote("Gravel"~"(%)"),
                             bquote("Median"~"Grainsize"~"("*phi*")"),
                             bquote("Accretion"~"Rate"~"(g/"*cm^2*"/yr)"),
                             bquote("Mean"~"Grainsize"~"("*phi*")"),
                             bquote("Sorting"~"("*phi*")"),
                             bquote("C:P"~"Ratio"),
                             bquote(scriptstyle(atop(137,))*"Cs"~"Activity"~"(Bq/kg)"),
                             bquote(scriptstyle(atop(210,))*"Pb"~"Excess"~"(Bq/kg)"),
                             bquote(scriptstyle(atop(137,))*"Cs"~"Activity"~"(Bq/kg)"),
                             bquote(scriptstyle(atop(210,))*"Pb"~"Excess"~"(Bq/kg)")
                           )
                         ),
                         factor2=as.character(
                           c(
                             bquote("Location"),
                             bquote(atop("Depth","(cm)")),
                             bquote("%"*"N"),
                             bquote(delta^15*N~'(‰)'),
                             bquote("%"*C[Total]),
                             bquote(delta^13*C[Total]~'(‰)'),
                             bquote("%"*C[Organic]),
                             bquote(delta^13*C[Organic]~'(‰)'),
                             bquote("Count"),
                             bquote("%"*P[inorg]),
                             bquote(P[Total]~x~10^-2~'(%)'),
                             bquote("%"*P[organic]),
                             bquote("N:P"~"Ratio"),
                             bquote("C:N"~"Ratio"),
                             bquote("BSi"~"(%)"),
                             bquote("Si:P"~"Ratio"),
                             bquote("Si:N"~"Ratio"),
                             bquote(atop("N"~"Accum.","Rate")),
                             bquote("Year"),
                             bquote("Clay"~"(%)"),
                             bquote("Sand"~"(%)"),
                             bquote("Gravel"~"(%)"),
                             bquote(atop("Median","Grainsize"~"("*phi*")")),
                             bquote(atop("Accretion","Rate"~"(g/"*cm^2*"/yr)")),
                             bquote(atop("Mean","Grainsize"~"("*phi*")")),
                             bquote("Sorting"~"("*phi*")"),
                             bquote("C:P"~"Ratio"),
                             bquote(scriptstyle(atop(137,))*"Cs"~"Activity"~"(Bq/kg)"),
                             bquote(scriptstyle(atop(210,))*"Pb"~"Excess"~"(Bq/kg)"),
                             bquote(scriptstyle(atop(137,))*"Cs"~"Activity"~"(Bq/kg)"),
                             bquote(scriptstyle(atop(210,))*"Pb"~"Excess"~"(Bq/kg)")
                           )
                         )
)

save(ylabels.df,file="Rdata/graphing_labels.Rdata")