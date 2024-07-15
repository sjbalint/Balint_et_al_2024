rm(list=ls())

# install packages --------------------------------------------------------

library(bibtex)

write.bib(c('tidyverse',
            'vegan',
            "ggsci",
            "cluster",
            "factoextra",
            "dendextend",
            "readxl",
            "car",
            "dunn.test",
            "pgirmess",
            "heplots",
            "rstatix",
            "cowplot",
            "ggsignif",
            "ggpubr",
            "ggrepel",
            "ggtukey",
            "RColorBrewer",
            "scales",
            "timevis"
            ), file='references') ## for multiple packages

write.bib(c(            "scales",
                        "timevis"
), file='references') ## for multiple packages
