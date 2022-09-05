

library(ggplot2)
library(gridExtra)
library(grid)

# fig 1
load("Result/consistency_3week_low_medium_high.Rda")
load("Result/consistency_3week_low_medium_high_modified_threshold.Rda")
load("Result/consistency_3week_low_medium_high_modified_threshold_b5.Rda")

fig1 = grid.arrange(fig_consis_3week_box_LMH,
                    fig_consis_3week_box_LMH_MT_b5,
                    fig_consis_3week_box_LMH_MT,
                    nrow = 1, 
                    ncol = 3,
                    top = textGrob("A) Distributions of 3-week community risk level consistency rates\n",
                                    gp = gpar(fontsize = 18),
                                   x = 0,
                                   hjust = 0))

ggsave("Result/Figures/fig1.jpg",
       fig1, 
       height=2,width=7,scale=1.65)



#fig 2
load("Result/consistency_3week_low_medium_high_suppNA.Rda")
load("Result/consistency_3week_low_medium_high_MT_suppNA_b5.Rda")
load("Result/consistency_3week_low_medium_high_MT_suppNA.Rda")

fig2 = grid.arrange(fig_consis_3week_box_LMH_suppNA,
                    fig_consis_3week_box_LMH_MT_suppNA_b5,
                    fig_consis_3week_box_LMH_MT_suppNA,
                    nrow = 1, 
                    ncol = 3,
                    top = textGrob("B) Distributions of 3-week community risk level consistency rates (without suppressed hospital data)\n",
                                   gp = gpar(fontsize = 18),
                                   x = 0,
                                   hjust = 0))

ggsave("Result/Figures/fig2.jpg",
       fig2, 
       height=2,width=7,scale=1.65)



fig_merge = grid.arrange(fig1,
                    fig2,
                    nrow = 2, 
                    ncol = 1)

ggsave("Result/Figures/fig_merge.jpg",
       fig_merge, 
       height=4,width=7,scale=1.65)

