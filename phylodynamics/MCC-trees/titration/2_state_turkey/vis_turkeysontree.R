library(ggtree)
library(treeio)

setwd("~/Documents/HPAI/HPAI_2344b/DTA-BEAST/RF/results/RF-dta_2stateturkey/bybincturkey/")
# Load your BEAST tree
beast_tree <- read.beast("1_2/RF_Wbybturk_1_2.mcc.tree")

# Plot the tree, color branches by the 'domwild' annotation
p <- ggtree(beast_tree, right=TRUE, mrsd="2023-08-11", aes(color = domwild)) +
  theme_tree2()

# Annotate tips with "turkey" or "domestic"
p <- p + geom_point2(aes(subset = grepl("turkey", label) & grepl("domestic", label)), 
                     size = 2, shape = 15, color = "red" 
                     )  +
  scale_color_manual(name = "Host", values = c("wild" ="#2664A5", "domestic" = "#5CA7A4")) +
  ggtitle("1:2")


print(p)