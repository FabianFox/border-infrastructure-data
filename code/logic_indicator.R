# Create a codebook for the 'Border Infrastructure Data'

# Load/install packages
### ------------------------------------------------------------------------ ###
if(!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "DiagrammeR")

# Load data
### ------------------------------------------------------------------------ ###
grViz(" 
digraph stakeholders {

graph [overlap = true, fontsize = 10, fontname = Montserrat]
rankdir = TB;

node [shape = box]
nodeA [label = 'Border\nbarrier'];
nodeB [label = 'BCP'];
nodeC [label = 'At specific\npoints only?'];
nodeD [label = 'Dismantling by\nagreement'];

{
rank=same
node [shape = circle, fixedsize = TRUE, width = 1.3]
nodeE [label = 'No-man’s-land\nborder'];
nodeF [label = 'Landmark\nborder'];
nodeG [label = 'Checkpoint\nborder'];
nodeH [label = 'Barrier\nborder'];
nodeI [label = 'Fortified\nborder'];
}

nodeA->nodeB [label=' No', fontsize = 12];
nodeA->nodeC [label=' Yes', fontsize =12];
nodeB->nodeD [label=' No', fontsize = 12];
nodeB->nodeG [label=' Yes', fontsize =12];
nodeC->nodeH [label=' Yes', fontsize =12];
nodeC->nodeI [label=' No', fontsize = 12];
nodeD->nodeE [label=' No', fontsize = 12];
nodeD->nodeF [label=' Yes', fontsize =12];

}")
