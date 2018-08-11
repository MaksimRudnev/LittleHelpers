# Documentation for brunching
#
# library(DiagrammeR)
# grViz(engine="neato", diagram='
#
# digraph savengo_g {
# size="8,10";
#       ratio=0.9
#
#
#
#       node[shape = "rect", fillcolor="white,rounded"]
#       edge[fontname="Courier", fontsize=10]
#
#       dataset -> ramify[dir=none, minlen=1]
#       ramify -> means [xlabel="branch(1)"]
#       ramify -> regression[label=" branch(2)"]
#       regression -> tidy
#       ramify -> correlations  [label="branch(3)"]
#       correlations -> ramify2[dir=none, minlen=1]
#       ramify2 -> rcor [label="branch(1,2)"]
#       ramify2 -> zcor[label="branch(2,2)"]
#
#       {rank=same; means, regression, correlations;}
#       {rank=same; tidy, rcor, zcor;}
#       {rcor zcor} -> harvest2
#       {means tidy harvest2} -> harvest
#
#       harvest -> list[dir=none]
#
#
#       tidy[label="tidy coefficients"]
#       rcor[label="rounded correlations"]
#       zcor[label="z-transformed correlations"]
#       ramify[label="ramify(1)", fontname="Courier", fontsize=10, fillcolor="#11111140", style="filled"]
#       ramify2[label="ramify(2)", fontname="Courier", fontsize=10, fillcolor="#11111140", style="filled"]
#       harvest[label="harvest(1)", fontname="Courier", fontsize=10, fillcolor="#11111140", style="filled"]
#       harvest2[label="harvest(2)", fontname="Courier", fontsize=10, fillcolor="#11111140", style="filled"]
#       list[label="List of means, \ntidy coefficients, \nrounded and \nz-transformed\ncorrelations."]
#
#
#
#       }
#
#
#       ')
