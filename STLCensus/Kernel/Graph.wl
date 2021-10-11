

BeginPackage["STLCensus`"]

STLCensus`$BlockGraph
STLCensus`WardGraphPlot

Begin["`Private`"]

STLCensus`$BlockGraph:=$blockGraph

$blockGraph:=$blockGraph=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Graphs"
    ],
    "BlockGraph2Pts.wxf"
    }]];

blockNeighbors[geoid_]:=Complement[VertexList@NeighborhoodGraph[$blockGraph, geoid,1],Flatten[{geoid}]]

connectedGroupsQ[groups:{_List..}]:=AllTrue[groups,ConnectedGraphQ[Subgraph[$blockGraph,#]]&]


STLCensus`WardGraphPlot[groups_]:=GraphPlot[EdgeList[$BlockGraph], 
    VertexStyle -> Flatten[Thread[groups[[#]] -> ColorData[14, Mod[#,14]+1]] & /@ Range[Length[groups]]]]

End[]

EndPackage[]