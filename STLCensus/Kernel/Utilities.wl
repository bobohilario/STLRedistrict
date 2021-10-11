
BeginPackage["STLCensus`"]

Clear[STLCensus`$BlockIDs,STLCensus`$BlockDataset];

STLCensus`$BlockIDs
STLCensus`$BlockDataset

Begin["`Private`"]


$BlockIDs:=
$BlockIDs=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Datasets"
    ],
    "STLGeoIDs.wxf"
    }]];



$BlockDataset:=
$BlockDataset=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Datasets"
    ],
    "CombinedDataset.wxf"
    }]];

$BlockPopulations:=
$BlockPopulations=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Datasets"
    ],
    "BlockPopulations.wxf"
    }]];


$BlockVotingPopulations:=
$BlockVotingPopulations=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Datasets"
    ],
    "BlockVotingPopulations.wxf"
    }]];

$SingleRacePopulations:=
$SingleRacePopulations=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Datasets"
    ],
    "SingleRacePopulations.wxf"
    }]];

STLCensus`$Debug=True
STLCensus`$DebugData=<||>;

debugLog[tag_,event_]:=(STLCensus`$DebugData[tag]=event)/;TrueQ[STLCensus`$Debug]
resetDebug[]:=STLCensus`$DebugData=<||>;

End[]

EndPackage[]