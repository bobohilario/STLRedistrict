

BeginPackage["STLCensus`"]

STLCensus`$ScoreMultipliers

STLCensus`WardScore
STLCensus`RandomSwap
STLCensus`$WardCount=14;

STLCensus`AutoDistict

Begin["`Private`"]

$ScoreMultipliers=<|
        "Continuous"->10^6,
        "Population"->5,
        "VoterPopulation"->1,
        "Race"->1,
        "Compactness"->1/2
|>

STLCensus`WardScore[args___]:=Catch[createScore[args]]

createScore[groups_]:=createScore[groups,None];
createScore[groups:{_List..},statusid_]:=
    With[{
        popscore=(
            censusProcessDetails[statusid]:="Population Scores";
            populationScore[groups]),
        votepopscore=(
            censusProcessDetails[statusid]:="Voting Population Scores";
            votingAgePopulationScore[groups]
            )
        ,
        racescore=(
            censusProcessDetails[statusid]:="Race Scores";
            Total[raceScores[groups]]),
        compactscore=(
            censusProcessDetails[statusid]:="Compactness Scores";
            compactnessScore[groups]),
        cont=connectedGroupsQ[groups]
    },

    censusProcessDetails[statusid]:="Completed Scoring";
    debugLog["found scores",True];
    applyMultipliers[Association@@{
        "Continuous"->(Boole[cont]-1),
        "Population"->popscore,
        "VoterPopulation"->votepopscore,
        "Race"->racescore,
        "Compactness"->compactscore
    }]
    ]

applyMultipliers[as_]:=Append[#,"Total"->Total[#]]&[$ScoreMultipliers*as]


populationScore[groups_]:=With[{grpop=groupPopulation/@groups},
    1/(1+N@Variance[grpop/Mean[grpop]])
]

votingAgePopulationScore[groups_]:=With[{grpop=groupVotingPopulation/@groups},
    1/(1+N@Variance[grpop/Mean[grpop]])
]
groupPopulation[geoids_]:=Total@Lookup[$BlockPopulations,geoids,0]
groupVotingPopulation[geoids_]:=Total@Lookup[$BlockVotingPopulations,geoids,0]

raceScores[groups_]:=With[{grrace=groupRace/@groups},
    N@Flatten[{
        (Count[grrace,_?((#["P3_004N"]/#["P1_001N"])>(1/2)&)]/14)/($raceCategoryTotal["P3_004N"]/$totalPopulation),
        Max[Lookup[grrace,#,0]]/$raceCategoryTotal[#]&/@{"P3_005N","P3_006N","P3_007N","P3_008N"}
    }]
]

groupRace[geoids_]:=Total[KeyTake[$SingleRacePopulations,geoids]]


compactnessScore[groups_]:=Module[{$ii=1},
    With[{crcomp=regionCompactness/@groups},
    debugLog["compactnessScore",True];
    ( Mean[crcomp]+Min[crcomp])/2
    ]
]


$totalPopulation=301578;



STLCensus`RandomSwap[args___]:=Catch[randomSwap[args]]

$DefaultSwapCount=50;

randomSwap[groups_]:=randomSwap[groups,$DefaultSwapCount]

randomSwap[groups0_,n_]:=Module[{groups=groups0},
    Do[
        groups=randomswap[groups],
        {i,n}];
    groups
]

randomswap[groups0_]:=Module[{rand=RandomInteger[{1,$WardCount}], swapid,groups},
    swapid=RandomChoice[blockNeighbors[groups0[[rand]]]];
    groups=DeleteCases[groups0,swapid,{2}];
    groups[[rand]]=Append[groups[[rand]],swapid];
    If[connectedGroupsQ[groups],groups,groups0]
]

$raceCategoryTotal=<|"P3_001N" -> 247547, "P3_002N" -> 234455, "P3_003N" -> 118016, 
 "P3_004N" -> 99043, "P3_005N" -> 768, "P3_006N" -> 10944, 
 "P3_007N" -> 81, "P3_008N" -> 5603, "P3_009N" -> 13092, 
 "P3_010N" -> 12144, "P3_011N" -> 2297, "P3_012N" -> 2166, 
 "P3_013N" -> 1447, "P3_014N" -> 69, "P3_015N" -> 4460, 
 "P3_016N" -> 814, "P3_017N" -> 182, "P3_018N" -> 17, 
 "P3_019N" -> 501, "P3_020N" -> 20, "P3_021N" -> 0, "P3_022N" -> 63, 
 "P3_023N" -> 36, "P3_024N" -> 67, "P3_025N" -> 5, "P3_026N" -> 841, 
 "P3_027N" -> 363, "P3_028N" -> 61, "P3_029N" -> 4, "P3_030N" -> 109, 
 "P3_031N" -> 24, "P3_032N" -> 0, "P3_033N" -> 131, "P3_034N" -> 50, 
 "P3_035N" -> 45, "P3_036N" -> 0, "P3_037N" -> 12, "P3_038N" -> 0, 
 "P3_039N" -> 28, "P3_040N" -> 6, "P3_041N" -> 5, "P3_042N" -> 0, 
 "P3_043N" -> 0, "P3_044N" -> 0, "P3_045N" -> 0, "P3_046N" -> 3, 
 "P3_047N" -> 87, "P3_048N" -> 23, "P3_049N" -> 0, "P3_050N" -> 47, 
 "P3_051N" -> 6, "P3_052N" -> 2, "P3_053N" -> 0, "P3_054N" -> 6, 
 "P3_055N" -> 1, "P3_056N" -> 0, "P3_057N" -> 0, "P3_058N" -> 0, 
 "P3_059N" -> 2, "P3_060N" -> 0, "P3_061N" -> 0, "P3_062N" -> 0, 
 "P3_063N" -> 16, "P3_064N" -> 9, "P3_065N" -> 6, "P3_066N" -> 0, 
 "P3_067N" -> 0, "P3_068N" -> 0, "P3_069N" -> 1, "P3_070N" -> 4, 
 "P3_071N" -> 4|>;


STLCensus`AutoDistict[args___]:=Catch[autoDistrict[args]]

autoDistrict[]:=autoDistrict[FindGraphPartition[$BlockGraph, $WardCount]]

autoDistrict[l_List]:=autoDistrict["Steps",l]

autoDistrict[mode_String]:=autoDistrict[mode,FindGraphPartition[$BlockGraph, $WardCount]]

autoDistrict["Steps",init_]:=(
resetDebug[];
DynamicModule[{stop=False,groups=init,map="",tempmap="",
score=Null,
status=Row[{"Creating initial wards",ProgressIndicator[Appearance -> "Percolate"]}],tempgroups,tempscore="",
$attempts=0,$accepted=0},

    Panel[Grid[
       { {Style["Autodistricting",Bold,20],SpanFromLeft},
        {Dynamic@status,SpanFromLeft},
        {Dynamic@Row[{"swaps accepted/attempted (",$accepted,"/",$attempts,")"}],SpanFromLeft},
        {Style["Current Wards",FontFamily->"Source Sans Pro",Italic],Style["Last Attempted Swaps (rejected)",FontFamily->"Source Sans Pro",Italic]},
        {Item[Dynamic@map,ItemSize->40],Item[Dynamic@tempmap,ItemSize->40]},
        {Dynamic@formatScore@score,Dynamic@formatScore@tempscore},
        {
            Row[{
            Button["Compute random block swaps",
                resetDebug[];
                $attempts++;
                tempmap=tempscore="";
                status=Row[{"Swapping blocks",ProgressIndicator[Appearance -> "Percolate"]}];
                tempgroups=RandomSwap[groups];
                status=Row[{"Calculating Score",ProgressIndicator[Appearance -> "Percolate"]}];
                tempscore=WardScore[tempgroups];
                If[TrueQ[tempscore["Total"]>score["Total"]],
                    $accepted++;
                    status=Row[{"Swaps accepted"}];
                    groups=tempgroups;
                    score=tempscore;
                    tempscore="";
                    map=Null;
                    status=Row[{"Swaps accepted, building Map",ProgressIndicator[Appearance -> "Percolate"]}];
                    map=BlockPlainMap[groups];
                    tempmap="\[LeftArrow] New Wards Accepted";
                    status=Row[{"New wards complete"}];
                    ,
                    status=Row[{"New wards rejected, lower score"}];
                    tempmap=BlockPlainMap[tempgroups];
                ],
                Method->"Queued"
            ],
            Button["Print current GeoIDs by ward",Print[groups]]
            ,
            Button["Copy wards to clipboard",CopyToClipboard[Iconize[groups, "geo ids by ward"]]]
            ,
            Button["Print graph of wards",Print@WardGraphPlot[groups],
                Method->"Queued"]}],SpanFromLeft
        }},
        Dividers->{{False,True,False},False}
    ],BaseStyle ->{FontFamily -> "Source Sans Pro",FontSize->16}],
    Initialization:>(
                status=Row[{"Calculating Score",ProgressIndicator[Appearance -> "Percolate"]}];
                score=WardScore[init];
                status=Row[{"Building Map",ProgressIndicator[Appearance -> "Percolate"]}];
                map=BlockPlainMap[init];
                status=Row[{"Initial Wards Complete"}];

    )
                ,
SynchronousInitialization -> False

]
)


autoDistrict["Run",init_]:=(
resetDebug[];
DynamicModule[{stop=False,groups=init,map="",tempmap="",$running=False,
score=Null,
status=Row[{"Creating initial wards",ProgressIndicator[Appearance -> "Percolate"]}],tempgroups,tempscore="",
$attempts=0,$accepted=0,run},
    run[]:=(
                resetDebug[];
                $attempts++;
                tempmap=tempscore="";
                status=Row[{"Swapping blocks",ProgressIndicator[Appearance -> "Percolate"]}];
                tempgroups=RandomSwap[groups];
                status=Row[{"Calculating Score",ProgressIndicator[Appearance -> "Percolate"]}];
                tempscore=WardScore[tempgroups];
                If[TrueQ[tempscore["Total"]>score["Total"]],
                    $accepted++;
                    status=Row[{"Swaps accepted"}];
                    groups=tempgroups;
                    score=tempscore;
                    tempscore="";
                    map=Null;
                    status=Row[{"Swaps accepted, building Map",ProgressIndicator[Appearance -> "Percolate"]}];
                    map=BlockPlainMap[groups];
                    tempmap="\[LeftArrow] New Wards Accepted";
                    status=Row[{"New wards complete"}];
                    ,
                    status=Row[{"New wards rejected, lower score"}];
                    tempmap=BlockPlainMap[tempgroups]
                ];
                If[TrueQ[$running],run[]]
        );
    Panel[Grid[
       { {Style["Autodistricting",Bold,20],SpanFromLeft},
        {Dynamic@status,Dynamic[If[TrueQ[$running],run[];Green,Red]]},
        {Dynamic@Row[{"swaps accepted/attempted (",$accepted,"/",$attempts,")"}],SpanFromLeft},
        {Style["Current Wards",FontFamily->"Source Sans Pro",Italic],Style["Last Attempted Swaps (rejected)",FontFamily->"Source Sans Pro",Italic]},
        {Item[Dynamic@map,ItemSize->40],Item[Dynamic@tempmap,ItemSize->40]},
        {Dynamic@formatScore@score,Dynamic@formatScore@tempscore},
        {
            




            Row[{
            Button["Start",$running=True],
            Button["Stop",$running=False],
            Button["Print current GeoIDs by ward",Print[groups]]
            ,
            Button["Copy wards to clipboard",CopyToClipboard[Iconize[groups, "geo ids by ward"]]]
            ,
            Button["Print graph of wards",Print@WardGraphPlot[groups],
                Method->"Queued"]}],SpanFromLeft
        }},
        Dividers->{{False,True,False},False}
    ],BaseStyle ->{FontFamily -> "Source Sans Pro",FontSize->16}],
    Initialization:>(
                status=Row[{"Calculating Score",ProgressIndicator[Appearance -> "Percolate"]}];
                score=WardScore[init];
                status=Row[{"Building Map",ProgressIndicator[Appearance -> "Percolate"]}];
                map=BlockPlainMap[init];
                status=Row[{"Initial Wards Complete"}];

    )
                ,
SynchronousInitialization -> False

]
)

autoDistrict["Min",init_List]:=
autoDistrict["Min",init,10]

autoDistrict["Min",n_Integer]:=
autoDistrict["Min",FindGraphPartition[$BlockGraph, $WardCount],n]

autoDistrict["Min",init_,n_]:=Module[{$i=0, updateQ,groups=init,score,
$status="Starting up",$accepted=1,$history={init},$scorehistory,lastscore,id=Unique[]},
censusProcessDetails[id]:="Calculating Score";
Progress`EvaluateWithProgress[
    score=WardScore[init];
    $scorehistory={score};
    Do[
        $i++;
        {updateQ,groups,score,lastscore}=autodistrictStep[{groups,score},id];
        If[updateQ,$accepted++;
            AppendTo[$history,groups];
            AppendTo[$scorehistory,score];
        ];
        $status=Grid[{{"Step:",Row[{$i,"/",n}]},{"Accepted:",$accepted},{"Score:",score["Total"]},
        {"Last",Grid[List@@@Normal[lastscore]]},
        {Dynamic[censusProcessDetails[id]],SpanFromLeft}},Alignment->Left];
    ,n];

    <|"Result"->groups,"Score"->score,"ResultHistory"->$history,"ScoreHistory"->$scorehistory|>

, <|"Progress" -> Dynamic[$i/n], "ItemTotal" -> n,"Text"->Dynamic[$status]|>, "Delay" -> 0.5]
]


autodistrictStep[{groups_,score_},id_]:=Block[{tempgroups,tempscore},
        censusProcessDetails[id]:="Choosing Swaps";
        tempgroups=RandomSwap[groups];
        censusProcessDetails[id]:="Calculating Score";
        tempscore=WardScore[tempgroups, id];
        If[TrueQ[tempscore["Total"]>score["Total"]],
            {True,tempgroups,tempscore,tempscore}
            ,
            {False,groups,score,tempscore}
        ]
]

formatScore[as_Association]:=Grid[Prepend[MapAt[Style[#,Bold]&,List@@@Normal[KeyDrop[as,"Continuous"]],{All,1}],
{Style["Scores",Bold],SpanFromLeft}],Alignment->Left,Frame->All]

formatScore[expr_]:=""

End[]

EndPackage[]