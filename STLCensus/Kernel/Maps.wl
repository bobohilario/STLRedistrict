

BeginPackage["STLCensus`"]

STLCensus`BlockGeoMap
STLCensus`BlockPlainMap

Begin["`Private`"]

$blockGeos:=$blockGeos=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Maps"
    ],
    "BlockPolygons.wxf"
    }]];

$blockBounds:=$blockBounds=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Maps"
    ],
    "BlockBounds.wxf"
    }]];

$blockAreas:=$blockAreas=Import[
    FileNameJoin[{
    PacletManager`PacletResource[
        "STLCensus",
        "Maps"
    ],
    "BlockAreas.wxf"
    }]];


blockGeo[geoid_]:=Lookup[$blockGeos,geoid,{}]

blockBounds[geoid_]:=Lookup[$blockBounds,geoid,{}]

blockAreas[geoid_]:=Lookup[$blockAreas,geoid,{}]

mapRegion[ids_]:=blockGeo[ids]

STLCensus`BlockGeoMap[groups:{_List..}]:=GeoGraphics[
    MapIndexed[{ FaceForm[ColorData[1,Mod[#2[[1]],15]]],mapRegion[#]}&,groups]]

STLCensus`BlockGeoMap[geoids_]:=GeoGraphics[{Black,EdgeForm[{Thick,Black}], FaceForm[Red],mapRegion[geoids]}]



STLCensus`BlockPlainMap[geoids_]:=Graphics[{Black,mapRegion[geoids]}]


STLCensus`BlockPlainMap[groups:{_List..}]:=Graphics[
    MapIndexed[{ FaceForm[ColorData[1,Mod[#2[[1]],15]]],mapRegion[#]}&,groups],ImageSize->Large]

rUnion[pieces_] := RegionUnion[(
    Flatten@
     Cases[pieces, 
      HoldPattern[GeoPosition][ls : {_List ..}] :> 
       BoundaryDiscretizeGraphics@Polygon[ls], {1, Infinity}]
    
    
    )];


bCircle[points_] := 
 BoundingRegion[Flatten[points, 1], "MinDisk"]

Clear[regionCompactness]
regionCompactness[geos_] := Total[blockAreas[geos]]/Area[bCircle[blockBounds[geos]]]/2000 (* fudge factor *)

End[]

EndPackage[]