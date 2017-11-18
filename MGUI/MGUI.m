(* ::Package:: *)

(*Author: Kuba kuba.pod@gmail.com*)


(* ::Section:: *)
(*begin*)


BeginPackage["MGUI`"];

  Unprotect @ "MGUI`*";
  ClearAll @ "MGUI`*";

  MSorter;
  NestedView;
  


  BeginPackage["`Common`"];
    (*each controller should defined within own context 
      so put here stuff you need to be seen inside
     *)
     $panelWrapper
     $errorPanels
     setForwardCompatibility
     BoxLoadingWrapper
     GuiToBoxes
  
  EndPackage[];


  Begin["`Private`"];


(* ::Section:: *)
(*body*)


(* ::Subsection::Closed:: *)
(*commons*)


$panelWrapper = Tooltip[
  Style["\[WarningSign]",40,RGBColor[1, 0.25, 0]]
, #
, TooltipStyle->{"Panel","Message"}
]&;


$errorPanels = $panelWrapper /@ <|
   "forwardCompatibility" -> "Your MGUI` package is too old to display this output. Please update the package."
 , "failedLoading" -> "Failed to load MGUI`, rendering aborted."    

|>


setForwardCompatibility[controllerDisplay_Symbol]:= With[{panel = $errorPanels["forwardCompatibility"]}
, controllerDisplay[___]:= panel
];



BoxLoadingWrapper // Attributes = {HoldFirst};
BoxLoadingWrapper[ box_ ]:= With[{ failedLoadingPanel = $errorPanels["failedLoading"]}
, ToBoxes @ DynamicModule[
  {ready, display, initialization}
, Dynamic[
    If[
      TrueQ @ ready
    , display  
    , ProgressIndicator[Appearance->"Percolate"]
    ]
  , TrackedSymbols:>{ready}  
  ]
, UnsavedVariables :> { ready, display }
, Initialization :> (
    initialization[]:= (
      If[
        Check[Needs["MGUI`"]; True, False]
      , display = box
      , display = failedLoadingPanel
      ]
    ; ready = True
    );
    initialization[]
  )  
, SynchronousInitialization->False
]] 


GuiToBoxes[a___, r___]:= ToBoxes @ $panelWrapper["Unknown syntax error. Failed to typeset: "<>ToString[a,InputForm]]


(* ::Subsection:: *)
(*MSorter*)




(*TODO: support for parts*)
(*TODO: don't rebuild controller[] on external shuffle*)
(*TODO: allow more flexibility with sizes *)
Begin["`MSorter`"];

  MSorter::usage = "MSorter[Dynamic@list, OptionsPattern[]] creates gui to control list's elements ordering.";

  MSorter // ClearAll
  MSorter // Options = {
   ContinuousAction -> False(*,
   "EmbeddingMethod" \[Rule] Automatic*)
  };
  
  MSorter /: MakeBoxes[controller_MSorter, fmt_]:= GuiToBoxes[controller, fmt];
  
  (*MSorter /: GuiToBoxes[
    MSorter[Dynamic[list_], opts1___, "EmbeddingMethod" \[Rule] None, opts2___]
  , fmt_
  ]:= ToBoxes @ Dynamic[MSorterDisplay[1, Dynamic[list], opts1,opts2], None]
  *)
  
  GuiToBoxes[controller: MSorter[static_List, patt :OptionsPattern[]], fmt_] := MakeBoxes[
    Interpretation[{ var = static}
    , MSorter[Dynamic[var], patt]
    , MSorter[var, patt]
    ]
  , fmt  
  ];
  
  GuiToBoxes[controller: MSorter[dVar_Dynamic, patt :OptionsPattern[]], fmt_] := With[
    {controllerBoxes = BoxLoadingWrapper @ MSorterBox[1, dVar, patt]}
  , InterpretationBox[controllerBoxes, controller]
  ];
  
  MSorterBox // Options = MSorter // Options;
  
  MSorterBox // setForwardCompatibility
  
  MSorterBox[1, Dynamic[list_], OptionsPattern[]] :=  Deploy[
    DynamicModule[ (* position refers to y-coordinate*)
      {
        baseLabels
      , n                          (*list length*)
      , active = False             (*true if mouse is pressed/dragged over controller*)
      , basePositions
      , tempPositions
      , activeLocator
      , nearests
      , positionUpdateFunction
      , finalUpdateFunction
      , labelSize = {100, 35}   
      , graphicsStandardForm
      , labelPane
      , continuousAction = OptionValue[ContinuousAction]
      , this
      , controller
      , print 
      
      }
    , Pane[
        DynamicWrapper[
          
            (*EventHandler[
              Dynamic[controller, TrackedSymbols\[RuleDelayed]{controller}]
            , { "MouseUp"\[RuleDelayed] (active = False)}
            , PassEventsDown\[Rule]True
            ]*)
            Dynamic[controller[], TrackedSymbols:>{controller}] 
          
          
        , list;If[ (*;list needs to be outside, otherwise FE tries to be too smart and does not update*)
            print[Grid[{{"from dynamic wrapper",""},{"active", active}, {"active loc", activeLocator}}]]
          ; Not @ active
          , this@"init"
          ] (*on external changes*)
        , TrackedSymbols:> {list}
        
        ] 
      ]
      
    , Initialization :>(
        print = Print[Framed@Column[{#}]]&;
        print = Function[,Null,HoldAll]
        
       ; this["init"]:= (
          print["init"]
                  
        ; activeLocator = 0        
        ; n = Length @ list
        ; baseLabels = list
        ; basePositions = Table[1-1/(2. * n)- i/n, {i, 0, n - 1} ]
        ; tempPositions = basePositions
        ; controller[] = Graphics[
            {
              Table[With[{i = i}
                , Locator[  
                    Dynamic[ 
                      Scaled[{0.5, tempPositions[[i]]}]  
                    , {positionUpdateFunction[i], finalUpdateFunction[]}
                    ]
                  ,  labelPane @ baseLabels[[i]]
                  ]
                ]
              , {i, n}
              ]
            , Dynamic[
                If[
                  active
                , Inset[  labelPane[ baseLabels[[activeLocator]], Appearance -> "Pressed"]
                  , Dynamic@Scaled[{0.5, tempPositions[[activeLocator]]}]
                  ]
                , {}
                ]
               (* , TrackedSymbols\[RuleDelayed]{active}*)
              ]
            }
          , ImageSize -> (labelSize{1,n}+0{4, 2n+2})
          , AspectRatio -> Full
          , Frame -> False
          , PlotRangePadding -> None
          , ImageMargins -> 0
          , BaseStyle->{ CacheGraphics ->False}
          , DefaultBaseStyle -> {}
          , FormatType -> StandardForm
          ]
        )
        
       
    
       ; positionUpdateFunction[i_] := Function[
          active = True  
        ; activeLocator = i      
        ; tempPositions[[i]] = MousePosition[ "GraphicsScaled",{0, 0}][[2]]
        ; nearests = Nearest[ReplacePart[tempPositions,i->2] -> All, tempPositions[[i]], 1][[1]]
        
        ; If[
            Less[nearests["Distance"], 1 / (2 * n)]
          , (basePositions[[{i, #}]] = basePositions[[{#, i}]])& @ nearests["Index"]
          ; (tempPositions[[#]] = basePositions[[#]])& @ nearests["Index"]
          ; If[ 
              continuousAction
            , print[{"from pos update", basePositions}]
            ; list = baseLabels[[Ordering[-basePositions]]]
            
            ] 
          ]
        ]    
        
      ; labelPane = MouseAppearance[
          Button[Row[{Pane["\[UpDownArrow]",55],#}],{},##2, ImageSize -> labelSize,  Alignment->{Left,Center},FrameMargins->5(*,RoundingRadius\[Rule]5,FrameStyle \[Rule] Orange*)]
        , "LinkHand"
        ]&
      
      ; finalUpdateFunction[]:=Function[
          tempPositions = basePositions       
        ; activeLocator = 0
        ; list = baseLabels[[Ordering[-basePositions]]]
        ; active = False
        ; print["final"]
        
         
        ]
        
      ; this["init"]  
     
      )
    ]
  ]

End[]


(* ::Section:: *)
(*end*)



End[];

Protect @ "MGUI`*";

EndPackage[]
