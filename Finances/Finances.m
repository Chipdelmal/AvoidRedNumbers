(*Mathematica Package*)
(*Programmer: Hector Manuel Sanchez Castellanos*)
(*Created by the Wolfram Workbench May 5, 2013*)

BeginPackage["Finances`"]
AccumulatedExpenses::usage
AccumulatedExpensesPlot::usage

Begin["`Private`"]
AccumulatedExpenses[expensesList_] := Module[{expenses, days},
  expenses = Accumulate[expensesList[[All, 1]]];
  days = expensesList[[All, 3, 3]];
  Transpose[{days, expenses}]
]

Options[AccumulatedExpensesPlot] = {
	PlotMarkers -> Automatic, 
   	Joined -> True, 
   	PlotLabel -> "", 
   	AxesLabel -> {"Date", "Accumulated Expenses"}, 
   	PlotStyle -> {PointSize -> Medium}, 
   	ImageSize -> 800, 
   	PlotRange -> Automatic
};
AccumulatedExpensesPlot[accumulatedExpensesList_, OptionsPattern[]] := Module[{},
  ListPlot[accumulatedExpensesList,
  	PlotLabel -> Style[OptionValue[PlotLabel], Bold],
   	Joined -> OptionValue[Joined],
   	PlotMarkers -> OptionValue[PlotMarkers],
   	PlotStyle -> Style[OptionValue[PlotStyle], Bold],
   	AxesLabel -> OptionValue[AxesLabel],
   	ImageSize -> OptionValue[ImageSize],
   	PlotRange -> OptionValue[PlotRange]
  ]
]


End[]

EndPackage[]

