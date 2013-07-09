(*Mathematica Package*)
(*Programmer: Hector Manuel Sanchez Castellanos*)
(*Created by the Wolfram Workbench May 5, 2013*)

BeginPackage["Finances`"]
SortByDate::usage
ExpenseFilter::usage
IncomeFilter::usage
AccumulatedExpenses::usage
AccumulatedIncomes::usage
AccumulatedTransactions::usage
ExpensesDates::usage
IncomesDates::usage
TransactionsDates::usage
ExpensesDays::usage
IncomesDays::usage
TransactionsDays::usage
GetPlottableAccumulatedExpenses::usage
GetPlottableAccumulatedIncomes::usage
GetPlottableAccumulatedTransactions::usage
GetPlottableYearAccumulatedExpenses::usage
GetPlottableYearAccumulatedIncomes::usage
GetPlottableYearAccumulatedTransactions::usage
GetYearStatistics::usage
YearStatisticsGrid::usage
GetLabels::usage
GetTransactionsByType::usage
GetTotalTransactionsByType::usage
PieChartExpenses::usage
PieChartExpensesMonthlyMean::usage

Begin["`Private`"]

ExpenseFilter[input_] := If[input < 0, input, ## &[]]
IncomeFilter[input_] := If[input >= 0, input, ## &[]]
SortByDate[transactionsList_] := SortBy[transactionsList, {#[[3]], {"Year", "Month", "Day"}} &]

AccumulatedExpenses[transactionsList_] := Module[{transactionsValues, expenses},
  transactionsValues = SortByDate[transactionsList][[All, 1]];
  expenses = Map[ExpenseFilter, transactionsValues];
  Accumulate[expenses]
]
AccumulatedIncomes[transactionsList_] := Module[{transactionsValues, income},
  transactionsValues = SortByDate[transactionsList][[All, 1]];
  income = Map[IncomeFilter, transactionsValues];
  Accumulate[income]
]
AccumulatedTransactions[transactionsList_] := Accumulate[SortByDate[transactionsList][[All, 1]]]

ExpensesDates[transactionsList_] := Module[{transactionsValues},
  transactionsValues = SortByDate[transactionsList][[All, 1]];
  Table[
  		If[transactionsValues[[i]] < 0, 
    	transactionsList[[i]][[3]], 
    	## &[]]
	,{i, 1, Length[transactionsList]}]
]
IncomesDates[transactionsList_] := Module[{transactionsValues},
  transactionsValues = SortByDate[transactionsList][[All, 1]];
  Table[
   		If[transactionsValues[[i]] >= 0,
   		transactionsList[[i]][[3]], 
   		## &[]]
   	,{i, 1, Length[transactionsList]}]
]
TransactionsDates[transactionsList_] := SortByDate[transactionsList][[All,3]]

ExpensesDays[transactionsList_] := ExpensesDates[transactionsList][[All, 3]]
IncomesDays[transactionsList_] := IncomesDates[transactionsList][[All, 3]]
TransactionsDays[transactionsList_] := TransactionsDates[transactionsList][[All, 3]]

GetPlottableAccumulatedExpenses[monthlyTransactionsList_] := Module[{},
  Transpose[{ExpensesDays[monthlyTransactionsList], 
    AccumulatedExpenses[monthlyTransactionsList]}]
  ]
GetPlottableAccumulatedIncomes[monthlyTransactionsList_] := 
 Module[{},
  Transpose[{IncomesDays[monthlyTransactionsList], 
    AccumulatedIncomes[monthlyTransactionsList]}]
  ]
GetPlottableAccumulatedTransactions[monthlyTransactionsList_] := 
 Module[{},
  Transpose[{TransactionsDays[monthlyTransactionsList], 
    AccumulatedTransactions[monthlyTransactionsList]}]
  ]

GetPlottableYearAccumulatedExpenses[yearTransactionsList_]:= Map[GetPlottableAccumulatedExpenses, yearTransactionsList]
GetPlottableYearAccumulatedIncomes[yearTransactionsList_]:= Map[GetPlottableAccumulatedIncomes, yearTransactionsList]
GetPlottableYearAccumulatedTransactions[yearTransactionsList_]:= Map[GetPlottableAccumulatedTransactions, yearTransactionsList]

GetYearStatistics[yearTransactionsList_] := Module[{accumulatedExpenses, accumulatedIncomes, accumulatedTransactions, monthTotals, yearStatistics},
	accumulatedExpenses = Abs[Map[AccumulatedExpenses, yearTransactionsList]];
  	accumulatedIncomes = Map[AccumulatedIncomes, yearTransactionsList];
  	accumulatedTransactions = Map[AccumulatedTransactions, yearTransactionsList];
  	monthTotals = {
    		Map[Last, accumulatedExpenses],
    		Map[Last, accumulatedIncomes],
    		Map[Last, accumulatedTransactions]
    	};
  	yearStatistics = {
    		{Min[monthTotals[[1]]]//N, Mean[monthTotals[[1]]]//N, Max[monthTotals[[1]]]//N},
    		{Min[monthTotals[[2]]]//N, Mean[monthTotals[[2]]]//N, Max[monthTotals[[2]]]//N},
    		{Min[monthTotals[[3]]]//N, Mean[monthTotals[[3]]]//N, Max[monthTotals[[3]]]//N}
   	 	}
]
YearStatisticsGrid[yearTransactionsList_] := Module[{stats},
  	stats = GetYearStatistics[yearTransactionsList];
  	Grid[
  		{
    		{"", "Min", "Mean", "Max"},
    		Prepend[stats[[1]], "Expenses"],
    		Prepend[stats[[2]], "Incomes"],
    		Prepend[stats[[3]], "Balance"]
    	}, Dividers -> All]
]

GetLabels[monthExpenses_] := Union[monthExpenses[[All, 4]]]
GetTransactionsByType[monthExpenses_] := Module[{labels},
  	labels = GetLabels[monthExpenses];
  	Table[Cases[monthExpenses, {_, _, _, labels[[i]]}], {i, 1, Length[labels]}]
]
GetTotalTransactionsByType[expensesByType_] := Module[{},
  	Table[{Total[expensesByType[[i]][[All, 1]]], expensesByType[[i]][[1]][[4]]}, {i, 1, Length[expensesByType]}]
]
PieChartExpenses[transactionsList_] := Module[{transactions, expenses},
  	transactions = GetTotalTransactionsByType[GetTransactionsByType[transactionsList]];
  	expenses = Cases[transactions, {a_, _} /; a < 0];
  	PieChart[Abs[expenses[[All, 1]]], ChartLabels -> Placed[expenses[[All, 2]], "VerticalCallout"], ChartStyle -> "Pastel"]
]
PieChartExpensesMonthlyMean[transactionsByMonth_] := Module[{monthlyTransactionsByType, labels, transactions, meanTransactions, expenses},
  	monthlyTransactionsByType = Table[GetTotalTransactionsByType[GetTransactionsByType[transactionsByMonth[[i]]]], {i, 1, Length[transactionsByMonth]}];
  	labels = Union[Flatten[monthlyTransactionsByType, 1][[All, 2]]];
  	transactions = Table[Cases[Flatten[monthlyTransactionsByType, 1], {_, i}], {i, labels}];
  	meanTransactions = Table[{Total[transactions[[i]][[All, 1]]]/Length[monthlyTransactionsByType] // N, transactions[[i]][[1]][[2]]}, {i, 1, Length[transactions]}];
  	expenses = Cases[meanTransactions, {a_, _} /; a < 0];
  	PieChart[Abs[expenses[[All, 1]]], ChartLabels -> Placed[expenses[[All, 2]], "VerticalCallout"], ChartStyle -> "Pastel"]
]


End[]
EndPackage[]

