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
    		{Min[monthTotals[[1]]], Mean[monthTotals[[1]]], Max[monthTotals[[1]]]},
    		{Min[monthTotals[[2]]], Mean[monthTotals[[2]]], Max[monthTotals[[2]]]},
    		{Min[monthTotals[[3]]], Mean[monthTotals[[3]]], Max[monthTotals[[3]]]}
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

End[]
EndPackage[]

