java_import(['strategy.candidate']).

declareSet(concreteStrategies, 
	['SortingContext.bubbleSort__int[]', 
	 'SortingContext.linearSort__int[]']).
declareSet(context, ['SortingContext']).

hideScopeSetFrom(concreteStrategies, context).
