java_import(['command.candidate', 
			'command.candidate.ButtonAction',
			'command.candidate.ButtonAction2']).

hideScopeSetFrom(action, invoker).

declareSet(action, ['clicked__void']).

declareSet(invoker, ['Button', 'ButtonAction', 'ButtonAction2']).
			