%% Include file defining records for testing the expansion of records.

%% Untyped and typed record definitions.

-record(urec, {
	  a = 49,
	  b,
	  c = x:y(1)
	 }).

-record(trec, {
	  a = 49 :: integer(),
	  b      :: list(string()),
	  c = x:y(1) :: tuple()
	 }).
