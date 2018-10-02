-module(luaop).

-compile([export_all]).

optimize(Exp) ->
	case Exp of
		%%
		%% inteiro, inteiro
		%%
		{binop, {'+', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A+B}};
		{binop, {'-', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A-B}};
		{binop, {'*', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A*B}};
		{binop, {'/', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A div B}};
		{binop, {'^', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, round(math:pow(A,B))}};
		{binop, {'%', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A rem B}};
%%		{binop, {'..', L}, {integer, _, A}, {integer, _, B}} ->
%%			{true, {integer, L, list_to_integer(integer_to_list(A) ++ integer_to_list(B))}};
		{binop, {'<', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {A<B, L}};
		{binop, {'<=', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {A=<B, L}};
		{binop, {'>', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {A>B, L}};
		{binop, {'>=', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {A>=B, L}};
		{binop, {'==', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {A==B, L}};
		{binop, {'~=', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {A/=B, L}};
		{binop, {'and', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A band B}};
		{binop, {'or', L}, {integer, _, A}, {integer, _, B}} ->
			{true, {integer, L, A bor B}};
		%%
		%% inteiro ou float, float
		%%
		{binop, {'+', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A+B}};
		{binop, {'-', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A-B}};
		{binop, {'*', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A*B}};
		{binop, {'/', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A / B}};
		{binop, {'^', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {float, L, math:pow(A,B)}};
		{binop, {'%', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A - trunc(A) + float(trunc(A) rem B)}};
%%		{binop, {'..', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
%%			{true, {float, L, list_to_integer(float_to_list(A) ++ integer_to_list(B))}};
		{binop, {'<', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {A<B, L}};
		{binop, {'<=', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {A=<B, L}};
		{binop, {'>', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {A>B, L}};
		{binop, {'>=', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {A>=B, L}};
		{binop, {'==', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {A==B, L}};
		{binop, {'~=', L}, {Type, _, A}, {integer, _, B}} when Type == integer; Type == float ->
			{true, {A/=B, L}};
		%%
		%% float, inteiro ou float
		%%
		{binop, {'+', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A+B}};
		{binop, {'-', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A-B}};
		{binop, {'*', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A*B}};
		{binop, {'/', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {float, L, A / B}};
		{binop, {'^', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {float, L, math:pow(A,B)}};
%%		{binop, {'%', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
%%			{true, {float, L, A - trunc(A) + float(trunc(A) rem B)}};
%%		{binop, {'..', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
%%			{true, {float, L, list_to_integer(float_to_list(A) ++ integer_to_list(B))}};
		{binop, {'<', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {A<B, L}};
		{binop, {'<=', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {A=<B, L}};
		{binop, {'>', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {A>B, L}};
		{binop, {'>=', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {A>=B, L}};
		{binop, {'==', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {A==B, L}};
		{binop, {'~=', L}, {integer, _, A}, {Type, _, B}} when Type == integer; Type == float ->
			{true, {A/=B, L}};
		%%
		%% float, float
		%%
		{binop, {'+', L}, {float, _, A}, {float, _, B}} ->
			{true, {float, L, A+B}};
		{binop, {'-', L}, {float, _, A}, {float, _, B}} ->
			{true, {float, L, A-B}};
		{binop, {'*', L}, {float, _, A}, {float, _, B}} ->
			{true, {float, L, A*B}};
		{binop, {'/', L}, {float, _, A}, {float, _, B}} ->
			{true, {float, L, A / B}};
		{binop, {'^', L}, {float, _, A}, {float, _, B}} ->
			{true, {float, L, math:pow(A,B)}};
		{binop, {'<', L}, {float, _, A}, {float, _, B}} ->
			{true, {A<B, L}};
		{binop, {'<=', L}, {float, _, A}, {float, _, B}} ->
			{true, {A=<B, L}};
		{binop, {'>', L}, {float, _, A}, {float, _, B}} ->
			{true, {A>B, L}};
		{binop, {'>=', L}, {float, _, A}, {float, _, B}} ->
			{true, {A>=B, L}};
		{binop, {'==', L}, {float, _, A}, {float, _, B}} ->
			{true, {A==B, L}};
		{binop, {'~=', L}, {float, _, A}, {float, _, B}} ->
			{true, {A/=B, L}};

		%%
		%% impossivel otimizar
		%%
		_ ->
			{false, {Exp}}
	end.

