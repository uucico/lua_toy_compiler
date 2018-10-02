-module(luagc3).

-compile([debug_info, export_all]).

-include("recs.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_code(P, Instr, A, B, C) ->
	case Instr of 
		none ->	P;
		_ ->	P#gcp{	c = lists:append(P#gcp.c, [{Instr, A, B, C}]),
						sc = P#gcp.sc + 1 }
	end.

is_member(L, Sym) ->
	Nome = Sym#sym.n,
	R = lists:keyfind(Nome, 2, L),

	if R == false ->
		{false, nil};
	true ->
		{true, R}
	end.

symbol(P, Sym) ->
	%eh local?
	case is_member(P#gcp.l, Sym) of
		{true, R} ->
			{local, R};
		{false, nil} -> %seria global?
			case is_member(P#gcp.k, Sym) of
				{true, R} ->
					{global, R};
				{false, nil} ->
					{novar, 0}
			end
	end.

load_symbol(P, Sym) ->
	P2 = case Sym#sym.s of
		local ->
			add_code(P, move, P#gcp.r+1, Sym#sym.r, 0);
		global ->
			add_code(P, loadk, P#gcp.r+1, Sym#sym.r, 0);
		gblvar ->
			add_code(P, getglobal, P#gcp.r+1, Sym#sym.r, 0)
	end,
	P2#gcp{r=P2#gcp.r+1}.

add_symbol(P, Type, Sym) ->
	P2 = case Type of
		local ->  	Sym2 = Sym#sym{r=P#gcp.r, s=local},
					P#gcp{ l=[Sym2|P#gcp.l], sl=P#gcp.sl+1, r=P#gcp.r+1 }; 

		global ->	Sym2 = Sym#sym{r=P#gcp.sk, s=global},
					P#gcp{ k=[Sym2|P#gcp.k], sk=P#gcp.sk+1 }
	end,
	{P2, Type, Sym2}.


store_symbol(P, Type, Sym, From) ->
	DestReg = case Type of
		local -> Sym#sym.r;
		global -> P#gcp.r
	end,

	{Instr, A, B} = case From#sym.t of
		boolean -> 	Val = case From#sym.n of
						false -> 0;
						true -> 1
					end,
					{loadbool, DestReg, Val};
		nil ->		{loadnil, DestReg, DestReg};
		_ ->
			case From#sym.s of
				local ->
					{move, Sym#sym.r, From#sym.r};
				gblvar ->
					{getglobal, DestReg, From#sym.r};
				global ->
					{loadk, DestReg, From#sym.r}
			end
	end,

	case Type of
		local ->  	add_code(P, Instr, A, B, 0);

		global ->	P2 = add_code(P, Instr, A, B, 0),
					add_code(P2, setglobal, A, Sym#sym.r, 0)
	end.

eval_binop(P, Op, Exp1, Exp2) ->
	{PR, SymR} = gc_exp(P#gcp{dr=P#gcp.dr+3}, Exp2),
	{PL, SymL} = gc_exp(PR#gcp{dr=P#gcp.dr+2}, Exp1),

%	io:format("Tentando evaluar SymL=~w, SymR=~w~n", [SymL, SymR]),

	{P2, B} = case SymR#sym.s of
		local -> {PL, SymR#sym.r};
		global -> {PL, SymR#sym.r + 256};
		gblvar ->	{PL, SymR#sym.r}
	end,
%	io:format("saiu do primeiro com B = ~w~n", [B]),

	{P3, A} = case SymL#sym.s of
		local -> {P2, SymL#sym.r};
		global -> {P2, SymL#sym.r + 256};
		gblvar ->	{P2, SymL#sym.r}
%					{PGblA, global, SymNL} = add_symbol(P2, global, SymL),
%					{store_symbol(PGblA, global, SymNL, SymL), SymNL#sym.r}
	end,
%	io:format("saiu do segundo com A = ~w~n", [A]),

	P4 = case Op of
		{'>',_} ->
			add_code(P3, lt, 0, A, B);
		{'<',_} ->
			add_code(P3, lt, 1, A, B);
		{'>=',_} ->
			add_code(P3, le, 0, A, B);
		{'<=',_} ->
			add_code(P3, le, 1, A, B);
 		{'+',_} ->
			add_code(P3, add, P#gcp.dr, A, B);
		{'-', _} ->
			add_code(P3, sub, P#gcp.dr, A, B);
		{'*', _} ->
			add_code(P3, mul, P#gcp.dr, A, B);
		{'/', _} ->
			add_code(P3, dive, P#gcp.dr, A, B);
		{'%', _} ->
			add_code(P3, mod, P#gcp.dr, A, B);
		{'^', _} ->
			add_code(P3, pow, P#gcp.dr, A, B)
	end,
	{P4, #sym{r=P#gcp.dr, s=local}}.

gc_exp(P, {binop, Op, Exp1, Exp2}) ->
	{R, Exp} = luaop:optimize({binop, Op, Exp1, Exp2}),

	case R of
		true ->
			io:format("gc_exp: operacao binaria otimizada para ~w~n", [Exp]),
			gc_exp(P, Exp);
		false ->
%			eval_binop(P#gcp{r=P#gcp.r+1}, Op, Exp1, Exp2)
			eval_binop(P, Op, Exp1, Exp2)
	end;


gc_exp(P, {Type,_,N}) when Type == integer; Type == float; Type == string ->
	Tipo = case Type of
		integer -> number;
		float -> number;
		string -> string
	end,

	A = #sym{n=N, t=Tipo},
	{SymbolType, Sym} = symbol(P, A),

	case SymbolType of
		global ->
			{P, Sym};
		novar ->
			{P2, global, SymA} = add_symbol(P, global, A),
%			P4 = store_symbol(P2, global, SymA, A),
%			P3 = add_code(P2, loadk, P#gcp.r, SymA#sym.r, 0),
%			P4 = add_code(P3, setglobal, P#gcp.r, SymA#sym.r, 0),
			{P2, SymA}
	end;

gc_exp(P, {var, {name, Nome,_}}) ->
	A = #sym{n=Nome, t=string},

	{SymbolType, SymA} = symbol(P, A),

	case SymbolType of
		novar ->
			case P#gcp.ctx of
				call ->
		%					io:format("chamada para variavel ~w ~n", [A]),

					%chamada de funcao, cria global e retorna simbolo.
					{P2, global, SymB} = add_symbol(P, global, A),
					{P2, SymB#sym{s = global}};
				_ ->
					io:format("gc_exp: variavel ~w inexistente!~n.", [A]),
					{P, novar}
			end;
		global ->
					{P, SymA#sym{s = gblvar}};
		local -> 	{P, SymA#sym{s = local}}
					
	end;

gc_exp(P, {unop, {'-',L}, {Type, _, Val}}) ->
	Res = -1*Val,
	gc_exp(P, {Type, L, Res});

gc_exp(P, {nil, _}) ->
	{P, #sym{n=nil, t=nil}};

gc_exp(P, {true, _}) ->
	{P, #sym{n=true, t=boolean}};

gc_exp(P, {false, _}) ->
	{P, #sym{n=false, t=boolean}};

gc_exp(P, Exp) ->
	io:format("gc_exp: geral com ~w~n", [Exp]),

	{P, Exp}.

gc_stat(P, ifblock, A, B) ->
	io:format("ifblock com ~w e ~w ~n", [A,B]),

	{P2, Sym} = gc_exp(P, A),

	P2;

gc_stat(P, call, A, B) ->
	{P2, SymA} = gc_exp(P#gcp{ctx = call}, A),
	{args, L} = B,

	C = fun(Arg, Accin) ->	
		{PL, N} = Accin,
		{PL2, SymL} = gc_exp(PL, Arg),
		io:format("argumento: ~w ~n", [SymL]),
		PL3 = load_symbol(PL2, SymL),
		{PL3, N+1}
	end,

	In = {P2#gcp{r=P2#gcp.r+1}, 0},
	{P3, N} = lists:foldl(C, In, L),

	P4 = add_code(P3, getglobal, P3#gcp.r-N, SymA#sym.r, 0),
	P5 = add_code(P4, call, P3#gcp.r-N, N+1, 1),

%	io:format("chamada de funcao ~w com ~w elementos ~w ~n", [A, R,L]),
	P5#gcp{ctx=any};
	

gc_stat(P, Oper, {name,Nome,_}, B) when Oper == localassign; Oper == assign ->
	A = #sym{n=Nome, t=string},

	{SymbolType, SymA} = symbol(P, A),

	case Oper of
		localassign ->
			case SymbolType of
				local ->
					{P2, SymExp} = gc_exp(P, B),
					store_symbol(P2, local, SymA, SymExp);
				_ -> %global ou nao existente
					{P2, local, Sym} = add_symbol(P, local, A),
					{P2L, SymExp} = gc_exp(P2, B),
					store_symbol(P2L, local, Sym, SymExp)
			end;

		assign ->
			%queremos fazer um assign global. ja existe variavel global?
			case SymbolType of
				global -> %simplesmente atualiza.
					{P2, SymExp} = gc_exp(P, B),
					store_symbol(P2, global, SymA, SymExp);
				local -> %atualiza localmente.
					{P2, _} = gc_exp(P#gcp{dr=SymA#sym.r}, B),
					P2;
%					io:format("guardando simbolo ~w~n", [SymA]),
%					store_symbol(P2, local, SymA, SymExp);
				novar ->
					{P2, global, Sym} = add_symbol(P, global, A),
					{P2G, SymExp} = gc_exp(P2, B),
					store_symbol(P2G, global, Sym, SymExp)
			end
	end.

generate(List) ->
	P = #gcp{},
	C = fun(Elem, Accin) ->	
		{Code, A, B} = Elem,
		gc_stat(Accin, Code, A, B)
	end,

	io:format("Gerando codigo...~n"),

	R = lists:foldl(C, P, List),
	add_code(R, return, 0, 1, 0).

