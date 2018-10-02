-module(luadesmonta).
-compile([export_all]).
-include("recs.hrl").

%%
%% parse/1
%%
parse(Filename) when is_list(Filename) ->
	{ok, Bin} = file:read_file(Filename),
	parse(Bin, Filename).
 
%%
%% parse/2
%%
parse(Bin, Arg) when is_binary(Bin), is_list(Arg) ->
	io:format("Parsing file ~s.~n", [Arg]),
	parse(Bin, {header, null});

parse(Bin, {header, _}) ->
	<<Hdr:4/binary,
	Cversion,
	Cformat,
	Cendianess,
	Csint,
	Cssize_t,
	Csinstr,
	Cslua_number,
	Cintegral,
	R/binary>> = Bin,
 
	Conf = #conf{   version = Cversion,
					format = Cformat,
					endianess = Cendianess,
					sint = Csint,
					ssize_t = Cssize_t,
					sinstr = Csinstr,
					slua_number = Cslua_number,
					integral = Cintegral },

	if Hdr =/= <<27,"Lua">> ->
		false;
	true ->

		io:format("~n* header:~n"),
		io:format("    magic string: ~s~n", [binary_to_list(Hdr)]),
		io:format("    version: ~2.16B~n", [Conf#conf.version]),
		io:format("    format: ~2.16B~n", [Conf#conf.format]),
		io:format("    endianess: ~B~n", [Conf#conf.endianess]),
		io:format("    integer: ~B~n", [Conf#conf.sint]),
		io:format("    size_t: ~B~n", [Conf#conf.ssize_t]),
		io:format("    instruction: ~B~n", [Conf#conf.sinstr]),
		io:format("    lua_Number: ~B~n", [Conf#conf.slua_number]),
		io:format("    Integral: ~B~n", [Conf#conf.integral]),
		parse(R, {functionblock, Conf})
	end;
 
parse(Bin, {functionblock, Conf}) ->
	io:format("~n* function block:~n"),

	Source = get_string(Bin, Conf),
	Bin2 = advance_size(Bin, Source#str.size + Conf#conf.ssize_t),

	Sint = Conf#conf.sint,
	
	<<Ldef:Sint/little-unsigned-unit:8,
	LLdef:Sint/little-unsigned-unit:8,
	Upvalues:8/unsigned,
	Parameters:8/unsigned,
	Is_vararg:8/unsigned,
	Maxstack:8/unsigned,
	Lists/binary>> = Bin2,

	io:format("    source file: ~s~n", [Source#str.text]),
	io:format("    Line defined: ~B~n", [Ldef]),
	io:format("    Last line: ~B~n", [LLdef]),
	io:format("    Upvalues: ~B~n", [Upvalues]),
	io:format("    Parameters: ~B~n", [Parameters]),
	io:format("    Is VarArg: ~B~n", [Is_vararg]),
	io:format("    Max Stack: ~B~n", [Maxstack]),

	%%code
	{N, Code, Next} = get_list(Lists, {instruction, Conf}),
	io:format("~n* code: ~B instructions.~n", [N]),
	if N > 0 ->
		io:format("        opcode    A    B    C   Bx  sBx~n");
		true ->
			false
	end,
	[ io:format("    ~10s ~4B ~4B ~4B ~4B ~4B ~n",
    [A#op.name, A#op.a, A#op.b, A#op.c, A#op.bx, A#op.sbx]) ||
	 A <- read_code2(Code) ],

	%%constants
	{NC, Const, Bin3} = get_list(Next, {constant, Conf}),
	io:format("~n* constant: ~B constants.~n", [NC]),
	if NC > 0 ->
		io:format("    type value~n");
		true ->
			false
	end,
	[ io:format("    ~p~n", [X]) || X <- Const ],

	%%prototypes/function block
	{NF, _, Bin4}= get_list(Bin3, {prototypes, Conf}),
	io:format("~n* functions: ~B functions.~n", [NF]),

	%%debug information
	%%source line position list	
	{NSL, SL, Bin5} = get_list(Bin4, {sourceline, Conf}),
	if NSL > 0 ->
		io:format("~n * source line informations: ~B~n", [NSL]);
		true ->
			false
	end,
	[ io:format("    ~B~n", [X]) || X <- SL ],

	%% local list
	{NLL, LL, Bin6} = get_list(Bin5, {local, Conf}),
    if NLL > 0 ->
        io:format("~n * local informations: ~B~n", [NLL]);
        true ->
            false
    end,
    [ io:format("    ~p~n", [X]) || X <- LL ],

	%%upvalue
    {NUV, UV, Bin7} = get_list(Bin6, {upvalue, Conf}),
    if NUV > 0 ->
        io:format("~n * upvalue informations: ~B~n", [NUV]);
        true ->
            false
    end,
    [ io:format("    ~p~n", [X]) || X <- UV ],
	Bin7.

%%
%% get_string2
%%
get_string2(Bin, _) ->
	F = fun() ->
		false
	end,
	lists:reverse(lists:foldl(F, [], Bin)).

get_string(Bin, Conf) ->
	Size_t = Conf#conf.ssize_t,
	<<Size:Size_t/little-unsigned-unit:8, Rest/binary>> = Bin,
	if Size > 0 ->
		<<R:Size/binary-unit:8, _/binary>> = Rest,
		#str{size=Size, text=lists:reverse(tl(lists:reverse(binary_to_list(R))))};
	true ->
		#str{size=0}
	end.

advance_size(Bin, Size) ->
	<<_:Size/unit:8, R/binary>> = Bin,
	R.

get_list(Bin, {Type, Conf}) ->
	Sint = Conf#conf.sint,
	<<Sizelist:Sint/little-unsigned-unit:8, Rest/binary>> = Bin,
	get_list({Type, Conf}, Sizelist, Sizelist, Rest, []).

get_list({_, _}, Sizelist, 0, R, Acc) ->
	{Sizelist, lists:reverse(Acc), R};

get_list({Type, Conf}, Sizelist, N, R, Acc) ->
	Sint = Conf#conf.sint,

	{Elem, Rest} = case Type of
		instruction ->
			%% fixed 32 bit size
			<<Elem2:4/binary, Rest2/binary>> = R,
			{Elem2, Rest2};
		constant ->
			%%variable 1 byte + Const (0-> none, 1-> 1 byte boolean, 3->number, 4->string
			<<TOC:1/little-unsigned-unit:8, Const/binary>> = R,
			get_const(TOC, Conf, Const);
		prototypes ->
			{Sizelist, parse(R, {functionblock, Conf})};
		sourceline ->
			<<Elem2:Sint/little-unsigned-unit:8, Rest2/binary>> = R,
			{Elem2, Rest2};
		local ->
			Str = get_string(R, Conf),
			Rest2 = advance_size(R, Str#str.size + Conf#conf.ssize_t),
			<<Startpc:Sint/little-unsigned-unit:8, Endpc:Sint/little-unsigned-unit:8, Rest3/binary>> = Rest2,
			{{Str, Startpc, Endpc}, Rest3};
		upvalue ->
			Str = get_string(R, Conf),
			Rest2 = advance_size(R, Str#str.size + Conf#conf.ssize_t),
			{Str, Rest2}
	end,
	get_list({Type, Conf}, Sizelist, N-1, Rest, [Elem|Acc]).

get_const(TOC, Conf, Const) ->
	Snum = Conf#conf.slua_number,

	Elem = case TOC of
		0 ->
			{{0, nil}, Const};
		1 ->
			<<Bool:1/little-unsigned-unit:8, Rest/binary>> = Const,
			{{1, Bool}, Rest};
		3 ->
			<<Num:Snum/little-float-unit:8, Rest/binary>> = Const,
			{{3, Num}, Rest};
		4 ->
			Str = get_string(Const, Conf),
			Rest = advance_size(Const, Str#str.size + Conf#conf.ssize_t),
			{{4, Str}, Rest}
	end,
	Elem.

read_code2(List) ->
	P = fun(Elem, Acc) ->
		<<Op:1/little-unsigned-unit:32>> = Elem,
		<<End:18/binary-unit:1, A:8, Opcode:1/little-unsigned-unit:6>> = <<Op:32>>,

%		io:format("read_code2: Elem = ~w, op=~w~n", [Elem, <<Op:32>>]),
		<<B:9, C:9>> = End,
		<<Biased:18>> = End,
		SBx = Biased - 131071,

		Oper = #op{ opcode=Opcode,
					name=oputil:opname(Opcode),
					type=oputil:optype(Opcode),
					a = A,
					b = B,
					c = C,
					bx = Biased,
					sbx = SBx},
		[Oper|Acc]
	end,
	lists:reverse(lists:foldl(P, [], List)).
