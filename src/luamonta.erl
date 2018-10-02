-module(luamonta).
-compile([export_all]).
-include("recs.hrl").

monta(File, P) when is_list(File) ->
	file:write_file(File, monta(P)).


monta(P) ->
	Conf = #conf{version = 16#51,
				format = 0,
				endianess = 1,
				sint = 4,
				ssize_t = 4,
				sinstr = 4,
				slua_number = 8,
				integral = 0 },

	B1 = dump_header(Conf),
	B2 = dump_functionhdr(Conf, P),
	B3 = dump_code(Conf, P),
	B4 = dump_constants(Conf, P),
	B5 = dump_funcproto(Conf, P),

	io:format("header:		~w~n", [B1]),
	io:format("function:    ~w~n", [B2]),
	io:format("code:		~w~n", [B3]),
	io:format("const:		~w~n", [B4]),

	<<B1/binary, B2/binary, B3/binary, B4/binary, B5/binary, 0:32, 0:32, 0:32>>.

dump_header(Conf) ->
	Magic = <<27,"Lua">>,

	V = Conf#conf.version,
	F = Conf#conf.format,
	E = Conf#conf.endianess,
	SI = Conf#conf.sint,
	SS = Conf#conf.ssize_t,
	SIN = Conf#conf.sinstr,
	SLN = Conf#conf.slua_number,
	INT = Conf#conf.integral,


	%ver formato do header acima.
	<<Magic:4/binary, V, F, E, SI, SS, SIN, SLN, INT>>.

dump_string(Conf, S) ->
	Size_t = Conf#conf.ssize_t,
	Sz = string:len(S) + 1,
	Bin = list_to_binary(S),

	<<Sz:Size_t/little-unsigned-unit:8, Bin/binary, 0>>.

dump_functionhdr(Conf, P) ->
	Size_t = Conf#conf.ssize_t,

	Srcname = dump_string(Conf, P#gcp.srcname),
	Ldef = 0,
	LLdef = 0,
	Upvalues = 0,
	Parameters = 0,
	Is_Vararg = 2,
	Maxstack = P#gcp.r+10,

	<<Srcname/binary, Ldef:Size_t/little-unsigned-unit:8, LLdef:Size_t/little-unsigned-unit:8,
	Upvalues, Parameters, Is_Vararg, Maxstack>>.

dump_code(Conf, P) ->
	Size_t = Conf#conf.ssize_t,

	C = fun(Elem, Accin) ->
		Opc = dump_opcode(Elem),
		<<Accin/binary, Opc/binary>>
	end,

	Code = lists:foldl(C, <<>>, P#gcp.c),
	Szcode = P#gcp.sc,

	<<Szcode:Size_t/little-unsigned-unit:8, Code/binary>>.

dump_opcode({Code, A, B, C}) ->
	Opcode = oputil:opnum(atom_to_list(Code)),

	%falta outros tipos de opcodes.
	case oputil:optype(Opcode) of
		sABC ->
			<<Op:1/little-unsigned-unit:32>> = <<B:9, C:9, A:8, Opcode:6>>,
			<<Op:32>>;
		sABx ->
			<<Op:1/little-unsigned-unit:32>> = <<B:18, A:8, Opcode:6>>,
			<<Op:32>>
	end.

dump_constants(Conf, P) ->
	Size_t = Conf#conf.ssize_t,
	Sizek = P#gcp.sk,

	C = fun(Elem, Accin) ->
		{Conf, Bin} = Accin,
		Opc = dump_const(Conf, Elem),
		{Conf, <<Bin/binary, Opc/binary>>}
	end,

	{_, Consts} = lists:foldl(C, {Conf, <<>>}, lists:reverse(P#gcp.k)),

	<<Sizek:Size_t/little-unsigned-unit:8, Consts/binary>>.

dump_const(Conf, Sym) ->
	Snum = Conf#conf.slua_number,

	{Type, Val} = case Sym#sym.t of
		number -> 
			N = Sym#sym.n,
			{3, <<N:Snum/little-float-unit:8>>};
		string -> {4, dump_string(Conf, atom_to_list(Sym#sym.n))}
	end,

	io:format("dump constante tipo ~w valor ~w~n", [Type, Val]),
	<<Type, Val/binary>>.

dump_funcproto(_, _) ->	
	<<0:32>>.

