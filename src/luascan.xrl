Definitions.

ID	= [A-Za-z_][A-Za-z0-9_]*
WS	= ([\000-\s]|--.*|#.*)
D   = [0-9]

Rules.

==	:	{token,{'==',TokenLine}}.
~=  :	{token,{'~=',TokenLine}}.
<=  :	{token,{'<=',TokenLine}}.
>=  :	{token,{'>=',TokenLine}}.
<>  :	{token,{'<>',TokenLine}}.
\.\.  :	{token,{'..',TokenLine}}.
\.\.\. :
		{token,{'...',TokenLine}}.
[]()[}{|/^;:,%.*+#<>=-] :
		{token,{list_to_atom(TokenChars),TokenLine}}.

%%
%% identificadores
%%
{ID}* :	Word = list_to_atom(TokenChars),
		{token, case is_reserved(Word) of
					true -> {Word,TokenLine};
					false -> {name,Word,TokenLine}
				end}.

%%
%% strings quotadas
%% 
('|")(\\\^.|\\.|[^'"])*('|") :
			S = lists:sublist(TokenChars, 2, TokenLen - 2),
			case catch list_to_atom(string_gen(S)) of
			    {'EXIT',_} -> {error,"invalido " ++ TokenChars};
			    Atom -> {token,{string,TokenLine,Atom}}
			end.


%digito inteiro
{D}+		:	{token,{integer,TokenLine,list_to_integer(TokenChars)}}.

%representacao de ponto flutuante
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
				{token,{float,TokenLine,list_to_float(TokenChars)}}.

%whitespace - ignora.
{WS}+	: skip_token.

Erlang code.

-export([is_reserved/1]).

%% is_reserved/1
%% indica se a palavra e reservada.
is_reserved('and') -> true;
is_reserved('break') -> true;
is_reserved('do') -> true;
is_reserved('else') -> true;
is_reserved('elseif') -> true;
is_reserved('end') -> true;
is_reserved('false') -> true;
is_reserved('for') -> true;
is_reserved('function') -> true;
is_reserved('if') -> true;
is_reserved('in') -> true;
is_reserved('local') -> true;
is_reserved('nil') -> true;
is_reserved('not ') -> true;
is_reserved('or') -> true;
is_reserved('repeat') -> true;
is_reserved('return') -> true;
is_reserved('then') -> true;
is_reserved('true') -> true;
is_reserved('until') -> true;
is_reserved('while') -> true;
is_reserved(_) -> false.

%funcoes para quotar caracteres especiais
string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

