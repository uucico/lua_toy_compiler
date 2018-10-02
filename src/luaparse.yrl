Nonterminals
	explist exp prefixexp unop var namelist %binop
	args block chunk ifblock stat laststat varlist
	functioncall funcbody funcname.

Terminals
	and break do else elseif end false for function if local nil not or
	repeat return then true until while integer float string name
	'(' ')' '+' '-' '*'  '/' '^' '%' '<' '<=''>'
	'>=' '==' '=' '\;' '~=' uminus ','.

Rootsymbol
	chunk.


%%
%% PRECEDENCIAS
%%
Nonassoc 100 or.
Nonassoc 110 and.
Nonassoc 120 '<' '<=' '>' '>=' '~=' '=='.
Left 140 '+' '-'.
Left 150 '*' '/' '%'.
Unary 160 not uminus.     
Left 180 '^'.

%%
%% chunk
%
chunk -> stat		: ['$1'].
chunk -> chunk stat	: '$1' ++ ['$2'].
chunk -> laststat	: ['$1'].
chunk -> chunk '\;'	: '$1'.

%%
%% laststat
%%
%laststat -> return explist		: {return, '$2'}.
laststat -> return exp			: {return, '$2'}.
laststat -> break				: '$1'.

%%
%% stat
%%
stat -> var '=' exp	  				: {assign,'$1','$3'}.
stat -> local var '=' exp			: {localassign,'$2','$4'}.
stat -> functioncall				: '$1'.
stat -> do block end				: {do,'$2'}.
stat -> while exp do block end 		: {while,'$2','$4'}.
stat -> repeat block until exp 		: {repeat,'$2','$4'}.
stat -> if exp then block end		: {ifblock,'$2','$4'}.
stat -> for name '=' exp ',' exp do block end 	: {for2,{'$2','$4','$6'},'$8'}.
stat -> function name funcbody	: {function,'$2','$3'}.
%stat -> local function name funcbody: {localfunction,'$2','$3'}.

%%
%% funcname
%%
funcname -> name					: '$1'.
%funcname -> funcname '.' name		: {'$1','$3'}.
%funcname -> funcname ':' name		: {'$1','$3'}.	% rever...


%%
%% functioncall
%%
functioncall -> prefixexp args		: {call, '$1','$2'}.
%functioncall -> prefixexp ':' name args : {call, '$1','$3','$4'}.

%%
%% ifblock
%%
ifblock -> block					: '$1'.
ifblock -> ifblock elseif exp then block : {'$1','$3','$5'}.
ifblock -> ifblock else block 		: {'$1','$3'}.

%%
%% block
%%
block -> chunk						: '$1'.


%%
%%namelist
%%
namelist -> name				: ['$1'].
namelist -> namelist ',' name	: '$1' ++ ['$3'].

%%
%% funcbody
%%
funcbody -> '(' namelist ')' block 'end' 	: {'$2', '$4'}.

%%
%% args
%%
args -> '(' explist ')'				: {args,'$2'}.
args -> string		   			: {args,'$1'}.

%%
%% explist
%%
explist -> exp 					: ['$1'].
explist -> explist ',' exp 		: '$1' ++ ['$3'].
%explist -> functioncall			: ['$1'].

%%
%% binop
%%
%binop -> '+'					: '$1'.
%binop -> '-'					: '$1'.
%binop -> '*'					: '$1'.
%binop -> '/'					: '$1'.
%binop -> '^'					: '$1'.
%binop -> '%'					: '$1'.
%binop -> '<'					: '$1'.
%binop -> '<='					: '$1'.
%binop -> '>'					: '$1'.
%binop -> '>='					: '$1'.
%binop -> '=='					: '$1'.
%binop -> '~='					: '$1'.
%binop -> 'and'					: '$1'.
%binop -> 'or'					: '$1'.


%%
%% unop
%%
unop -> '-'						: '$1'.
%unop -> 'not'					: '$1'.
%unop -> '#'						: '$1'.



%%
%% exp
%%
exp -> 'nil'					: '$1'.
exp -> 'false'					: '$1'.
exp -> 'true'					: '$1'.
exp -> integer					: '$1'.
exp -> float					: '$1'.
exp -> string					: '$1'.
exp -> prefixexp				: '$1'.
exp -> exp '+' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '-' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '*' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '/' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '%' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '^' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '<' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '<=' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '>' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '>=' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '==' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp '~=' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp 'and' exp			: {binop,'$2', '$1', '$3'}.
exp -> exp 'or' exp			: {binop,'$2', '$1', '$3'}.
exp -> unop exp					: {unop,'$1', '$2'}.

%%
%% prefixexp
%%
prefixexp -> var			: {var,'$1'}.
prefixexp -> '(' exp ')'		: '$2'.

%%
%% var
%%
var -> name						: '$1'.

Erlang code.

