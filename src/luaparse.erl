-module(luaparse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("luaparse.yrl", 171).



-file("c:/PROGRA~1/ERL58~1.5/lib/parsetools-2.0.6/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{{M, F}, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("luaparse.erl", 190).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, break, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, for, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, function, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, local, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, repeat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, return, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_prefixexp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_chunk(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_chunk(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_stat(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_6(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, for, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, function, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, local, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, repeat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, nil, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_laststat(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_9: see yeccpars2_0

yeccpars2_10(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_11(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_12: see yeccpars2_7

yeccpars2_13(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_15: see yeccpars2_0

%% yeccpars2_16: see yeccpars2_7

%% yeccpars2_17: see yeccpars2_7

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_prefixexp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_19: see yeccpars2_7

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_21(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_21(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unop(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_exp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_29: see yeccpars2_7

%% yeccpars2_30: see yeccpars2_7

%% yeccpars2_31: see yeccpars2_7

%% yeccpars2_32: see yeccpars2_7

%% yeccpars2_33: see yeccpars2_7

%% yeccpars2_34: see yeccpars2_7

%% yeccpars2_35: see yeccpars2_7

%% yeccpars2_36: see yeccpars2_7

%% yeccpars2_37: see yeccpars2_7

%% yeccpars2_38: see yeccpars2_7

%% yeccpars2_39: see yeccpars2_7

%% yeccpars2_40: see yeccpars2_7

%% yeccpars2_41: see yeccpars2_0

%% yeccpars2_42: see yeccpars2_7

%% yeccpars2_43: see yeccpars2_7

yeccpars2_44(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_44_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_and(Stack),
 yeccgoto_exp(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_45(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_45_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_46(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, for, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, function, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, local, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, repeat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, while, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_block(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_chunk(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_chunk(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_51_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_51_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_51_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_51_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_51_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_53_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_53_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_53_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_53_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_53_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_and(Stack),
 yeccgoto_exp(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_53_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_53(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_54(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_54_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_54_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_54_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_54_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_54_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_and(Stack),
 yeccgoto_exp(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_54_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_55(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_55_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_55_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_55_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_55_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_55_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_and(Stack),
 yeccgoto_exp(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_56(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_56_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_56_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_56_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_56_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_56_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_and(Stack),
 yeccgoto_exp(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_57(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_57_$end'(Stack),
 yeccgoto_exp(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_57_('(Stack),
 yeccgoto_exp(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_57_)'(Stack),
 yeccgoto_exp(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_57_,'(Stack),
 yeccgoto_exp(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = 'yeccpars2_57_;'(Stack),
 yeccgoto_exp(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_and(Stack),
 yeccgoto_exp(hd(Nss), 'and', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, do, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_do(Stack),
 yeccgoto_exp(hd(Nss), do, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_end(Stack),
 yeccgoto_exp(hd(Nss), 'end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, for, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_for(Stack),
 yeccgoto_exp(hd(Nss), for, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, function, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_function(Stack),
 yeccgoto_exp(hd(Nss), function, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_if(Stack),
 yeccgoto_exp(hd(Nss), 'if', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, local, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_local(Stack),
 yeccgoto_exp(hd(Nss), local, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, name, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_name(Stack),
 yeccgoto_exp(hd(Nss), name, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_or(Stack),
 yeccgoto_exp(hd(Nss), 'or', Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, repeat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_repeat(Stack),
 yeccgoto_exp(hd(Nss), repeat, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, then, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_then(Stack),
 yeccgoto_exp(hd(Nss), then, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, until, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_until(Stack),
 yeccgoto_exp(hd(Nss), until, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_S, while, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_while(Stack),
 yeccgoto_exp(hd(Nss), while, Nss, NewStack, T, Ts, Tzr);
yeccpars2_57(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_58(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_59(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_60(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_exp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_laststat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_65(S, until, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_66: see yeccpars2_7

yeccpars2_67(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_69: see yeccpars2_7

yeccpars2_70(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, then, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_21(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_72: see yeccpars2_0

yeccpars2_73(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_75(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_77(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_78(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_namelist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_0

yeccpars2_81(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_namelist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_funcbody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_86: see yeccpars2_7

yeccpars2_87(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_21(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_7

yeccpars2_89(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_21(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_0

yeccpars2_91(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_21(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_prefixexp(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_functioncall(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_98: see yeccpars2_7

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_100(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_101(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_explist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_args(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_103: see yeccpars2_7

yeccpars2_104(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_explist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_105: see yeccpars2_7

yeccpars2_106(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '~=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_stat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_args(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_block(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_block(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_chunk(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_chunk(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_chunk(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_chunk(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_chunk(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_chunk(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_chunk(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_exp(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(63, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(57, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(56, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exp(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(106, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_explist(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(100, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_funcbody(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_functioncall(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functioncall(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_laststat(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_laststat(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_laststat(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_laststat(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_laststat(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_laststat(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_laststat(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_namelist(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_prefixexp(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefixexp(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_stat(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(46=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_stat(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unop(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(42, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unop(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(19, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_var(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(13, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(42=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(72, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-file("luaparse.yrl", 160).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { var , __1 }
  end | __Stack].

-compile({inline,yeccpars2_2_/1}).
-file("luaparse.yrl", 26).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("luaparse.yrl", 28).
yeccpars2_4_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("luaparse.yrl", 160).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { var , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_44_$end'/1}).
-file("luaparse.yrl", 152).
'yeccpars2_44_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_('/1}).
-file("luaparse.yrl", 152).
'yeccpars2_44_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_)'/1}).
-file("luaparse.yrl", 152).
'yeccpars2_44_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_,'/1}).
-file("luaparse.yrl", 152).
'yeccpars2_44_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_44_;'/1}).
-file("luaparse.yrl", 152).
'yeccpars2_44_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_and/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_and(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_do/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_end/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_for/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_function/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_if/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_local/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_name/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_or/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_repeat/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_then/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_until/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_44_while/1}).
-file("luaparse.yrl", 152).
yeccpars2_44_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_$end'/1}).
-file("luaparse.yrl", 154).
'yeccpars2_45_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_('/1}).
-file("luaparse.yrl", 154).
'yeccpars2_45_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_)'/1}).
-file("luaparse.yrl", 154).
'yeccpars2_45_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_,'/1}).
-file("luaparse.yrl", 154).
'yeccpars2_45_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_45_;'/1}).
-file("luaparse.yrl", 154).
'yeccpars2_45_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_do/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_end/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_for/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_function/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_if/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_local/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_name/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_repeat/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_then/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_until/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_45_while/1}).
-file("luaparse.yrl", 154).
yeccpars2_45_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("luaparse.yrl", 45).
yeccpars2_48_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { while , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("luaparse.yrl", 27).
yeccpars2_49_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("luaparse.yrl", 29).
yeccpars2_50_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,'yeccpars2_51_$end'/1}).
-file("luaparse.yrl", 153).
'yeccpars2_51_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_51_('/1}).
-file("luaparse.yrl", 153).
'yeccpars2_51_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_51_)'/1}).
-file("luaparse.yrl", 153).
'yeccpars2_51_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_51_,'/1}).
-file("luaparse.yrl", 153).
'yeccpars2_51_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_51_;'/1}).
-file("luaparse.yrl", 153).
'yeccpars2_51_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_do/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_end/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_for/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_function/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_if/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_local/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_name/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_or/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_repeat/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_then/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_until/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_51_while/1}).
-file("luaparse.yrl", 153).
yeccpars2_51_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("luaparse.yrl", 146).
yeccpars2_52_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_53_$end'/1}).
-file("luaparse.yrl", 150).
'yeccpars2_53_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_53_('/1}).
-file("luaparse.yrl", 150).
'yeccpars2_53_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_53_)'/1}).
-file("luaparse.yrl", 150).
'yeccpars2_53_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_53_,'/1}).
-file("luaparse.yrl", 150).
'yeccpars2_53_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_53_;'/1}).
-file("luaparse.yrl", 150).
'yeccpars2_53_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_and/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_and(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_do/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_end/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_for/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_function/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_if/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_local/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_name/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_or/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_repeat/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_then/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_until/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_53_while/1}).
-file("luaparse.yrl", 150).
yeccpars2_53_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_54_$end'/1}).
-file("luaparse.yrl", 149).
'yeccpars2_54_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_54_('/1}).
-file("luaparse.yrl", 149).
'yeccpars2_54_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_54_)'/1}).
-file("luaparse.yrl", 149).
'yeccpars2_54_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_54_,'/1}).
-file("luaparse.yrl", 149).
'yeccpars2_54_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_54_;'/1}).
-file("luaparse.yrl", 149).
'yeccpars2_54_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_and/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_and(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_do/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_end/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_for/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_function/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_if/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_local/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_name/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_or/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_repeat/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_then/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_until/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_54_while/1}).
-file("luaparse.yrl", 149).
yeccpars2_54_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_55_$end'/1}).
-file("luaparse.yrl", 151).
'yeccpars2_55_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_55_('/1}).
-file("luaparse.yrl", 151).
'yeccpars2_55_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_55_)'/1}).
-file("luaparse.yrl", 151).
'yeccpars2_55_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_55_,'/1}).
-file("luaparse.yrl", 151).
'yeccpars2_55_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_55_;'/1}).
-file("luaparse.yrl", 151).
'yeccpars2_55_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_and/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_and(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_do/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_end/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_for/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_function/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_if/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_local/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_name/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_or/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_repeat/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_then/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_until/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_55_while/1}).
-file("luaparse.yrl", 151).
yeccpars2_55_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_56_$end'/1}).
-file("luaparse.yrl", 148).
'yeccpars2_56_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_56_('/1}).
-file("luaparse.yrl", 148).
'yeccpars2_56_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_56_)'/1}).
-file("luaparse.yrl", 148).
'yeccpars2_56_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_56_,'/1}).
-file("luaparse.yrl", 148).
'yeccpars2_56_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_56_;'/1}).
-file("luaparse.yrl", 148).
'yeccpars2_56_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_and/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_and(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_do/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_end/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_for/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_function/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_if/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_local/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_name/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_or/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_repeat/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_then/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_until/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_56_while/1}).
-file("luaparse.yrl", 148).
yeccpars2_56_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_57_$end'/1}).
-file("luaparse.yrl", 147).
'yeccpars2_57_$end'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_57_('/1}).
-file("luaparse.yrl", 147).
'yeccpars2_57_('(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_57_)'/1}).
-file("luaparse.yrl", 147).
'yeccpars2_57_)'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_57_,'/1}).
-file("luaparse.yrl", 147).
'yeccpars2_57_,'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,'yeccpars2_57_;'/1}).
-file("luaparse.yrl", 147).
'yeccpars2_57_;'(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_and/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_and(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_do/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_do(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_end/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_end(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_for/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_for(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_function/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_function(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_if/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_if(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_local/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_local(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_name/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_name(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_or/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_or(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_repeat/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_repeat(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_then/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_then(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_until/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_until(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_57_while/1}).
-file("luaparse.yrl", 147).
yeccpars2_57_while(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("luaparse.yrl", 144).
yeccpars2_58_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("luaparse.yrl", 142).
yeccpars2_59_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("luaparse.yrl", 141).
yeccpars2_60_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("luaparse.yrl", 143).
yeccpars2_61_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("luaparse.yrl", 145).
yeccpars2_62_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { binop , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("luaparse.yrl", 155).
yeccpars2_63_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { unop , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("luaparse.yrl", 35).
yeccpars2_64_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { return , __2 }
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("luaparse.yrl", 46).
yeccpars2_67_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { repeat , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("luaparse.yrl", 42).
yeccpars2_70_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { localassign , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("luaparse.yrl", 47).
yeccpars2_74_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { ifblock , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("luaparse.yrl", 49).
yeccpars2_76_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { function , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("luaparse.yrl", 82).
yeccpars2_79_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("luaparse.yrl", 83).
yeccpars2_82_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-file("luaparse.yrl", 88).
yeccpars2_84_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("luaparse.yrl", 48).
yeccpars2_92_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { for2 , { __2 , __4 , __6 } , __8 }
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("luaparse.yrl", 44).
yeccpars2_94_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { do , __2 }
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("luaparse.yrl", 161).
yeccpars2_96_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("luaparse.yrl", 63).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { call , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("luaparse.yrl", 94).
yeccpars2_99_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { args , __1 }
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("luaparse.yrl", 99).
yeccpars2_101_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("luaparse.yrl", 93).
yeccpars2_102_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { args , __2 }
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("luaparse.yrl", 100).
yeccpars2_104_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("luaparse.yrl", 41).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { assign , __1 , __3 }
  end | __Stack].


-file("luaparse.yrl", 174).
