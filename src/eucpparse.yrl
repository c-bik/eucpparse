%% -*- erlang -*-
Header "%% Copyright (C) Bikram Chatterjee"
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email razorpeak@gmail.com".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 ucps
 ucp
 pdu
 header
 op
 ot
 data
 checksum
.

Terminals
 STX
 ETX
 DAT
 '/'
 'O'
 'R'
 ','
.

Rootsymbol ucps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ucps -> '$empty'   : [].
ucps -> ucp ucps   : ['$1'|'$2'].

ucp -> STX pdu ETX : '$2'.

pdu -> header '/' data '/' checksum : #{header => '$1', data => '$3',
                                        checksum => '$5'}.

header -> DAT '/' DAT '/' op  '/' ot : #{trn => unwrap('$1'),
                                         len => unwrap('$3'),
                                         op => '$5', ot => '$7'}.

op -> 'O'   :   'OPERATION'.
op -> 'R'   :   'RESULT'.

ot -> DAT   : unwrap('$1').

data -> DAT : unwrap('$1').
checksum -> DAT : unwrap('$1').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

% parser and compiler interface
-export([unpack/1, parse_i/1]).

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------
unwrap({_,_,X}) -> X.
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec unpack(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, tuple()}.
unpack(Ucp) ->
   case parse_i(Ucp) of
       {ok, {ParseTree, _Tokens}} -> {ok, ParseTree};
       Error -> Error
   end.

-spec parse_i(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {tuple(), list()}}.
parse_i(<<2, _/binary>> = Ucp) ->
    parse_i(binary_to_list(Ucp));
parse_i([]) -> error(badarg);
parse_i(Ucp) when is_list(Ucp), length(Ucp) > 0 ->
    case lists:last(Ucp) of
        3 ->
            case eucpparse_lex:string(Ucp) of
                {ok, Toks, _} ->
                    case eucpparse:parse(Toks) of
                        {ok, PTree} -> {ok, {PTree, Toks}};
                        {error, {Line, Module, Message}} ->
                            {parse_error,
                             {Line, lists:flatten(Module:format_error(Message)), Toks}}
                    end;
                LexErrorInfo -> {lex_error, eucpparse_lex:format_error(LexErrorInfo)}
            end;
        _ -> error(badarg)
    end;
parse_i(_) -> error(badarg).
%%-----------------------------------------------------------------------------


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%-----------------------------------------------------------------------------
%%                               EUnit test
%%-----------------------------------------------------------------------------

parse_test() ->
    ?debugMsg("==========================================="),
    ?debugMsg("|    J S O N   P A T H   P A R S I N G    |"),
    ?debugMsg("==========================================="),
    catch application:start(?MODULE),
    Cwd = filename:absname(""),
    {ShowParseTree, Tests} =
        case file:consult(filename:join([Cwd, "..", "test", "test.txt"])) of
            {ok, [show_parse_tree, T]}  -> {true, T};
            {ok, [_, T]}                -> {false, T};
            {ok, [T]}                   -> {false, T};
            {error, Error}              -> ?assertEqual(ok, Error)
        end,
    ?debugFmt("Test result ~p parse tree"
              , [if ShowParseTree -> with; true -> without end]),
    test_parse(1, ShowParseTree, Tests).

test_parse(_, _, []) -> ok;
test_parse(N, ShowParseTree, [{Test,Target}|Tests]) when is_binary(Test) ->
    test_parse(N, ShowParseTree, [{binary_to_list(Test),Target}|Tests]);
test_parse(N, ShowParseTree, [{Test,Target}|Tests]) ->
    ?debugFmt("[~p]----------------------------------------",[N]),
    ?debugFmt("~ts", [Test]),
    {Tokens,EndLine} = case t_tokenize(Test) of
        {ok,T,E} -> {T,E};
        {error, Error} ->
            ?debugFmt("Tokenize Error ~p", [Error]),
            ?assertEqual(ok, tokenize_error)
    end,
    PTree = case t_parse(Tokens) of
        {ok, PT} -> PT;
        {error, {Line, PError}} ->
            ?debugFmt("Parse Error at ~p : ~s", [Line, PError]),
            ?debugFmt("Tokens ~p:~p", [EndLine,Tokens]),
            ?assertEqual(ok, parsing_error)
    end,
    ?assertEqual(Target, PTree),
    if ShowParseTree -> ?debugFmt("~p", [PTree]); true -> ok end,
    FoldTest = case jpparse:string(PTree) of
        {ok, Ft} -> Ft;
        {error, FError} ->
            ?debugFmt("Folding Error : ~p", [FError]),
            ?debugFmt("ParseTree :~p", [PTree]),
            ?assertEqual(ok, fold_error)
    end,
    ?assertEqual(re:replace(Test, "[[:space:]]*", "", [global,{return,list}]),
                 binary_to_list(FoldTest)),
    test_parse(N+1, ShowParseTree, Tests).

t_tokenize(Test) ->
    case jsonpath_lex:string(Test) of
        {ok,Tokens,EndLine} -> {ok,Tokens,EndLine};
        ErrorInfo -> {error, jsonpath_lex:format_error(ErrorInfo)}
    end.

t_parse(Tokens) ->
    case jpparse:parse(Tokens) of
        {ok, PTree} -> {ok, PTree};
        {error, {Line, Module, Message}} ->
            {error, {Line, lists:flatten(Module:format_error(Message))}}
    end.

%%-----------------------------------------------------------------------------

-endif.
