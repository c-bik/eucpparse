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
 header_data
 op
 o60
 r60
 opt
 checksum
.

Terminals
 STX
 ETX
 DAT
 O60
 R60
 '/'
% ','
.

Rootsymbol ucps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ucps -> '$empty'   : [].
ucps -> ucp ucps   : ['$1'|'$2'].

ucp -> STX pdu ETX : '$2'.

pdu -> header_data '/' checksum : ('$1')#{checksum => '$3'}.

header_data -> DAT '/' DAT '/' op : ('$5')#{trn => uw('$1'),
                                            len => uw('$3')}.

op -> O60 '/' o60 : ('$3')#{op => 'O', ot => 60}.
op -> R60 '/' r60 : ('$3')#{op => 'R', ot => 60}.

%      1       3       5       7       9       11      13      15      17      19      21      23
o60 -> DAT '/' opt '/' opt '/' DAT '/' DAT '/' opt '/' DAT '/' opt '/' opt '/' opt '/' opt '/' opt
       : #{oadc => uw('$1'), oton => '$3', onpi => '$5', stype => uw('$7'),
           pwd => str(uw('$9')), npwd => str('$11'), vers => uw('$13'),
           ladc => '$15', lton => '$17', lnpi => '$19', opid => '$21',
           res1 => '$23'}.

r60 -> DAT '/' opt          : #{ack => true, sm => '$3'}.
r60 -> DAT '/' DAT '/' opt  : #{ack => false, ec => uw('$3'), sm => '$5'}.

opt -> '$empty' : none.
opt -> DAT      : uw('$1').

checksum -> DAT : uw('$1').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

% parser and compiler interface
-export([unpack/1, parse_i/1]).

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------
uw({_,_,X}) -> X.

str(none) -> none;
str([]) -> [];
str([A,B|R]) -> [list_to_integer([A,B],16) | str(R)].

% ira([]) -> [];
% ira([C|R]) -> [integer_to_list(C,16) | ira(R)].
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
parse_i([2|_] = Ucp) when is_list(Ucp) ->
    case lists:last(Ucp) of
        3 ->
            case eucpparse_lex:string(Ucp) of
                {ok, Toks, _} ->
                    case eucpparse:parse(Toks) of
                        {ok, PTree} -> {ok, {PTree, Toks}};
                        {error, {Line, Module, Message}} ->
                            {parse_error,
                             {Line, lists:flatten(
                                      Module:format_error(Message)), Toks}}
                    end;
                LexErrorInfo ->
                    {lex_error, eucpparse_lex:format_error(LexErrorInfo)}
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

parse_test_() ->
    {inparallel,
     [{U, fun() ->
                  U0 = lists:flatten([2,U,3]),
                  {ok, Toks, 1} = eucpparse_lex:string(U0),
                  %?debugFmt("Toks ~p", [Toks]),
                  {ok,[Pt]} = eucpparse:parse(Toks),
                  %?debugFmt("Pt ~p", [Pt]),
                  ?assertEqual(T, Toks),
                  ?assertEqual(P, Pt)
          end}
      || {U,T,P} <-
         [{"53/00060/O/60/30035/6/5/1/74657374736D63683231//0100//////8A",
           [{'STX',1}, {'DAT',1,"53"}, {'/',1}, {'DAT',1,"00060"}, {'/',1},
            {'O60',1}, {'/',1}, {'DAT',1,"30035"}, {'/',1}, {'DAT',1,"6"},
            {'/',1}, {'DAT',1,"5"}, {'/',1}, {'DAT',1,"1"}, {'/',1},
            {'DAT',1,"74657374736D63683231"}, {'/',1}, {'/',1},
            {'DAT',1,"0100"}, {'/',1}, {'/',1}, {'/',1}, {'/',1}, {'/',1},
            {'/',1}, {'DAT',1,"8A"}, {'ETX',1}],
           #{checksum => "8A", ladc => none, len => "00060", lnpi => none,
             lton => none, npwd => none, oadc => "30035", onpi => "5",
             op => 'O', opid => none, ot => 60, oton => "6",
             pwd => "testsmch21", res1 => none, stype => "1", trn => "53",
             vers => "0100"}},
          {"53/00080/O/60/30035/2/1/2/70617373776F7264/6E657770617373776F7264/"
           "0100////39//D4",
           [{'STX',1}, {'DAT',1,"53"}, {'/',1}, {'DAT',1,"00080"}, {'/',1},
            {'O60',1}, {'/',1}, {'DAT',1,"30035"}, {'/',1}, {'DAT',1,"2"},
            {'/',1}, {'DAT',1,"1"}, {'/',1}, {'DAT',1,"2"}, {'/',1},
            {'DAT',1,"70617373776F7264"}, {'/',1},
            {'DAT',1,"6E657770617373776F7264"}, {'/',1}, {'DAT',1,"0100"},
            {'/',1}, {'/',1}, {'/',1}, {'/',1}, {'DAT',1,"39"}, {'/',1},
            {'/',1}, {'DAT',1,"D4"}, {'ETX',1}],
           #{checksum => "D4", ladc => none, len => "00080", lnpi => none,
             lton => none, npwd => "newpassword", oadc => "30035", onpi => "1",
             op => 'O', opid => "39", ot => 60, oton => "2", pwd => "password",
             res1 => none, stype => "2", trn => "53", vers => "0100"}}
         ]
       ]
    }.

%%-----------------------------------------------------------------------------

-endif.
