%% Copyright (c) 2024 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : lfe_scan.erl
%% Author  : Robert Virding
%% Purpose : Token scanner for the Lisp Flavoured Erlang language.

%%% Much of the basic structure has been taken from erl_scan.
%%%
%%% We chain a state, #lfe_scan{}, through the scanner but we never
%%% use it for anything. I was inherited from erl_scan and we left it
%%% just in case we might need it.

-module(lfe_scan).

-export([string/1,string/2,string/3,token/2,token/3,tokens/2,tokens/3,
         format_error/1]).

-export([start_symbol_char/1,symbol_char/1]).

-export([token_test/1,token_test/2]).

format_error({illegal,S}) ->
    ["illegal characters in ",io_lib:write_string(S)];
format_error(tq_string) ->
    "bad triple-quoted string";
format_error({user,S}) -> S;
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

%% Nothing in here yet, but who knows.
-record(lfe_scan, {}).

%% LFE definitions of these types.
-define(WHITE_SPACE(C), (C >= $\000 andalso C =< $\s)).
-define(DIGIT(C), (C >= $0 andalso C =< $9)).
-define(CHAR(C), (C >= O andalso C < 16#110000)).
-define(UNICODE(C),
        (is_integer(C) andalso
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF))).

-define(UNI255(C), (is_integer(C) andalso 0 =< C andalso C =< 16#ff)).

%% string(String) ->
%% string(String, StartLine) ->
%% string(String, StartLine, Options) ->
%%     {ok,Tokens,LastLine}  | {error,Error,LastLine}.
%%  Scan a string for the tokens in it.

string(String) ->
    string(String, 1, []).

string(String, StartLocation) ->
    string(String, StartLocation, []).

string(String, StartLocation, _Options) ->
    string1(String, StartLocation, 1, [], #lfe_scan{}).

%% string1(Chars, Line, Column, Tokens, State) ->
%%     {ok,Tokens,LastLine} | {error,Error,LastLine}.

string1(Cs, Line, _Col, Toks, _St) when Cs =:= [] orelse Cs =:= eof ->
    %% No more chars left in the string.
    {ok,lists:reverse(Toks),anno(Line)};
string1(Cs0, Line0, Col0, Toks0, St0) ->
    %% Keep going!
    case scan1(Cs0, Line0, Col0, St0) of
        {more,{Cs1,Line1,Col1,St1,Extra,Fun}} ->
            %% Needs more, try again by appending eof.
            case Fun(Cs1 ++ eof, Line1, Col1, St1, Extra) of
                %% What to do when we get more here.
                {more,{Cs2,Line2,Col2,St2,Extra2,Fun2}} ->
                    Fun2(Cs2, Line2, Col2, St2, Extra2);
                {ok,Tok,_Rest,Line2,_Col2, _St2} ->
                    %% It worked and all is done.
                    {ok,lists:reverse([Tok|Toks0]),anno(Line2)};
                {{error,_,_}=Error,_Rest} ->
                    %% Really bad!
                    Error
            end;
        {none,Rest,Line1,Col1,St1} ->
            %% Nothing from this call so try again.
            string1(Rest, Line1, Col1, Toks0, St1);
        {ok,Tok,Rest,Line1,Col1,St1} ->
            %% It worked but still has chars left, try agiain.
            string1(Rest, Line1, Col1, [Tok|Toks0], St1);
        {{error,_,_}=Error,_Rest} ->
            %% Bad!
            Error
    end.

%% token(Continuation, String) ->
%% token(Continuation, String, StartLine) ->
%%     {more,Continuation} | {done,ReturnVal,RestChars}.
%%  Scan all tokens in the repeated calls threading the continuation
%%  through the calls.

token(Cont, Chars) ->
    token(Cont, Chars, 1, []).

token(Cont, Chars, StartLine) ->
    token(Cont, Chars, StartLine, []).

token([], Chars, Line, _Options) ->
    %% io:format("t4 ~p\n", [{[],Chars,Line,_Options}]),
    token1(Chars, Line, 1, #lfe_scan{}, [], fun scan/5); 
token({lfe_scan_token,Cs,Line,Col,St,Extra,Fun},
       Chars, _Line, _Options) ->
    %% io:format("t4 ~p\n", [{lfe_scan_token,Chars,_Line,_Options}]),
    token1(Cs ++ Chars, Line, Col, St, Extra, Fun).

%% token1(Chars, Line, Column, State, Extra, Fun) ->
%%     {done,{ok,Token,Line},Cs} | {done,Error,Cs} | {more,Continuation}.

token1(eof, Line, Col, St, Extra, Fun) ->
    Fun(eof, Line, Col, St, Extra);
token1([], Line, Col, St, Extra, Fun) ->
    {more,{lfe_scan_token,[],Line,Col,St,Extra,Fun}};
token1(Cs0, Line0, Col0, St0, Extra0, Fun0) ->
    %% io:format("t11 ~p\n", [{Cs0,Fun0,fun scan/5]),
    case Fun0(Cs0, Line0, Col0, St0, Extra0) of
        {more,{Cs1,Line1,Col1,St1,Extra1,Fun1}} ->
            %% io:format("tf1 ~p\n", [{Cs1,Line1,Col1,Fun1,fun scan/5}]),
            {more,{lfe_scan_token,Cs1,Line1,Col1,St1,Extra1,Fun1}};
        {none,Rest,Line1,Col1,St1} ->
            %% Nothing from this call so try again.
            token1(Rest, Line1, Col1, St1, Extra0, Fun0);
        {ok,Token,Rest,Line1,_Col1,_St1} ->
            {done,{ok,Token,Line1},Rest};
        {{error,_,_}=Error,Rest} ->
            {done,Error,Rest}
    end.

%% tokens(Continuation, String) ->
%% tokens(Continuation, String, StartLine) ->
%% tokens(Continuation, String, StartLine, Options) ->
%%     {more,Continuation} | {done,ReturnVal,RestChars}.
%%  Scan all tokens in the repeated calls threading the continuation
%%  through the calls.

tokens(Cont, Chars) ->
    %% io:format("ts2 ~p\n", [{Cont,Chars}]),
    tokens(Cont, Chars, 1, []).

tokens(Cont, Chars, StartLine) ->
    %% io:format("ts3 ~p\n", [{Cont,Chars,StartLine}]),
    tokens(Cont, Chars, StartLine, []).

tokens([], Chars, Line, _Options) ->
    %% io:format("ts4 ~p\n", [{[],Chars,Line,_Options}]),
    tokens1(Chars, Line, 1, [], #lfe_scan{}, [], fun scan/5); 
tokens({lfe_scan_tokens,Cs,Line,Col,Toks,St,Extra,Fun},
       Chars, _Line, _Options) ->
    %% io:format("ts4 ~p\n", [{lfe_scan_tokens,Cs,Chars,Toks,Extra,_Line,_Options}]),
    tokens1(Cs ++ Chars, Line, Col, Toks, St, Extra, Fun).

%% tokens1(Chars, Line, Column, State, Extra, Fun) ->
%%     {done,{ok,Token,Line},Cs} | {done,Error,Cs} | {more,Continuation}.
%%  We loop inside this function for as long as we can until we need
%%  more characters or there is an error.

tokens1(eof, Line, _Col, Toks, _St, _Extra, _Fun) ->
    %% io:format("ts1 ~p\n", [{done,Line,Toks}]),
    {done,{ok,lists:reverse(Toks),anno(Line)},[]};
tokens1([], Line, Col, Toks, St, Extra, Fun) ->
    %% io:format("ts1 ~p\n", [{[],Line,length(Toks)}]),
    {more,{lfe_scan_tokens,[],Line,Col,Toks,St,Extra,Fun}};
tokens1(Cs, Line0, Col0, Toks, St0, Extra0, Fun0) ->
    %% io:format("ts1 ~p\n", [{cs,Line0,length(Toks0)}]),
    case Fun0(Cs, Line0, Col0, St0, Extra0) of
        {more,{Cs1,Line1,Col1,St1,Extra1,Fun1}} ->
            %% io:format("more here\n", []),
            {more,{lfe_scan_tokens,Cs1,Line1,Col1,Toks,St1,Extra1,Fun1}};
        {none,Rest,Line1,Col1,St1} ->
            %% Nothing from this call so try again.
            tokens1(Rest, Line1, Col1, Toks, St1, Extra0, fun scan/5);
        {ok,Tok,Rest,Line1,Col1,St1} ->
            %% io:format("ok ~p\n", [{Tok,Rest}]),
            %% We have a token so now we "restart" a new one with scan.
            tokens1(Rest, Line1, Col1, [Tok|Toks], St1, [], fun scan/5);
        {{error,_,_}=Error,Rest} ->
            {done,Error,Rest}
    end.

%%
%% Now to the actual scanning and collecting tokens.
%%

%% scan(Chars, Line, Column, St, Extra) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan one token.

scan(Chars, Line, Col, St, _Extra) ->
    scan1(Chars, Line, Col, St).

%% scan1(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | None|  ScanError.

%% scan1_fun(Cs, Line, Col, St, _Extra) ->
%%     scan1(Cs, Line, Col, St).

%% Strings
scan1([$"|Cs], Line, Col, St) ->
    %% We handle both normal strings and triple quote strings here.
    scan_string(Cs, Line, Col, St);
%% Newlines and white space.
scan1([$\n|Cs], Line, _Col, St) ->
    scan1(Cs, Line+1, 1, St);
scan1([C|Cs], Line, Col, St) when ?WHITE_SPACE(C) ->
    scan1(Cs, Line, Col+1, St);
%% Comments.
scan1([$;|Cs], Line, Col, St) ->
    scan_line_comment(Cs, Line, Col+1, St);
scan1([$#,$||Cs], Line, Col, St) ->
    scan_block_comment(Cs, Line, Col+2, St); 
%% These not start symbol chars must be handled specially.
scan1([$||Cs], Line, Col, St) ->
    scan_qsymbol(Cs, Line, Col, St);
scan1([$#|Cs], Line, Col, St) ->
    scan_hash(Cs, Line, Col, St);
%% We do the one character separators which are also start symbol chars.
scan1([$'|Cs], Line, Col, St) ->
    {ok,{'\'',anno(Line)},Cs,Line,Col+1,St};
scan1([$`|Cs], Line, Col, St) ->
    {ok,{'\`',anno(Line)},Cs,Line,Col+1,St};
scan1([$.|Cs], Line, Col, St) ->
    {ok,{'.',anno(Line)},Cs,Line,Col+1,St};
scan1([$,,$@|Cs], Line, Col, St) ->
    {ok,{',@',anno(Line)},Cs,Line,Col+2,St};
scan1([$,|Cs], Line, Col, St) ->
    {ok,{',',anno(Line)},Cs,Line,Col+1,St};
%% Now it gets a little tricky, everything starts with a start symbol
%% char and is a symbol or single char or a NUMBER!
scan1([C|Cs], Line, Col, St) ->
    case start_symbol_char(C) of
        true ->
            scan_symbol([C|Cs], Line, Col, St);
        false ->
            Tok = {list_to_atom([C]),anno(Line)},
            {ok,Tok,Cs,Line,Col,St}
    end;
scan1(Cs, Line, Col, St) when Cs =:= [] orelse Cs =:= eof ->
    {none,Cs,Line,Col,St}.

%% scan_line_comment(Chars, Line, Column, State) ->
%%     {ok,Tokens,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Skip to the end of the line.
    
scan_line_comment_fun(Chars, Line, Col, St, _Extra) -> 
    scan_line_comment(Chars, Line, Col, St).

scan_line_comment([$\n|Cs], Line, _Col, St) ->
    {none,Cs,Line+1,0,St};
scan_line_comment([_C|Cs], Line, Col, St) ->
    scan_line_comment(Cs, Line, Col+1, St);
scan_line_comment([]=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan_line_comment_fun/5}};
scan_line_comment(eof=Cs, Line, Col, St) ->
    {none,Cs,Line,Col,St}.

%% scan_block_comment(Chars, Line, Column, State) ->
%%     {ok,Tokens,Chars,Line,Column} | {more,Continuation} |  | ScanError.
%%  Skip to the end of the block comment.

scan_block_comment_fun(Chars, Line, Col, St, _Extra) ->
    scan_block_comment(Chars, Line, Col, St).

scan_block_comment("|#" ++ Cs, Line, Col, St) ->
    {none,Cs,Line,Col,St};
scan_block_comment([$\n|Cs], Line, _Col, St) ->
    scan_block_comment(Cs, Line+1, 0, St);
scan_block_comment("#|" ++ Cs, Line, Col, _St) ->
    scan_error({illegal,"nested block comment"}, Line, Col, Line, Col, Cs);
scan_block_comment([_C|Cs], Line, Col, St) ->
    scan_block_comment(Cs, Line, Col+1, St);
scan_block_comment([]=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan_block_comment_fun/5}};
scan_block_comment(eof=Cs, Line, Col, _St) ->
    %% No ending.
    scan_error({illegal,"block comment"}, Line, Col, Line, Col, Cs).

%% scan_hash(Chars, Line, Column, St) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan a hash character symbol.

scan_hash(Cs0, Line, Col, St) ->
    %% Skip over any leading digits which we ignore here.
    Cs1 = lists:dropwhile(fun (C) -> ?DIGIT(C) end, Cs0),
    scan_hash1(Cs1, Line, Col, St).

scan_hash1_fun(Cs, Line, Col, St, _Extra) ->
    scan_hash1(Cs, Line, Col, St).

scan_hash1([$\\,C|Cs], Line, Col, St) ->
    {ok,{number,Line,C},Cs,Line,Col,St};
scan_hash1([$\(|Cs], Line, Col, St) ->
    {ok,{'#(',Line},Cs,Line,Col,St};
scan_hash1([$.|Cs], Line, Col, St) ->
    {ok,{'#.',Line},Cs,Line,Col,St};
scan_hash1([$`|Cs], Line, Col, St) ->
    {ok,{'#`',Line},Cs,Line,Col,St};
scan_hash1([$;|Cs], Line, Col, St) ->
    {ok,{'#;',Line},Cs,Line,Col,St};
scan_hash1([$,,$@|Cs], Line, Col, St) ->        %Get this before #,
    {ok,{'#,@',Line},Cs,Line,Col,St};
scan_hash1([$,|Cs], Line, Col, St) ->
    {ok,{'#,',Line},Cs,Line,Col,St};
scan_hash1([$"|Cs], Line, Col, St) ->
    scan_binary_string(Cs, Line, Col+1, St);
scan_hash1([$'|Cs], Line, Col, St) ->
    scan_fun(Cs, Line, Col+1, St);
%% Scan binary and map tokens, these must come before the based number.
scan_hash1([C,$\(|Cs], Line, Col, St) when (C =:= $b) or (C =:= $B) ->
    {ok,{'#B(',Line},Cs,Line,Col,St};
scan_hash1([C,$\(|Cs], Line, Col, St) when (C =:= $m) or (C =:= $M) ->
    {ok,{'#M(',Line},Cs,Line,Col,St};
%% Scan based numbers, these must come after binary and map tokens.
scan_hash1([$*|Cs], Line, Col, St) ->
    scan_bnumber(Cs, 2, Line, Col+1, St);
scan_hash1([C,C1|Cs], Line, Col, St) when       %Must not clash with #b(
      (C =:= $b) or (C =:= $B) andalso C1 =/= $( ->
    scan_bnumber([C1|Cs], 2, Line, Col+1, St);
scan_hash1([C|Cs], Line, Col, St) when (C =:= $o) or (C =:= $O) ->
    scan_bnumber(Cs, 8, Line, Col+1, St);
scan_hash1([C|Cs], Line, Col, St) when (C =:= $d) or (C =:= $D) ->
    scan_bnumber(Cs, 10, Line, Col+1, St);
scan_hash1([C|Cs], Line, Col, St) when (C =:= $x) or (C =:= $X) ->
    scan_bnumber(Cs, 16, Line, Col+1, St);
%% scan_hash1([C|Cs], Line, Col, St) when (C =:= $r) or (C =:= $R) ->
%%     scan_bnumber(Cs, 2, Line, Col+1, St);
%% Eof, [] or other chars, so we are really done!
scan_hash1(Cs, Line, Col, St) when Cs =/= eof ->
    {more,{Cs,Line,Col,St,[],fun scan_hash1_fun/5}};
scan_hash1(eof=Cs, Line, Col, _St) when Cs =/= eof ->
    scan_error({illegal,"#"}, Line, Col, Line, Col+1, Cs).

%% scan_fun(Chars, Line, Column, St) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan a fun symbol.

scan_fun(Cs, Line, Col, St) ->
    scan_fun1(Cs, Line, Col, [], St).

scan_fun1_fun(Cs, Line, Col, St, Symcs) ->
    scan_fun1(Cs, Line, Col, Symcs, St).

scan_fun1([C|Cs], Line, Col, Symcs, St) ->
    case symbol_char(C) of
        true ->
            scan_fun1(Cs, Line, Col+1, Symcs ++ [C], St);
        false ->
            scan_fun_ret(Symcs, [C|Cs], Line, Col, St)
    end;
scan_fun1([]=Cs, Line, Col, Symcs, St) ->
    {more,{Cs,Line,Col,St,Symcs,fun scan_fun1_fun/5}};
scan_fun1(eof, Line, Col, Symcs, St) ->
    scan_fun_ret(Symcs, eof, Line, Col, St).

scan_fun_ret(Symcs, Cs, Line, Col, St) ->
    case split_fun_chars(Symcs, [], []) of
        {Pre,[]} ->
            scan_error({illegal,[$#,$'|Pre]}, Line, Col, Line, Col, Symcs);
        {Pre,[$/|Ds]=After} ->
            Field = Pre ++ After,
            Token = case lists:any(fun (C) -> not ?DIGIT(C) end, Ds) of
                        true -> {symbol,Line,list_to_atom(Field)};
                        false -> {'#\'',Line,Field}
                    end,
            {ok,Token,Cs,Line,Col,St}
    end.

%% split_fun_chars(Chars, Pre, After) -> {Pre,After}.

split_fun_chars([$/|Cs], Pre, After) ->
    split_fun_chars(Cs, Pre ++ After, [$/]);
split_fun_chars([C|Cs], Pre, After) when After =:= [] ->
    split_fun_chars(Cs, Pre ++ [C], After);
split_fun_chars([C|Cs], Pre, After) when After =/= [] ->
    split_fun_chars(Cs, Pre, After ++ [C]);
split_fun_chars([], Pre, After) ->
    {Pre,After}.

%% scan_bnumber(Chars, Base, Line, Column, St) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan a based number.

scan_bnumber(Cs, Base, Line, Col, St) ->
    scan_bnumber_sign(Cs, Line, Col, Base, St).

scan_bnumber_sign([$+|Cs], Line, Col, Base, St) ->
    scan_bnumber_digits(Cs, Line, Col, 0, Base, +1, St);
scan_bnumber_sign([$-|Cs], Line, Col, Base, St) ->
    scan_bnumber_digits(Cs, Line, Col, 0, Base, -1, St);
scan_bnumber_sign(Cs, Line, Col, Base, St) ->
    %% Pass the buck!
    scan_bnumber_digits(Cs, Line, Col, 0, Base, +1, St).

%% scan_bnumber_1((Cs, Line, Col, Base, St) ->
%%     {more,{Cs,Line,Col,St,Base,fun scan_bnumber1_fun/5};
%% scan_bnumber_1(eof=Cs, Line, Col, _Base, St) ->
%%     scan_error({illegal,"based number"}, Line, Col, Line, Col, Cs);

scan_bnumber_digits_fun(Cs, Line, Col, St, {Number,Base,Sign}) ->
    scan_bnumber_digits(Cs, Line, Col, Number, Base, Sign, St).

scan_bnumber_digits([C|Cs], Line, Col, Number0, Base, Sign, St) ->
    %% io:format("sbt ~p\n",
    %%           [{C,Base,Number0,base_collect_char(C, Base, Number0)}]),
   case symbol_char(C) andalso base_collect_char(C, Base, Number0) of
       {yes,Number1} ->
           scan_bnumber_digits(Cs, Line, Col+1, Number1, Base, Sign, St);
       no -> 
           %% Illegal based number.
           scan_error({illegal,"based number"}, Line, Col, Line, Col, [C|Cs]);
       false ->
           %% Done here.
           Token = {number,Line,Sign*Number0},
           {ok,Token,[C|Cs],Line,Col,St}
   end;
scan_bnumber_digits([]=Cs, Line, Col, Number, Base, Sign, St) ->
    {more,{Cs,Line,Col,St,{Number,Base,Sign},fun scan_bnumber_digits_fun/5}};
scan_bnumber_digits(eof=Cs, Line, Col, Number, _Base, Sign, St) ->
    Token = {number,Line,Sign*Number},
    {ok,Token,Cs,Line,Col,St}.

%% base_collect(Chars, Base, Number) -> {Number,RestChars}.
%%  Collect all numeric characters of base Base.

base_collect_char(C, Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    {yes,SoFar * Base + (C - $0)};
base_collect_char(C, Base, SoFar) when C >= $a, C =< $z, C < Base + $a - 10 ->
    {yes,SoFar * Base + (C - $a + 10)};
base_collect_char(C, Base, SoFar) when C >= $A, C =< $Z, C < Base + $A - 10 ->
    {yes,SoFar * Base + (C - $A + 10)};
base_collect_char(_C, _Base, _SoFar) ->
    no.

%% scan_symbol(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan unquoted "symbols". This is a little tricky as numbers will
%%  also match here. We try to solve this by scanning in all the
%%  matching characters and then first checking whether it is a
%%  number, otherwise it is a symbol.

scan_symbol(Cs, Line, Col, St) ->
    scan_symbol1(Cs, Line, Col, [], St).

scan_symbol1_fun(Cs, Line, Col, St, Symcs) ->
    %% io:format("ssf ~p\n", [{Cs,Symcs}]),
    scan_symbol1(Cs, Line, Col, Symcs, St).

scan_symbol1([C|Cs], Line, Col, Symcs, St) ->
    case symbol_char(C) of
        true ->
            scan_symbol1(Cs, Line, Col+1, Symcs ++ [C], St);
        false ->
            %% Check whether we have an integer, a float or a symbol.
            Token = make_symbol_token(Symcs, Line),
            {ok,Token,[C|Cs],Line,Col+1,St}
    end;
scan_symbol1([]=Cs, Line, Col, Symcs, St) ->
    {more,{Cs,Line,Col,St,Symcs,fun scan_symbol1_fun/5}};
scan_symbol1(eof=Cs, Line, Col, Symcs, St) ->
    Token = make_symbol_token(Symcs, Line),
    {ok,Token,Cs,Line,Col,St}.

make_symbol_token(Chars, Line) ->
    try
        Integer = list_to_integer(Chars),
        {number,Line,Integer}
    catch
        _:_ ->
            try
                Float = list_to_float(Chars),
                {number,Line,Float}
            catch
                _:_ ->
                    Symbol = list_to_atom(Chars),
                    {symbol,Line,Symbol}
            end
    end.

%% start_symbol_char(Char) -> true | false.
%% symbol_char(Char) -> true | false.
%%  Define start symbol chars and symbol chars.

start_symbol_char($#) -> false;
start_symbol_char($`) -> false;
start_symbol_char($') -> false;                 %'
start_symbol_char($,) -> false;
start_symbol_char($|) -> false;                 %Symbol quote character
start_symbol_char(C) -> symbol_char(C).

symbol_char($() -> false;
symbol_char($)) -> false;
symbol_char($[) -> false;
symbol_char($]) -> false;
symbol_char(${) -> false;
symbol_char($}) -> false;
symbol_char($") -> false;
symbol_char($;) -> false;
symbol_char(C) -> ((C > $\s) and (C =< $~)) orelse (C > $\240).

%% scan_qsymbol(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan quoted symbols.

scan_qsymbol(Cs, Line, Col, St) ->
    scan_qsymbol1(Cs, Line, Col, Line, Col, [], St).

scan_qsymbol1_fun(Cs, Line, Col, St, {Sline,Scol,Symcs}) ->
    scan_qsymbol1(Cs, Line, Col, Sline, Scol, Symcs, St).

scan_qsymbol1([$\\,C|Cs], Line, Col, Sline, Scol, Symcs, St) ->
    %% Take the quoted character as is.
    scan_qsymbol1(Cs, Line, Col+2, Sline, Scol, Symcs ++ [C], St);
scan_qsymbol1([$||Cs], Line, Col, Sline, _Scol, Symcs, St) ->
    Qsymb = list_to_atom(Symcs),
    {ok,{symbol,Sline,Qsymb},Cs,Line,Col+1,St};
scan_qsymbol1([$\n|Cs], Line, _Col, Sline, Scol, Symcs, St) ->
    scan_qsymbol1(Cs, Line+1, 0, Sline, Scol, Symcs ++ [$\n], St);
scan_qsymbol1([C|Cs], Line, Col, Sline, Scol, Symcs, St) when C =/= $\\ ->
    scan_qsymbol1(Cs, Line, Col+1, Sline, Scol, Symcs ++ [C], St);
scan_qsymbol1(Cs, Line, Col, Sline, Scol, Symcs, St) when Cs =/= eof ->
    {more,{Cs,Line,Col,St,{Sline,Scol,Symcs},fun scan_qsymbol1_fun/5}};
scan_qsymbol1(eof, Line, Col, Sline, Scol, Symcs, _St) ->
    scan_error({illegal,[$| | Symcs]}, Line, Col, Sline, Scol, eof).

%% scan_string(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%% scan_binary_string(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan strings.

scan_string(Cs, Line, Col, St) ->
    scan_string(Cs, Line, Col, string, St).

scan_binary_string(Cs, Line, Col, St) ->
    scan_string(Cs, Line, Col, binary, St).

%% scan_string(Chars, Line, Column, Type, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Find which string type to use. Note the first " has already been
%%  scanned. Here we work out if is a normal string or a triple quote
%%  string and then pass the buck.

scan_string(Cs, Line, Col, Type, St) ->
    scan_string1(Cs, Line, Col, Type, St).
    
scan_string1_fun(Cs, Line, Col, St, Type) ->
    scan_string1(Cs, Line, Col, Type, St).

scan_string1([$",$"|Cs], Line, Col, Type, St) ->
    %% This is a triple quoted string.
    scan_tqstring(Cs, Line, Col+3, Line, Col, Type, St);
scan_string1([$",C|_]=Cs, Line, Col, Type, St) when C =/= $" ->
    %% This is a empty normal string which we pass on.
    scan_qstring(Cs, Line, Col+1, Line, Col, Type, St);
scan_string1([$"]=Cs, Line, Col, Type, St) ->
    %% We don't know yet.
    {more,{Cs,Line,Col,St,Type,fun scan_string1_fun/5}};
scan_string1(Cs, Line, Col, Type, St) ->
    %% This is a normal string which we pass on.
    scan_qstring1(Cs, Line, Col+1, Line, Col, [], Type, St).

%% scan_qstring(Chars, Line, Column, StartLine, StartColumn,
%%              Type, State) ->
%%     {ok,Token,Chars,Line,Column} | ScanError | ScanMore.
%%  Single quote "normal" strings. Note that the first " has already
%%  been scanned.

scan_qstring(Chars, Line, Column, Sline, Scol, Type, State) ->
    scan_qstring1(Chars, Line, Column, Sline, Scol, [], Type, State).

scan_qstring1_fun(Cs, Line, Col, St, {Sline,Scol,Symcs,Type}) ->
    scan_qstring1(Cs, Line, Col, Sline, Scol, Symcs, Type, St).

scan_qstring1([$\\,C|Cs], Line, Col, Sline, Scol, Symcs, Type, St) ->
    scan_qstring1(Cs, Line, Col+2, Sline, Scol, 
                 Symcs ++ [escape_char(C)], Type, St);
scan_qstring1([$"|Cs], Line, Col, Sline, _Scol, Symcs, Type, St) ->
    Token = string_token(Symcs, Sline, Type),
    {ok,Token,Cs,Line,Col+1,St};
scan_qstring1([$\n|Cs], Line, _Col, Sline, Scol, Symcs, Type, St) ->
    scan_qstring1(Cs, Line+1, 1, Sline, Scol, Symcs ++ [$\n], Type, St);
scan_qstring1([$\\], Line, Col, Sline, Scol, Symcs, Type, St) ->
    {more,{[$\\],Line,Col,St,{Sline,Scol,Symcs,Type},fun scan_qstring1_fun/5}};
scan_qstring1([C|Cs], Line, Col, Sline, Scol, Symcs, Type, St) ->
    scan_qstring1(Cs, Line, Col+1, Sline, Scol, Symcs ++ [C], Type, St);
scan_qstring1([]=Cs, Line, Col, Sline, Scol, Symcs, Type, St) ->
    {more,{Cs,Line,Col,St,{Sline,Scol,Symcs,Type},fun scan_qstring1_fun/5}};
scan_qstring1(eof=Cs, Line, Col, Sline, Scol, Symcs, _Type, _St) ->
    scan_error({illegal,[$" | Symcs]}, Line, Col, Sline, Scol, Cs).

escape_char($b) -> $\b;                %\b = BS
escape_char($t) -> $\t;                %\t = TAB
escape_char($n) -> $\n;                %\n = LF
escape_char($v) -> $\v;                %\v = VT
escape_char($f) -> $\f;                %\f = FF
escape_char($r) -> $\r;                %\r = CR
escape_char($e) -> $\e;                %\e = ESC
escape_char($s) -> $\s;                %\s = SPC
escape_char($d) -> $\d;                %\d = DEL
escape_char(C) -> C.                   %\Other = Other

string_token(String, StartLine, Type) ->
    case Type of
        string -> {string,StartLine,String};
        binary ->
            Binary = unicode:characters_to_binary(String, utf8, utf8),
            {binary,StartLine,Binary}
    end.

%% scan_tqstring(Chars, Line, Column, StartLine, StartCol, Type, State) ->
%%     {ok,Token,Char,Line,Column} | {more,Continuation} | ScanError.
%%  Scan triple quoted strings. Note that the first """ has already
%%  been scanned.

scan_tqstring(Chars, Line, Column, StartLine, StartCol, Type, State) ->
    scan_tqstring_1(Chars, Line, Column, StartLine, StartCol, Type, State).

%% scan_tqstring_1(Chars, Line, Column, StartLine, StartCol, Type, State) ->
%%     {ok,Token,Char,Line,Column} | {more,Continuation} | ScanError.
%%  Scan the first line and check the format.

scan_tqstring_1_fun(Cs, Line, Col, St, {Sline,Scol,Type}) ->
    scan_tqstring_1(Cs, Line, Col, Sline, Scol, Type, St).
    
scan_tqstring_1([$\s|Cs], Line, Col, Sline, Scol, Type, St) ->
    scan_tqstring_1(Cs, Line, Col, Sline, Scol, Type, St);
scan_tqstring_1([$\n|Cs], Line, _Col, Sline, Scol, Type, St) ->
    scan_tqstring_lines(Cs, Line+1, 1, Sline, Scol, [], [], Type, St);
scan_tqstring_1([]=Cs, Line, Col, Sline, Scol, Type, St) ->
    {more,{Cs,Line,Col,St,{Sline,Scol,Type},fun scan_tqstring_1_fun/5}};
scan_tqstring_1(Cs, Line, Col, Sline, Scol, _Type, _St) ->
    %% io:format("ls ~p\n", [{st1,Cs}]),
    %% An error for illegal character or eof.
    scan_error(tq_string, Line, Col, Sline, Scol, Cs).

%% scan_tqstring_lines(Chars, Line, Col, StartLine, StartCol, LineChars, Lines,
%%                     Type, State) ->
%%      {ok,Token,Char,Line,Column,State} | {more,Continuation} | ScanError.
%%  Scan and collect the following lines up to a valid end """.

scan_tqstring_lines_fun(Chars, Line, Col, St, {Sline,Scol,Lcs,Lines,Type}) ->
    scan_tqstring_lines(Chars, Line, Col, Sline, Scol, Lcs, Lines, Type, St).

scan_tqstring_lines([$\n|Cs], Line, _Col, Sline, Scol, Lcs, Lines, Type, St) ->
    scan_tqstring_lines(Cs, Line+1, 0, Sline, Scol, [], Lines ++ [Lcs], Type, St);
scan_tqstring_lines([$"|Cs], Line, Col, Sline, Scol, Lcs, Lines, Type, St) -> 
    scan_tqstring_tq(Cs, Line, Col, Sline, Scol, Lcs, Lines, Type, St);
scan_tqstring_lines([C|Cs], Line, Col, Sline, Scol, Lcs, Lines, Type, St) ->
    scan_tqstring_lines(Cs, Line, Col, Sline, Scol, Lcs ++ [C], Lines, Type, St);
scan_tqstring_lines([]=Cs, Line, Col, Sline, Scol, Lcs, Lines, Type, St) ->
    {more,{Cs,Line,Col,St,{Sline,Scol,Lcs,Lines,Type},
           fun scan_tqstring_lines_fun/5}};
scan_tqstring_lines(eof=Cs, Line, Col, Sline, Scol, _Lcs, _Lines, _Type, _St) ->
    %% io:format("ls ~w\n", [{stl,Cs,Sline,Line,_Lcs,_Lines}]),
    scan_error(tq_string, Line, Col, Sline, Scol, Cs).

%% scan_tqstring_tq(Chars, Line, Col, StartLine, StartCol, LineChars, Lines,
%%                  Type, State) ->
%%      {ok,Token,Char,Line,Column,State} | {more,Continuation} | ScanError.
%%  Check if we have valid end of the """ or whether we must go
%%  on. Note that the first " has already been scanned.

scan_tqstring_tq_fun(Cs, Line, Col, St, {Sline,Scol,Lcs,Lines,Type}) ->
    %%  io:format("ls ~p\n\n", [{sttf,Cs,Sline,Line,Lcs,Lines}]),
    scan_tqstring_tq(Cs, Line, Col, Sline, Scol, Lcs, Lines, Type, St).

scan_tqstring_tq([$",$"|Cs], Line, Col, Sline, Scol, Lcs, Lines, Type, St) ->
    %% This is a triple quote, check if this is a valid end line.
    case blank_line(Lcs) of
        true ->
	    %% io:format("ls ~p\n\n", [{stt1,Cs,Sline,Line,Lcs,Lines}]),
            scan_tqstring_end(Cs, Line, Col+3, Sline, Scol,
                              Lcs, Lines, Type, St);
        false ->
	    %% io:format("ls ~p\n", [{stt2,Cs,Sline,Line,Lcs ++ [$",$",$"],Lines}]),
            scan_tqstring_lines(Cs, Line, Col+3, Sline, Scol, 
                                Lcs ++ [$",$",$"], Lines, Type, St)
    end;
scan_tqstring_tq([$",C|Cs], Line, Col, Sline, Scol, Lcs, Lines, Type, St) when
      C =/= $" ->
    io:format("ls ~p\n", [{stt3,[C|Cs],Sline,Line,Lcs ++ [$"],Lines}]),
    %% This is not a triple quote here, it is a normal line. So we
    %% pass the buck, but don't forget the ".
    scan_tqstring_lines([C|Cs], Line, Col, Sline, Scol,
                        Lcs ++ [$"], Lines, Type, St);
scan_tqstring_tq([$"]=Cs, Line, Col, Sline, Scol, Lcs, Lines, Type, St) ->
    %% io:format("ls ~p\n", [{stt4,Cs,Sline,Line,Lcs,Lines}]),
    {more,{Cs,Line,Col,St,{Sline,Scol,Lcs,Lines,Type},
	   fun scan_tqstring_tq_fun/5}};
scan_tqstring_tq([]=Cs, Line, Col, Sline, Scol, Lcs, Lines, Type, St) ->
    %% io:format("ls ~p\n", [{stt5,Cs,Sline,Line,Lcs,Lines}]),
    {more,{Cs,Line,Col,St,{Sline,Scol,Lcs,Lines,Type},
	   fun scan_tqstring_tq_fun/5}};
scan_tqstring_tq(eof, Line, Col, Sline, Scol, _Lcs, _Lines, _Type, _St) ->
    scan_error(tq_string, Line, Col, Sline, Scol, eof);
scan_tqstring_tq(Cs, Line, Col, Sline, Scol, Lcs, Lines, Type, St) ->
    %% This is not a triple quote here, it is a normal line. So we
    %% pass the buck, but don't forget the ".
    scan_tqstring_lines(Cs, Line, Col, Sline, Scol,
                        Lcs ++ [$"], Lines, Type, St).

%% scan_tqstring_end(Chars, Line, Col, StartLine, StartCol, Prefix, Lines,
%%                   Type, State) ->
%%      {ok,Token,Char,Line,Column,State} | ScanError.
%%  Check if we have a valid end line.

scan_tqstring_end(Cs, Line, Col, Sline, _Scol, _Prefix, [], Type, St) ->
    %% No lines so it just is empty.
    Token = string_token([], Sline, Type),
    {ok,Token,Cs,Line,Col,St};
scan_tqstring_end(Cs, Line, Col, Sline, Scol, Prefix, Lines, Type, St) ->
    %% Here we will check what we have.
    case collect_tqstring_lines(Lines, Prefix, []) of
        {yes,CheckedLines} ->
            %% Skip the leading newline added by the fold.
            [_Lc|String] = lists:foldr(fun (Lcs, Scs) -> [$\n|Lcs] ++ Scs end,
                                     [], CheckedLines),
	    %% io:format("ls ~p\n", [{ste,Prefix,Lines,CheckedLines,String}]),
            Token = string_token(String, Sline, Type),
            {ok,Token,Cs,Line,Col,St};
        no ->
	    %% io:format("ls ~p\n", [{ste,Cs,Sline,Line,Lines,Prefix}]),
            scan_error(tq_string, Line, Col, Sline, Scol, Cs)
    end.

blank_line(Cs) ->
    lists:all(fun (C) -> C =:= $\s end, Cs).

collect_tqstring_lines([Lcs|Lines], Prefix, Plines) ->
    case check_tqstring_prefix(Prefix, Lcs) of
        {yes,Lcs1} ->
            collect_tqstring_lines(Lines, Prefix, Plines ++ [Lcs1]);
        no -> no
    end;
collect_tqstring_lines([], _Prefix, Plines) ->
    {yes,Plines}.

check_tqstring_prefix([P|Ps], [C|Lcs]) ->
    if P =:= C ->
            check_tqstring_prefix(Ps, Lcs);
       true -> no
    end;
check_tqstring_prefix([], Lcs) ->
    {yes,Lcs}.

%% scan_error(Error, StartLine, StartCol, EndLine, EndCol, RestChars) ->
%%     {Error,Rest}.

scan_error(Error, Line, _Col, EndLine, _EndCol, Rest) ->
    Loc = Line,                                 %location(Line, Col)
    EndLoc = EndLine,                           %location(EndLine, EndCol)
    scan_error(Error, Loc, EndLoc, Rest).

scan_error(Error, ErrorLoc, EndLoc, Rest) ->
    {{error,{ErrorLoc,?MODULE,Error},EndLoc},Rest}.

%% location(Line, no_col) ->
%%     Line;
%% location(Line, Col) when is_integer(Col) ->
%%     {Line,Col}.

%% -compile({inline,[anno/1,incr_column/2,new_column/2,int_column/1]}).

anno(Location) ->
    erl_anno:new(Location).

token_test(String) ->
    token_test([], String).

token_test(Cont0, [C1,C2,C3,C4,C5|String]) ->
    case lfe_scan:token(Cont0, [C1,C2,C3,C4,C5]) of
        {more,Cont1} ->
            token_test(Cont1, String);
        Other ->
            {Other,String}                      %So we can see what is left
    end;
token_test(Cont0, [C|String]) ->
    case lfe_scan:token(Cont0, [C]) of
        {more,Cont1} ->
            token_test(Cont1, String);
        Other ->
            {Other,String}                      %So we can see what is left
    end;
token_test(Cont, []) ->
    lfe_scan:token(Cont, eof).
