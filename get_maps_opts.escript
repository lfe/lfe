#! /usr/bin/env escript
%% -*- mode: erlang; indent-tabs-mode: nil -*-
%% Define the makefile variables HAS_MAPS and HAS_FULL_KEYS depending
%% on whether this version of erlang has maps (17) and general map
%% keys (18), or neither.

-define(MAP_STRING, "#{X => 1}.").
-define(HAS_OPT, "-DHAS_MAPS=true").
-define(FULL_OPT, "-DHAS_FULL_KEYS=true").

main(_) ->
    MapsOpts = maps_opts(),
    file:write_file("maps_opts.mk", "MAPS_OPTS = " ++ MapsOpts ++ "\n").

maps_opts() ->
    case erl_scan:string(?MAP_STRING) of
        {ok,Ts,_} ->
            case erl_parse:parse_exprs(Ts) of
                {ok,Es} ->                      %We have maps!
                    Binds = [{'X',49}],         %We need to bind X
                    case erl_lint:exprs(Es, Binds) of
                        {ok,_} -> ?HAS_OPT ++ " " ++ ?FULL_OPT;
                        {error,_,_} -> ?HAS_OPT
                    end;
                {error,_} -> ""
            end;
        {error,_,_} -> ""
    end.
