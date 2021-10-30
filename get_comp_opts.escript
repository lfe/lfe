#! /usr/bin/env escript
%% -*- mode: erlang; indent-tabs-mode: nil -*-
%% Define a number of compiler options. We first work out the current
%% Erlang version and from the we can define the various options.

%% Define the makefile variables HAS_MAPS, HAS_FULL_KEYS,
%% NEW_REC_CORE, NEW_RAND, HAS_FLOOR, HAS_CEIL and NEW_STACKTRACE
%% depending on version of Erlang.

main(_) ->
    Version = otp_release(),
    CompOpts = comp_opts(Version),
    file:write_file("comp_opts.mk", "COMP_OPTS = " ++ CompOpts ++ "\n").

%% Get the release number.
%% We have stolen the idea and most of the code from rebar3.

otp_release() ->
    case erlang:system_info(otp_release) of
        [$R,N1|Rest] when is_integer(N1) ->
            %% If OTP <= R16, take the digits.
            [N1|Rest];
        Rel ->
            %% If OTP >= 17.x, erlang:system_info(otp_release) returns
            %% just the major version number.
            File = filename:join([code:root_dir(),"releases",Rel,"OTP_VERSION"]),
            case file:read_file(File) of
                {error, _} -> Rel;
                {ok, Vsn} ->
                    Size = byte_size(Vsn),
                    %% The shortest vsn string consists of at least
                    %% two digits followed by "\n". Therefore, it's
                    %% safe to assume Size >= 3.
                    case binary:part(Vsn, {Size, -3}) of
                        <<"**\n">> ->
                            binary:bin_to_list(Vsn, {0, Size - 3});
                        _ ->
                            binary:bin_to_list(Vsn, {0, Size - 1})
                    end
            end
    end.

comp_opts(Version) ->
    Copts0 = "-DERLANG_VERSION=\\\"" ++ Version ++ "\\\"" ++ " ",
    Copts0 ++ append_copts(Version, [{"17","HAS_MAPS"},
                                     {"18","HAS_FULL_KEYS"},
                                     {"19","NEW_REC_CORE"},
                                     {"19","NEW_RAND"},
                                     {"20","NEW_BOOL_GUARD"},
                                     {"20","HAS_FLOOR"},
                                     {"20","HAS_CEIL"},
                                     {"21","NEW_STACKTRACE"},
                                     {"23","EEP48"}]).

append_copts(Version, [{Ver,Opt}|Opts]) ->
    Rest = append_copts(Version, Opts),
    if Version >= Ver ->
            "-D" ++ Opt ++ "=true" ++ " " ++ Rest;
       true -> Rest
    end;
append_copts(_Version, []) -> [].
