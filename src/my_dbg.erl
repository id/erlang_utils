-module(my_dbg).

-export([ export_all/1
        , pid/1
        , pid/2
        , c/1
        , c/2
        , ma/1
        , bt/1
        , bt/3
        , lm/0
        , lrm/0
        , nl/0
        , src/1
        , beam/1
        , pinfo/1
        , pinfo/2
        , children/1
        , ptree/1
        , fmt_now/0
        , fmt_now/1
        , pname/1
        , tracer_loop/1
        , toggle_trace/4
        , tracer/1
        , trace_modules/1
        , meta_trace_modules/2
        , meta_trace/4
        , stop_meta_trace/1
        , stop_meta_trace/4
        ]).

export_all(M) ->
  case code:which(M) of
    non_existing -> no_such_module;
    F ->
      {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(F,[abstract_code]),
      {ok,_,B} = compile:forms(AC,[export_all]),
      code:soft_purge(M),
      code:load_binary(M,"",B)
  end.


pid(I2,I3)                    -> pid({I2,I3}).
pid({I1,I2,I3})               -> c:pid(I1,I2,I3);
pid({I2,I3})                  -> pid({0,I2,I3});
pid(Pid)  when is_pid(Pid)    -> Pid;
pid(Atom) when is_atom(Atom)  -> whereis(Atom);
pid(I2)   when is_integer(I2) -> pid({0,I2,0});
pid(Str)  when hd(Str)==$<    -> list_to_pid(Str);
pid(Str)  when is_list(Str)   -> pid("<"++Str++">").

c(M) ->
  c(M, []).

c(M, Opts) ->
  case shellc(M, Opts) of
    error ->
      try src(M) of
          S -> shellc(S, Opts ++ [{outdir,filename:dirname(beam(M))}])
      catch error: E -> E
      end;
    O -> O
  end.

shellc(M, Opts) ->
  shell_default:c(M, Opts++[debug_info]).

src(Module) ->
  Rules = [{"", ""}, {"ebin", "src"}, {"ebin", "test"}],
  AddSuffix = fun({error, _Reason} = Error) -> Error;
                 ({Path, _Options}) -> io_lib:format("~s.erl", [Path])
              end,
  AddSuffix(filename:find_src(Module, Rules)).

beam(Module) ->
  code:which(Module).

%% Lookup macros
ma(X) -> '?'(X).
'?'(X) -> rr:m(X).

bt(Pid) when is_pid(Pid) ->
  case pinfo(Pid, backtrace) of
    {backtrace, Bin} ->
      io:format("~s\n", [binary_to_list(Bin)]);
    _ ->
      undefined
  end;
bt(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> undefined;
    Pid -> bt(Pid)
  end.

bt(X,Y,Z) ->
  bt(c:pid(X,Y,Z)).

lm() ->
  [c:l(M) || M <- modified_modules()].

lrm() ->
  [lrm(What,M) || {What,M} <- modified_removed_modules()].

nl() ->
  [rpc:call(N,?MODULE,lm,[]) || N <- [node() | nodes()]].

lrm(true,M)    -> c:l(M);
lrm(removed,M) -> code:purge(M), code:delete(M), {removed,M}.

modified_modules() ->
  [M || {M, _} <-  code:all_loaded(), module_modified(M) =:= true].

modified_removed_modules() ->
  [{What, M} || {M, _} <-  code:all_loaded(),
                (What = module_modified(M)) =/= false].

module_modified(Module) ->
  case code:is_loaded(Module) of
    {file, preloaded} ->
      false;
    {file, []} -> false; %% compiled from abstract forms, no file
    {file, Path} ->
      CompileOpts = erlang:get_module_info(Module, compile),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      module_modified(Path, CompileTime, Src);
    false ->
      false
  end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
  case find_module_file(Path) of
    non_existing ->
      removed;
    ModPath ->
      case beam_lib:chunks(ModPath, ["CInf"]) of
        {ok, {_, [{_, CB}]}} ->
          CompileOpts =  binary_to_term(CB),
          CompileTime = proplists:get_value(time, CompileOpts),
          Src = proplists:get_value(source, CompileOpts),
          not ((CompileTime == PrevCompileTime) and (Src == PrevSrc));
        _ ->
          false
      end
  end.

find_module_file(Path) ->
  case file:read_file_info(Path) of
    {ok, _} ->
      Path;
    _ ->
      %% maybe the path was changed?
      code:where_is_file(filename:basename(Path))
  end.

pinfo(Pid) ->
  N = node(Pid),
  case
    case N =:= node() of
      true -> [];
      false-> [{node,N}]
    end ++
    case rpc:call(N,erlang,process_info,[Pid]) of
      L when is_list(L) -> L;
      _ -> []
    end of
    [] -> [];
    I -> [{pid,Pid}|I]
  end.

pinfo(Pid, Item) ->
  case is_alive() of
    true -> rpc:call(node(Pid), erlang, process_info, [Pid, Item]);
    false -> process_info(Pid, Item)
  end.

children(Pid) ->
  Links = element(2, erlang:process_info(Pid, links)),
  PidStr = pid_to_list(Pid),
  %% filter out ancestors
  lists:filter(fun(_Pid) when not is_pid(_Pid) -> false;
                  (_Pid) -> pid_to_list(_Pid) > PidStr
               end, Links).

ptree(Pid) ->
  lists:delete(Pid, ptree(children(Pid), [Pid])).

ptree([], Acc) -> Acc;
ptree([Pid | Pids], Acc) when is_pid(Pid) ->
  case lists:member(Pid, Acc) of
    true  -> ptree(Pids, Acc);
    false -> ptree(Pids, ptree(children(Pid), [Pid | Acc]))
  end;
ptree([_ | Pids], Acc) ->
  ptree(Pids, Acc).

fmt_now() ->
  fmt_now(erlang:now()).

fmt_now({_, _, Micro} = Now) ->
  {{Y,M,D}, {HH,MM,SS}} = calendar:now_to_local_time(Now),
  lists:flatten(io_lib:format("~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~w",
                              [Y, M, D, HH, MM, SS, Micro])).

initial_call(Pid) ->
  {initial_call, Call} = process_info(Pid, initial_call),
  Call.

pname(Pid) when not is_pid(Pid) -> Pid;
pname(Pid) ->
  case is_process_alive(Pid) of
    true ->
      case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        undefined               -> Pid;
        [] ->
          case erlang:process_info(Pid, dictionary) of
            {dictionary, []} -> {Pid, initial_call(Pid)};
            undefined        -> {Pid, initial_call(Pid)};
            {dictionary, L} ->
              case lists:keyfind('$initial_call', 1, L) of
                {'$initial_call', Call} -> {Pid, Call};
                false                   -> {Pid, initial_call(Pid)}
              end
          end
      end;
    false ->
      Pid
  end.

%%% Use tracing functions from erlang shell like this:
%% trace_modules([m1, m2, m3]).
%% f(Tracer), Tracer = tracer("/tmp/trace.out").
%% f(Pids), Pids = [whereis(reg_name_1)] ++ ptree(whereis(reg_name_2)).
%% toggle_trace(Pids, [send, 'receive', call], Tracer, true).
%% <do stuff>
%% toggle_trace(Pids, [send, 'receive', call], Tracer, false).

%% trace all calls to all functions in specified modules
trace_modules(Modules) ->
  Match = [{'_', [], [{return_trace}]}],
  lists:foreach(
    fun(M) ->
        erlang:trace_pattern({M, '_', '_'}, Match, [local])
    end, Modules).

meta_trace_modules(Tracer, Modules) ->
  Match = [{'_', [], [{return_trace}]}],
  lists:foreach(
    fun(M) ->
        {module, M} = c:l(M),
        erlang:trace_pattern({M, '_', '_'}, Match, [{meta, Tracer}])
    end, Modules).

meta_trace(Tracer, M, F, A) ->
  Match = [{'_', [], [{return_trace}]}],
  erlang:trace_pattern({M, F, A}, Match, [{meta, Tracer}]).

stop_meta_trace(Tracer) ->
  stop_meta_trace(Tracer, '_', '_', '_').

stop_meta_trace(Tracer, M, F, A) ->
  erlang:trace_pattern({M, F, A}, false, [{meta, Tracer}]).

tracer(File) ->
  {ok, Fd} = file:open(File, [write]),
  proc_lib:spawn(fun() -> ?MODULE:tracer_loop(Fd) end).

toggle_trace(PidSpec, Flags, Tracer, How) when is_atom(PidSpec);
                                               is_pid(PidSpec) ->
  erlang:trace(PidSpec, How, [timestamp, {tracer, Tracer} | Flags]);
toggle_trace(Pids, Flags0, Tracer, How) when is_list(Pids) ->
  Flags = [timestamp, {tracer, Tracer} | Flags0],
  lists:foreach(fun(Pid) -> erlang:trace(Pid, How, Flags) end, Pids).

tracer_loop(Fd) ->
  receive
    stop ->
      io:format(Fd, "stop~n", []),
      file:close(Fd),
      ok;
    {trace_ts, Pid, call, {M, F, A}, Now} ->
      io:format(Fd, "[~s] ~p called ~p:~p(~180p)~n",
                [fmt_now(Now), pname(Pid), M, F, A]),
      ?MODULE:tracer_loop(Fd);
    {trace_ts, Pid, return_from, {M, F, A}, Ret, Now} ->
      io:format(Fd, "[~s] ~p return from ~p:~p/~p: ~180p~n",
                [fmt_now(Now), pname(Pid), M, F, A, Ret]),
      ?MODULE:tracer_loop(Fd);
    {trace_ts, Pid, send, Msg, To, Now} ->
      io:format(Fd, "[~s] ~p sent to ~p: ~180p~n",
                [fmt_now(Now), pname(Pid), pname(To), Msg]),
      ?MODULE:tracer_loop(Fd);
    {trace_ts, Pid, send_to_non_existing_process, Msg, To, Now} ->
      io:format(Fd, "[~s] ~p sent to non existing process ~p: ~180p~n",
                [fmt_now(Now), pname(Pid), To, Msg]),
      ?MODULE:tracer_loop(Fd);
    {trace_ts, Pid, 'receive', Msg, Now} ->
      io:format(Fd, "[~s] ~p received ~180p~n",
                [fmt_now(Now), pname(Pid), Msg]),
      ?MODULE:tracer_loop(Fd);
    {trace_ts, Pid, Tag, Msg, Now} when Tag =:= in_exiting;
                                        Tag =:= out_exiting;
                                        Tag =:= out_exited ->
      io:format(Fd, "[~s] ~p is ~s: ~180p~n",
                [fmt_now(Now), pname(Pid), Tag, Msg]),
      ?MODULE:tracer_loop(Fd);
    Msg ->
      exit({unexpected_msg, Msg})
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
