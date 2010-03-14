-module(mysql_sup).

-export([start_link/0]).

-behaviour(supervisor).

-export([init/1]).

%% @spec start_link() ->  Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link() -> 
  supervisor:start_link(mysql_sup, []).

init([]) -> 
  MysqlConfig = [pool, "localhost", "flukso", "xpsCcVsbecJMVCYF", "flukso"],
  Mysql = {mysql,
             {mysql, start_link, MysqlConfig},
             permanent, 3000, worker, [mysql]},
  Processes = [Mysql],
  {ok, {{one_for_one, 5, 10}, Processes}}.
