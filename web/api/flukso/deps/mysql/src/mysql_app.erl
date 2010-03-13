-module(mysql_app).

-export([start/0, stop/0]).
-behavior(application).
-export([start/2, stop/1]).


start() -> 
  application:start(mysql).

start(_Type, _Args) -> 
  mysql_sup:start_link().

stop() -> 
  application:stop(mysql).

stop(_State) -> 
  ok. 
