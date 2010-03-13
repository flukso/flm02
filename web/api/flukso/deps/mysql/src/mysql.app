{application, mysql,
 [{description, "erlang mysql driver"},
  {vsn, "1.0"},
  {modules, [
    mysql,
    mysql_app,
    mysql_sup
  ]},
  {registered, [mysql, mysql_sup]},
  {mod, {mysql_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
