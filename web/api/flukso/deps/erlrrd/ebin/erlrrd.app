{application, erlrrd,
 [{description, "erlang rrdtool port"},
  {vsn, "1.0"},
  {modules, [
    erlrrd,
    erlrrd_app,
    erlrrd_sup
  ]},
  {registered, [erlrrd, erlrrd_sup]},
  {mod, {erlrrd_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
