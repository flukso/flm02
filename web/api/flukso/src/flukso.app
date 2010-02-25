{application, flukso,
 [{description, "flukso"},
  {vsn, "0.1"},
  {modules, [
    flukso,
    flukso_app,
    flukso_sup,
    flukso_deps,
    flukso_resource
  ]},
  {registered, []},
  {mod, {flukso_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
