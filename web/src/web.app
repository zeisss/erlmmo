{application, web,
 [{description, "web"},
  {vsn, "0.1"},
  {modules, [
    web,
    web_app,
    web_sup,
    web_deps,
    resource_index,
    resource_auth,
    resource_status
  ]},
  {registered, []},
  {mod, {web_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
