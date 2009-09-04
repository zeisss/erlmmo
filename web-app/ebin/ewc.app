{application, ewc,
 [{description, "ewc"},
  {vsn, "0.01"},
  {modules, [
    ewc,
    ewc_app,
    ewc_sup,
    ewc_web,
    ewc_deps
  ]},
  {registered, []},
  {mod, {ewc_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
