{application, erlmmo,
  [
   {description,  "A try on an erlang based mmo"},
%   {id,           erlmmo},
   {vsn,          "1.0"},
   {modules,      [main_app, main_sup, user_server]},
   {registered,   [main_sup, user_server]},
%   {included_applications, Apps},
   {applications, [kernel, stdlib]},
%   {env,          Env},
   {mod,          {main_app, []}}
%   {start_phases, Phases}
  ]
}.