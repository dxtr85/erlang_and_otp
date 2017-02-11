{application, http_interface,
 [{description, "http interface for apps"},
  {vsn, "0.1.0"},
  {modules, [
    hi_app,
    hi_sup,
    hi_server]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {hi_app, []}}
 ]}.
