{application, basic_driver,
  [{description, "Demo driver"},
   {vsn, 0.1},
   {modules, [
               basic_driver
              ,driver_comm
             ]},
   {registered, []},
   {applications, [kernel, stdlib]}]}.
