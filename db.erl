-module (db).
-include ("stdlib/include/qlc.hrl").
-include ("car.hrl").
-author("Eddie & Waleed").


init() ->
	mnesia:create_table(car, [{attributes, record_info(fields, car)}]),
	mnesia:start().
