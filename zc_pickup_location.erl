- module (zc_pickup_location).

start_link(Spaces, Occupied) -> {ok, LocRef}.

pickup_car(LocRef) -> {ok, CarRef} | {error, empty}.

return_car(LocRef, CarRef) -> ok | {error, full}.

get_info(LocRef) -> {ok, [{spaces, Spaces}, {occupied, Occupied}, {free, Free}]}.
