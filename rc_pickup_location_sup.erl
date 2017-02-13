- module (rc_pickup_location_sup).

start_link() -> {ok, SupPid}.

add_pickup_location(SupPid, Spaces, Occupied) -> {ok, LocRef}.
