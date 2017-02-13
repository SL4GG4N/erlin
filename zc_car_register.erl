%% hej
- module (inl).

-record (car, {brand, model, regplate, color, owner}).

start_link() -> ok.

get_cars(LocRef, Count) -> {ok, CarRefs}.

car_pickup(LocRef, CarRef) -> ok.

car_return(LocRef, CarRef) -> ok.

cars_at(LocRef) -> CarRefs.
