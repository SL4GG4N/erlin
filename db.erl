-module(db).
-author("martin").
-export([init/0, write/3,read/1, delete/1, cars_at/1, get_cars/2,car_pickup/2,traverse_table_and_show/0,pickup_car/1]).
-record (car, {key = empty, model = empty, location = empty,is_in_use = false}).
-include_lib("stdlib/include/qlc.hrl").

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(type, car)
  catch
    exit: _ ->
      mnesia:create_table(car,[{attributes, record_info(fields, car)}, {type,set}, {disc_copies, [node()]}])
  after
    io:format("~n Db init.")
  end,
    ok.

write(Key, Model, Location) ->
  Query = fun() ->
    mnesia:write(#car{key=Key, model = Model, location = Location,is_in_use = false})
  end,
  mnesia:transaction(Query).

read(Key) ->
  Query = fun() ->
    Q = qlc:q([X || X <- mnesia:table(car), X#car.key =:= Key]),
    Result = qlc:e(Q),
    lists:map(fun(Item) -> [Item#car.key, Item#car.model, Item#car.location] end, Result)
  end,
  {_, Car} = mnesia:transaction(Query),
  Car.

delete(Key) ->
  Query1 = fun() ->
    Q1 = qlc:q([X || X <- mnesia:table(car), X#car.key =:= Key]),
    Result = qlc:e(Q1),

    Query2 = fun() ->
      mnesia:delete_object(Result)
    end,
    mnesia:transaction(Query2)

  end,
  mnesia:transaction(Query1).

%______________________________________________________________

cars_at(LocRef) ->
  Query = fun() ->
    Q = qlc:q([X || X <- mnesia:table(car), X#car.location =:= LocRef]),
    Result = qlc:e(Q),
    lists:map(fun(Item) -> [Item#car.key, Item#car.model, Item#car.location] end, Result)
  end,
  {_, CarRefs} = mnesia:transaction(Query),
  CarRefs.

get_cars(LocRef, Count) ->
  Query = fun() ->
    C = qlc:cursor(qlc:q([X || X <- mnesia:table(car), X#car.location =:= LocRef])),
    Result = qlc:next_answers(C,Count),
    ok = qlc:delete_cursor(C),
    lists:map(fun(Item) -> [Item#car.key, Item#car.model, Item#car.location,Item#car.is_in_use] end, Result)
  end,
  {_, CarRefs} = mnesia:transaction(Query),
  CarRefs.


car_pickup(LocRef,CarRef) ->
  Query = fun() ->
    Q = qlc:q([X || X <- mnesia:table(car), X#car.location =:= LocRef, X#car.model =:= CarRef,X#car.is_in_use =:= false]),
    Result = qlc:e(Q),
    lists:map(fun(Item) -> 
      mnesia:write(#car{key=Item#car.key, model = Item#car.model, location = nil, is_in_use = true}),
      [Item#car.key, Item#car.model, Item#car.location,Item#car.is_in_use] end, Result)
  end,
  {_, CarRefs} = mnesia:transaction(Query),
  CarRefs.


pickup_car(LocRef) ->
  Query = fun() ->
    Q = qlc:q([X || X <- mnesia:first(car), X#car.location =:= LocRef,X#car.is_in_use =:= false]),
    Result = qlc:e(Q),
    lists:map(fun(Item) -> 
      mnesia:write(#car{location = nil, is_in_use = true}),
      [Item#car.key, Item#car.model, Item#car.location,Item#car.is_in_use] end, Result)

  end,
  {_, CarRefs} = mnesia:transaction(Query),
  CarRefs.

traverse_table_and_show()->
  Iterator =  fun(Rec,_)->
                  io:format("~p~n",[Rec]),
                  []
              end,
  case mnesia:is_transaction() of
      true -> mnesia:foldl(Iterator,[],car);
      false -> 
          Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
          mnesia:activity(transaction,Exec,[{Iterator,car}],mnesia_frag)
  end.







