%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% APIs for Managing Numbers and Labels
% Author : Rahib Rasheed
%          09/10/2018
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(callhinder_api).
-export([handle/2,init/3,terminate/3]).

-record(state,{}).
-record(directory, {number, label, notes}).
-record(labels, {label, description}).

init(_Type, Req, _Opts) ->
  {ok, Req, #state{}}.

-spec handle(any(), any()) -> any().
handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path, Req2} = cowboy_req:path_info(Req1),
  io:format("method : ~p, path : ~p~n",[Method, Path]),
  handle_req(Method, Path, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_req(<<"POST">>, [<<"create">>], Req) ->
  {ok, Body, _Req2} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  {_Key1, Numbers} = lists:keyfind(<<"numbers">>, 1, Data),
  add_number(Numbers),
  cowboy_req:reply(200,[{<<"content-type">>, <<"text/plain">>}], "Numbers added to spam directory", Req);

handle_req(<<"POST">>, [<<"read">>], Req) ->
  {ok, Body, _Req2} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  {_Key1, Labels} = lists:keyfind(<<"label">>, 1, Data),
  Result = read_label(Labels, []),
  cowboy_req:reply(200,[{<<"content-type">>, <<"application/json">>}], Result, Req);

handle_req(<<"POST">>, [<<"update">>], Req) ->
  {ok, Body, _Req2} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  {_Key1, Numbers} = lists:keyfind(<<"numbers">>, 1, Data),
  add_number(Numbers),
  cowboy_req:reply(200,[{<<"content-type">>, <<"text/plain">>}], "Number updated successfully", Req);

handle_req(<<"POST">>, [<<"delete">>], Req) ->
  {ok, Body, _Req2} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  {_Key1, Numbers} = lists:keyfind(<<"numbers">>, 1, Data),
  delete_numbers(Numbers),
  cowboy_req:reply(200,[{<<"content-type">>, <<"text/plain">>}], "Numbers deleted successfully", Req);

handle_req(<<"POST">>, [<<"add_labels">>], Req) ->
  {ok, Body, _Req2} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  {_Key1, Labels} = lists:keyfind(<<"labels">>, 1, Data),
  add_labels(Labels),
  cowboy_req:reply(200,[{<<"content-type">>, <<"text/plain">>}], "Labels Added Succesfully", Req);

handle_req(<<"POST">>, [<<"labels">>], Req) ->
  Result = get_all_labels(),
  Resp = {[{<<"labels">>, Result}]},
  Resp1 = jiffy:encode(Resp),
  cowboy_req:reply(200,[{<<"content-type">>, <<"application/json">>}], Resp1, Req);

handle_req(<<"POST">>, [<<"delete_labels">>], Req) ->
  {ok, Body, _Req2} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  {_Key1, Labels} = lists:keyfind(<<"labels">>, 1, Data),
  delete_labels(Labels),
  cowboy_req:reply(200,[{<<"content-type">>, <<"text/plain">>}], "Labels deleted successfully", Req);

handle_req(_ , _, Req) ->
  io:format("Some other path"),
  cowboy_req:reply(404,[{<<"content-type">>, <<"text/plain">>}], "Method not found", Req).

add_number([]) ->
  ok;
add_number([{D}|Data]) ->
  {_Key1, Number} = lists:keyfind(<<"number">>, 1, D),
  {_Key2, Label} = lists:keyfind(<<"label">>, 1, D),
  {_Key3, Notes} = lists:keyfind(<<"notes">>, 1, D),
  Rec = #directory{number = Number,
                   label = Label,
                   notes = Notes},
  mnesia:dirty_write(Rec),
  add_number(Data).

read_label([], Result) ->
  Resp = {[{<<"data">>, Result}]},
  jiffy:encode(Resp);
read_label([L|Label], Result) ->
  Data = mnesia:dirty_index_read(directory, L, label),
  NumberData = get_number_data(Data, []),
  R = {[{L, NumberData}]},
  read_label(Label, [R|Result]).

get_number_data([], Result) ->
  Result;
get_number_data([D|Data], Result) ->
  Number = element(2, D),
  Notes = element(4, D),
  R = {[{<<"number">>, Number}, {<<"notes">>, Notes}]},
  get_number_data(Data, [R|Result]).

delete_numbers([]) ->
  ok;
delete_numbers([N|Numbers]) ->
  mnesia:dirty_delete(directory, N),
  delete_numbers(Numbers).

add_labels([]) ->
  ok;
add_labels([L|Labels]) ->
  Rec = #labels{label = L,
                description = " "},
  mnesia:dirty_write(Rec),
  add_labels(Labels).

get_all_labels() ->
  mnesia:dirty_all_keys(labels).

delete_labels([]) ->
  ok;
delete_labels([L|Labels]) ->
  mnesia:dirty_delete(labels, L),
  delete_labels(Labels).
