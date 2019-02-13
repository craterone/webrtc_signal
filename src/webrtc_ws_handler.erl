-module (webrtc_ws_handler).

-export ([init/2]).
-export ([websocket_init/1]).
-export ([websocket_handle/2]).
-export ([websocket_info/2]).
-export ([terminate/3]).


init(Req, _) ->
  % only set idle_timeout infinity in debug
  Opts = #{idle_timeout => infinity},
  Room = cowboy_req:binding(room, Req),
  State = #{room => Room, authenticated => false},
  {cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
  {ok,State}.


websocket_handle({text, <<"ping">>}, State) ->
  {reply, {text, <<"pong">>}, State};
websocket_handle({text, Text}, State = #{authenticated := false, room := Room}) ->
  case authenticate(Text) of
    {success, Username} ->
      lager:debug("socket authenticated"),

      PeerId = peer_id(),
      
      join_room(Room, Username, PeerId),

      State2 = State#{authenticated => true,
                      username => Username,
                      peer_id => PeerId},
      {reply, webrtc_utils:text_event(authenticated, #{peer_id => PeerId}), State2};
    Reason ->
      lager:debug("bad authentication: ~p ~p", [Reason, Text]),
      {reply, webrtc_utils:text_event(unauthorized),State}
  end;

websocket_handle({text, Text}, State = #{authenticated := true, room := Room, peer_id := ThisPeer}) ->
  lager:debug("Received text frame ~p", [Text]),

  case webrtc_utils:json_decode(Text) of
    #{to := OtherPeer} = Message ->
      {Pid, {_Username, _PeerId, Room}} = syn:find_by_key(OtherPeer, with_meta),

      Message2 = Message#{from => ThisPeer},
      Pid ! {text, webrtc_utils:json_encode(Message2)},
      {ok, State};
    _ ->
      {reply, webrtc_utils:text_event(invalid_message), State}
  end;

websocket_handle(Frame, State) ->
  lager:warning("Received non text frame ~p~p", [Frame, State]),
  {ok, State}.

websocket_info({text,Text}, State = #{authenticated := true}) ->
  lager:debug("Sending to client ~p", [Text]),
  {reply, {text, Text}, State};

websocket_info(Info, State) ->
  lager:warning("Received unexpected info ~p~p", [Info, State]),
  {ok, State}.


terminate(_Reason, _Req, #{room := Room, username := Username, peer_id := PeerId}) ->
  OtherUsers = [Name || {Pid, {Name, _PeerId}} <- syn:get_members(Room, with_meta), Pid /= self()],
  syn:publish(Room, webrtc_utils:text_event(left, #{username => Username,peer_id => PeerId})),
  ok;

terminate(_Reason, _Req, _State) ->
  ok.


authenticate(Data) ->
  lager:debug("authenticate ~p",[Data]),
  try webrtc_utils:json_decode(Data) of
    #{event := <<"authenticate">>, data := #{username := Username, password := Password}} ->
      lager:debug("username ~p , password ~p",[Username,Password]),
      % todo : add a function checking password
      {success, Username};
    _ -> invalid_format

  catch
    Type:Error ->
      lager:debug("invalid json ~p ~p", [Type, Error]),
      invalid_json
  end.

join_room(Room, Username, PeerId) ->
  OtherMembers = syn:get_members(Room, with_meta),
  syn:register(PeerId, self(), {Username, PeerId, Room}),
  syn:join(Room, self(), {Username, PeerId}),

  Message = webrtc_utils:text_event(joined, #{peer_id => PeerId,username => Username}),

  lists:foreach(fun({Pid, _}) -> Pid ! Message end, OtherMembers).

peer_id() ->
  base64:encode(crypto:strong_rand_bytes(10)).