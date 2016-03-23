-module(mod_ios_subscription).
-author("hjbolide@gmail.com").

-behavior(gen_mod).

-export([start/2,
         init/2,
         stop/1]).


-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").


-record(ios_subscription,
        {
          jid = #jid{} :: jid() | '_',
          device_token = <<"">> :: binary()
        }).


send_payload(Host, Payload, Token) ->
    Address = gen_mod:get_module_opt(
                Host, ?MODULE, address,
                fun(V) -> binary_to_list(V) end, undefined),
    Port    = gen_mod:get_module_opt(
                Host, ?MODULE, port,
                fun(V) -> V end, undefined),
    Cert    = gen_mod:get_module_opt(
                Host, ?MODULE, certfile,
                fun(V) -> binary_to_list(V) end, undefined),
    Key     = gen_mod:get_module_opt(
                Host, ?MODULE, keyfile,
                fun(V) -> binary_to_list(V) end, undefined),
    Password = gen_mod:get_module_opt(
                Host, ?MODULE, password,
                 fun(V) -> binary_to_list(V) end, undefined),
    ?DEBUG("mod_ios_subscription: trying to send payload", []),
    
    case Key of
        undefined ->
            Options = [{certfile, Cert}, {password, Password}, {mode, binary}];
        _ ->
            Options = [{certfile, Cert}, {keyfile, Key}, {mode, binary}]
    end,
    
    case ssl:connect(Address, Port, Options, ?Timeout) of
        {ok, Socket} ->
            PayloadBin = list_to_binary(Payload),
            PayloadLength = size(PayloadBin),
            TokenNum = erlang:binary_to_integer(Token, 16),
            TokenBin = <<TokenNum:32/integer-unit:8>>,
            Packet = <<
                       0:8,
                       32:16/big,
                       TokenBin/binary,
                       PayloadLength:16/big,
                       PayloadBin/binary
                     >>,
            ssl:send(Socket, Packet),
            ssl:close(Socket),
            ?DEBUG("mod_ios_subscription: Payload sent", []),
            ok;
        {error, Reason} ->
            ?ERROR_MSG("mod_ios_subscription: Error occured: ~p", [Reason]),
            Reason
    end.


create_json(List1, List2) ->
    lists:append(
      [
       "{\"aps\":{",
       create_keyvalue(List1),
       "}, ",
       create_keyvalue(List2)
      ]).


create_keyvalue([Head]) ->
    create_pair(Head);
create_keyvalue([Head|Tail]) ->
    lists:append([create_pair(Head), ",", create_keyvalue(Tail)]).


create_pair({Key, Value}) ->
    lists:append([add_quotes(atom_to_list(Key)), ":", add_quotes(Value)]).


add_quotes(String) ->
    lists:append(["\"", String, "\""]).


message(From, To, Packet) ->
    Type = fxml:get_tag_attr_s(<<"type">>, Packet),
    ?DEBUG("Offline message", []),
    case Type of
        "normal" ->
            ok;
        _ ->
            JFrom = jlib:jid_to_string(
                      From#jid{
                        user = From#jid.user,
                        server = From#jid.server,
                        resource = <<"">>
                       }),
            JTo = jlib:jid_to_string(
                    To#jid{
                      user = To#jid.user,
                      server = To#jid.server,
                      resource = <<"">>}),
            ToUser = To#jid.user,
            ToServer = To#jid.server,
            Body = fxml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
            
            {Subscription, _Groups} =
                ejabberd_hooks:run_fold(
                  roster_get_jid_info, ToServer,
                  {none, []}, [ToUser, ToServer, From]),
            case Subscription of
                both ->
                    case Body of
                        <<>> ->
                            ok;
                        _ ->
                            Result = mnesia:dirty_read(ios_subscription, {ToUser}),
                            
            

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_ios_subscription", []),
    register(?PROCNAME, spawn(?MODULE, init, [Host, Opts])),
    ok.


stop(Host) ->
    ?INFO_MSG("Stopping mod_ios_subscription", []),
    ok.


init(Host, Opts) ->
    case gen_mod:db_type(Host, Opts) of
        mnesia ->
            mnesia:create_table(
              ios_subscription,
              [
               {disc_copies, [node()]},
               {attributes, record_info(fields, ios_subscription)}
              ]),
            mnesia:update_tables();
        _ -> ok
    end.
