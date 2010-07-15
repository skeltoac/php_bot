-module(php_bot).
-behavior(gen_server).
-behavior(gen_mod).

-export([start_link/3]).

-export([start/2,
	 stop/1,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([packet_filter/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-compile(export_all).

-record(state, {host,
		jids,
		php_code_ref}).

-define(PROCNAME, php_bot).

-define(PHP_REQUIRE,
	"if ( ! function_exists('php_bot_receive') ) {"
	" function php_bot_receive() {"
	"  return 'The PHP function php_bot_receive must be defined before I can do anything other than repeat this message.';"
	" }"
	"}").

start_link(Host, Opts, Ref) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts, Ref], []).

start(Host, Opts) ->
    ok = php:start(),
    Ref = php:require_code(?PHP_REQUIRE),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc,
		 {?MODULE, start_link, [Host, Opts, Ref]},
		 temporary,
		 1000,
		 worker,
		 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

init([Host, Opts, Ref]) ->
    JIDs = case catch proplists:get_all_values(jid, Opts) of
	       List when is_list(List), length(List) > 0 -> List;
	       _ -> ["bot@"++Host]
	   end,
    compile_php_bot_check(JIDs),
    ?DEBUG("php_bot starting with these JIDs: ~p~n", [JIDs]),
    ejabberd_hooks:add(filter_packet, ?MODULE, packet_filter, 1),
    {ok, #state{host = Host, jids = JIDs, php_code_ref = Ref}}.

compile_php_bot_check(JIDs) ->
    Src = php_bot_check_src(JIDs),
    {Mod, Code} = dynamic_compile:from_string(Src),
    code:load_binary(Mod, "php_bot_check.erl", Code).

php_bot_check_src(JIDs) ->
    "% Compiling the bot check maximizes packet filter efficiency.
    -module(php_bot_check).
    -export([jid/1]).
    " ++ jid_clauses(JIDs, []) ++ "
    jid(_) -> false.
    ".

jid_clauses([], Funs) ->
    string:join(Funs, "\n");
jid_clauses([JID | JIDs], Funs) ->
    Fun = "jid(\""++JID++"\") -> true;",
    jid_clauses(JIDs, [Fun | Funs]).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ejabberd_hooks:delete(filter_packet, ?MODULE, packet_filter, 1),
    php:unrequire(State#state.php_code_ref),
    ?DEBUG("php_bot stopped: ~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

packet_filter({From, #jid{user = U, server = S} = To, Packet}) ->
    case catch php_bot_check:jid(U++"@"++S) of
	true ->
	    bot_receive(From, To, Packet);
	_ ->
	    {From, To, Packet}
    end.

bot_receive(From, To, {xmlelement, "presence", _, _} = Packet) ->
    case xml:get_tag_attr_s("type", Packet) of
	"unavailable" ->
	    ok;
	"subscribe" ->
	    send_presence(To, From, "subscribe");
	"subscribed" ->
	    send_presence(To, From, "subscribed"),
	    send_presence(To, From, "");
	"unsubscribe" ->
	    send_presence(To, From, "unsubscribed"),
	    send_presence(To, From, "unsubscribe");
	"unsubscribed" ->
	    send_presence(To, From, "unsubscribed");
	"" ->
	    send_presence(To, From, "");
	"probe" ->
	    send_presence(To, From, "");
	_Other ->
	    ?INFO_MSG("Other kind of presence~n~p", [Packet])
    end,
    {stop, drop};
bot_receive(From, To, {xmlelement, "message", _, _} = Packet) ->
    case xml:get_subtag_cdata(Packet, "body") of
	"" ->
	    ok;
	Body ->
	    case xml:get_tag_attr_s("type", Packet) of
		"error" ->
		    ?ERROR_MSG("Received error message~n~p -> ~p~n~p", [From, To, Packet]);
		_ ->
		    spawn(?MODULE, php_bot_receive, [From, To, Body, Packet])
	    end
    end,
    {stop, drop};
bot_receive(_From, _To, {xmlelement, _Other, _, _} = _Packet) ->
    {stop, drop}.

php_bot_receive(UserJID, BotJID, Body, Packet) ->
    Responses = php:return("php_bot_receive",
			   [jlib:jid_to_string(UserJID),
			    jlib:jid_to_string(BotJID),
			    Body,
			    xml:element_to_string(Packet)]),
    bot_respond(BotJID, UserJID, Responses).

bot_respond(_From, _To, []) ->
    ok;
bot_respond(From, To, Binary) when is_binary(Binary) ->
    route(From, To, binary_to_list(Binary)),
    ok;
bot_respond(From, To, [Message | Rest]) ->
    route(From, To, Message),
    bot_respond(From, To, Rest).

route(From, To, Plain) ->
    ejabberd_router:route(From, To, {xmlelement, "message", [{"to", jlib:jid_to_string(To)}], [{xmlelement, "body", [], [{xmlcdata,Plain}]}]}).

send_presence(From, To, "") ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});

send_presence(From, To, TypeStr) ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [{"type", TypeStr}], []}).

