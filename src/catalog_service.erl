%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(catalog_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{catalog,
	       app_spec,
	       dns}).


%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(CATALOG_HEARTBEAT,40*1000).
-define(CATALOG_URL,"https://github.com/joqerlang/catalog.git/").
-define(CATALOG_DIR,"catalog").
-define(CATALOG_FILENAME,"catalog.info").
-define(APP_SPEC_URL,"https://github.com/joqerlang/app_config.git/").
-define(APP_SPEC_DIR,"app_config").
-define(APP_SPEC_FILENAME,"app.spec").

-export([% Catalog part
	 get_service_config/1,update_catalog/0,
	 %% App spec part
	 app_spec_update/0,
	 get_service/1,
	 available/0,missing/0,obsolite/0,
	 %% Dns support
	 dns_update/0,dns_all/0,dns_get/1,dns_add/2,dns_delete/2
	]).

-export([start/0,
	 stop/0,
	 ping/0,
	 heart_beat/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals



%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------

%% App spec functions
get_service(ServiceId)->
    gen_server:call(?MODULE, {get_service,ServiceId},infinity).
available()->
    gen_server:call(?MODULE, {available},infinity).
missing()->    
    gen_server:call(?MODULE, {missing},infinity).
obsolite()->
    gen_server:call(?MODULE, {obsolite},infinity).

app_spec_update()->
    gen_server:cast(?MODULE, {app_spec_update}).
%% Catalog functions
get_service_config(ServiceId)->
    gen_server:call(?MODULE, {get_service_config,ServiceId},infinity).
update_catalog()->
     gen_server:call(?MODULE, {update_catalog},infinity).

%% Dns support functions

dns_all()->
    gen_server:call(?MODULE, {dns_all},infinity).
dns_get(ServiceId)->
    gen_server:call(?MODULE, {dns_get,ServiceId},infinity).
dns_add(ServiceId,Node)->
    gen_server:cast(?MODULE, {dns_add,ServiceId,Node}).
dns_delete(ServiceId,Node)->
    gen_server:cast(?MODULE, {dns_delete,ServiceId,Node}).
dns_update()->
    gen_server:cast(?MODULE, {dns_update}).
heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,Catalog}=catalog:update(?CATALOG_URL,?CATALOG_DIR,?CATALOG_FILENAME),
    {ok,AppSpec}=app_spec:update(?APP_SPEC_URL,?APP_SPEC_DIR,?APP_SPEC_FILENAME),
    {ok,DnsInfo}=dns:update(Catalog),
    spawn(fun()->h_beat(?CATALOG_HEARTBEAT,DnsInfo) end),
    {ok, #state{catalog=Catalog,app_spec=AppSpec,dns=DnsInfo}}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

%% Dns functions
handle_call({dns_all},_From,State) ->
    Reply=dns:all(State#state.dns),
    {reply, Reply, State};

handle_call({dns_get,ServiceId},_From,State) ->
    Reply=dns:get(ServiceId,State#state.dns),
    {reply, Reply, State};

%% App spec functions

handle_call({available}, _From, State) ->
    Reply=app_spec:available(State#state.catalog),
    {reply, Reply,State};

handle_call({missing}, _From, State) ->
      Reply=app_spec:missing(State#state.catalog,State#state.app_spec),
    {reply, Reply,State};

handle_call({obsolite}, _From, State) ->
      Reply=app_spec:obsolite(State#state.catalog,State#state.app_spec),
    {reply, Reply,State};

%% Catalog functions

handle_call({get_service,all}, _From, State) ->
    Reply=catalog:all(State#state.catalog),
    {reply, Reply,State};

handle_call({get_service,WantedServiceId}, _From, State) ->
    Reply=catalog:get_service(WantedServiceId,State#state.catalog),
    {reply, Reply,State};
handle_call({update_catalog}, _From, State) ->
    Reply=case catalog:update_catalog(?CATALOG_URL,?CATALOG_DIR,?CATALOG_FILENAME) of
	      {ok,Catalog}->
		  NewState=State#state{catalog=Catalog},
		  ok;
	      {error,Err}->
		  NewState=State,
		  {error,Err}
	  end,
    {reply, Reply,NewState};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({heart_beat,Interval}, State) ->
    spawn(fun()->h_beat(Interval,State#state.dns) end),    
    {noreply, State};

handle_cast({dns_update}, State) ->
    {ok,DnsInfo}=dns:update(State#state.catalog),
    NewState=State#state{dns=DnsInfo},
    dns:update_local_dns(DnsInfo),
    {noreply, NewState};

handle_cast({dns_add,ServiceId,Node}, State) ->
    {ok,DnsInfo}=dns:add(ServiceId,Node,State#state.dns),
    NewState=State#state{dns=DnsInfo},
    dns:update_local_dns(DnsInfo),
    {noreply, NewState};

handle_cast({dns_delete,ServiceId,Node}, State) ->
    {ok,DnsInfo}=dns:delete(ServiceId,Node,State#state.dns),
    NewState=State#state{dns=DnsInfo},
    dns:update_local_dns(DnsInfo),
    {noreply, NewState};


handle_cast({app_spec_update}, State) ->
   NewState= case app_spec:update(?APP_SPEC_URL,?APP_SPEC_DIR,?APP_SPEC_FILENAME) of
		 {ok,AppSpec}->
		     State#state{app_spec=AppSpec};
		 {error,Err}->
		     io:format("error ~p~n",[{?MODULE,?LINE,Err}]),
		     State
	     end,
    {noreply, NewState};


handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
h_beat(Interval,DnsInfo)->
    {ok,Catalog}=catalog:update(?CATALOG_URL,?CATALOG_DIR,?CATALOG_FILENAME),
    {ok,NewDnsInfo}=dns:update(Catalog),
    io:format("Catalog = ~p~n",[{?MODULE,?LINE,Catalog}]),
    io:format("NewDnsInfo = ~p~n",[{?MODULE,?LINE,NewDnsInfo}]),
    spawn(fun()->catalog_service:dns_update() end),
    case dns:get("iaas_service",DnsInfo) of
	[{_,IaasNode}|_]->
	    ListOfNodes=rpc:call(IaasNode,iaas_service,available,[]),
	    [rpc:cast(Node,boot_service,dns_update,[DnsInfo])||{_,Node}<-ListOfNodes];
	Err->
	    io:format("iaas_service not available Err = ~p~n",[{?MODULE,?LINE,Err}])
    end,
    spawn(fun()->catalog_service:app_spec_update() end),
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,heart_beat,[Interval]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
