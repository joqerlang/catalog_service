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
-include("common_macros.hrl").
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{catalog}).


%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(MASTER_HEARTBEAT,40*1000).
-define(CATALOG_URL,"https://github.com/joqerlang/catalog.git/").
-define(CATALOG_DIR,"catalog").
-define(CATALOG_FILENAME,"catalog.info").

-export([get_service/1,get_all/0,update_catalog/0
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

%%
get_all()->
    gen_server:call(?MODULE, {get_all},infinity).
get_service(ServiceId)->
    gen_server:call(?MODULE, {get_service,ServiceId},infinity).
update_catalog()->
     gen_server:call(?MODULE, {update_catalog},infinity).

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
    {ok,Catalog}=catalog:update_catalog(?CATALOG_URL,?CATALOG_DIR,?CATALOG_FILENAME),
    {ok, #state{catalog=Catalog}}.   
    
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

handle_call({get_all}, _From, State) ->
    Reply=State#state.catalog,
    {reply, Reply,State};

handle_call({get_service,WantedServiceId}, _From, State) ->
    Reply=[{ServiceId,Type,Source}||{ServiceId,Type,Source}<-State#state.catalog,
				   ServiceId==WantedServiceId],
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
    spawn(fun()->h_beat(Interval) end),    
    {noreply, State};

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
h_beat(Interval)->
    case rpc:call(node(),orchistrater,campaign,[],60*1000) of
	ok->
	    ok;
	Err->
	    rpc:call(node(),lib_service,log_event,[?MODULE,?LINE,orchistrater,campaign,error,[Err]])
    end,
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
