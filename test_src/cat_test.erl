%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(cat_test).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").



-ifdef(dir).
-define(CHECK_CATALOG,check_catalog_dir()).
-else.
-define(CHECK_CATALOG,check_catalog_git()).
-endif.


%% --------------------------------------------------------------------
-export([start/0]).
%-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================

%% 
%% ----------------------------------------------- ---------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    ?assertEqual(ok,application:start(catalog_service)),
    ?debugMsg("get_all"),
    ?assertEqual(ok,get_all()),
    ?debugMsg("get_service"),
    ?assertEqual(ok,get_service()),

    ?debugMsg("update_catalog"),
    ?assertEqual(ok,update_catalog()),
    ?assertEqual(ok,application:stop(catalog_service)),		 
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
get_all()->
    ?assertEqual([{"adder_service",git,"https://github.com/joq62/"},
		  {"divi_service",git,"https://github.com/joq62/"},
		  {"dns_service",dir,"/home/pi/erlang/erl_infra/"},
		  {"log_service",dir,"/home/pi/erlang/erl_infra/"},
		  {"lib_service",dir,"/home/pi/erlang/erl_infra/"},
		  {"master_service",dir,"/home/pi/erlang/erl_infra/"}],catalog_service:get_all()),
    ok.

get_service()->
    ?assertEqual([{"adder_service",git,"https://github.com/joq62/"}],catalog_service:get_service("adder_service")),
    ok.

update_catalog()->
    ?assertEqual(ok,catalog_service:update_catalog()),
    ok.
