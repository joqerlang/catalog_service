%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(service_test).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").


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
    ?debugMsg("start_service"),
    ?assertEqual(ok,start_service()),

    ?debugMsg("catalog"),
    ?assertEqual(ok,catalog_test()),
    ?debugMsg("app_spec"),
    ?assertEqual(ok,app_spec_test()),

    ?debugMsg("dns"),
    ?assertEqual(ok,dns_test()),	 
    ?debugMsg("stop_service"),
    ?assertEqual(ok,stop_service()),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
start_service()->
    application:start(catalog_service).
stop_service()->
    application:stop(catalog_service).

catalog_test()->
    ?assertEqual(glurk,catalog_service:get_service_config("adder_service")),
    ok.

app_spec_test()->
    ?assertEqual(glurk,catalog_service:get_service_addr("adder_service")),
    ?assertEqual(glurk,catalog_service:available()),
       
    ok.

dns_test()->
    ?assertEqual(glurk,catalog_service:dns_all()),
    ok.
