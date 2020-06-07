%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(app_spec_test).  
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-define(GIT_URL,"https://github.com/joqerlang/").
%% --------------------------------------------------------------------
-export([start/0]).
%-compile(export_all).
-define(CATALOG,[
		 {"adder_service",git,"https://github.com/joqerlang/"},
		 {"divi_service",git,"https://github.com/joqerlang/"},
		 {"subtract_service",git,"https://github.com/joqerlang/"},
		 {"multi_service",git,"https://github.com/joqerlang/"}
		]).


-define(APP_SPEC,[
		  {"adder_service",'catalog_dir_test@asus'},
		  {"divi_service",'catalog_dir_test@asus'},
		  {"subtract_service",'catalog_dir_test@asus'}
		]).


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

    ?debugMsg("check clone"),
    ?assertEqual(ok,check_clone()),
    ?debugMsg("check no services"),
    ?assertEqual(ok,no_services()),
    ?debugMsg("check one  services"),
    ?assertEqual(ok,one_services()),
    ?debugMsg("check all  services"),
    ?assertEqual(ok,all_services()),	
    ?debugMsg("check obsolite  services"),
    ?assertEqual(ok,obsolite_services()),	 
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_clone()->
    ServiceId="adder_service",
    ?assertMatch({error,{"no such file or directory",
			 _}},application:start(list_to_atom(ServiceId))),  
    ?assertMatch(ok,start_app(ServiceId)),
    ?assertMatch(ok,stop_app(ServiceId)),
    ?assertMatch({error,{"no such file or directory",
			 _}},application:start(list_to_atom(ServiceId))), 

   ok.

no_services()->
    ?assertEqual([],app_spec:available(?CATALOG)),
    ?assertEqual([{"adder_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"subtract_service",catalog_dir_test@asus}],app_spec:missing(?CATALOG,?APP_SPEC)),
    ?assertEqual([],app_spec:obsolite(?CATALOG,?APP_SPEC)),
    ok.

one_services()->
    start_app("adder_service"),
    ?assertEqual([{"adder_service",catalog_dir_test@asus}],app_spec:available(?CATALOG)),
    ?assertEqual([{"divi_service",catalog_dir_test@asus},
		  {"subtract_service",catalog_dir_test@asus}],app_spec:missing(?CATALOG,?APP_SPEC)),
    ?assertEqual([],app_spec:obsolite(?CATALOG,?APP_SPEC)),
    stop_app("adder_service"),
    ok.
all_services()->
    [start_app(ServiceId)||{ServiceId,_}<-?APP_SPEC],
    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],app_spec:available(?CATALOG)),
    ?assertEqual([],app_spec:missing(?CATALOG,?APP_SPEC)),
    ?assertEqual([],app_spec:obsolite(?CATALOG,?APP_SPEC)),
    [stop_app(ServiceId)||{ServiceId,_}<-?APP_SPEC],
    ok.

obsolite_services()->
    [start_app(ServiceId)||{ServiceId,_}<-?APP_SPEC],
    start_app("multi_service"),
    ?assertEqual([{"multi_service",catalog_dir_test@asus},
		  {"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],app_spec:available(?CATALOG)),
    ?assertEqual([],app_spec:missing(?CATALOG,?APP_SPEC)),
    ?assertEqual([{"multi_service",catalog_dir_test@asus}],app_spec:obsolite(?CATALOG,?APP_SPEC)),
    [stop_app(ServiceId)||{ServiceId,_}<-?APP_SPEC],
    stop_app("multi_service"),
    ok.

start_app(ServiceId)->
    EbinDir=filename:join(ServiceId,"ebin"),
    stop_app(ServiceId),
    os:cmd("git clone "++?GIT_URL++ServiceId++".git"),
    ?assertEqual(true,code:add_path(EbinDir)),
    ?assertMatch(ok,application:start(list_to_atom(ServiceId))), 
    ok.

stop_app(ServiceId)->
    EbinDir=filename:join(ServiceId,"ebin"),
    application:stop(list_to_atom(ServiceId)),
    application:unload(list_to_atom(ServiceId)),
    code:del_path(EbinDir),      
    os:cmd("rm -rf "++ServiceId),
    ok.    
