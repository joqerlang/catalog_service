%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dns_test).  
    
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
    ?debugMsg("check no services"),
    ?assertEqual(ok,no_services()),
    ?debugMsg("check one  services"),
    ?assertEqual(ok,one_services()),
    ?debugMsg("check all  services"),
    ?assertEqual(ok,all_services()),	
    ?debugMsg("dns add service"),
    ?assertEqual(ok,add_service()),
    ?debugMsg("dns delete service"),
    ?assertEqual(ok,delete_service()), 
    ?debugMsg("dns cleanup"),
    ?assertEqual(ok,clean_up()), 
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

no_services()->
    {ok,DnsInfo}=dns:update(?CATALOG),
    ?assertEqual([],DnsInfo),
    ?assertEqual([],dns:all(DnsInfo)),
    ?assertEqual([],dns:get("adder_service",DnsInfo)),
    ok.

one_services()->
    start_app("adder_service"),
    {ok,DnsInfo}=dns:update(?CATALOG),
    ?assertEqual([{"adder_service",catalog_dir_test@asus}],DnsInfo),
    ?assertEqual([{"adder_service",catalog_dir_test@asus}],dns:all(DnsInfo)),
    ?assertEqual([{"adder_service",catalog_dir_test@asus}],dns:get("adder_service",DnsInfo)),
    ?assertEqual([],dns:get("multi_service",DnsInfo)),
    stop_app("adder_service"),
    ok.
all_services()->
    [start_app(ServiceId)||{ServiceId,_}<-?APP_SPEC],
    {ok,DnsInfo}=dns:update(?CATALOG),
    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],DnsInfo),
    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],dns:all(DnsInfo)),
    ?assertEqual([{"adder_service",catalog_dir_test@asus}],dns:get("adder_service",DnsInfo)),
    ?assertEqual([],dns:get("multi_service",DnsInfo)),
    ok.

add_service()->
    {ok,DnsInfo}=dns:update(?CATALOG),
    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],DnsInfo),
    {ok,NewDnsInfo}=dns:add("new_service",catalog_dir_test@new,DnsInfo),
    ?assertEqual([{"new_service",catalog_dir_test@new},
		  {"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],NewDnsInfo),
    ?assertEqual([{"new_service",catalog_dir_test@new}],dns:get("new_service",NewDnsInfo)),
    ok.
delete_service()->
    {ok,DnsInfo}=dns:update(?CATALOG),
    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus},
		  {"adder_service",catalog_dir_test@asus}],DnsInfo),
    {ok,NewDnsInfo}=dns:delete("adder_service",catalog_dir_test@asus,DnsInfo),

    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus}],NewDnsInfo),
    ?assertEqual([{"subtract_service",catalog_dir_test@asus},
		  {"divi_service",catalog_dir_test@asus}],dns:all(NewDnsInfo)), 
    ?assertEqual([],dns:get("adder_service",NewDnsInfo)),   
    ok.


clean_up()->
    [stop_app(ServiceId)||{ServiceId,_}<-?APP_SPEC],
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
