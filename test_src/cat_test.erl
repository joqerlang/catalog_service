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

-define(CATALOG_URL,"https://github.com/joqerlang/catalog.git/").
-define(CATALOG_DIR,"catalog").
-define(CATALOG_FILENAME,"catalog.info").

-define(TEST_CATALOG_INFO,[
			   {"s1_service",git,"https://github.com/s1/"},
			   {"s2_service",git,"https://github.com/s2/"},
			   {"s3_service",dir,"/home/pi/erlang/S3/"},
			   {"S4_service",dir,"/home/pi/erlang/S4/"}]).


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
    ?debugMsg("get_all"),
    ?assertEqual(ok,get_all()),
    ?debugMsg("get_service"),
    ?assertEqual(ok,get_service()),

  %  ?debugMsg("update_catalog"),
 %   ?assertEqual(ok,update_catalog()),	 
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
get_all()->
    ?assertEqual([{"s1_service",git,"https://github.com/s1/"},
		  {"s2_service",git,"https://github.com/s2/"},
		  {"s3_service",dir,"/home/pi/erlang/S3/"},
		  {"S4_service",dir,"/home/pi/erlang/S4/"}],catalog:all(?TEST_CATALOG_INFO)),
    ok.

get_service()->
    ?assertEqual([],catalog:get_service("adder_service",?TEST_CATALOG_INFO)),
    ?assertEqual([{"s1_service",git,"https://github.com/s1/"}],catalog:get_service("s1_service",?TEST_CATALOG_INFO)),
       
    ok.

update_catalog()->
    ?assertEqual({ok,[
		      {"catalog_service",git,"https://github.com/joqerlang/"},
		      {"boot_service",git,"https://github.com/joqerlang/"},
		      {"iaas_service",git,"https://github.com/joqerlang/"},
		      {"orchistrate_service",git,"https://github.com/joqerlang/"},
		      {"adder_service",git,"https://github.com/joqerlang/"},
		      {"multi_service",git,"https://github.com/joqerlang/"},
		      {"subtract_service",git,"https://github.com/joqerlang/"},
		      {"divi_service",git,"https://github.com/joqerlang/"}]},catalog:update(?CATALOG_URL,?CATALOG_DIR,?CATALOG_FILENAME)),
		 ok.
