%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(catalog).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%-compile(export_all).
-export([all/1,get_service/2,
	 update_catalog/3]).




%% ====================================================================
%% External functions
%% ====================================================================
all(Catalog)->
    Catalog.

get_service(WantedServiceId,Catalog)->
    [{ServiceId,Type,Source}||{ServiceId,Type,Source}<-Catalog,
				   ServiceId==WantedServiceId].

%% @doc: update_catalog(GitUrl,Dir,FileName)->{ok,Config}|{error,Err} retreives the latets  config spec from git

-spec(update_catalog(GitUrl::string(),Dir::string(),FileName::string())->{ok,Config::[tuple()]}|{error,Err::string()}).
update_catalog(GitUrl,Dir,FileName)->
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitUrl),
    {R,Info}=file:consult(filename:join(Dir,FileName)),
    {R,Info}.

%% --------------------------------------------------------------------
%% 
%%
%% --------------------------------------------------------------------
