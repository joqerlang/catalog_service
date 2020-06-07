%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(dns).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%-compile(export_all).
-export([get_all/1,get/2,update_info/1,dns_add/3,dns_delete/3]).




%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% 
%% 
%% {"master_sthlm_1",'master_sthlm_1@asus'}
%% --------------------------------------------------------------------
%% @doc: get(ServiceId) returns of a list of nodes that have ServiceId all running applications

-spec(get_all(DnsInfo::[{ServiceId::string(),Node::atom()}])->[{ServiceId::string(),Node::atom()}]|[]).
get_all(DnsInfo)->
    DnsInfo.


%% @doc: get(ServiceId) returns of a list of nodes that have ServiceId all running applications
-spec(get(ServiceId::string(),DnsInfo::[{ServiceId::string(),Node::atom()}])->[{ServiceId::string(),Node::atom()}]|[]).
get(WantedServiceId,DnsInfo)->
    ActiveServices=[{ServiceId,Node}||{ServiceId,Node}<-DnsInfo,
				      WantedServiceId==ServiceId],
    ActiveServices.

%% @doc: update_info(Catalog) update the dns list

-spec(update_info(Catalog::[{ServiceId::string(),Type::atom(),Source::string()}])->[{ServiceId::string(),Node::atom()}]| []).
update_info(Catalog)->
    AvailableServices=app_spec:available(Catalog),
    {ok,AvailableServices}.

%% @doc: dns_add(ServiceId,Node,DnsInfo) -> New DnsInfo list

-spec(dns_add(ServiceId::string(),Node::atom(),DnsInfo::[tuple()])->DnsInfo::[tuple()]|[]).
dns_add(ServiceId,Node,DnsInfo)->
    Removed=[{S1,N1}||{S1,N1}<-DnsInfo,
		      {ServiceId,Node}/={S1,N1}],
    NewDnsInfo=[{ServiceId,Node}|Removed],
    {ok,NewDnsInfo}.


%% @doc:dns_delete(ServiceId,Node,DnsInfo) -> New DnsInfo list

-spec(dns_delete(ServiceId::string(),Node::atom(),DnsInfo::[tuple()])->DnsInfo::[tuple()]|[]).
dns_delete(ServiceId,Node,DnsInfo)->
    NewDnsInfo=[{S1,N1}||{S1,N1}<-DnsInfo,
		      {ServiceId,Node}/={S1,N1}],
    {ok,NewDnsInfo}.
