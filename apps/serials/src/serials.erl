-module(serials).
-behaviour(gen_server).

-include("serials.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(CONFIG(Term), serials_app:get_config(Term, [])).

-record(state, {
          %% config
          feed,
          period,
          err_delay,
          err_retries,
          files,
          torrents,
          %% internal
          timer,      % current timer ref
          err_count=0 % error retries counter
         }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    inets:start(),
    State = load_config(#state{}),
    self() ! fetch_rss,
    %% fake timer for first erlang:cancel_timer/1
    {ok, State#state{timer=make_ref()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(fetch_rss, #state{period=Period,
                          err_delay=ErrDelay,
                          err_retries=ErrRetries,
                          err_count=ErrCount}=State) ->
    erlang:cancel_timer(State#state.timer),
    %% download feed, handle next timer and error count
    State1 = case download_http(State#state.feed) of
                 {ok, Feed} ->
                     process_feed(Feed, State),
                     State#state{err_count=0, timer=set_timer(Period)};
                 {error, Error} ->
                     lager:warning("Feed download error: ~p", [Error]),
                     if ErrRetries =:= 0 -> %% next infinite error attempt
                             State#state{timer=set_timer(ErrDelay)};
                        ErrCount < ErrRetries -> %% next error attempt
                             State#state{err_count=ErrCount+1, timer=set_timer(ErrDelay)};
                        true -> %% no more error attempts, full period delay
                             State#state{err_count=0, timer=set_timer(Period)}
                     end
             end,
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Apply config values to state
load_config(State) ->
    State#state{
      feed=?CONFIG(rss_feed),
      period=?CONFIG(fetch_period) * 60000,
      err_delay=?CONFIG(error_delay) * 1000,
      err_retries=?CONFIG(error_retries),
      files=?CONFIG(files_dir),
      torrents=?CONFIG(torrents_dir)}.

%% @doc Set timer for periodical rss fetch action
set_timer(Timeout) ->
    erlang:send_after(Timeout, self(), fetch_rss).

%% @doc Load http resource 
download_http(Url) ->
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _, Body}} -> {ok, Body};
        {error, E} -> {error, E};
        _ -> {error, "Unknown response"}
    end.

%% @doc Process all episode links in feed 
process_feed(Feed, State) ->
    {ok, MP} = re:compile("<link>(.*\.S\\d+E\\d+.*\.torrent)</link>"),
    case re:run(Feed, MP, [global,{capture, all_but_first, list}]) of
        nomatch -> lager:error("Can't get links from feed");
        {match, Links} -> [process_link(Link, State) || [Link] <- Links]
    end.

%% @doc Process episode link
process_link(Link, State) ->
    case extract_episode(Link) of
        false -> lager:error("Can't extract episode name from link");
        Name ->
            case check_file(State#state.files++Name) of
                true -> lager:info("Episode already exists: ~s", [Name]);
                _ -> download_torrent(Link, State#state.torrents)
            end
    end.

%% @doc Extract episode name from link
extract_episode(Link) ->
    {ok, MP} = re:compile(".*&amp;(.*)\.torrent"),
    case re:run(Link, MP, [{capture, [1], list}]) of
        {match, [Name]} -> Name;
        _ -> false
    end.

%% @doc Check episode file for existence in any video format
check_file(File) -> check_file(File, ?VIDEO_EXT).
check_file(File, [Ext|T]) ->
    case filelib:is_regular(File++Ext) of
        true -> true;
        false -> check_file(File, T)
    end;
check_file(_, []) -> false.

%% @doc Download torrent to target dir
download_torrent(Link, Dir) ->
    Name = extract_episode(Link),
    OutFile = Dir++Name++".torrent",
    case filelib:is_regular(OutFile) of
        true -> lager:info("Torrent already exists: ~s", [Name]);
        false ->
            case download_http(Link) of
                {error, E} -> lager:warning("Download error ~s: ~p", [Link, E]);
                {ok, Body} -> file:write_file(OutFile, Body, [raw])
            end
    end.
