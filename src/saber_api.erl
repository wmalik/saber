-module(saber_api).

%% Date: 2013-01-03
%  This gen_server is stateless and is the owner of the ets table created by
%  saber. According to the man page (http://www.erlang.org/doc/man/ets.html),
%  an ets table is destroyed if its owner process terminates.
%%
-behaviour(gen_server).

%% API
-export([
         start_link/1, get_all_values/1, get_value/2, get_all_conf/0,
         get_conf/1, reload/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONF_TABLE, saber_conf).
-define ( UNIX_TIME_IN_GREGORIAN,
    calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).



-record(state, {}).

% The response format of an evaluated AB test
-define(RESP_FORMAT(TestGroup, Value),
            [{<<"testgroup">>, TestGroup}, {<<"value">>, Value}]).

%%%===================================================================
%%% API
%%%===================================================================

% Fetches the AB test configuration from ConfigFilePath and inserts them in an
% ets table
start_link(ConfigFilePath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ConfigFilePath], []).

% Returns the evaluated values of all AB tests
% WARNING: This is an expensive function and should be used wisely
get_all_values(UserId) when is_integer(UserId) -> %evaluated values
    eval_all_abtests(UserId).

% Returns the evaluated value for an AB test key. If the key does not exist, it
% returns abtest_undefined
get_value(UserId, Key) when is_integer(UserId) ->
    try
        % fetch the config of a specific AB test
        ABTestConf = case catch( ets:lookup_element(?CONF_TABLE, Key, 2) ) of
            A = [{_,_}|_] -> A;
            _             -> throw(abtest_undefined)
        end,

        % Evaluate the AB test config
        {TestGroup, Value} = eval_abtest(UserId, ABTestConf),
        ?RESP_FORMAT(TestGroup, Value)
    catch
        abtest_undefined -> abtest_undefined
    end.

% Returns un-evaluated AB test conf
get_all_conf() ->
    get_ets_tuples().

% Returns un-evaluated AB test conf for a specific key
get_conf(Key) ->
    ets:lookup_element(?CONF_TABLE, Key, 2).

% Reloads the configuration from ConfigFile. Deletes the ets objects which are
% not present in the new ConfigFile.  Updates the rest of the objects with new
% values from ConfigFile. The ets objects are updated one by one so the reload
% is not transactional. However, the ConfigFile is validated first, and an
% exception is thrown if there is a syntax error.
reload(ConfigFile) ->
    NewConf = read_config(ConfigFile),

    % Delete all ets objects which have been removed from the ConfigFile
    Step = fun({Key, _Val}, Acc) ->
            case proplists:is_defined(Key, NewConf) of
                false -> [Key|Acc];
                true  -> Acc
            end
    end,
    KeysToDelete = ets:foldl(Step, [], ?CONF_TABLE),

    % delete the removed keys from ets table
    lists:foreach(fun(X) ->
                    io:format("Removing ~p~n", [X]),
                    ets:delete(?CONF_TABLE, X) end,
                  KeysToDelete),
    % refresh the conf
    lists:foreach(fun(X) -> ets:insert(?CONF_TABLE, X) end, NewConf).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ConfigFile]) ->
    ets:new(?CONF_TABLE,
        [ordered_set, public, named_table,
        {write_concurrency, false},
        {read_concurrency, true}
    ]),

    ABTestsConf = read_config(ConfigFile),

    % copy all tests from config file to ets table
    Step = fun(TestTuple) ->
            case ets:insert_new(?CONF_TABLE, TestTuple) of
                true  -> ok;
                Err -> error_logger:error_msg("Unable to insert config ~p~n",
                            [Err])
            end
    end,
    lists:foreach(Step, ABTestsConf),
    {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


read_config(File) ->
    case file:consult(File) of
        {ok, [Terms]} -> proplists:get_value(abtests, Terms);
        Error ->
            error_logger:error_msg("Unable to read abtests config from ~s ~p",
                [File, Error]
            ),
            throw({saber, read_config_failed})
    end.

%% Returns the list of ets tuples in ?CONF_TABLE
get_ets_tuples() ->
    % concatenate all values in the ets table
    FunCat = fun(E, Acc) -> [E|Acc] end,
    ets:foldl(FunCat, [], ?CONF_TABLE).

% Evaluates all AB tests
eval_all_abtests(UserId) ->
    ABTests = get_ets_tuples(),

    Step = fun({ABTestName,ABTestConf}, Acc) ->
             {TestGroup, Value} = eval_abtest(UserId, ABTestConf),
             TestJson = ?RESP_FORMAT(TestGroup, Value),
             lists:append([{ABTestName, TestJson}], Acc)
    end,
    lists:foldl(Step, [], ABTests).

% Evaluate an AB test configuration
eval_abtest(UserId, ABTestConf) ->
    Modulo     = proplists:get_value(modulo, ABTestConf),
    TestGroups = proplists:get_value(test_groups, ABTestConf),
    WhiteList  = proplists:get_value(test_users, ABTestConf),
    TestVal    = proplists:get_value(test_value, ABTestConf),
    DefaultVal = proplists:get_value(default_value, ABTestConf),
    EndTime    = proplists:get_value(end_time, ABTestConf, undefined),

    case is_test_user(Modulo, TestGroups, WhiteList, UserId)
         andalso
         is_test_active(EndTime) of
             true  -> {true,  TestVal};
             false -> {false, DefaultVal}
    end.

% Check whether the UserId is part of a test group. Returns boolean
is_test_user(Modulo, RemainderList, WhiteList, UserId) ->
    lists:member(UserId rem Modulo, RemainderList)
    or
    lists:member(UserId, WhiteList).

%TODO, add to test conf also end_time
is_test_active(undefined) ->
    true;
is_test_active(EndTime) ->
    EndTime > unix_time().

unix_time() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_universal_time(os:timestamp())
    ) - ?UNIX_TIME_IN_GREGORIAN.
