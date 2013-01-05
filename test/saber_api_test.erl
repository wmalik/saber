-module (saber_api_test).
-compile (export_all).
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").

before_suite() ->
    application:start(saber),
    ok.

before_test() ->
    nothing.

after_test() ->
    nothing.

after_suite() ->
    application:stop(saber),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTS FOR ERLANG TERMS API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_get_all_values() -> % validates the response of saber:get_all_values/1

    Tests111 = saber_api:get_all_values(111),
    ?assert_equal(true, proplists:is_defined(client_feature_test, Tests111)),
    ?assert_equal(true, proplists:is_defined(diamonds_test, Tests111)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests111)),
    R111 = proplists:get_value(integ_test, Tests111),
    ?assert_equal("something", proplists:get_value(<<"value">>, R111)),
    ?assert_equal(true, proplists:get_value(<<"testgroup">>, R111)),

    Tests333 = saber_api:get_all_values(333),
    ?assert_equal(true, proplists:is_defined(client_feature_test, Tests333)),
    ?assert_equal(true, proplists:is_defined(diamonds_test, Tests333)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests333)),
    R333 = proplists:get_value(integ_test, Tests333),
    ?assert_equal("something_else", proplists:get_value(<<"value">>, R333)),
    ?assert_equal(false, proplists:get_value(<<"testgroup">>, R333)),

    Tests666 = saber_api:get_all_values(666),
    ?assert_equal(true, proplists:is_defined(client_feature_test, Tests666)),
    ?assert_equal(true, proplists:is_defined(diamonds_test, Tests666)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests666)),
    R666 = proplists:get_value(integ_test, Tests666),
    ?assert_equal("something", proplists:get_value(<<"value">>, R666)),
    ?assert_equal(true, proplists:get_value(<<"testgroup">>, R666)).

test_get_value() ->
    Tests111 = saber_api:get_value(111, integ_test),
    ?assert_equal(true, proplists:get_value(<<"testgroup">>, Tests111)),
    ?assert_equal("something", proplists:get_value(<<"value">>, Tests111)).

test_get_all_conf() ->
    AllConf = saber_api:get_all_conf(),
    ?assert_equal(true, proplists:is_defined(client_feature_test, AllConf)),
    ?assert_equal(true, proplists:is_defined(diamonds_test, AllConf)),
    ?assert_equal(true, proplists:is_defined(integ_test, AllConf)),

    Tests111 = proplists:get_value(integ_test, AllConf),
    ?assert_equal(true, proplists:is_defined(modulo, Tests111)),
    ?assert_equal(true, proplists:is_defined(test_groups, Tests111)),
    ?assert_equal(true, proplists:is_defined(test_users, Tests111)),
    ?assert_equal(true, proplists:is_defined(test_value, Tests111)),
    ?assert_equal(true, proplists:is_defined(default_value, Tests111)).

test_get_conf() ->
    Tests111 = saber_api:get_conf(integ_test),
    ?assert_equal(true, proplists:is_defined(modulo, Tests111)),
    ?assert_equal(true, proplists:is_defined(test_groups, Tests111)),
    ?assert_equal(true, proplists:is_defined(test_users, Tests111)),
    ?assert_equal(true, proplists:is_defined(test_value, Tests111)),
    ?assert_equal(true, proplists:is_defined(default_value, Tests111)).

test_reload() ->
    % if we reload saber with a new configfile
    saber_api:reload("test/sample_small.abtests"),

    %.. then the new changes should be visible from the API
    Tests111 = saber_api:get_all_values(111),
    ?assert_equal(false, proplists:is_defined(client_feature_test, Tests111)),
    ?assert_equal(false, proplists:is_defined(diamonds_test, Tests111)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests111)),

    %% CLEANUP
    saber_api:reload("config/conf.abtests").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTS FOR JSON API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test_get_all_values_json() ->
    todo.

test_get_value_json() ->
    todo.

test_get_all_conf_json() ->
    todo.

test_get_conf_json() ->
    todo.

