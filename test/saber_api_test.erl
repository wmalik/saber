-module (saber_api_test).
-compile (export_all).
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").

before_suite() ->
    application:start(saber).

before_test() ->
    nothing.

after_test() ->
    nothing.

after_suite() ->
    application:stop(saber).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTS FOR ERLANG TERMS API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_get_all_values() -> % validates the response of saber:get_all_values/1

    Tests111 = saber_api:get_all_values(111),
    ?assert_equal(true, proplists:is_defined(client_feature_test, Tests111)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests111)),
    R111 = proplists:get_value(integ_test, Tests111),
    ?assert_equal("something", proplists:get_value(<<"value">>, R111)),
    ?assert_equal(true, proplists:get_value(<<"testgroup">>, R111)),
    ?assert_equal(1, proplists:get_value(<<"version">>, R111)),

    Tests333 = saber_api:get_all_values(333),
    ?assert_equal(true, proplists:is_defined(client_feature_test, Tests333)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests333)),
    R333 = proplists:get_value(integ_test, Tests333),
    ?assert_equal("something_else", proplists:get_value(<<"value">>, R333)),
    ?assert_equal(false, proplists:get_value(<<"testgroup">>, R333)),
    ?assert_equal(1, proplists:get_value(<<"version">>, R333)),

    Tests666 = saber_api:get_all_values(666),
    ?assert_equal(true, proplists:is_defined(client_feature_test, Tests666)),
    ?assert_equal(true, proplists:is_defined(integ_test, Tests666)),
    R666 = proplists:get_value(integ_test, Tests666),
    ?assert_equal("something", proplists:get_value(<<"value">>, R666)),
    ?assert_equal(true, proplists:get_value(<<"testgroup">>, R666)),
    ?assert_equal(1, proplists:get_value(<<"version">>, R666)).

test_get_value() ->
    % if a test exists in the configuration file..
    Tests111 = saber_api:get_value(111, integ_test),
    % .. the response should be in the correct format
    ?assert_equal(true, proplists:get_value(<<"testgroup">>, Tests111)),
    ?assert_equal("something", proplists:get_value(<<"value">>, Tests111)),
    ?assert_equal(1, proplists:get_value(<<"version">>, Tests111)),

    % if its some nonexistent test..
    Test222 = saber_api:get_value(222, some_nonexistent_test),

    % .. abtest_undefined should be returned
    ?assert_equal(abtest_undefined, Test222).

test_get_value_for_a_test_with_no_version() ->
    % if a test has no version
    Tests111 = saber_api:get_value(111, expired_test),
    % .. then the version should be -1
    ?assert_equal(-1, proplists:get_value(<<"version">>, Tests111)).

test_get_value_for_an_expired_abtest() ->
    % if a test has expired
    Tests111 = saber_api:get_value(111, expired_test),
    % .. the response should be the default value even if the user falls in the
    % test group
    ?assert_equal("something_else", proplists:get_value(<<"value">>, Tests111)).

test_get_value_for_test_with_no_test_group() ->
    Tests111 = saber_api:get_value(111, no_groups),
    ?assert_equal("something_else", proplists:get_value(<<"value">>, Tests111)),

    Tests222 = saber_api:get_value(666, no_groups),
    ?assert_equal("something", proplists:get_value(<<"value">>, Tests222)).


test_get_all_conf() ->
    AllConf = saber_api:get_all_conf(),
    ?assert_equal(true, proplists:is_defined(client_feature_test, AllConf)),
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
    % if we reload saber with a new configfile which doesnt have
    % client_feature_test
    saber_api:reload("test/sample_small.abtests"),

    %.. then the new changes should be visible from the API
    Tests111 = saber_api:get_all_values(111),
    ?assert_equal(false, proplists:is_defined(client_feature_test, Tests111)),
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

