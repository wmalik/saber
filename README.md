## Saber
A Simple A/B testing configuration manager for Erlang applications

## Usage
#### Start the erlang shell
```shell
erl -pa ebin -pa deps/*/ebin -name saber_app@`hostname`
```
#### Start the saber supervisor
```erlang
1> saber_sup:start_link(["config/conf.abtests"]).
{ok,<0.33.0>}
```




## Examples
#### Fetch config of a specific A/B test

```erlang
4> saber_api:get_conf(expired_test).
[{modulo,10},
 {test_groups,[0,1]},
 {test_users,[666,777]},
 {test_value,"something"},
 {default_value,"something_else"},
 {end_time,489110400}]
```

#### Fetch value of a specific A/B test

```erlang
5> saber_api:get_value(123123, expired_test).
[{<<"testgroup">>,false},
 {<<"value">>,"something_else"},
 {<<"version">>,-1}]
```

#### Fetch all A/B test configuration

```erlang
2> saber_api:get_all_conf().
[{no_version,[{modulo,10},
              {test_groups,[0,1]},
              {test_users,[666,777]},
              {test_value,"something"},
              {default_value,"something_else"},
              {end_time,489110400}]},

...

 {expired_test,[{modulo,10},
                {test_groups,[0,1]},
                {test_users,[666,777]},
                {test_value,"something"},
                {default_value,"something_else"},
                {end_time,489110400}]},
 {client_feature_test,[{modulo,10},
                       {test_groups,[0,1,2,3,4]},
                       {test_users,[666,777]},
                       {test_value,<<"251d">>},
                       {default_value,<<"ce44">>}]}]
```

#### Fetch all A/B test values

```erlang
3> saber_api:get_all_values(123123).
[{client_feature_test,[{<<"testgroup">>,true},
                       {<<"value">>,<<"251d">>},
                       {<<"version">>,-1}]},
 {expired_test,[{<<"testgroup">>,false},
                {<<"value">>,"something_else"},
                {<<"version">>,-1}]},
 
 ...
 
 {no_version,[{<<"testgroup">>,false},
              {<<"value">>,"something_else"},
              {<<"version">>,-1}]}]
```

