%%%-------------------------------------------------------------------
%%% @author pingjianwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2017 16:59
%%%-------------------------------------------------------------------
-module(initdata).
-author("pingjianwei").

%% API
-export([table_data_init/0]).

do_table_data_init(Table, VL) ->
  R = pg_model:new(Table, VL),
  pg_repo:save(R).

table_data_init() ->
  VLs =
    [
      [
        {id, 1}
        , {mcht_full_name, <<"test1">>}
        , {payment_method, [gw_collect1]}
        , {up_term_no, <<"12345678">>}
      ],
      [
        {id, 2}
        , {mcht_full_name, <<"test2">>}
        , {payment_method, [gw_collect]}
        , {up_term_no, <<"12345670">>}
      ],

      [
        {id, 3}
        , {mcht_full_name, <<"test3">>}
        , {payment_method, [gw_xm_up_bankcard]}
        , {up_term_no, <<"12345670">>}
      ]
    ],

  [do_table_data_init(repo_mchants_pt, VL) || VL <- VLs].