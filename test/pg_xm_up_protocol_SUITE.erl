%%%-------------------------------------------------------------------
%%% @author simon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十月 2017 13:23
%%%-------------------------------------------------------------------
-module(pg_xm_up_protocol_SUITE).
-author("simon").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

-define(M_R, pg_up_protocol_t_repo_up_txn_log_pt).
-define(M_R_MCHANTS, pg_up_protocol_t_repo_mchants_pt).

%%-define(M_R, repo_up_txn_log_pt).
%%-define(M_R_MCHANTS, repo_mchants_pt).

-define(M_P_MCHT_REQ, pg_mcht_protocol_req_collect).
-define(APP, pg_up_protocol).

-compile(export_all).



setup() ->
  lager:start(),

  application:start(inets),

  env_init(),

  application:start(pg_xm_up_protocol),
  application:start(xm_up_config),
  pg_test_utils:setup(mnesia),

  env_init(),

  table_init(),
  table_data_init(),


  ok.

env_init() ->
  Cfgs = [
    {pg_xm_up_protocol,
      [
        {mchants_repo_name, pg_up_protocol_t_repo_mchants_pt}
        , {up_repo_name, pg_up_protocol_t_repo_up_txn_log_pt}
        , {debug, true}

      ]
    }
    ,{pg_protocol,
      [
        {debug_convert_config, true}
      ]
    }
    , {pg_convert,
      [
        {debug, false}
        , {field_existance_validate, true}

      ]
    }
  ],


  pg_test_utils:env_init(Cfgs),
  ok.

do_table_init(Table) when is_atom(Table) ->
  pg_repo:drop(Table),
  pg_repo:init(Table),
  ok.

table_init() ->
  [do_table_init(Table) || Table <- [?M_R, ?M_R_MCHANTS]].

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

  [do_table_data_init(?M_R_MCHANTS, VL) || VL <- VLs].


%%-------------------------------------------------------------

my_test_() ->
  {
    setup
    , fun setup/0
    ,
    {
      inorder,
      [
        {timeout, 120, fun repo_data_test_1/0}
        , {timeout, 120, fun save_test_1/0}
        , {timeout, 120, fun verify_test_1/0}
        , {timeout, 120, fun send_xm_up_bankcard_test_1/0}
        , {timeout, 120, fun resp_convert_test_1/0}
      ]
    }
  }.

%%---------------------------------------------------
repo_data_test_1() ->
  [R] = pg_repo:read(?M_R_MCHANTS, 3),
  ?assertEqual([gw_xm_up_bankcard], pg_model:get(?M_R_MCHANTS, R, payment_method)),
  ok.
%%---------------------------------------------------

qs(req) ->
  [
    {<<"customer_code">>, <<"B8999001">>}
    , {<<"out_trade_no">>, <<"jf20171204112426372744464">>}
    , {<<"tran_time">>, <<"2017-12-04 11:24:26">>}
    , {<<"verify_type">>, <<"0040">>}
    , {<<"acct_no">>, <<"6225220113392773">>}
    , {<<"name">>, <<232, 181, 181, 233, 135, 145, 231, 142, 178>>}
    , {<<"cert_no">>, <<"220625198910180328">>}
    , {<<"phone">>, <<"13721422283">>}
    , {<<"mcht_index_key">>, {<<"000003">>, <<"20171202">>, <<"1512201789372">>}}
    , {<<"sign">>, <<"DZOVa0/AcYTqv6qFn2cg8xwJwrcG3b6gFW3bASmsuvtwthxmxKdBnrP59UZRfjYSwC97zVnyHUhNXHiiO8YTKoGGO26zf9FU/MeLxif1/ygdv+pQZGYlSG9OceNQ5iuRHjKELacbfytWiw8962NthNdegePNEBTsdtWFZa9T4HY=">>}
  ];

qs(xm_up_bankcard) ->
  [
    {<<"merchId">>, <<"000003">>}
    , {<<"tranDate">>, <<"20171202">>}
    , {<<"tranId">>, <<"1512201789372">>}
    , {<<"tranTime">>, <<"160309">>}
    , {<<"bankCardNo">>, <<"6225220113392773">>}
    , {<<"certifName">>, <<"赵金玲"/utf8>>}
    , {<<"certifId">>, <<"220625198910180328">>}
    , {<<"phoneNo">>, <<"13721422283">>}
    , {<<"signature">>, <<"oGoeRE/5k+6+UOcw02Th36zmU7yNBhDrpL6hOhbahe7hQVzpnuZW+zSVf1sbQAHCANUL2DfUVsXkSjRKuj7pAkMmuVam/hHlG4MbhmgU5iShLvtQgaMDOetXpbIeJJN4xnXoFjhCmaSG7sM5O+XugCfVGh0SA5uIHjAAs2DS1Eg=">>}
  ].





protocol(req) ->
  Repo = pg_protocol:out_2_in(pg_xm_up_protocol_req_bankcard, qs(req)),
  pg_model:set(pg_xm_up_protocol_req_bankcard,Repo,[{mcht_index_key, {<<"000003">>, <<"20171202">>, <<"1512201789372">>}}]);
protocol(mcht_req) ->
  pg_protocol:out_2_in(?M_P_MCHT_REQ, qs(mcht_req));
protocol(mcht_req_xm_up_bankcard) ->
  pg_protocol:out_2_in(pg_mcht_protocol_req_xm_up_bankcard, qs(xm_up_bankcard)).

%%---------------------------------------------------
verify_test_1() ->
  XmUPReqModel = protocol(req),
  M = pg_xm_up_protocol_req_bankcard,
  PublicKey = xm_up_config:get_mer_prop(<<"B8999001">>,publicKey),
  SignStr =  pg_xm_up_protocol:sign_string(M,XmUPReqModel),
  ?debugFmt(" B8999001 SignStr = ~ts", [SignStr]),
  Sig = pg_model:get(M,XmUPReqModel,signature),
  ?assertEqual(true, public_key:verify(SignStr, 'sha', base64:decode(Sig), PublicKey)).

save_test_1() ->
  M = pg_xm_up_protocol_req_bankcard,
  XmUpReqModel = protocol(req),
  ?debugFmt(" XmUpReqModel = ~p", [XmUpReqModel]),
  ?assertEqual({<<"B8999001">>, <<"2017-12-04 11:24:26">>, <<"jf20171204112426372744464">>},
    pg_xm_up_protocol:get(M, XmUpReqModel, up_index_key)),

  ?assertEqual(
    {up_txn_log, {<<"000003">>, <<"20171202">>, <<"1512201789372">>},
      gw_xm_up_bankcard, <<"B8999001">>,
      <<"2017-12-04 11:24:26">>, <<"jf20171204112426372744464">>, undefined,
      undefined, undefined, undefined,
      {<<"B8999001">>, <<"2017-12-04 11:24:26">>,
        <<"jf20171204112426372744464">>},
      undefined, undefined, undefined, undefined,
      undefined, undefined, undefined, undefined,
      waiting, <<"6225220113392773">>, undefined,
      undefined, <<"220625198910180328">>,
      <<232, 181, 181, 233, 135, 145, 231, 142, 178>>,
      <<"13721422283">>,undefined, undefined, undefined}
    , pg_convert:convert(pg_xm_up_protocol_req_bankcard, XmUpReqModel, save_req)),

  ?assertEqual(ok,pg_xm_up_protocol:save(M, XmUpReqModel)),

  ok.



%%-----------------------------------------------------------------------------------
send_xm_up_bankcard_test_1() ->
  PMchtReq = protocol(mcht_req_xm_up_bankcard),
  ?debugFmt("PMchtReq = ~p", [PMchtReq]),
%% convert from mcht model to xm_up_bankcard model
  PUpReq = pg_convert:convert(pg_xm_up_protocol_req_bankcard, PMchtReq),
%%  generate sign
  {_, Sig} = pg_xm_up_protocol:sign(pg_xm_up_protocol_req_bankcard, PUpReq),
  PXmUpReqWithSig = pg_model:set(pg_xm_up_protocol_req_bankcard, PUpReq, signature, Sig),
  ?debugFmt("PXmUpReqWithSig = ~p", [PXmUpReqWithSig]),

%%  PostBody = pg_up_protocol:post_string(pg_up_protocol_req_collect, PUpReqWithSig),
  JosnStr = pg_xm_up_protocol:in_2_out(pg_xm_up_protocol_req_bankcard, PXmUpReqWithSig, json),
  ?debugFmt("JosnStr = ~ts", [JosnStr]),
  PostBody = pg_xm_up_protocol:des3_ecb_encrypt(xm_up_config:get_config(xm_des_key), JosnStr),

%%  get request url
  Url = <<(xm_up_config:get_config(xm_up_bankcard_url))/binary,
    (pg_model:get(pg_xm_up_protocol_req_bankcard, PXmUpReqWithSig, customer_code))/binary>>,

  ?debugFmt("PostString = ~ts,Url = ~p", [PostBody, Url]),

  {ok, {Status, Headers, Body}} = httpc:request(post,
    {binary_to_list(Url), [], "application/x-www-form-urlencoded", iolist_to_binary(PostBody)},
    [], [{body_format, binary}]),
  ?debugFmt("http Statue = ~p~nHeaders  = ~p~nBody=~ts~n", [Status, Headers, Body]),

  %% parse resp
  MResp = pg_xm_up_protocol_resp_bankcard,

%%  convert json  string  to response model  inside
  XmBankcarRespModel = pg_xm_up_protocol:out_2_in(MResp, jsx:decode(Body)),
  ?debugFmt("XmBankcarModel =~p", [XmBankcarRespModel]),
  ?assertEqual(ok, pg_xm_up_protocol:verify(MResp, XmBankcarRespModel)),
  timer:sleep(1000),

  ok.

resp_convert_test_1()->
  XmBankcardResoModel ={pg_xm_up_protocol_resp_bankcard,<<"1008">>,
    <<232,174,164,232,175,129,229,164,177,232,180,165,44,230,
      151,160,230,179,149,230,160,161,233,170,140,91,57,48,48,
      50,49,93>>,
    <<"l7Gt9ovv4FFmgOHnYgX5DY4/CK2THrtwkLFNwMQeHEiv4btltDA5c9u5diNrCGE24hKfkt9+PXvT6tAZxKDNaTv3Xu0wbGZOf9JaDQ8Ta1gO7PirwisrdK4WfK87mfD/oPYUjQYeBwNO5MbbHkG06QJnMM6QpkVA5vbMQf2HAl4=">>,
    <<"jf20171205140537277842347">>,<<"0">>,
    <<"Tue Dec 05 14:05:38 CST 2017">>},

  Vl =pg_convert:convert(pg_xm_up_protocol_resp_bankcard,XmBankcardResoModel),
  ?debugFmt("convert result  =~p", [Vl]),
  ?assertEqual( [
    {up_respCode,<<"1008">>}
    , {up_respMsg,<<232,174,164,232,175,129,229,164,177,232,180,165,44,230,151,160,230,179,149,230,160,161,233,170,140,91,57,48,48,50,49,93>>}
    , {txnAmt, <<"0">>}
    , {txn_status, fail}
  ],lists:reverse(Vl)),
  ok.


