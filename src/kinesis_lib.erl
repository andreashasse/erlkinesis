-module(kinesis_lib).

-ignore_xref([delete_stream/2]).

-export([create_stream/3,
         delete_stream/2,
         describe_stream/3,
         get_records/3,
         get_shard_iterator/4,
         list_streams/2,
         put_record/4
         ]).

-include("../include/kinesis.hrl").

create_stream(Config, StreamName, ShardCount) ->
    Payload = {[
                {<<"ShardCount">>, ShardCount},
                {<<"StreamName">>, StreamName}
               ]},
    Operation = "Kinesis_20131202.CreateStream",
    case do_op(Config, Payload, Operation) of
        {ok, _Hrds, _Body} ->
            ok;
        Res ->
            Res
    end.

delete_stream(Config, StreamName) ->
    Payload = {[{<<"StreamName">>, StreamName}]},
    Operation = "Kinesis_20131202.DeleteStream",
    case do_op(Config, Payload, Operation) of
        {ok, _Hrds, _Body} ->
            ok;
        Res ->
            Res
    end.

describe_stream(Config, Limit, StreamName) ->
    Payload = {[
                %%{<<"ExclusiveStartShardId">>, <<"string">>},
                {<<"Limit">>, Limit}, %% number
                {<<"StreamName">>, StreamName}
               ]},
    Operation = "Kinesis_20131202.DescribeStream",
    case do_op(Config, Payload, Operation) of
        {ok, _Hrds, Body} ->
            {ok, jiffy:decode(Body)};
        Res ->
            Res
    end.

get_records(Config, ShardIterator, Limit) ->
    Payload = {[{<<"Limit">>, Limit},
                {<<"ShardIterator">>, ShardIterator}
               ]},
    Operation = "Kinesis_20131202.GetRecords",
    case do_op(Config, Payload, Operation) of
        {ok, _Hrds, Body} ->
            {ok, jiffy:decode(Body)};
        Res ->
            Res
    end.

get_shard_iterator(Config, ShardId, ShardIteratorType, StreamName) ->
    Payload = {[
                {<<"ShardId">>, ShardId},
                {<<"ShardIteratorType">>, ShardIteratorType},
                %% {<<"StartingSequenceNumber">>, StartingSequenceNumber},
                {<<"StreamName">>, StreamName}
               ]},
    Operation = "Kinesis_20131202.GetShardIterator",
    case do_op(Config, Payload, Operation) of
        {ok, _Hdrs, Body} ->
            {ok, jiffy:decode(Body)};
        Res ->
            Res
    end.

list_streams(Config, Limit) ->
    Payload = {[
                %%{<<"ExclusiveStartStreamName">>, <<"string">>},
                {<<"Limit">>, Limit}
               ]},
    Operation = "Kinesis_20131202.ListStreams",
    case do_op(Config, Payload, Operation) of
        {ok, _Hdrs, Body} ->
            {ok, jiffy:decode(Body)};
        Err ->
            Err
    end.

put_record(Config, StreamName, PartitionKey, Data) ->
    Payload = {[{<<"Data">>, Data},
                %% {<<"ExplicitHashKey">>, <<"string">>},
                {<<"PartitionKey">>, PartitionKey},
                %% {<<"SequenceNumberForOrdering">>, <<"string">>},
                {<<"StreamName">>, StreamName}]},
    Operation = "Kinesis_20131202.PutRecord",
    case do_op(Config, Payload, Operation) of
        {ok, _Hdrs, _Body} ->
            ok;
        Err ->
            Err
    end.

do_op(Config, Payload, Operation) ->
    Body = jiffy:encode(Payload),
    Url = "https://kinesis.us-east-1.amazonaws.com",
    Host = "kinesis.us-east-1.amazonaws.com",
    Date = httpd_util:rfc1123_date(),
    Headers = [{"Date", Date},
               {"Content-Type", "application/x-amz-json-1.1"},
               {"Connection", "keep-alive"},
               {"x-amz-target", Operation},
               {"Host", Host}],
    NewHeaders = aws_auth_v4:sign_v4(Config, Headers, Body,
                                     "us-east-1", "kinesis"),
    Options = [{max_connections, 100}],
    Timeout = 5000,
    Method = post,
    {ok, {{Code, _}, ResHeaders, ResBody}} =
        lhttpc:request(Url, Method, NewHeaders, Body, Timeout, Options),
    case Code of
        200 ->
            {ok, ResHeaders, ResBody};
        _ ->
            {error, Code, ResHeaders, ResBody}
    end.
