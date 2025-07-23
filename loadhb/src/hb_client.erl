%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Client Module
%%% 
%%% This module provides functionality to build, sign, and send messages
%%% to a HyperBEAM server using the included HyperBEAM modules.
%%% 
%%% Example usage:
%%%   Wallet = ar_wallet:new(),
%%%   Msg = #{
%%%       <<"path">> => <<"/process~process@1.0/compute/at-slot">>,
%%%       <<"method">> => <<"POST">>,
%%%       <<"target">> => <<"Kv6jQCcs8GwNpioj6tkTt06zD130YgqIHX7QNnZQYQc">>,
%%%       <<"accept-bundle">> => <<"true">>,
%%%       <<"accept-codec">> => <<"httpsig@1.0">>,
%%%       <<"signingFormat">> => <<"ANS-104">>
%%%   },
%%%   {ok, Response} = hb_client:send_message("https://test.hyperbeam", Msg, Wallet).
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(hb_client).
-export([
    send_message/3,
    send_message/4,
    build_message/2,
    sign_message/2,
    send_signed_message/2,
    send_signed_message/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Send a message to a HyperBEAM server
%% @param ServerURL The base URL of the HyperBEAM server
%% @param Message The message map to send
%% @param Wallet The wallet to use for signing
-spec send_message(binary(), map(), ar_wallet:wallet()) -> {ok, term()} | {error, term()}.
send_message(ServerURL, Message, Wallet) ->
    send_message(ServerURL, Message, Wallet, #{}).

%% @doc Send a message to a HyperBEAM server with options
%% @param ServerURL The base URL of the HyperBEAM server
%% @param Message The message map to send  
%% @param Wallet The wallet to use for signing
%% @param Opts Options map
-spec send_message(binary(), map(), ar_wallet:wallet(), map()) -> 
    {ok, term()} | {error, term()}.
send_message(ServerURL, Message, Wallet, Opts) ->
    try
        Result = build_and_sign_message(Message, Wallet, Opts),
        case Result of
            {ok, SignedMessage} ->
                send_signed_message(ServerURL, SignedMessage, Opts);
            {error, BuildError} ->
                {error, BuildError}
        end
    catch
        Error:Reason:Stacktrace ->
            {error, {Error, Reason}}
    end.

%% @doc Build a message using HyperBEAM message building functions
%% @param Message The message map to build
%% @param Opts Options map
-spec build_message(map(), map()) -> {ok, map()} | {error, term()}.
build_message(Message, Opts) ->
    try
        RequiredFields = [<<"path">>, <<"method">>],
        FieldCheck = check_required_fields(Message, RequiredFields),
        case FieldCheck of
            ok ->
                Codec = <<"httpsig@1.0">>,
                BuiltMessage = hb_message:convert(Message, Codec, Opts),
                {ok, BuiltMessage};
            {error, BuildReason} ->
                {error, BuildReason}
        end
    catch
        Error:CatchReason:Stacktrace ->
            {error, {Error, CatchReason}}
    end.

%% @doc Sign a message using HyperBEAM signing functions
%% @param Message The message to sign
%% @param Wallet The wallet to use for signing
-spec sign_message(map(), ar_wallet:wallet()) -> {ok, map()} | {error, term()}.
sign_message(Message, Wallet) ->
    try
        Opts = #{priv_wallet => Wallet},
        SignedMessage = hb_message:commit(Message, Opts),
        {ok, SignedMessage}
    catch
        Error:Reason:Stacktrace ->
            {error, {Error, Reason}}
    end.

%% @doc Send a pre-signed message to a HyperBEAM server
%% @param ServerURL The base URL of the HyperBEAM server
%% @param SignedMessage The signed message to send
-spec send_signed_message(binary(), map()) -> {ok, term()} | {error, term()}.
send_signed_message(ServerURL, SignedMessage) ->
    send_signed_message(ServerURL, SignedMessage, #{}).

%% @doc Send a pre-signed message to a HyperBEAM server with options
%% @param ServerURL The base URL of the HyperBEAM server
%% @param SignedMessage The signed message to send
%% @param Opts Options map
-spec send_signed_message(binary(), map(), map()) -> 
    {ok, term()} | {error, term()}.
send_signed_message(ServerURL, SignedMessage, Opts) ->
    try
        UrlResult = parse_server_url(ServerURL),
        case UrlResult of
            {ok, {Host, Port, Path}} ->
                send_to_parsed_url(SignedMessage, Host, Port, Path, Opts);
            {error, ParseError} ->
                {error, ParseError}
        end
    catch
        Error:CatchReason:Stacktrace ->
            {error, {Error, CatchReason}}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Send message to parsed URL components
send_to_parsed_url(SignedMessage, Host, Port, Path, Opts) ->
    try
        DefaultPath = <<"/">>,
        RequestPath = hb_ao:get(<<"path">>, SignedMessage, DefaultPath, Opts),
        % Fix double-slash issue: if both Path and RequestPath start with "/", avoid doubling
        FullPath = case {Path, RequestPath} of
            {<<"/">>, RequestPath} when byte_size(RequestPath) > 0 ->
                RequestPath;  % Use RequestPath directly if Path is just "/"
            _ ->
                <<Path/binary, RequestPath/binary>>
        end,
        
        Method = hb_ao:get(<<"method">>, SignedMessage, <<"POST">>, Opts),
        
        ConvertResult = hb_message:convert(SignedMessage, <<"httpsig@1.0">>, 
                                          Opts),
        case ConvertResult of
            {ok, HTTPMessage} ->
                send_http_request(HTTPMessage, Host, Port, FullPath, Method, 
                                 Opts);
            {error, ConvertError} ->
                {error, ConvertError};
            HTTPMessage when is_map(HTTPMessage) ->
                send_http_request(HTTPMessage, Host, Port, FullPath, Method, 
                                 Opts);
            Other ->
                {error, {unexpected_convert_result, Other}}
        end
    catch
        Error:Reason:Stacktrace ->
            {error, {Error, Reason}}
    end.

%% @doc Send HTTP request with message data
send_http_request(HTTPMessage, Host, Port, FullPath, Method, Opts) ->
    try
        Headers = build_headers(HTTPMessage, Opts),
        
        Body = hb_ao:get(<<"body">>, HTTPMessage, <<>>, Opts),
        
        % Format peer as URI string instead of tuple for hb_http_client
        PeerURI = iolist_to_binary([<<"http://">>, Host, <<":">>, integer_to_binary(Port)]),
        RequestArgs = #{
            peer => PeerURI,
            path => FullPath,
            method => Method,
            headers => Headers,
            body => Body
        },
        
        % Use httpc instead of gun to avoid URI parsing issues
        HttpcOpts = maps:put(http_client, httpc, Opts),
        HttpResult = hb_http_client:req(RequestArgs, HttpcOpts),
        case HttpResult of
            {ok, StatusCode, RespHeaders, RespBody} ->
                {ok, #{
                    status => StatusCode,
                    headers => RespHeaders,
                    body => RespBody
                }};
            {error, SendError} ->
                {error, SendError}
        end
    catch
        Error:Reason:Stacktrace ->
            {error, {Error, Reason}}
    end.

%% @doc Build and sign a message
build_and_sign_message(Message, Wallet, Opts) ->
    try
        case build_message(Message, Opts) of
            {ok, BuiltMessage} ->
                sign_message(BuiltMessage, Wallet);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Error:CatchReason:Stacktrace ->
            {error, {Error, CatchReason}}
    end.

%% @doc Check if required fields are present in a message
check_required_fields(Message, RequiredFields) ->
    MissingFields = [Field || Field <- RequiredFields, 
                              not maps:is_key(Field, Message)],
    case MissingFields of
        [] -> ok;
        _ -> {error, {missing_fields, MissingFields}}
    end.

%% @doc Parse a server URL into host, port, and path components
parse_server_url(ServerURL) when is_binary(ServerURL) ->
    try
        URLString = binary_to_list(ServerURL),
        SchemeResult = string:split(URLString, "://", leading),
        case SchemeResult of
            [Scheme, Rest] ->
                parse_url_rest(Rest, Scheme);
            _ ->
                {error, invalid_url}
        end
    catch
        _:_ ->
            {error, invalid_url}
    end.

%% @doc Parse the rest of URL after scheme
parse_url_rest(Rest, Scheme) ->
    case string:split(Rest, "/", leading) of
        [HostPort] ->
            {Host, Port} = parse_host_port(HostPort, Scheme),
            {ok, {Host, Port, <<"/">>}};
        [HostPort, PathPart] ->
            {Host, Port} = parse_host_port(HostPort, Scheme),
            Path = <<"/", PathPart/binary>>,
            {ok, {Host, Port, Path}}
    end.

%% @doc Parse host and port from a host:port string
parse_host_port(HostPort, Scheme) ->
    case string:split(HostPort, ":", trailing) of
        [Host, PortStr] ->
            Port = list_to_integer(PortStr),
            {list_to_binary(Host), Port};
        [Host] ->
            DefaultPort = case Scheme of
                "https" -> 443;
                "http" -> 80;
                _ -> 80
            end,
            {list_to_binary(Host), DefaultPort}
    end.

%% @doc Build HTTP headers from a message
build_headers(HTTPMessage, Opts) ->
    BaseHeaders = #{
        <<"content-type">> => <<"application/json">>,
        <<"accept">> => <<"application/json">>
    },
    
    % Get nested headers from the message
    MessageHeaders = hb_ao:get(<<"headers">>, HTTPMessage, #{}, Opts),
    
    % Add HTTP signature headers directly from the message
    SignatureHeaders = #{
        <<"signature">> => maps:get(<<"signature">>, HTTPMessage, undefined),
        <<"signature-input">> => maps:get(<<"signature-input">>, HTTPMessage, undefined)
    },
    
    % Filter out undefined values
    FilteredSigHeaders = maps:filter(fun(_, V) -> V =/= undefined end, SignatureHeaders),
    
    % Merge all headers together
    AllHeaders = hb_maps:merge(BaseHeaders, MessageHeaders, Opts),
    hb_maps:merge(AllHeaders, FilteredSigHeaders, Opts).
