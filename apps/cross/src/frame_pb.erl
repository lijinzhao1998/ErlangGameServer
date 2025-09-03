%% Frame packing/unpacking for protobuf payload, following the 24-byte header design:
%% Header layout (big-endian):
%% <<Magic:32, Version:8, Serializer:8, Flags:8, MsgType:8, MsgId:64, PayloadLen:32, Crc32:32>>
%%
%% This module demonstrates:
%% - pack_protobuf_message/5  : 构造 header + payload（protobuf binary）
%% - send_protobuf/6          : 直接通过 gen_tcp:send 发送
%% - recv_and_unpack/1        : 从 socket 读取并解析一条 framed protobuf 消息
%%
%% 注意：本示例把 serializer 常量设为 1 表示 protobuf（参见协议定义）。
%% Replace actual read_exact implementation with your project's socket reader.

-module(frame_pb).
-export([pack_protobuf_message/5, send_protobuf/6, recv_and_unpack/1]).

-include_lib("kernel/include/logger.hrl").

-define(MAGIC, 16#E1E2E3E4).
-define(VERSION, 1).
-define(SERIALIZER_PROTOBUF, 1).
-define(DEFAULT_FLAGS, 0).
-define(MAX_PAYLOAD, 16*1024*1024). %% 16MB default max

%% Pack a protobuf payload (binary) into a framed message (binary)
%% MsgType: application-level type (0=request,1=response,...)
%% MsgId: uint64 correlation id
%% Flags: flags byte (compression etc.)
pack_protobuf_message(MsgId, MsgType, Flags, ProtoBin, Serializer) when is_binary(ProtoBin) ->
    SerializerByte = case Serializer of
        undefined -> ?SERIALIZER_PROTOBUF;
        N when is_integer(N) -> N
    end,
    PayloadLen = byte_size(ProtoBin),
    %% Checks
    if PayloadLen > ?MAX_PAYLOAD ->
            {error, payload_too_large};
       true ->
            Crc = erlang:crc32(ProtoBin),
            Header = <<
                ?MAGIC:32,
                ?VERSION:8,
                SerializerByte:8,
                Flags:8,
                MsgType:8,
                MsgId:64,
                PayloadLen:32,
                Crc:32
            >>,
            <<Header/binary, ProtoBin/binary>>
    end.

%% Send protobuf message through GenTCP socket (active once/active false accepted)
%% Socket: socket
%% MsgId, MsgType, Flags, ProtoBin: see above
%% Serializer: optional serializer id (default protobuf)
send_protobuf(Socket, MsgId, MsgType, Flags, ProtoBin, Serializer) ->
    case pack_protobuf_message(MsgId, MsgType, Flags, ProtoBin, Serializer) of
        {error, Reason} -> {error, Reason};
        FrameBin ->
            gen_tcp:send(Socket, FrameBin)
    end.

%% recv_and_unpack(Socket) -> {ok, #{header := HeaderMap, payload := PayloadBin}} | {error, Reason}
%% This function demonstrates blocking reads; integrate with your socket loop/read_exact.
recv_and_unpack(Socket) ->
    case read_exact(Socket, 24) of
        {ok, HeaderBin} ->
            <<Magic:32, Version:8, Serializer:8, Flags:8, MsgType:8, MsgId:64, PayloadLen:32, Crc:32>> = HeaderBin,
            case Magic =:= ?MAGIC of
                false ->
                    {error, magic_mismatch};
                true ->
                    if PayloadLen > ?MAX_PAYLOAD ->
                           {error, payload_too_large};
                       true ->
                           case read_exact(Socket, PayloadLen) of
                               {ok, PayloadBin} ->
                                   CalcCrc = erlang:crc32(PayloadBin),
                                   case CalcCrc =:= Crc of
                                       false ->
                                           {error, checksum_mismatch};
                                       true ->
                                           HeaderMap = #{
                                               version => Version,
                                               serializer => Serializer,
                                               flags => Flags,
                                               msg_type => MsgType,
                                               msg_id => MsgId,
                                               payload_len => PayloadLen
                                           },
                                           {ok, #{header => HeaderMap, payload => PayloadBin}}
                                   end;
                               {error, Reason} -> {error, Reason}
                           end
                    end
            end;
        {error, Reason} -> {error, Reason}
    end.

%% read_exact(Socket, N) : read exactly N bytes from socket (blocking)
%% This is a helper illustration. Replace with your project's robust implementation.
read_exact(Socket, N) when N >= 0 ->
    read_exact(Socket, N, <<>>).

read_exact(_Socket, 0, Acc) ->
    {ok, Acc};
read_exact(Socket, N, Acc) ->
    case gen_tcp:recv(Socket, N) of
        {ok, Data} ->
            Len = byte_size(Data),
            case Len of
                L when L == N -> {ok, <<Acc/binary, Data/binary>>};
                L when L < N  -> read_exact(Socket, N - L, <<Acc/binary, Data/binary>>);
                L when L > N  ->
                    %% gen_tcp:recv typically returns up to N bytes; in case of extra, slice.
                    <<Part: N/binary, Rest/binary>> = Data,
                    %% Note: Rest needs to be pushed back to a buffer or handled by active mode.
                    {ok, <<Acc/binary, Part/binary>>}
            end;
        {error, closed} -> {error, closed};
        {error, Reason} -> {error, Reason}
    end.