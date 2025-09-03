%% Frame packing/unpacking for protobuf payload, adapted for SSL sockets (ssl:recv / ssl:send).
%% Header layout (big-endian):
%% <<Magic:32, Version:8, Serializer:8, Flags:8, MsgType:8, MsgId:64, PayloadLen:32, Crc32:32>>

-module(frame_pb_ssl).
-export([
    pack_protobuf_message/5,
    send_protobuf_ssl/6,
    recv_and_unpack_ssl/1
]).

-define(MAGIC, 16#E1E2E3E4).
-define(VERSION, 1).
-define(SERIALIZER_PROTOBUF, 1).
-define(DEFAULT_FLAGS, 0).
-define(MAX_PAYLOAD, 16*1024*1024). %% 16MB default max

%% Pack a protobuf payload (binary) into a framed message (binary)
pack_protobuf_message(MsgId, MsgType, Flags, ProtoBin, Serializer) when is_binary(ProtoBin) ->
    SerializerByte = case Serializer of
        undefined -> ?SERIALIZER_PROTOBUF;
        N when is_integer(N) -> N
    end,
    PayloadLen = byte_size(ProtoBin),
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

%% Send protobuf message through SSL socket
send_protobuf_ssl(Socket, MsgId, MsgType, Flags, ProtoBin, Serializer) ->
    case pack_protobuf_message(MsgId, MsgType, Flags, ProtoBin, Serializer) of
        {error, Reason} -> {error, Reason};
        FrameBin ->
            %% ssl:send returns ok | {error,Reason}
            ssl:send(Socket, FrameBin)
    end.

%% Recv and unpack one framed message from an SSL socket
recv_and_unpack_ssl(Socket) ->
    case read_exact_ssl(Socket, 24) of
        {ok, HeaderBin} ->
            <<Magic:32, Version:8, Serializer:8, Flags:8, MsgType:8, MsgId:64, PayloadLen:32, Crc:32>> = HeaderBin,
            case Magic =:= ?MAGIC of
                false ->
                    {error, magic_mismatch};
                true ->
                    if PayloadLen > ?MAX_PAYLOAD ->
                           {error, payload_too_large};
                       true ->
                           case read_exact_ssl(Socket, PayloadLen) of
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

%% Helper: read_exact_ssl(Socket, N)
read_exact_ssl(_Socket, 0) -> {ok, <<>>};
read_exact_ssl(Socket, N) when N > 0 ->
    read_exact_ssl(Socket, N, <<>>).

read_exact_ssl(_Socket, 0, Acc) -> {ok, Acc};
read_exact_ssl(Socket, N, Acc) ->
    case ssl:recv(Socket, N) of
        {ok, Data} ->
            Len = byte_size(Data),
            case Len of
                L when L == N -> {ok, <<Acc/binary, Data/binary>>};
                L when L < N  -> read_exact_ssl(Socket, N - L, <<Acc/binary, Data/binary>>);
                L when L > N  ->
                    %% ssl:recv usually returns exactly what requested; if extra appears, slice.
                    <<Part: N/binary, _Rest/binary>> = Data,
                    {ok, <<Acc/binary, Part/binary>>}
            end;
        {error, Reason} -> {error, Reason}
    end.