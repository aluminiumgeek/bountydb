-module(bloom).

-export([new/1, new/2, is_bloom/1, is_element/2, add_element/2]).

-import(math, [log/1, pow/2]).

-import(erlang, [phash2/2]).

-record(bloom, {
    m      = 0,       % The size of the bitmap in bits.
    bitmap = <<>>,    % The bitmap.
    k      = 0,       % The number of hashes.
    n      = 0,       % The maximum number of keys.
    keys   = 0        % The current number of keys.
}).

new(N) ->
    new(N, 0.001).

new(N, E) when N > 0, is_float(E), E > 0, E =< 1 ->
    {M, K} = calc_least_bits(N, E),
    #bloom{m=M, bitmap = <<0:((M+7) div 8 * 8)>>, k=K, n=N}.

is_bloom(#bloom{}) ->
    true;

is_bloom(_) ->
    false.

is_element(Key, B) -> is_element(Key, B, calc_idxs(Key, B)).

is_element(_, _, []) -> true;

is_element(Key, B, [Idx | T]) ->
    ByteIdx = Idx div 8,
    <<_:ByteIdx/binary, Byte:8, _/binary>> = B#bloom.bitmap,
    Mask = 1 bsl (Idx rem 8),
    case 0 =/= Byte band Mask of
         true -> is_element(Key, B, T);
        false -> false
    end.

add_element(Key, #bloom{keys=Keys, n=N, bitmap=Bitmap} = B) when Keys < N ->
    Idxs = calc_idxs(Key, B),
    Bitmap0 = set_bits(Bitmap, Idxs),
    case Bitmap0 == Bitmap of
         true -> B;    % Don't increment key count for duplicates.
        false -> B#bloom{bitmap=Bitmap0, keys=Keys+1}
    end.

set_bits(Bin, []) -> Bin;

set_bits(Bin, [Idx | Idxs]) ->
    ByteIdx = Idx div 8,
    <<Pre:ByteIdx/binary, Byte:8, Post/binary>> = Bin,
    Mask = 1 bsl (Idx rem 8),
    Byte0 = Byte bor Mask,
    set_bits(<<Pre/binary, Byte0:8, Post/binary>>, Idxs).

calc_least_bits(N, E) -> calc_least_bits(N, E, 1, 0, 0).

calc_least_bits(N, E, K, MinM, BestK) ->
    M = -1 * K * N / log(1 - pow(E, 1/K)),
    {CurM, CurK} = if M < MinM -> {M, K}; true -> {MinM, BestK} end,
    case K of
          1 -> calc_least_bits(N, E, K+1, M, K);
        100 -> {trunc(CurM)+1, CurK};
          _ -> calc_least_bits(N, E, K+1, CurM, CurK)
    end.

calc_idxs(Key, #bloom{m=M, k=K}) ->
    X = phash2(Key, M),
    Y = phash2({"salt", Key}, M),
    calc_idxs(M, K - 1, X, Y, [X]).

calc_idxs(_, 0, _, _, Acc) -> Acc;

calc_idxs(M, I, X, Y, Acc) ->
    Xi = (X+Y) rem M,
    Yi = (Y+I) rem M,
    calc_idxs(M, I-1, Xi, Yi, [Xi | Acc]).
